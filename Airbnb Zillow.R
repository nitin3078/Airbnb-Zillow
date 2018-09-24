library(tidyverse)
library(grid)
library(gridExtra)


setwd("C:\\Users\\nitin\\Desktop")

Airbnb<-read.csv('listings.csv',stringsAsFactors=FALSE)




# keeping only more useful variables for visualization and prediction for this particular goal

Airbnb <- Airbnb[,c(44,53,55,56,57,61,80)]
str(Airbnb)




# Modify column types
Airbnb$price<-parse_number(Airbnb$price)
Airbnb$zipcode<-parse_number(Airbnb$zipcode)


# There are many wrong zipcodes. Removing anything before 10001 and after 11694. Also removing records where zipcode
#is missing. There are 611 records with zipcode missing. zipcodes can be found by location but since it is small fraction and even smaller for 2 bedroom apartments,
# We will just remove it. NA will also be removed

Airbnb<-Airbnb %>% filter(Airbnb$zipcode>=10001 & Airbnb$zipcode<=11694)

# Converting zipcode back to factor
Airbnb$zipcode<-as.factor(Airbnb$zipcode)

# keeping only 2 bedroom apartments. Also now data contains only Entire home/apt and Private room
Airbnb<-Airbnb %>% filter(Airbnb$bedrooms==2)



# generating a new column 1 year revenue by multiplying per night rate with 365 and 0.75 occupancy rate

Airbnb<-Airbnb %>% mutate(YearlyRev=price*365*.75)




Zillow <- read.csv('Zip_Zhvi_2bedroom.csv',stringsAsFactors=FALSE)
str(Zillow)

# Only keeping 2017-05 prices 
Zillow<-Zillow[,c(2,3,4,7,261)]

# converting RegionName to number
Zillow$RegionName<-as.numeric(Zillow$RegionName)

# renaming price column 

colnames(Zillow)[5]<-"Cost"


# Only keeping Newyork city zipcodes (Zipcodes between 10001 and 11694 only)
Zillow<-Zillow %>% filter(Zillow$RegionName>=10001 & Zillow$RegionName<=11694)

# Converting RegionName(zipcode) back to factor
Zillow$RegionName<-as.factor(Zillow$RegionName)



# Merging two datasets (Inner join)

airzil<-inner_join(Airbnb, Zillow, by = c("zipcode" = "RegionName"))

# dropping datasets Airbnb and Zillow as they will not be used further
rm(Airbnb,Zillow)

# converting zipcode back to factor
airzil$zipcode<-as.factor(airzil$zipcode)



# calculating average YearlyRev by zipcode
airzil<-airzil %>% 
    group_by(zipcode) %>% 
    mutate(YearlyRev_zip = mean(YearlyRev))
# Creating column "return_per_dollar_zip for return per  dollar by zipcode
airzil<-airzil %>% 
  mutate(return_per_dollar_zip = (YearlyRev_zip/Cost))




# calculating average YearlyRev by room_type and by zipcode

airzil<-airzil %>% 
  group_by(room_type,zipcode) %>% 
  mutate(YearlyRev_zip_rm = mean(YearlyRev))
# Creating column "return_per_dollar_rm for return per  dollar by rooom_type and by zipcode
airzil<-airzil %>% 
  mutate(return_per_dollar_rm = (YearlyRev_zip_rm/Cost))



# calculating average YearlyRev by no. of bathrooms and by zipcode
airzil<-airzil %>% 
  group_by(as.factor(bathrooms),zipcode) %>% 
  mutate(YearlyRev_zip_bath = mean(YearlyRev))

# Creating column "return_per_dollar_bath for return per  dollar by bathroom and by zipcode
airzil<-airzil %>% 
  mutate(return_per_dollar_bath = (YearlyRev_zip_bath/Cost))


# calculating average YearlyRev by no. of beds and by zipcode
airzil<-airzil %>% 
  group_by(as.factor(beds),zipcode) %>% 
  mutate(YearlyRev_zip_beds = mean(YearlyRev))
# Creating column "return_per_dollar_beds for return per  dollar by beds and by zipcode
airzil<-airzil %>% 
  mutate(return_per_dollar_beds = (YearlyRev_zip_beds/Cost))



# calculating average YearlyRev by no. of review_scores_rating and by zipcode
airzil<-airzil %>% 
  group_by(as.factor(review_scores_rating),zipcode) %>% 
  mutate(YearlyRev_zip_rating = mean(YearlyRev))
# Creating column "return_per_dollar_rating for return per  dollar by review_scores_rating and by zipcode
airzil<-airzil %>% 
  mutate(return_per_dollar_rating = (YearlyRev_zip_rating/Cost))

# dropping zipcodes from datasets where no. of observations are below 3
airzil<-airzil %>% group_by(zipcode) %>% filter(n()>3)
airzil$zipcode <- droplevels(airzil$zipcode)


# Visualization- plot of revenue and zipcode

# adding color-blind friendly palette
cbPalette <- c( "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

unique(airzil %>% select(zipcode,return_per_dollar_zip)) %>%
  ggplot(aes(x = reorder(zipcode, -return_per_dollar_zip), y = return_per_dollar_zip)) + geom_bar(fill="#E69F00",stat = "identity")+
  geom_text(aes(label=round(return_per_dollar_zip,3)), position=position_dodge(width=0.9), vjust=-0.25)+
  theme(axis.text.x = element_text(angle = 70, hjust = 1))+labs(x = "Zipcode", y="Return per Dollar")


# creating rank table by zipcode
x<-airzil
x$ranking <- NA
x<-unique(x %>% select(zipcode,return_per_dollar_zip,ranking))
x$ranking[order(-x$return_per_dollar_zip)] <- 1:nrow(x)
x<-x[order(x$ranking),]
grid.table(x)

# Create column for total years to break even point
airzil<-airzil %>% 
  mutate(break_even_years = (1/return_per_dollar_zip))

# visualizing break even  graph

ggplot(airzil, aes(x=zipcode, y=break_even_years, group=1))+ geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+labs(x = "Zipcode", y="Breakeven Point (in years)")+
  geom_smooth(method="loess", se=F)


# visualizing relationship of room_type and zipcode with revenue

unique(airzil %>% select(zipcode,room_type,return_per_dollar_rm)) %>%
  ggplot(aes(x = zipcode, y = return_per_dollar_rm)) + geom_bar(aes(fill = room_type),position = "dodge",stat = "identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+labs(x = "Zipcode", y="Return per Dollar")+
  scale_fill_manual(values=cbPalette)
  


# visualizing relationship of bathroom and zipcode with revenue

na.omit(unique(airzil %>% select(zipcode,bathrooms,return_per_dollar_bath))) %>%
  ggplot(aes(x = zipcode, y = return_per_dollar_bath)) + geom_bar(aes(fill = as.factor(bathrooms)),position = "dodge",stat = "identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+labs(x = "Zipcode", y="Return per Dollar")+
  scale_fill_discrete(name = "No. of bathrooms")


# visualizing relationship of beds and zipcode with revenue

unique(airzil %>% select(zipcode,beds,return_per_dollar_beds)) %>%
  ggplot(aes(x = zipcode, y = return_per_dollar_beds)) + geom_bar(aes(fill = as.factor(beds)),position = "dodge",stat = "identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+labs(x = "Zipcode", y="Return per Dollar")+
  scale_fill_discrete(name = "No. of beds")



# visualizing relationship of review_scores_rating and zipcode with revenue

na.omit(unique(airzil %>% select(zipcode,review_scores_rating,return_per_dollar_rating))) %>%
  ggplot(aes(x = zipcode, y = return_per_dollar_rating)) + geom_bar(aes(fill = as.factor(review_scores_rating)),position = "dodge",stat = "identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+labs(x = "Zipcode", y="Return per Dollar")+
  scale_fill_discrete(name = "Review score rating")


#####################################################################################
# density plots

ggplot(airzil, aes(bathrooms, fill = zipcode, colour = zipcode)) +
  geom_density(alpha = 0.1) +facet_wrap( ~ zipcode)

ggplot(airzil, aes(beds, fill = zipcode, colour = zipcode)) +
  geom_density(alpha = 0.1) +facet_wrap( ~ zipcode)

ggplot(airzil, aes(room_type, fill = zipcode, colour = zipcode)) +
  geom_density(alpha = 0.1) +facet_wrap( ~ zipcode)

ggplot(airzil, aes(price,fill = zipcode, colour = zipcode)) +  geom_density(alpha = 0.1)+
  scale_x_continuous()

ggplot(airzil, aes(review_scores_rating, colour = zipcode)) +
  geom_density(alpha = 0.1) +facet_wrap( ~ zipcode)

