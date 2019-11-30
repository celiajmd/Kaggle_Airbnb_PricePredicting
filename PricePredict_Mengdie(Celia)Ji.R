# For the following code to work, ensure analysisData.csv and scoringData.csv are in your working directory.
setwd("/Users/celiaji/Desktop/2019 Fall CU class/5200/Kaggle")
getwd()

####################################################################1.READ DATA
# 1. Read data and pick all the missing values.
data = read.csv("analysisData.csv",na.strings = c("NA","N/A",""))


# 2. Read scoring data and apply model to generate predictions.
scoringData = read.csv('scoringData.csv',na.strings = c("NA","N/A",""))


# 3. Build RMSE function.
rmse = function(dataset,predicted){
  sqrt(mean((dataset - predicted)^2))
}
###################################################################2. EXPLORE DATA
# 1. delete rows where price is 0. There are 15 rows = 0.
data = data[data$price>0,]

# 2.explore first row to have a general look at the data.
#exploredata = data.frame(data[1,])
#write.csv(exploredata, 'exploredata.csv',row.names = T)

# 3. Examine the businessmeaningful variables about levels, correlation with price, correlation with each other, whether the value is highly skewed.
cor(data1$summary,data1$price) #0.03
cor(data1$description,data1$price) #0.05
str(data$neighborhood_overview) #21899 levels, too many levels!
cor(data1$price,data1$neighborhood_overview) #0.04
cor(data1$price,data1$transit) #0.0016
cor(data1$price,data1$access) #-0.028
str(data1$transit)#22948 levels, too many levels!
str(data1$access)#20054 levels, too many levels!
cor(data1$host_since,data1$price) #0.19
boxplot(price~host_since,data1)
str(data1$host_location)#1212 levels, too many levels!
str(data1$host_is_superhost)
barplot(table(data1$host_is_superhost))
plot(price~host_location,data1)
str(data$host_neighbourhood)#374 levels, too many levels!
boxplot(price~host_neighbourhood, data)
plot(price~host_neighbourhood,data)
cor(data1$host_listings_count,data1$price)#0.03
cor(data1$host_total_listings_count,data1$price)#0.03
boxplot(price~host_has_profile_pic,data1)
boxplot(price~host_identity_verified,data1)
str(data1$street)#281 levels, too many levels!
str(data1$neighbourhood)#203 levels, too many levels!
str(data1$neighbourhood_cleansed)#219 levels, too many levels!
str(data1$neighbourhood_group_cleansed)
str(data1$city)#273 levels, too many levels!
str(data1$state)# too many new york. almost all!
str(data$zipcode)#191 levels, too many levels!
summary(data$zipcode)
boxplot(price~zipcode,data)
str(data$market)
str(scoringData1$market)
boxplot(price~market,data)
summary(data1$market)
summary(scoringData1$market)
summary(data$smart_location)#283 levels, too many levels!
sum(is.na(data1$is_location_exact))
summary(data$square_feet)
mode(data$square_feet)
boxplot(data$square_feet)
plot(price~square_feet,data)
sum(is.na(scoringData1$is_location_exact))
str(data1$property_type)#31 levels
str(data1$room_type)#
sum(is.na(data1$room_type))
sum(is.na(scoringData1$room_type))
str(data1$accommodates)
cor(data1$accommodates,data1$price) #0.58, choose it!
sum(is.na(data1$accommodates))
sum(is.na(scoringData1$accommodates))
str(data1$bathrooms)
cor(data1$bathrooms,data1$price) #0.29, choose it!
sum(is.na(data1$bathrooms))
sum(is.na(scoringData1$bathrooms))
str(data1$bedrooms)
cor(data1$bedrooms,data1$price) #0.46, choose it!
sum(is.na(data1$bedrooms))
sum(is.na(scoringData1$bedrooms))
str(data1$beds)
cor(data1$beds,data1$price) #0.46, choose it!
sum(is.na(data1$beds))
sum(is.na(scoringData1$beds))
summary(data1$bed_type)
summary(scoringData1$bed_type)
sum(is.na(data1$beds))
sum(is.na(scoringData1$beds))
str(data1$amenities)
cor(data1$amenities,data1$price) #0.11
str(data1$cleaning_fee)
cor(data1$cleaning_fee,data1$price) #0.54,choose it!
str(data1$guests_included)
cor(data1$guests_included,data1$price)#0.38,choose it!
sum(is.na(data1$guests_included))
sum(is.na(scoringData1$guests_included))
str(data1$extra_people)
cor(data1$extra_people,data1$price)#0.114
sum(is.na(data1$guests_included))
sum(is.na(scoringData1$guests_included))
str(data1$minimum_nights)
cor(data1$minimum_nights,data1$price)#0.008,
sum(is.na(data1$minimum_nights))
sum(is.na(scoringData1$minimum_nights))
str(data1$maximum_nights)
cor(data1$maximum_nights,data1$price)#-0.0003
sum(is.na(data1$minimum_nights))
sum(is.na(scoringData1$minimum_nights))
summary(data1$calendar_updated)#74 levels, too many levels!
str(data1$availability_30)
cor(data1$availability_30,data1$price)#0.03
sum(is.na(data1$availability_30))
sum(is.na(scoringData1$availability_30))
cor(data1$availability_60,data1$price)#0.03
sum(is.na(data1$availability_60))
sum(is.na(scoringData1$availability_60))
cor(data1$availability_90,data1$price)#0.03
sum(is.na(data1$availability_90))
sum(is.na(scoringData1$availability_90))
cor(data1$availability_365,data1$price)#0.08
sum(is.na(data1$availability_365))
sum(is.na(scoringData1$availability_365))
cor(data1$availability_30,data1$availability_60)#0.94 #high correlation
cor(data1$availability_90,data1$availability_60)#0.97 #high correlation
cor(data1$availability_30,data1$availability_90)# 0.8903685 #high correlation
cor(data1$availability_90,data1$availability_365)#0.71 
str(data1$number_of_reviews)
cor(data1$number_of_reviews,data1$price)#-0.016
sum(is.na(data1$number_of_reviews))
sum(is.na(scoringData1$number_of_reviews))
str(data1$number_of_reviews_ltm)
cor(data1$number_of_reviews_ltm,data1$price)#-0.013
sum(is.na(data1$number_of_reviews_ltm))
sum(is.na(scoringData1$number_of_reviews_ltm))
cor(data1$number_of_reviews,data1$number_of_reviews_ltm) #0.76
cor(data1$first_review,data1$price)#0.017
sum(is.na(data1$first_review))
sum(is.na(scoringData1$first_review))
cor(data1$last_review,data1$price)#-0.014
sum(is.na(data1$last_review))
sum(is.na(scoringData1$last_review))
cor(data1$review_scores_rating,data1$price)#-0.06
cor(data1$review_scores_accuracy,data1$price)#0.02
cor(data1$review_scores_cleanliness,data1$price)#0.08
cor(data1$review_scores_checkin,data1$price)#0.0012
cor(data1$review_scores_communication,data1$price)#0.011
cor(data1$review_scores_location,data1$price)#0.14
cor(data1$review_scores_value,data1$price)#-0.017
str(data1$instant_bookable)
boxplot(price~instant_bookable,data1)
sum(is.na(data1$instant_bookable))
sum(is.na(scoringData1$instant_bookable))
str(data1$cancellation_policy)
summary(data1$cancellation_policy)
summary(scoringData1$cancellation_policy)
cor(data1$calculated_host_listings_count,data1$price)#0.02
cor(data1$calculated_host_listings_count_entire_homes,data1$price)#0.08
cor(data1$calculated_host_listings_count_private_rooms,data1$price)#-0.14
cor(data1$calculated_host_listings_count_shared_rooms,data1$price)#-0.08
cor(data1$reviews_per_month,data1$price)#-0.019
summary(data1$calculated_host_listings_count_shared_rooms) #value is too little.
cor(data1$host_total_listings_count,data1$price)#0.03
summary(data1$host_total_listings_count)
boxplot(data1$host_total_listings_count)


# 3. DELETE columns from both datasets for reasons: too many NAs / too many levels / just one level / seem don't effect price / high correlated variables
delcol <- c('space','notes','interaction','host_name','host_about','host_response_time','host_response_rate',
            'host_acceptance_rate','host_verifications','square_feet','weekly_price','monthly_price','license',
            'jurisdiction_names','id','name','security_deposit','country_code','country','minimum_minimum_nights',
            'minimum_maximum_nights','maximum_minimum_nights','maximum_maximum_nights','minimum_nights_avg_ntm',
            'maximum_nights_avg_ntm','has_availability','requires_license','license','is_business_travel_ready',
            'require_guest_phone_verification','require_guest_profile_picture',  'host_location','host_neighbourhood','street','neighbourhood','neighbourhood_cleansed',
            'city','state','smart_location','calendar_updated','house_rules',
            'calculated_host_listings_count_shared_rooms','availability_30','availability_90')
data1 <- data[,!names(data)%in% delcol]
scoringData1 <-scoringData[,!names(scoringData)%in% delcol]



############################################################# 3. CLEAN DATA DEEPLY (for both datasets simultaneously)
# deal with text summary: Transit from text to the number of characters. Fill in NAs with 0.
data1$summary <- nchar(as.character(data1$summary))
scoringData1$summary <- nchar(as.character(scoringData1$summary))
sum(is.na(data1$summary))
sum(is.na(scoringData1$summary))
mask_summary_NAs = is.na(data1$summary)
data1[mask_summary_NAs, 'summary'] = 0
mask_summary_NAs1 = is.na(scoringData1$summary)
scoringData1[mask_summary_NAs1, 'summary'] = 0
summary(data1$summary)t

# deal with text description: Transit from text to the number of characters. Fill in NAs with 0.
data1$description <- nchar(as.character(data1$description))
scoringData1$description <- nchar(as.character(scoringData1$description))
sum(is.na(data1$description))
sum(is.na(scoringData1$description))
mask_description_NAs = is.na(data1$description)
data1[mask_description_NAs, 'description'] = 0
mask_description_NAs1 = is.na(scoringData1$description)
scoringData1[mask_description_NAs1, 'description'] = 0
summary(data1$description)
summary(scoringData1$description)

# deal with text neighborhood_overview: Transit from text to the number of characters. Fill in NAs with 0.
data1$neighborhood_overview <- nchar(as.character(data1$neighborhood_overview))
scoringData1$neighborhood_overview <- nchar(as.character(scoringData1$neighborhood_overview))
sum(is.na(data1$neighborhood_overview))
sum(is.na(scoringData1$neighborhood_overview))
mask_neighborhood_overview_NAs = is.na(data1$neighborhood_overview)
data1[mask_neighborhood_overview_NAs, 'neighborhood_overview'] = 0
mask_neighborhood_overview_NAs1 = is.na(scoringData1$neighborhood_overview)
scoringData1[mask_neighborhood_overview_NAs1, 'neighborhood_overview'] = 0
summary(data1$neighborhood_overview)
summary(scoringData1$neighborhood_overview)
str(data1$neighborhood_overview)

# deal with text transit: Transit from text to the number of characters. Fill in NAs with 0.
data1$transit <- nchar(as.character(data1$transit))
scoringData1$transit <- nchar(as.character(scoringData1$transit))
sum(is.na(data1$transit))
sum(is.na(scoringData1$transit))
mask_transit_NAs = is.na(data1$transit)
data1[mask_transit_NAs, 'transit'] = 0
mask_transit_NAs1 = is.na(scoringData1$transit)
scoringData1[mask_transit_NAs1, 'transit'] = 0
summary(data1$transit)
summary(scoringData1$transit)
str(data1$transit)

# deal with text access: Transit from text to the number of characters. Fill in NAs with 0.
data1$access <- nchar(as.character(data1$access))
scoringData1$access <- nchar(as.character(scoringData1$access))
sum(is.na(data1$access))
sum(is.na(scoringData1$access))
mask_access_NAs = is.na(data1$access)
data1[mask_access_NAs, 'access'] = 0
mask_access_NAs1 = is.na(scoringData1$access)
scoringData1[mask_access_NAs1, 'access'] = 0
summary(data1$access)
summary(scoringData1$access)
str(data1$access)

# deal with date of host_since: Transit from date to the days duration till now. Fill in NAs with meaning value.
data1$host_since <- as.integer(as.Date("2019-11-13")-as.Date(data1$host_since))
scoringData1$host_since <- as.integer(as.Date("2019-11-13")-as.Date(scoringData1$host_since))
sum(is.na(data1$host_since)) # 1 NA, I use mean value to replace it.
sum(is.na(scoringData1$host_since)) # 2 NA, I use mean value to replace it.
data1[is.na(data1$host_since),"host_since"] <- mean(data1$host_since,na.rm=T)
scoringData1[is.na(scoringData1$host_since),"host_since"] <- mean(scoringData1$host_since,na.rm=T)

#Replace host_is_superhost with f where value is NA. Because most of the value is f (f/t = 30175/6648)
sum(is.na(data1$host_is_superhost))  # 1 NA.
sum(is.na(scoringData1$host_is_superhost)) # 2 NA.
data1[is.na(data1$host_is_superhost),"host_is_superhost"] <- 'f'
scoringData1[is.na(scoringData1$host_is_superhost),"host_is_superhost"] <- 'f'

#Replace host_listings_count with mean value where value is NA.
sum(is.na(data1$host_listings_count))  # 1 NA.
sum(is.na(scoringData1$host_listings_count)) # 2 NA.
data1[is.na(data1$host_listings_count),"host_listings_count"] <- mean(data1$host_listings_count,na.rm=T)
scoringData1[is.na(scoringData1$host_listings_count),"host_listings_count"] <- mean(scoringData1$host_listings_count,na.rm=T)

#Replace host_has_profile_pic with f where value is NA. Because most of the value is t (f/t = 65/36758)
sum(is.na(data1$host_has_profile_pic))  # 1 NA.
sum(is.na(scoringData1$host_has_profile_pic)) # 2 NA.
data1[is.na(data1$host_has_profile_pic),"host_has_profile_pic"] <- 't'
scoringData1[is.na(scoringData1$host_has_profile_pic),"host_has_profile_pic"] <- 't'

#Replace host_identity_verified with f where value is NA. Because most of the value is t (f/t = 18136/18687)
sum(is.na(data1$host_identity_verified))  # 1 NA.
sum(is.na(scoringData1$host_identity_verified)) # 2 NA.
data1[is.na(data1$host_identity_verified),"host_identity_verified"] <- 't'
scoringData1[is.na(scoringData1$host_identity_verified),"host_identity_verified"] <- 't'

#Replace beds with bedrooms where beds is NA. Because most beds equals to bedrooms and bedrooms don't have NA.
cor(data1$beds,data1$price)
sum(is.na(data1$beds))
sum(is.na(scoringData1$beds))
sum(is.na(data1$bedrooms))
sum(is.na(scoringData1$bedrooms))
nrow(data1[which(data1$beds == data1$bedrooms),])
nrow(data1[which(scoringData1$beds == scoringData1$bedrooms),])
data1[is.na(data1$beds),"beds"] <- data1[is.na(data1$beds),"bedrooms"]
scoringData1[is.na(scoringData1$beds),"beds"] <- scoringData1[is.na(scoringData1$beds),"bedrooms"]

# Deal with text amenities to select key words
# Wifi    
library(stringr)
data1$Wifi[str_detect(data1$amenities,'Wifi')==TRUE] = 't'
data1$Wifi[str_detect(data1$amenities,'Wifi')==FALSE] = 'f'
data1$Wifi = as.factor(data1$Wifi)
summary(data1$Wifi)
scoringData1$Wifi[str_detect(scoringData1$amenities,'Wifi')==TRUE] = 't'
scoringData1$Wifi[str_detect(scoringData1$amenities,'Wifi')==FALSE] = 'f'
scoringData1$Wifi = as.factor(scoringData1$Wifi)
summary(scoringData1$Wifi)
#TV
data1$TV[str_detect(data1$amenities,'TV')==TRUE] = 't'
data1$TV[str_detect(data1$amenities,'TV')==FALSE] = 'f'
data1$TV = as.factor(data1$TV)
summary(data1$TV)
scoringData1$TV[str_detect(scoringData1$amenities,'TV')==TRUE] = 't'
scoringData1$TV[str_detect(scoringData1$amenities,'TV')==FALSE] = 'f'
scoringData1$TV = as.factor(scoringData1$TV)
summary(scoringData1$TV)
#Air conditioning
data1$Air[str_detect(data1$amenities,'Air conditioning')==TRUE] = 't'
data1$Air[str_detect(data1$amenities,'Air conditioning')==FALSE] = 'f'
data1$Air = as.factor(data1$Air)
summary(data1$Air)
scoringData1$Air[str_detect(scoringData1$amenities,'Air conditioning')==TRUE] = 't'
scoringData1$Air[str_detect(scoringData1$amenities,'Air conditioning')==FALSE] = 'f'
scoringData1$Air = as.factor(scoringData1$Air)
summary(scoringData1$Air)
#Elevator
data1$Elevator[str_detect(data1$amenities,'Elevator')==TRUE] = 't'
data1$Elevator[str_detect(data1$amenities,'Elevator')==FALSE] = 'f'
data1$Elevator = as.factor(data1$Elevator)
summary(data1$Elevator)
scoringData1$Elevator[str_detect(scoringData1$amenities,'Elevator')==TRUE] = 't'
scoringData1$Elevator[str_detect(scoringData1$amenities,'Elevator')==FALSE] = 'f'
scoringData1$Elevator = as.factor(scoringData1$Elevator)
summary(scoringData1$Elevator)
#Heating
data1$Heating[str_detect(data1$amenities,'Heating')==TRUE] = 't'
data1$Heating[str_detect(data1$amenities,'Heating')==FALSE] = 'f'
data1$Heating = as.factor(data1$Heating)
summary(data1$Heating)
scoringData1$Heating[str_detect(scoringData1$amenities,'Heating')==TRUE] = 't'
scoringData1$Heating[str_detect(scoringData1$amenities,'Heating')==FALSE] = 'f'
scoringData1$Heating = as.factor(scoringData1$Heating)
summary(scoringData1$Heating)
#Dryer
data1$Dryer[str_detect(data1$amenities,'Dryer')==TRUE] = 't'
data1$Dryer[str_detect(data1$amenities,'Dryer')==FALSE] = 'f'
data1$Dryer = as.factor(data1$Dryer)
summary(data1$Dryer)
scoringData1$Dryer[str_detect(scoringData1$amenities,'Dryer')==TRUE] = 't'
scoringData1$Dryer[str_detect(scoringData1$amenities,'Dryer')==FALSE] = 'f'
scoringData1$Dryer = as.factor(scoringData1$Dryer)
summary(scoringData1$Dryer)
#Private
data1$Private[str_detect(data1$amenities,'Private')==TRUE] = 't'
data1$Private[str_detect(data1$amenities,'Private')==FALSE] = 'f'
data1$Private = as.factor(data1$Private)
summary(data1$Private)
scoringData1$Private[str_detect(scoringData1$amenities,'Private')==TRUE] = 't'
scoringData1$Private[str_detect(scoringData1$amenities,'Private')==FALSE] = 'f'
scoringData1$Private = as.factor(scoringData1$Private)
summary(scoringData1$Private)
#Extra
data1$Extra[str_detect(data1$amenities,'Extra')==TRUE] = 't'
data1$Extra[str_detect(data1$amenities,'Extra')==FALSE] = 'f'
data1$Extra = as.factor(data1$Extra)
summary(data1$Extra)
scoringData1$Extra[str_detect(scoringData1$amenities,'Extra')==TRUE] = 't'
scoringData1$Extra[str_detect(scoringData1$amenities,'Extra')==FALSE] = 'f'
scoringData1$Extra = as.factor(scoringData1$Extra)
summary(scoringData1$Extra)
#DELETE amenities since it contains too many levels.
data1 <- subset(data1,select = -c(amenities))


#Replace cleaning_fee with 0 where value is NA.
sum(is.na(data1$cleaning_fee))  # 5990 NA.
sum(is.na(scoringData1$cleaning_fee)) # 1546 NA.
data1[is.na(data1$cleaning_fee),"cleaning_fee"] <- 0
scoringData1[is.na(scoringData1$cleaning_fee),"cleaning_fee"] <- 0

# deal with date of first_review: Transit from date to the days duration till now. Fill in NAs with meaning value.
data1$first_review <- as.integer(as.Date("2019-11-13")-as.Date(data1$first_review))
scoringData1$first_review <- as.integer(as.Date("2019-11-13")-as.Date(scoringData1$first_review))
#in both datasets, Replace first_review with mean value where value is NA.
sum(is.na(data1$first_review)) #3NA
sum(is.na(scoringData1$first_review))#0NA
data1[is.na(data1$first_review),"first_review"] <- mean(data1$first_review,na.rm=T)

# deal with date of last_review: Transit from date to the days duration till now. Fill in NAs with meaning value.
data1$last_review <- as.integer(as.Date("2019-11-13")-as.Date(data1$last_review))
scoringData1$last_review <- as.integer(as.Date("2019-11-13")-as.Date(scoringData1$last_review))
#in both datasets, Replace last_review with mean value where value is NA.
sum(is.na(data1$last_review)) #3NA
sum(is.na(scoringData1$last_review))#0NA
data1[is.na(data1$last_review),"last_review"] <- mean(data1$last_review,na.rm=T)

#Replace host_total_listings_count with 0 where value is NA.
sum(is.na(data1$host_total_listings_count))  # 1 NA.
sum(is.na(scoringData1$host_total_listings_count)) # 2 NA.
data1[is.na(data1$host_total_listings_count),"host_total_listings_count"] <- 0
scoringData1[is.na(scoringData1$host_total_listings_count),"host_total_listings_count"] <-0

#Replace reviews_per_month with 0 where value is NA.
sum(is.na(data1$reviews_per_month))
sum(is.na(scoringData1$reviews_per_month))
data1[is.na(data1$reviews_per_month),"reviews_per_month"] <- 0

#Replace host_listings_count with 0 where value is NA. Replace host_listings_count with mean value where value is too big (outlier).
scoringData1[is.na(scoringData1$host_listings_count),"host_listings_count"] <-0
sum(is.na(data1$host_listings_count))
sum(is.na(scoringData1$host_listings_count))
summary(data1$host_total_listings_count)
summary(scoringData1$host_total_listings_count)
boxplot(scoringData1$host_total_listings_count)
data1[which(data1$host_total_listings_count == 1483),"host_total_listings_count"] <- mean(data1$host_total_listings_count,na.rm = T)
scoringData1[which(scoringData1$host_total_listings_count == 1483),"host_total_listings_count"] <- mean(scoringData1$host_total_listings_count,na.rm = T)

#Replace property_type with factor from each other dataset.
str(data1$property_type)
str(scoringData1$property_type)
levels(data1$property_type)
levels(scoringData1$property_type)
data1type <- unique(data1$property_type)
data1type
scoringData1type <- unique(scoringData1$property_type)
scoringData1type
scoringData1$property_type <- ifelse((scoringData1$property_type %in% data1type),as.character(scoringData1$property_type), 'Other')
data1$property_type <- ifelse((data1$property_type %in% scoringData1type),as.character(data1$property_type), 'Other')
data1$property_type = as.factor(data1$property_type)
scoringData1$property_type = as.factor(scoringData1$property_type)

#Replace cancellation_policy with factor from each other dataset.
str(data1$cancellation_policy)
str(scoringData1$cancellation_policy)
levels(data1$cancellation_policy)
levels(scoringData1$cancellation_policy)
levels(scoringData1$cancellation_policy) <- levels(data1$cancellation_policy)

#Replace market with factor from each other dataset.Replace market with 'Other(Domestic)' where value is NA
summary(data1$market)
summary(scoringData1$market)
levels(data1$market)
levels(scoringData1$market)
data1_market <- unique(data1$market);data1_market
scoringData1_market <- unique(scoringData1$market);scoringData1_market
scoringData1$market <- ifelse((scoringData1$market %in% data1_market),as.character(scoringData1$market), 'Other (Domestic)')
data1$market <- ifelse((data1$market %in% scoringData1_market),as.character(data1$market), 'Other (Domestic)')
data1$market = as.factor(data1$market)
scoringData1$market = as.factor(scoringData1$market)
scoringData1[is.na(scoringData1$market),"market"] <-'Other (Domestic)'
data1[is.na(data1$market),"market"] <-'Other (Domestic)'

#Replace zipcodei with factor from each other dataset.Replace zipcode with '(Other)' where value is NA
summary(data1$zipcode)
summary(scoringData1$zipcode)
levels(data1$zipcode)
levels(scoringData1$zipcode)
data1_zipcode <- unique(data1$zipcode);data1_zipcode 
scoringData1_zipcode <- unique(scoringData1$zipcode);scoringData1_zipcode
scoringData1$zipcode <- ifelse((scoringData1$zipcode %in% data1_zipcode),as.character(scoringData1$zipcode), '(Other)')
data1$zipcode <- ifelse((data1$zipcode %in% scoringData1_zipcode),as.character(data1$zipcode), '(Other)')
data1$zipcode = as.factor(data1$zipcode)
scoringData1$zipcode = as.factor(scoringData1$zipcode)
data1[is.na(data1$zipcode),"zipcode"] <- '(Other)'
scoringData1[is.na(scoringData1$zipcode),"zipcode"] <- '(Other)'
summary(data1)

############################################################4. SPLIT to train with 80% and test with 20% of the data
library(caTools)
set.seed(617)
split = sample.split(data1$price,0.8)
train = data1[split,]
test = data1[!split,]
nrow(train)
nrow(test)
mean(train$price)
mean(test$price)
str(train)


################################################################5. Build model- Phase I (before 11.17, start with small tree numbers ) 
################################################################WITHOUT:  deeply clean data/ consider category variables/ consider text meanings.

#1. based on intuition, build initial model with only 2 features. RMSE 82.98611
model1 = lm(price~room_type+beds+review_scores_rating,train)
summary(model1)
pred_test1 = predict(model1,newdata = test)
summary(pred_test1)
rmse(test$price,pred_test1)

#2. add manully selected variables to model1, based on the factors. with RMSE 69.95377
model2 = lm(price~room_type+beds+review_scores_rating+neighbourhood_group_cleansed+accommodates
            +bathrooms+bedrooms+bed_type+maximum_nights+minimum_nights+guests_included+extra_people
            +number_of_reviews+review_scores_accuracy+calculated_host_listings_count
            +reviews_per_month+instant_bookable+number_of_reviews_ltm+host_listings_count
            +review_scores_cleanliness+review_scores_checkin+review_scores_communication
            +review_scores_location+review_scores_value+host_is_superhost+host_has_profile_pic
            +host_identity_verified+is_location_exact+availability_30+calculated_host_listings_count_entire_homes
            +calculated_host_listings_count_private_rooms+calculated_host_listings_count_shared_rooms
            +availability_60+availability_90+availability_365+host_total_listings_count,train)
summary(model2)
pred_test2 = predict(model2,newdata = test)
summary(pred_test2)
rmse(test$price,pred_test2)
pred_scoringData = predict(model2, newdata=scoringData1)
summary(pred_scoringData)
sum(is.na(scoringData1$minimum_nights))
# Construct submission from model2
submissionFile = data.frame(id = scoringData$id, price = pred_scoringData)
which(is.na(submissionFile))
submissionFile[is.na(submissionFile$price),"price"] <- mean(submissionFile$price,na.rm = T)
write.csv(submissionFile, 'submission_model2.csv',row.names = F)

#3.Plus text and date variables. RMSE = forget to write it down...
model3 = lm(price~room_type+beds+review_scores_rating+neighbourhood_group_cleansed+accommodates
            +bathrooms+bedrooms+bed_type+maximum_nights+minimum_nights+guests_included+extra_people
            +number_of_reviews+review_scores_accuracy+calculated_host_listings_count
            +reviews_per_month+instant_bookable+number_of_reviews_ltm+host_listings_count
            +review_scores_cleanliness+review_scores_checkin+review_scores_communication
            +review_scores_location+review_scores_value+host_is_superhost+host_has_profile_pic
            +host_identity_verified+is_location_exact+availability_30+calculated_host_listings_count_entire_homes
            +calculated_host_listings_count_private_rooms+calculated_host_listings_count_shared_rooms
            +availability_60+availability_90+availability_365+host_total_listings_count+summary+description
            +host_since+last_review,train)
summary(model3)
pred_test3 = predict(model3,newdata = test)
summary(pred_test3)
rmse(test$price,pred_test3)
pred_scoringData = predict(model3, newdata=scoringData1)
summary(pred_scoringData)
# Construct submission from model3
submissionFile = data.frame(id = scoringData$id, price = pred_scoringData)
submissionFile[is.na(submissionFile$price),"price"] <- mean(submissionFile$price,na.rm = T)
write.csv(submissionFile, 'submission_model3.csv',row.names = F)



# 4. Feature Selection
# 4-1. Best subset selection--- OUT OF TIME -- aborted.
#install.packages('leaps')
library(leaps)
subsets = regsubsets(price~room_type+beds+review_scores_rating+neighbourhood_group_cleansed+accommodates
                     +bathrooms+bedrooms+bed_type+maximum_nights+minimum_nights+guests_included+extra_people
                     +number_of_reviews+review_scores_accuracy+calculated_host_listings_count
                     +reviews_per_month+instant_bookable+number_of_reviews_ltm+host_listings_count
                     +review_scores_cleanliness+review_scores_checkin+review_scores_communication
                     +review_scores_location+review_scores_value+host_is_superhost+host_has_profile_pic
                     +host_identity_verified+is_location_exact+availability_30+calculated_host_listings_count_entire_homes
                     +calculated_host_listings_count_private_rooms+calculated_host_listings_count_shared_rooms
                     +availability_60+availability_90+availability_365+host_total_listings_count+summary+description
                     +host_since+last_review,data=train, nvmax=20)
summary(subsets)

# 4-2. Forward selection. 
start_mod = lm(price~1,data=train)
empty_mod = lm(price~1,data=train)
full_mod = lm(price~room_type+beds+review_scores_rating+neighbourhood_group_cleansed+accommodates
              +bathrooms+bedrooms+bed_type+maximum_nights+minimum_nights+guests_included+extra_people
              +number_of_reviews+review_scores_accuracy+calculated_host_listings_count
              +reviews_per_month+instant_bookable+number_of_reviews_ltm+host_listings_count
              +review_scores_cleanliness+review_scores_checkin+review_scores_communication
              +review_scores_location+review_scores_value+host_is_superhost+host_has_profile_pic
              +host_identity_verified+is_location_exact+availability_30+calculated_host_listings_count_entire_homes
              +calculated_host_listings_count_private_rooms+calculated_host_listings_count_shared_rooms
              +availability_60+availability_90+availability_365+host_total_listings_count+summary+description
              +host_since+last_review,data=train)
forwardStepwise = step(start_mod,
                       scope=list(upper=full_mod,lower=empty_mod),
                       direction='forward')
summary(forwardStepwise)
# 4-2-1. Use forward selection with lm,RMSE = 70.55142
# accommodates + neighbourhood_group_cleansed + room_type + 
#   bathrooms + bedrooms + review_scores_location + availability_90 + 
#   number_of_reviews_ltm + extra_people + minimum_nights + review_scores_cleanliness + 
#   review_scores_value + review_scores_rating + last_review + 
#   host_is_superhost + review_scores_communication + calculated_host_listings_count + 
#   availability_365 + reviews_per_month + guests_included + 
#   calculated_host_listings_count_shared_rooms + review_scores_checkin + 
#   host_listings_count + review_scores_accuracy + beds + number_of_reviews + 
#   host_has_profile_pic + availability_30 + is_location_exact + 
#   instant_bookable + host_since + availability_60
model_forward = lm(price ~ accommodates + neighbourhood_group_cleansed + 
     room_type + bathrooms + bedrooms + review_scores_location + 
     availability_90 + number_of_reviews_ltm + extra_people + 
     minimum_nights + review_scores_cleanliness + review_scores_value + 
     review_scores_rating + last_review + host_is_superhost + 
     review_scores_communication + calculated_host_listings_count + 
     availability_365 + reviews_per_month + guests_included + 
     calculated_host_listings_count_shared_rooms + review_scores_checkin + 
     host_listings_count + review_scores_accuracy + beds + number_of_reviews + 
     host_has_profile_pic + availability_30 + is_location_exact + 
     instant_bookable + host_since + availability_60, data = train)
pred_test_forward = predict(model_forward,newdata = test)
summary(pred_test_forward)
rmse(test$price,pred_test_forward) #RMSE = forget to write it down...

# 4-2-2. Use forward selection with randomForest,RMSE = 60.39637
library(randomForest)
set.seed(617)
forest = randomForest(price ~ accommodates + neighbourhood_group_cleansed + 
                        room_type + bathrooms + bedrooms + review_scores_location + 
                        availability_90 + number_of_reviews_ltm + extra_people + 
                        minimum_nights + review_scores_cleanliness + review_scores_value + 
                        review_scores_rating + last_review + host_is_superhost + 
                        review_scores_communication + calculated_host_listings_count + 
                        availability_365 + reviews_per_month + guests_included + 
                        calculated_host_listings_count_shared_rooms + review_scores_checkin + 
                        host_listings_count + review_scores_accuracy + beds + number_of_reviews + 
                        host_has_profile_pic + availability_30 + is_location_exact + 
                        instant_bookable + host_since + availability_60, data=train,ntree = 100)
predForest = predict(forest,newdata=test)
rmse(test$price,predForest)
pred_scoringData = predict(forest, newdata=scoringData1)
summary(pred_scoringData)
# Construct submission from forest
submissionFile = data.frame(id = scoringData$id, price = pred_scoringData)
submissionFile[is.na(submissionFile$price),"price"] <- mean(submissionFile$price,na.rm = T)
write.csv(submissionFile, 'submission_featureselection_RF.csv',row.names = F)

# 4-2-3. Use forward selection with boosting,RMSE = 80.31188
library(gbm)
set.seed(617)
boost = gbm(price ~ accommodates + neighbourhood_group_cleansed + 
              room_type + bathrooms + bedrooms + review_scores_location + 
              availability_90 + number_of_reviews_ltm + extra_people + 
              minimum_nights + review_scores_cleanliness + review_scores_value + 
              review_scores_rating + last_review + host_is_superhost + 
              review_scores_communication + calculated_host_listings_count + 
              availability_365 + reviews_per_month + guests_included + 
              calculated_host_listings_count_shared_rooms + review_scores_checkin + 
              host_listings_count + review_scores_accuracy + beds + number_of_reviews + 
              host_has_profile_pic + availability_30 + is_location_exact + 
              instant_bookable + host_since + availability_60,data=train,distribution="gaussian",
            n.trees = 100,interaction.depth = 3,shrinkage = 0.01)
summary(boost)
predBoostTest = predict(boost,test,n.trees = 100)
rmse(test$price,predBoostTest)


# 4-3. LASSO
library(glmnet)
x = model.matrix(price~room_type+beds+review_scores_rating+neighbourhood_group_cleansed+accommodates
                 +bathrooms+bedrooms+bed_type+maximum_nights+minimum_nights+guests_included+extra_people
                 +number_of_reviews+review_scores_accuracy+calculated_host_listings_count
                 +reviews_per_month+instant_bookable+number_of_reviews_ltm+host_listings_count
                 +review_scores_cleanliness+review_scores_checkin+review_scores_communication
                 +review_scores_location+review_scores_value+host_is_superhost+host_has_profile_pic
                 +host_identity_verified+is_location_exact+availability_30+calculated_host_listings_count_entire_homes
                 +calculated_host_listings_count_private_rooms+calculated_host_listings_count_shared_rooms
                 +availability_60+availability_90+availability_365+host_total_listings_count+summary+description
                 +host_since+last_review,data=train)
y = train$price
lassoModel = glmnet(x,y, alpha=1) 
plot(lassoModel,xvar='lambda',label=T)
set.seed(617)
cv.lasso = cv.glmnet(x,y,alpha=1)
c(cv.lasso$lambda.min, cv.lasso$lambda.1se)
lam = cv.lasso$lambda.1se
coef(lassoModel,lam)
#results:
# room_type,neighbourhood_group_cleansed,accommodates,bathrooms,bedrooms,minimum_nights,guests_included,
# extra_people,reviews_per_month,number_of_reviews_ltm,review_scores_cleanliness,review_scores_location,
# review_scores_value,availability_30,calculated_host_listings_count_shared_rooms,availability_60,
# availability_90,availability_365
#install.packages('glmnet')

# 4-3-1. Use LASSO selection with lm,RMSE = 70.93437
lassoModel_lm=lm(price~room_type+neighbourhood_group_cleansed+accommodates+bathrooms+bedrooms
                 +minimum_nights+guests_included+extra_people+reviews_per_month+number_of_reviews_ltm
                 +review_scores_cleanliness+review_scores_location+review_scores_value+availability_30
                 +calculated_host_listings_count_shared_rooms+availability_60+availability_90+availability_365
                 ,data=train)
predLasso = predict(lassoModel_lm,newdata=test)
rmse(test$price,predLasso)

# 4-3-2. Use LASSO with randomForest,RMSE = 61.89076
library(randomForest)
set.seed(617)
forestLasso = randomForest(price~room_type+neighbourhood_group_cleansed+accommodates+bathrooms+bedrooms
                      +minimum_nights+guests_included+extra_people+reviews_per_month+number_of_reviews_ltm
                      +review_scores_cleanliness+review_scores_location+review_scores_value+availability_30
                      +calculated_host_listings_count_shared_rooms+availability_60+availability_90+availability_365,
                      data=train,ntree = 100)
predForest = predict(forestLasso,newdata=test)
rmse(test$price,predForest)
pred_scoringData = predict(predForest, newdata=scoringData1)
summary(pred_scoringData)
# Construct submission from forest
submissionFile = data.frame(id = scoringData$id, price = pred_scoringData)
submissionFile[is.na(submissionFile$price),"price"] <- mean(submissionFile$price,na.rm = T)
write.csv(submissionFile, 'submission_LASSO_RF.csv',row.names = F)

#4-4 Try tree.
library(rpart)
library(rpart.plot)
Tree1 = rpart(price~room_type+beds+review_scores_rating+neighbourhood_group_cleansed+accommodates
              +bathrooms+bedrooms+bed_type+maximum_nights+minimum_nights+guests_included+extra_people
              +number_of_reviews+review_scores_accuracy+calculated_host_listings_count
              +reviews_per_month+instant_bookable+number_of_reviews_ltm+host_listings_count
              +review_scores_cleanliness+review_scores_checkin+review_scores_communication
              +review_scores_location+review_scores_value+host_is_superhost+host_has_profile_pic
              +host_identity_verified+is_location_exact+availability_30+calculated_host_listings_count_entire_homes
              +calculated_host_listings_count_private_rooms+calculated_host_listings_count_shared_rooms
              +availability_60+availability_90+availability_365+host_total_listings_count+summary+description
              +host_since+last_review,train)
summary(Tree1)
# 4-4-1. Tree in default cp. Apply the selected variables by Tree to LM. RMSE 72.79401
model4 = lm(price~room_type+calculated_host_listings_count_entire_homes+calculated_host_listings_count_private_rooms
            +accommodates+bathrooms+beds+bedrooms+neighbourhood_group_cleansed+guests_included,train)
summary(model4)
pred_test4 = predict(model4,newdata = test)
rmse(test$price,pred_test4)

# 4-4-2. Tree in  cp= 0.001.Apply the selected variables by Tree to LM.  RMSE 71.68822
Tree2 = rpart(price~room_type+beds+review_scores_rating+neighbourhood_group_cleansed+accommodates
              +bathrooms+bedrooms+bed_type+maximum_nights+minimum_nights+guests_included+extra_people
              +number_of_reviews+review_scores_accuracy+calculated_host_listings_count
              +reviews_per_month+instant_bookable+number_of_reviews_ltm+host_listings_count
              +review_scores_cleanliness+review_scores_checkin+review_scores_communication
              +review_scores_location+review_scores_value+host_is_superhost+host_has_profile_pic
              +host_identity_verified+is_location_exact+availability_30+calculated_host_listings_count_entire_homes
              +calculated_host_listings_count_private_rooms+calculated_host_listings_count_shared_rooms
              +availability_60+availability_90+availability_365+host_total_listings_count+summary+description
              +host_since+last_review,train,cp=0.001)
summary(Tree2)
model5 = lm(price~room_type+calculated_host_listings_count_entire_homes+calculated_host_listings_count_private_rooms
            +accommodates+bathrooms+bedrooms+beds+neighbourhood_group_cleansed+guests_included
            +review_scores_location+availability_365+host_listings_count+host_total_listings_count
            +minimum_nights+availability_90+calculated_host_listings_count,train)
summary(model5)
pred_test5 = predict(model5,newdata = test)
rmse(test$price,pred_test5)

# 4-4-3. Try tree with 10 fold validation. bestTune is cp=0.001.
library(caret)
trControl = trainControl(method="cv",number = 10)
tuneGrid = expand.grid(.cp = seq(0.001,0.1,0.001))
set.seed(617)
cvModel = train(price~room_type+beds+review_scores_rating+neighbourhood_group_cleansed+accommodates
                +bathrooms+bedrooms+bed_type+maximum_nights+minimum_nights+guests_included+extra_people
                +number_of_reviews+review_scores_accuracy+calculated_host_listings_count
                +reviews_per_month+instant_bookable+number_of_reviews_ltm+host_listings_count
                +review_scores_cleanliness+review_scores_checkin+review_scores_communication
                +review_scores_location+review_scores_value+host_is_superhost+host_has_profile_pic
                +host_identity_verified+is_location_exact+availability_30+calculated_host_listings_count_entire_homes
                +calculated_host_listings_count_private_rooms+calculated_host_listings_count_shared_rooms
                +availability_60+availability_90+availability_365+host_total_listings_count+summary+description
                +host_since+last_review,data=train,method="rpart",
                trControl = trControl,tuneGrid = tuneGrid)
cvModel$bestTune
head(cvModel$results)
plot(cvModel)

# 4-4-4Try randomForest. Apply the selected variables by Tree to RM.   RMSE 62.52535
#install.packages('raondomForest')
library(randomForest)
set.seed(617)
forest = randomForest(price~room_type+calculated_host_listings_count_entire_homes+calculated_host_listings_count_private_rooms
                      +accommodates+bathrooms+bedrooms+beds+neighbourhood_group_cleansed+guests_included
                      +review_scores_location+availability_365+host_listings_count+host_total_listings_count
                      +minimum_nights+availability_90+calculated_host_listings_count,data=train,ntree = 100)
predForest = predict(forest,newdata=test)
rmse(test$price,predForest)
pred_scoringData = predict(forest, newdata=scoringData1)
summary(pred_scoringData)
names(forest)
summary(forest)
plot(forest)
varImpPlot(forest)
importance(forest)
hist(treesize(forest))
# 4-4-5 cross validation--------OUT OF TIME. aborted.
library(caret)
trControl=trainControl(method="cv",number=10)
tuneGrid = expand.grid(mtry=1:16)
set.seed(617)
cvForest = train(price~room_type+calculated_host_listings_count_entire_homes+calculated_host_listings_count_private_rooms
                    +accommodates+bathrooms+bedrooms+beds+neighbourhood_group_cleansed+guests_included
                  +review_scores_location+availability_365+host_listings_count+host_total_listings_count
                   +minimum_nights+availability_90+calculated_host_listings_count,data=train,
                   method="rf",ntree=100,trControl=trControl,tuneGrid=tuneGrid )


# 4-4-6 Try boosting RMSE=100.33 
library(gbm)
set.seed(617)
boost = gbm(price~room_type+calculated_host_listings_count_entire_homes+calculated_host_listings_count_private_rooms
            +accommodates+bathrooms+bedrooms+beds+neighbourhood_group_cleansed+guests_included
            +review_scores_location+availability_365+host_listings_count+host_total_listings_count
            +minimum_nights+availability_90+calculated_host_listings_count,data=train,distribution="gaussian",
            n.trees = 100,interaction.depth = 3,shrinkage = 0.001)
summary(boost)
predBoostTest = predict(boost,test,n.trees = 100)
rmse(test$price,predBoostTest)

####################################################################################6. Build model in 11.17
################################################################################### With deep clean data and tunning.
# 1. build model0 with all the manually selected model. RMSE 69.3815
model0 = lm(price~.,train)
summary(model0) 
pred0 = predict(model0,newdata = test)
summary(pred0)
rmse(test$price,pred0)

# 2. use tree. RMSE 73.65942
library(rpart)
tree0 = rpart(price~.,train)
summary(tree0)
pred_tree0 = predict(tree0,newdata=test)
summary(pred_tree0)
rmse(test$price,pred_tree0)


# 3. use random forest. RMSE 58.96559
library(randomForest)
forest1 = randomForest(price~.,train,tree=500)
pred_forest1 = predict(forest1,newdata=test)
rmse(test$price,pred_forest1)
pred_scoringData = predict(forest1, newdata=scoringData1)
summary(pred_scoringData)
submissionFile = data.frame(id = scoringData$id, price = pred_scoringData)
write.csv(submissionFile, 'submission_forest1.csv',row.names = F)

# 4. use random forest. Tune the tree number. RMSE 59.04524
library(randomForest)
forest2 = randomForest(price~.,train,tree=1000)
pred_forest2 = predict(forest2,newdata=test)
rmse(test$price,pred_forest2)


# 5. use random forest. Tune the tree number. RMSE 58.96559
library(randomForest)
forest4 = randomForest(price~.,train,tree=800,cp=0.01)
pred_forest4 = predict(forest4,newdata=test)
rmse(test$price,pred_forest4)

#6. use random forest. Tune the tree number and cp value. RMSE 59.04438
library(randomForest)
forest5 = randomForest(price~.,train,tree=800,cp=0.001)
pred_forest5 = predict(forest5,newdata=test)
rmse(test$price,pred_forest5)

# 7.use boosting. RMSE 73.78254
set.seed(617)
boost1 = gbm(price~.,data=train,distribution="gaussian",
             n.trees = 800,interaction.depth = 40,shrinkage = 0.001)
summary(boost1)
predBoostTest = predict(boost1,test,n.trees = 800)
rmse(test$price,predBoostTest)

# 8. use boosting. Tune the tree number.RMSE 81.44627
set.seed(617)
boost3 = gbm(price~.,data=train,distribution="gaussian",
             n.trees = 500,interaction.depth = 40,shrinkage = 0.001)
summary(boost3)
predBoostTest = predict(boost3,newdata = test, n.trees = 500)
rmse(test$price,predBoostTest)

# 9. use boosting. Tune the tree number and shrinkage. RMSE 57.83948
set.seed(617)
boost1 = gbm(price~.,data=train,distribution="gaussian",
             n.trees = 1000,interaction.depth = 40,shrinkage = 0.01)
summary(boost1)
predBoostTest = predict(boost1,test,n.trees = 1000)
rmse(test$price,predBoostTest)
pred_scoringData = predict(boost1, newdata=scoringData1,n.trees=1000)
summary(pred_scoringData)
submissionFile = data.frame(id = scoringData$id, price = pred_scoringData)
write.csv(submissionFile, 'submission_boost1.csv',row.names = F)

# 10. use boosting. Tune the tree number and shrinkage. RMSE 73.69833
set.seed(617)
boost5 = gbm(price~.,data=train,distribution="gaussian",
             n.trees = 1000,interaction.depth = 40,shrinkage = 0.001)
summary(boost5)
predBoostTest = predict(boost5,test,n.trees = 1000)
rmse(test$price,predBoostTest)

# 11.use boosting. Tune the depth. RMSE 70.30545
set.seed(617)
boost4 = gbm(price~.,data=train,distribution="gaussian",
             n.trees = 1000,interaction.depth = 10,shrinkage = 0.001)
summary(boost4)
predBoostTest = predict(boost4,test,n.trees = 1000)
rmse(test$price,predBoostTest)

# 12.use boosting. Tune the depth.. RMSE 70.30545
set.seed(617)
boost2 = gbm(price~.,data=train,distribution="gaussian",
             n.trees = 1000,interaction.depth = 40,shrinkage = 0.001)
summary(boost2)
predBoostTest = predict(boost2,test,n.trees = 1000)
rmse(test$price,predBoostTest)


# 13. use LASSO to select feature.
library(glmnet)
x = model.matrix(price~.,data=train)
y = train$price
lassoModel = glmnet(x,y, alpha=1) 
plot(lassoModel,xvar='lambda',label=T)
set.seed(617)
cv.lasso = cv.glmnet(x,y,alpha=1)
plot(cv.lasso)
c(cv.lasso$lambda.min, cv.lasso$lambda.1se)
lam = cv.lasso$lambda.1se
coef(lassoModel,lam)
#the chosen variables 
# neighbourhood_group_cleansed+property_type+room_type+accommodates+bathrooms+bedrooms+cleaning_fee+guests_included
# +extra_people+minimum_nights+availability_30+availability_60+availability_90+number_of_reviews_ltm+last_review+review_scores_cleanliness
# +review_scores_location+calculated_host_listings_count+calculated_host_listings_count_private_rooms   


# 14. use tree to test lasso result.  RMSE  73.22365
library(rpart)   
tree1 = rpart(price~neighbourhood_group_cleansed+property_type+room_type+accommodates+bathrooms+bedrooms
              +cleaning_fee+guests_included+extra_people+minimum_nights+availability_30
              +availability_60+availability_90+number_of_reviews_ltm+last_review+review_scores_cleanliness
              +review_scores_location+calculated_host_listings_count+calculated_host_listings_count_private_rooms,train)
summary(tree1)
pred_tree1 = predict(tree1,newdata=test)
summary(pred_tree1)
rmse(test$price,pred_tree1)


# 15. forwardStepwise
start_mod = lm(price~1,data=train)
empty_mod = lm(price~1,data=train)
full_mod = lm(price~.,data=train)
forwardStepwise = step(start_mod,
                       scope=list(upper=full_mod,lower=empty_mod),
                       direction='forward')
summary(forwardStepwise)


# 16. use tree to test stepforward result.73.22365
tree2 = rpart(price ~ accommodates + neighbourhood_group_cleansed + 
                cleaning_fee + room_type + property_type + bedrooms + bathrooms + 
                review_scores_location + number_of_reviews_ltm + availability_60 + 
                minimum_nights + last_review + calculated_host_listings_count + 
                host_total_listings_count + host_is_superhost + review_scores_value + 
                review_scores_cleanliness + review_scores_rating + review_scores_checkin + 
                guests_included + cancellation_policy + review_scores_communication + 
                extra_people + is_location_exact + availability_90 + beds + 
                availability_30 + calculated_host_listings_count_entire_homes + 
                availability_365 + calculated_host_listings_count_private_rooms + 
                description + host_has_profile_pic + review_scores_accuracy, 
              data = train)
summary(tree2)
pred_tree2 = predict(tree2,newdata=test)
summary(pred_tree2)
rmse(test$price,pred_tree2)


# 17, use random forest with forwardstep wise selection.RMSE 59.36385
library(randomForest)
forest0 = randomForest(price ~ accommodates + neighbourhood_group_cleansed + 
                         cleaning_fee + room_type + property_type + bedrooms + bathrooms + 
                         review_scores_location + number_of_reviews_ltm + availability_60 + 
                         minimum_nights + last_review + calculated_host_listings_count + 
                         host_total_listings_count + host_is_superhost + review_scores_value + 
                         review_scores_cleanliness + review_scores_rating + review_scores_checkin + 
                         guests_included + cancellation_policy + review_scores_communication + 
                         extra_people + is_location_exact + availability_90 + beds + 
                         availability_30 + calculated_host_listings_count_entire_homes + 
                         availability_365 + calculated_host_listings_count_private_rooms + 
                         description + host_has_profile_pic + review_scores_accuracy
                       ,train,tree=500)
pred_forest0 = predict(forest0,newdata=test)
rmse(test$price,pred_forest0)
pred_scoringData = predict(forest0, newdata=scoringData1)
summary(pred_scoringData)
submissionFile = data.frame(id = scoringData$id, price = pred_scoringData)
write.csv(submissionFile, 'submission_forest0.csv',row.names = F)

# 18. use random forest with forwardstep wise selection. Tune the tree number. RMSE 59.27296
library(randomForest)
forest3 = randomForest(price ~ accommodates + neighbourhood_group_cleansed + 
                         cleaning_fee + room_type + property_type + bedrooms + bathrooms + 
                         review_scores_location + number_of_reviews_ltm + availability_60 + 
                         minimum_nights + last_review + calculated_host_listings_count + 
                         host_total_listings_count + host_is_superhost + review_scores_value + 
                         review_scores_cleanliness + review_scores_rating + review_scores_checkin + 
                         guests_included + cancellation_policy + review_scores_communication + 
                         extra_people + is_location_exact + availability_90 + beds + 
                         availability_30 + calculated_host_listings_count_entire_homes + 
                         availability_365 + calculated_host_listings_count_private_rooms + 
                         description + host_has_profile_pic + review_scores_accuracy
                       ,train,tree=1000)
pred_forest3 = predict(forest3,newdata=test)
rmse(test$price,pred_forest3)

# 19. use boosting with forwardstep wise selection.RMSE 59.85273
library(gbm)
set.seed(617)
boost = gbm(price ~ accommodates + neighbourhood_group_cleansed + 
              cleaning_fee + room_type + property_type + bedrooms + bathrooms + 
              review_scores_location + number_of_reviews_ltm + availability_60 + 
              minimum_nights + last_review + calculated_host_listings_count + 
              host_total_listings_count + host_is_superhost + review_scores_value + 
              review_scores_cleanliness + review_scores_rating + review_scores_checkin + 
              guests_included + cancellation_policy + review_scores_communication + 
              extra_people + is_location_exact + availability_90 + beds + 
              availability_30 + calculated_host_listings_count_entire_homes + 
              availability_365 + calculated_host_listings_count_private_rooms + 
              description + host_has_profile_pic + review_scores_accuracy,data=train,distribution="gaussian",
            n.trees = 500,interaction.depth = 20,shrinkage = 0.01)
summary(boost)
predBoostTest = predict(boost,test,n.trees = 500)
rmse(test$price,predBoostTest)


#############################################################################################5. Build model in 11.19
##############################################    WITH:  deeply clean data/ consider category variables/ consider text meanings.
#1. use boosting. RMSE 57.31812
set.seed(617)
boost1119_1 = gbm(price~.,data=train,distribution="gaussian",
                  n.trees = 1000,interaction.depth = 40,shrinkage = 0.01)
summary(boost1119_1)
predBoostTest = predict(boost1119_1,test,n.trees = 1000)
rmse(test$price,predBoostTest)
pred_scoringData = predict(boost1119_1, newdata=scoringData1,n.tree=1000)
summary(pred_scoringData)
submissionFile = data.frame(id = scoringData$id, price = pred_scoringData)
write.csv(submissionFile, 'submission_boost1119_1.csv',row.names = F)


# 2. use boosting with ADDING amentities keywords RMSE  57.02161   -----Here is my final submit.
set.seed(617)
boost1119_2 = gbm(price~.,data=train,distribution="gaussian",
                  n.trees = 1000,interaction.depth = 40,shrinkage = 0.01)
predBoostTest = predict(boost1119_2,test,n.trees = 1000)
rmse(test$price,predBoostTest)
pred_scoringData = predict(boost1119_2, newdata=scoringData1,n.tree=1000)
summary(pred_scoringData)
submissionFile = data.frame(id = scoringData$id, price = pred_scoringData)
write.csv(submissionFile, 'submission_boost1119_2.csv',row.names = F)

# 3. use boosting with ADDING MORE amentities keywords. RMSE  57.02813
set.seed(617)
boost1119_4 = gbm(price~.,data=train,distribution="gaussian",
                  n.trees = 1000,interaction.depth = 40,shrinkage = 0.01)
predBoostTest = predict(boost1119_4,test,n.trees = 1000)
rmse(test$price,predBoostTest)
pred_scoringData = predict(boost1119_4, newdata=scoringData1,n.tree=1000)
summary(pred_scoringData)
submissionFile = data.frame(id = scoringData$id, price = pred_scoringData)
write.csv(submissionFile, 'submission_boost1119_4.csv',row.names = F)

# 4. Use LASSO 
library(glmnet)
x = model.matrix(price~.,data=train)
y = train$price
lassoModel = glmnet(x,y, alpha=1) 
plot(lassoModel,xvar='lambda',label=T)
set.seed(617)
cv.lasso = cv.glmnet(x,y,alpha=1)
c(cv.lasso$lambda.min, cv.lasso$lambda.1se)
lam = cv.lasso$lambda.1se
coef(lassoModel,lam)

# 5. use boosting with ADDING amentities keywords. with LASSO selection. RMSE 57.53801 
set.seed(617)
boost1119_3 = gbm(price~transit+host_is_superhost+host_total_listings_count+
                    neighbourhood_group_cleansed+zipcode+is_location_exact+
                    property_type+room_type+accommodates+bathrooms+bedrooms+
                    cleaning_fee+guests_included+extra_people+minimum_nights+
                    availability_60+availability_365+number_of_reviews+number_of_reviews_ltm+
                    last_review+review_scores_rating+review_scores_cleanliness+
                    review_scores_location+review_scores_value+cancellation_policy+
                    calculated_host_listings_count+TV+Elevator+Dryer,data=train,distribution="gaussian",
                  n.trees = 1000,interaction.depth = 40,shrinkage = 0.01)
predBoostTest = predict(boost1119_3,test,n.trees = 1000)
rmse(test$price,predBoostTest) 


