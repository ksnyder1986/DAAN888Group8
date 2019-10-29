library(doBy)
library(dplyr)

summary(new_reviews)

user_list <- summaryBy(stars ~ user_id, data=new_reviews, FUN = c(length))
user_list <- user_list[user_list$stars.length > 3,]
new_reviews <- new_reviews %>% filter(user_id %in% user_list$user_id)

summaryby()

new_reviews$service_rating <- new_reviews$service*(new_reviews$stars+new_reviews$ave_sentiment)
new_reviews$speed_rating <- new_reviews$speed*(new_reviews$stars+new_reviews$ave_sentiment)
new_reviews$cleanliness_rating <- new_reviews$cleanliness*(new_reviews$stars+new_reviews$ave_sentiment)
new_reviews$taste_rating <- new_reviews$taste*(new_reviews$stars+new_reviews$ave_sentiment)
new_reviews$environment_rating <- new_reviews$environment*(new_reviews$stars+new_reviews$ave_sentiment)
new_reviews$location_rating <- new_reviews$location*(new_reviews$stars+new_reviews$ave_sentiment)

new_reviews$Like_Restaurant <- 0
new_reviews[new_reviews$stars >= 4,]$Like_Restaurant <- 1

new_reviews$U_Service <- NA
new_reviews$U_Speed <- NA
new_reviews$U_Cleanliness <- NA
new_reviews$U_Environment <- NA
new_reviews$U_Taste <- NA
new_reviews$U_Location <- NA
new_reviews$U_Eastern_Asian <- NA
new_reviews$U_Japanese <- NA
new_reviews$U_Mexican <- NA
new_reviews$U_Indian <- NA
new_reviews$U_Italian <- NA
new_reviews$U_Southeast_Asian <- NA
new_reviews$U_American <- NA
new_reviews$U_Middle_Eastern <- NA
new_reviews$U_Latin_American <- NA
new_reviews$U_Mediterranean <- NA
new_reviews$U_European <- NA
new_reviews$U_Fastfood <- NA
new_reviews$U_Pizza <- NA
new_reviews$U_Seafood <- NA
new_reviews$U_Breakfast <- NA
new_reviews$U_Cafe <- NA
new_reviews$U_Diners <- NA
new_reviews$U_Bars <- NA
new_reviews$B_Service <- NA
new_reviews$B_Speed <- NA
new_reviews$B_Cleanliness <- NA
new_reviews$B_Taste <- NA
new_reviews$B_Environment <- NA
new_reviews$B_Location <- NA

for (row in 1:nrow(new_reviews))
{
  review_id = new_reviews[row, "review_id"]
  user_id   = new_reviews[row, "user_id"]
  business_id = new_reviews[row, "business_id"]
  
  #perform summary of user_id
  sub_new_reviews <- new_reviews[new_reviews$user_id == user_id & new_reviews$review_id != review_id,]
  if (nrow(sub_new_reviews) > 0)
  {

    new_reviews[row,]$U_Service <- mean(sub_new_reviews$service)
    new_reviews[row,]$U_Speed <- mean(sub_new_reviews$speed)
    new_reviews[row,]$U_Cleanliness <- mean(sub_new_reviews$cleanliness)
    new_reviews[row,]$U_Environment <- mean(sub_new_reviews$environment)
    new_reviews[row,]$U_Taste <- mean(sub_new_reviews$taste)
    new_reviews[row,]$U_Location <- mean(sub_new_reviews$location)
    new_reviews[row,]$U_Eastern_Asian <- mean(sub_new_reviews$Eastern_Asian)
    new_reviews[row,]$U_Japanese <- mean(sub_new_reviews$Japanese)
    new_reviews[row,]$U_Mexican <- mean(sub_new_reviews$Mexican)
    new_reviews[row,]$U_Indian <- mean(sub_new_reviews$Indian)
    new_reviews[row,]$U_Southeast_Asian <- mean(sub_new_reviews$Southeast_Asian)
    new_reviews[row,]$U_American <- mean(sub_new_reviews$American)
    new_reviews[row,]$U_Italian <- mean(sub_new_reviews$Italian)
    new_reviews[row,]$U_Middle_Eastern <- mean(sub_new_reviews$Middle_Eastern)
    new_reviews[row,]$U_Latin_American <- mean(sub_new_reviews$Latin_American)
    new_reviews[row,]$U_Mediterranean <- mean(sub_new_reviews$Mediterranean)
    new_reviews[row,]$U_European <- mean(sub_new_reviews$European)
    new_reviews[row,]$U_Fastfood <- mean(sub_new_reviews$Fastfood)
    new_reviews[row,]$U_Seafood <- mean(sub_new_reviews$Seafood)
    new_reviews[row,]$U_Pizza <- mean(sub_new_reviews$Pizza)
    new_reviews[row,]$U_Breakfast <- mean(sub_new_reviews$Breakfast)
    new_reviews[row,]$U_Cafe <- mean(sub_new_reviews$Cafe)
    new_reviews[row,]$U_Diners <- mean(sub_new_reviews$Diners)
    new_reviews[row,]$U_Bars <- mean(sub_new_reviews$Bars)
  }

  #perform summary of business id
  sub_new_reviews <- new_reviews[new_reviews$business_id == business_id & new_reviews$review_id != review_id,]
  if (nrow(sub_new_reviews) > 0)
  {  

    new_reviews[row,]$B_Service <- sum(sub_new_reviews$service_rating)/sum(sub_new_reviews$service)
    new_reviews[row,]$B_Speed <- sum(sub_new_reviews$speed_rating)/sum(sub_new_reviews$speed)
    new_reviews[row,]$B_Cleanliness <- sum(sub_new_reviews$cleanliness_rating)/sum(sub_new_reviews$cleanliness)
    new_reviews[row,]$B_Taste <- sum(sub_new_reviews$taste_rating)/sum(sub_new_reviews$taste)
    new_reviews[row,]$B_Environment <- sum(sub_new_reviews$environment_rating)/sum(sub_new_reviews$environment)
    new_reviews[row,]$B_Location <- sum(sub_new_reviews$location_rating)/sum(sub_new_reviews$location)
  }
  print(row)
}


summary(new_reviews)

