library(doBy)

summary(new_reviews)

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
new_reviews$U_Location <- NA
new_reviews$U_Eastern_Asian <- NA
new_reviews$U_Japanese <- NA
new_reviews$U_Mexican <- NA
new_reviews$U_Indian <- NA
new_reviews$U_Southeast_Asian <- NA
new_reviews$U_American <- NA
new_reviews$U_Middle_Eastern <- NA
new_reviews$U_Latin_American <- NA
new_reviews$U_Mediterranean <- NA
new_reviews$U_European <- NA
new_reviews$U_Fastfood <- NA
new_reviews$U_Pizza <- NA
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
    user_revs <- summaryBy(service+speed+cleanliness+taste+
                           environment+location+Eastern_Asian+Japanese+
                           Mexican+Indian+Southeast_Asian+American+Italian+
                           Middle_Eastern+Latin_American+Mediterranean+
                           European+Fastfood+Seafood+Pizza+Breakfast+
                           Cafe+Diners+Bars+other
                         ~ user_id, data=sub_new_reviews, FUN=c(mean,sd,length))
    
    new_reviews[row,]$U_Service <- user_revs$service.mean
    new_reviews[row,]$U_Speed <- user_revs$speed.mean
    new_reviews[row,]$U_Cleanliness <- user_revs$cleanliness.mean
    new_reviews[row,]$U_Environment <- user_revs$environment.mean
    new_reviews[row,]$U_Taste <- user_revs$taste.mean
    new_reviews[row,]$U_Location <- user_revs$location.mean
    new_reviews[row,]$U_Eastern_Asian <- user_revs$Eastern_Asian.mean
    new_reviews[row,]$U_Japanese <- user_revs$Japanese.mean
    new_reviews[row,]$U_Mexican <- user_revs$Mexican.mean
    new_reviews[row,]$U_Indian <- user_revs$Indian.mean
    new_reviews[row,]$U_Southeast_Asian <- user_revs$Southeast_Asian.mean
    new_reviews[row,]$U_American <- user_revs$American.mean
    new_reviews[row,]$U_Italian <- user_revs$Italian.mean
    new_reviews[row,]$U_Middle_Eastern <- user_revs$Middle_Eastern.mean
    new_reviews[row,]$U_Latin_American <- user_revs$Latin_American.mean
    new_reviews[row,]$U_Mediterranean <- user_revs$Mediterranean.mean
    new_reviews[row,]$U_European <- user_revs$European.mean
    new_reviews[row,]$U_Fastfood <- user_revs$Fastfood.mean
    new_reviews[row,]$U_Seafood <- user_revs$Seafood.mean
    new_reviews[row,]$U_Pizza <- user_revs$Pizza.mean
    new_reviews[row,]$U_Breakfast <- user_revs$Breakfast.mean
    new_reviews[row,]$U_Cafe <- user_revs$Cafe.mean
    new_reviews[row,]$U_Diners <- user_revs$Diners.mean
    new_reviews[row,]$U_Bars <- user_revs$Bars.mean
  }

  #perform summary of business id
  sub_new_reviews <- new_reviews[new_reviews$business_id == business_id & new_reviews$review_id != review_id,]
  if (nrow(sub_new_reviews) > 0)
  {  
    bus_revs <- summaryBy(service_rating+service+speed_rating+speed+cleanliness_rating+cleanliness+taste_rating+taste+environment_rating+environment+location_rating+location ~ business_id, data=sub_new_reviews, FUN=c(mean,sd,length,sum))
    
    new_reviews[row,]$B_Service <- bus_revs$service_rating.sum/bus_revs$service.sum
    new_reviews[row,]$B_Speed <- bus_revs$speed_rating.sum/bus_revs$speed.sum
    new_reviews[row,]$B_Cleanliness <- bus_revs$cleanliness_rating.sum/bus_revs$cleanliness.sum
    new_reviews[row,]$B_Taste <- bus_revs$taste_rating.sum/bus_revs$taste.sum
    new_reviews[row,]$B_Environment <- bus_revs$environment_rating.sum/bus_revs$environment.sum
    new_reviews[row,]$B_Location <- bus_revs$location_rating.sum/bus_revs$location.sum
  }
}


summary(new_reviews)

