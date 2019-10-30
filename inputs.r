library(doBy)
library(dplyr)

summary(new_reviews)

user_list <- summaryBy(stars ~ user_id, data=new_reviews2, FUN = c(length))
user_list <- user_list[user_list$stars.length > 2,]
new_reviews <- new_reviews2 %>% filter(user_id %in% user_list$user_id)


user_revs <- summaryBy(service+speed+cleanliness+taste+
                         environment+location+Eastern_Asian+Japanese+
                         Mexican+Indian+Southeast_Asian+American+Italian+
                         Middle_Eastern+Latin_American+Mediterranean+
                         European+Fastfood+Seafood+Pizza+Breakfast+
                         Cafe+Diners+Bars+other
                       ~ user_id, data=new_reviews, FUN=c(mean,length))


new_reviews$service_rating <- new_reviews$service*(new_reviews$stars+new_reviews$ave_sentiment)
new_reviews$speed_rating <- new_reviews$speed*(new_reviews$stars+new_reviews$ave_sentiment)
new_reviews$cleanliness_rating <- new_reviews$cleanliness*(new_reviews$stars+new_reviews$ave_sentiment)
new_reviews$taste_rating <- new_reviews$taste*(new_reviews$stars+new_reviews$ave_sentiment)
new_reviews$environment_rating <- new_reviews$environment*(new_reviews$stars+new_reviews$ave_sentiment)
new_reviews$location_rating <- new_reviews$location*(new_reviews$stars+new_reviews$ave_sentiment)


bus_revs <- summaryBy(service_rating+speed_rating+cleanliness_rating+taste_rating+environment_rating+location_rating ~ business_id, data=new_reviews, FUN=c(mean,sd,length,sum))


new_reviews$Like_Restaurant <- 0
new_reviews[new_reviews$stars >= 4,]$Like_Restaurant <- 1

review_analysis_data <- merge(merge(new_reviews,
                                    user_revs,by='user_id')
                              ,bus_revs,by='business_id')


summary(review_analysis_data)

review_analysis_data$U_service <- ((review_analysis_data$service.mean*review_analysis_data$service.length) - review_analysis_data$service) / (review_analysis_data$service.length-1)
review_analysis_data$U_environment<- ((review_analysis_data$environment.mean*review_analysis_data$environment.length) - review_analysis_data$environment) / (review_analysis_data$environment.length-1)
review_analysis_data$U_Mexican<- ((review_analysis_data$Mexican.mean*review_analysis_data$Mexican.length) - review_analysis_data$Mexican) / (review_analysis_data$Mexican.length-1)
review_analysis_data$U_Middle_Eastern<- ((review_analysis_data$Middle_Eastern.mean*review_analysis_data$Middle_Eastern.length) - review_analysis_data$Middle_Eastern) / (review_analysis_data$Middle_Eastern.length-1)
review_analysis_data$U_European<- ((review_analysis_data$European.mean*review_analysis_data$European.length) - review_analysis_data$European) / (review_analysis_data$European.length-1)
review_analysis_data$U_Cafe<- ((review_analysis_data$Cafe.mean*review_analysis_data$Cafe.length) - review_analysis_data$Cafe) / (review_analysis_data$Cafe.length-1)
review_analysis_data$U_speed<- ((review_analysis_data$speed.mean*review_analysis_data$speed.length) - review_analysis_data$speed) / (review_analysis_data$speed.length-1)
review_analysis_data$U_location<- ((review_analysis_data$location.mean*review_analysis_data$location.length) - review_analysis_data$location) / (review_analysis_data$location.length-1)
review_analysis_data$U_Indian<- ((review_analysis_data$Indian.mean*review_analysis_data$Indian.length) - review_analysis_data$Indian) / (review_analysis_data$Indian.length-1)
review_analysis_data$U_Latin_American<- ((review_analysis_data$Latin_American.mean*review_analysis_data$Latin_American.length) - review_analysis_data$Latin_American) / (review_analysis_data$Latin_American.length-1)
review_analysis_data$U_Fastfood<- ((review_analysis_data$Fastfood.mean*review_analysis_data$Fastfood.length) - review_analysis_data$Fastfood) / (review_analysis_data$Fastfood.length-1)
review_analysis_data$U_Diners<- ((review_analysis_data$Diners.mean*review_analysis_data$Diners.length) - review_analysis_data$Diners) / (review_analysis_data$Diners.length-1)
review_analysis_data$U_cleanliness<- ((review_analysis_data$cleanliness.mean*review_analysis_data$cleanliness.length) - review_analysis_data$cleanliness) / (review_analysis_data$cleanliness.length-1)
review_analysis_data$U_Eastern_Asian<- ((review_analysis_data$Eastern_Asian.mean*review_analysis_data$Eastern_Asian.length) - review_analysis_data$Eastern_Asian) / (review_analysis_data$Eastern_Asian.length-1)
review_analysis_data$U_Southeast_Asian<- ((review_analysis_data$Southeast_Asian.mean*review_analysis_data$Southeast_Asian.length) - review_analysis_data$Southeast_Asian) / (review_analysis_data$Southeast_Asian.length-1)
review_analysis_data$U_Mediterranean<- ((review_analysis_data$Mediterranean.mean*review_analysis_data$Mediterranean.length) - review_analysis_data$Mediterranean) / (review_analysis_data$Mediterranean.length-1)
review_analysis_data$U_Seafood<- ((review_analysis_data$Seafood.mean*review_analysis_data$Seafood.length) - review_analysis_data$Seafood) / (review_analysis_data$Seafood.length-1)
review_analysis_data$U_Bars<- ((review_analysis_data$Bars.mean*review_analysis_data$Bars.length) - review_analysis_data$Bars) / (review_analysis_data$Bars.length-1)
review_analysis_data$U_taste<- ((review_analysis_data$taste.mean*review_analysis_data$taste.length) - review_analysis_data$taste) / (review_analysis_data$taste.length-1)
review_analysis_data$U_Japanese<- ((review_analysis_data$Japanese.mean*review_analysis_data$Japanese.length) - review_analysis_data$Japanese) / (review_analysis_data$Japanese.length-1)
review_analysis_data$U_American<- ((review_analysis_data$American.mean*review_analysis_data$American.length) - review_analysis_data$American) / (review_analysis_data$American.length-1)
review_analysis_data$U_Italian<- ((review_analysis_data$Italian.mean*review_analysis_data$Italian.length) - review_analysis_data$Italian) / (review_analysis_data$Italian.length-1)
review_analysis_data$U_Pizza<- ((review_analysis_data$Pizza.mean*review_analysis_data$Pizza.length) - review_analysis_data$Pizza) / (review_analysis_data$Pizza.length-1)
review_analysis_data$U_Breakfast<- ((review_analysis_data$Breakfast.mean*review_analysis_data$Breakfast.length) - review_analysis_data$Breakfast) / (review_analysis_data$Breakfast.length-1)

review_analysis_data$B_Service <- (review_analysis_data$service_rating.sum - review_analysis_data$service_rating) /(review_analysis_data$service_rating.length-1)
review_analysis_data$B_Speed <- (review_analysis_data$speed_rating.sum - review_analysis_data$speed_rating) /(review_analysis_data$speed_rating.length-1)
review_analysis_data$B_Cleanliness <- (review_analysis_data$cleanliness_rating.sum - review_analysis_data$cleanliness_rating) /(review_analysis_data$cleanliness_rating.length-1)
review_analysis_data$B_Taste <- (review_analysis_data$taste_rating.sum - review_analysis_data$taste_rating) /(review_analysis_data$taste_rating.length-1)
review_analysis_data$B_Environment <- (review_analysis_data$environment_rating.sum - review_analysis_data$environment_rating) /(review_analysis_data$environment_rating.length-1)
review_analysis_data$B_Location <- (review_analysis_data$location_rating.sum - review_analysis_data$location_rating) /(review_analysis_data$location_rating.length-1)
