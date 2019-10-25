library(doBy)

summary(new_reviews)

user_revs <- summaryBy(service+speed+cleanliness+taste+
                         environment+location+Eastern_Asian+Japanese+
                         Mexican+Indian+Southeast_Asian+American+Italian+
                         Middle_Eastern+Latin_American+Mediterranean+
                         European+Fastfood+Seafood+Pizza+Breakfast+
                         Cafe+Diners+Bars+other
                       ~ user_id, data=new_reviews, FUN=c(mean,sd,length))

user_revs$U_Service <- user_revs$service.mean
user_revs$U_Speed <- user_revs$speed.mean
user_revs$U_Cleanliness <- user_revs$cleanliness.mean
user_revs$U_Environment <- user_revs$environment.mean
user_revs$U_Taste <- user_revs$taste.mean
user_revs$U_Location <- user_revs$location.mean
user_revs$U_Eastern_Asian <- user_revs$Eastern_Asian.mean
user_revs$U_Japanese <- user_revs$Japanese.mean
user_revs$U_Mexican <- user_revs$Mexican.mean
user_revs$U_Indian <- user_revs$Indian.mean
user_revs$U_Southeast_Asian <- user_revs$Southeast_Asian.mean
user_revs$U_American <- user_revs$American.mean
user_revs$U_Italian <- user_revs$Italian.mean
user_revs$U_Middle_Eastern <- user_revs$Middle_Eastern.mean
user_revs$U_Latin_American <- user_revs$Latin_American.mean
user_revs$U_Mediterranean <- user_revs$Mediterranean.mean
user_revs$U_European <- user_revs$European.mean
user_revs$U_Fastfood <- user_revs$Fastfood.mean
user_revs$U_Seafood <- user_revs$Seafood.mean
user_revs$U_Pizza <- user_revs$Pizza.mean
user_revs$U_Breakfast <- user_revs$Breakfast.mean
user_revs$U_Cafe <- user_revs$Cafe.mean
user_revs$U_Diners <- user_revs$Diners.mean
user_revs$U_Bars <- user_revs$Bars.mean


new_reviews$service_rating <- new_reviews$service*(new_reviews$stars+new_reviews$ave_sentiment)
new_reviews$speed_rating <- new_reviews$speed*(new_reviews$stars+new_reviews$ave_sentiment)
new_reviews$cleanliness_rating <- new_reviews$cleanliness*(new_reviews$stars+new_reviews$ave_sentiment)
new_reviews$taste_rating <- new_reviews$taste*(new_reviews$stars+new_reviews$ave_sentiment)
new_reviews$environment_rating <- new_reviews$environment*(new_reviews$stars+new_reviews$ave_sentiment)
new_reviews$location_rating <- new_reviews$location*(new_reviews$stars+new_reviews$ave_sentiment)


bus_revs <- summaryBy(service_rating+service+speed_rating+speed+cleanliness_rating+cleanliness+taste_rating+taste+environment_rating+environment+location_rating+location ~ business_id, data=new_reviews, FUN=c(mean,sd,length,sum))

bus_revs$B_Service <- bus_revs$service_rating.sum/bus_revs$service.sum
bus_revs$B_Speed <- bus_revs$speed_rating.sum/bus_revs$speed.sum
bus_revs$B_Cleanliness <- bus_revs$cleanliness_rating.sum/bus_revs$cleanliness.sum
bus_revs$B_Taste <- bus_revs$taste_rating.sum/bus_revs$taste.sum
bus_revs$B_Environment <- bus_revs$environment_rating.sum/bus_revs$environment.sum
bus_revs$B_Location <- bus_revs$location_rating.sum/bus_revs$location.sum

new_reviews$Like_Restaurant <- 0
new_reviews[new_reviews$stars >= 4,]$Like_Restaurant <- 1

review_analysis_data <- merge(merge(new_reviews[,c('user_id','business_id','Like_Restaurant','Eastern_Asian',
                                                   'Japanese','Mexican','Indian','Southeast_Asian',
                                                   'American','Italian','Middle_Eastern','Latin_American',
                                                   'Mediterranean','European','Fastfood','Seafood',
                                                   'Pizza','Breakfast','Cafe','Diners','Bars')],
                                          user_revs[,c('user_id','U_Service','U_Speed','U_Cleanliness',
                                                       'U_Taste','U_Environment','U_Location','U_Eastern_Asian',
                                                       'U_Japanese','U_Mexican','U_Indian','U_Southeast_Asian',
                                                       'U_American','U_Italian','U_Middle_Eastern','U_Latin_American',
                                                       'U_Mediterranean','U_European','U_Fastfood','U_Seafood',
                                                       'U_Pizza','U_Breakfast','U_Cafe','U_Diners','U_Bars')],by='user_id')
                                    ,bus_revs[,c('business_id','B_Service','B_Speed','B_Cleanliness',
                                                 'B_Taste','B_Environment','B_Location')],by='business_id')


summary(review_analysis_data)

