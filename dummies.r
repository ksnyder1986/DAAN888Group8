library("RPostgreSQL")
library(DT)
library(tidytext)
library(dplyr)
library(stringr)
library(sentimentr)
library(ggplot2)
library(RColorBrewer)
library(readr)
library(SnowballC)
library(tm)
library(wordcloud)
library(reticulate)
library(crfsuite)
library(ggplot2)
library(textcat)
library(broom)
library(caTools)
library(rpart)
library(rpart.plot)
library(stargazer)
library(traitr)
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "DAAN888",  host = "localhost", port = 5432, user = "postgres", password = "")
categories <- dbGetQuery(con, 'Select cats.* from public."Business_Category" cats inner join public."Pitt_Review_Data" revs on cats.business_id = revs.business_id')

#Broader Categories
Eastern_Asian <- c('Chinese'
                  ,'Szechuan'
                  ,'Taiwanese'
                  ,'Dim Sum'
                  ,'Cantonese'
                  ,'Korean'
                  ,'Asian Fusion'
                  ,'Pan Asian'
                  ,'Mongolian'
                  ,'Uzbek')

Japanese <- c('Sushi Bars','Japanese','Ramen')

Mexican <- c('Mexican',	'Tex-Mex',	'Tacos',	'New-Mexican Cuisine')

Indian <- c('Pakistani',	'Indian',	'Himalayan/Nepalese')

Southeast_Asian <- c('Vietnamese'
                     ,'Thai'
                     ,'Burmese'
                     ,'Cambodian'
                     ,'Filipino'
                     ,'Bangladeshi'
                     ,'Indonesian'
                     ,'Laotian'
                     ,'Malaysian'
                     ,'Singaporean')

American <- c('American'
              ,'American'
              ,'Southern')




Italian <- c('Italian','Sicilian')

Middle_Eastern <- c('Middle Eastern'
                    ,'Halal'
                    ,'Moroccan'
                    ,'Egyptian'
                    ,'Persian/Iranian'
                    ,'Syrian')


Latin_American <- c('Latin American'
                    ,'Brazilian'
                    ,'Venezuelan'
                    ,'Argentine'
                    ,'Colombian'
                    ,'Caribbean'
                    ,'Cuban')

Mediterranean <- c('Greek'
                   ,'Turkish'
                   ,'Lebanese')

European <- c('Irish'
              ,'Modern European'
              ,'Polish'
              ,'German'
              ,'Spanish'
              ,'Portuguese'
              ,'Basque'
              ,'British'
              ,'Hungarian'
              ,'Irish Pub'
              ,'Iberian'
              ,'French')

Fastfood <- 'Fast Food'
Seafood <- 'Seafood'
Pizza <- 'Pizza'
Breakfast <- c('Breakfast','Waffles')
Cafe <- c('Caf')
Diners <- 'Diner'
Bars <- c('Pubs'
          ,'Beer'
          ,'Sports'
          ,'Bars'
          ,'Gastropubs'
          ,'Breweries'
          ,'Beer'
          ,'Gardens'
          ,'Brewpubs'
          ,'Speakeasies'
          ,'Coktail Bars')

categories$Eastern_Asian <- 0 
categories$Eastern_Asian[grep(paste(Eastern_Asian,collapse='|'),categories$categories)] <- 1

categories$Japanese <- 0 
categories$Japanese[grep(paste(Japanese,collapse='|'),categories$categories)] <- 1

categories$Mexican <- 0 
categories$Mexican[grep(paste(Mexican,collapse='|'),categories$categories)] <- 1

categories$Indian <- 0 
categories$Indian[grep(paste(Indian,collapse='|'),categories$categories)] <- 1

categories$Indian <- 0 
categories$Indian[grep(paste(Indian,collapse='|'),categories$categories)] <- 1

categories$Southeast_Asian <- 0 
categories$Southeast_Asian[grep(paste(Southeast_Asian,collapse='|'),categories$categories)] <- 1

categories$American <- 0 
categories$American[grep(paste(American,collapse='|'),categories$categories)] <- 1

categories$Italian <- 0 
categories$Italian[grep(paste(Italian,collapse='|'),categories$categories)] <- 1

categories$Middle_Eastern <- 0 
categories$Middle_Eastern[grep(paste(Middle_Eastern,collapse='|'),categories$categories)] <- 1

categories$Latin_American <- 0 
categories$Latin_American[grep(paste(Latin_American,collapse='|'),categories$categories)] <- 1

categories$Mediterranean <- 0 
categories$Mediterranean[grep(paste(Mediterranean,collapse='|'),categories$categories)] <- 1

categories$European <- 0 
categories$European[grep(paste(European,collapse='|'),categories$categories)] <- 1

categories$Fastfood <- 0 
categories$Fastfood[grep(paste(Fastfood,collapse='|'),categories$categories)] <- 1

categories$Seafood <- 0 
categories$Seafood[grep(paste(Seafood,collapse='|'),categories$categories)] <- 1

categories$Pizza <- 0 
categories$Pizza[grep(paste(Pizza,collapse='|'),categories$categories)] <- 1

categories$Breakfast <- 0 
categories$Breakfast[grep(paste(Breakfast,collapse='|'),categories$categories)] <- 1

categories$Cafe <- 0 
categories$Cafe[grep(paste(Cafe,collapse='|'),categories$categories)] <- 1

categories$Diners <- 0 
categories$Diners[grep(paste(Diners,collapse='|'),categories$categories)] <- 1

categories$Bars <- 0 
categories$Bars[grep(paste(Bars,collapse='|'),categories$categories)] <- 1

cats <- categories %>% group_by(business_id) %>% summarise(Eastern_Asian = max(Eastern_Asian),
                                                   Japanese = max(Japanese),
                                                   Mexican = max(Mexican),
                                                   Indian = max(Indian),
                                                   Southeast_Asian = max(Southeast_Asian),
                                                   American = max(American),
                                                   Italian = max(Italian),
                                                   Middle_Eastern = max(Middle_Eastern),
                                                   Latin_American = max(Latin_American),
                                                   Mediterranean = max(Mediterranean),
                                                   European = max(European),
                                                   Fastfood = max(Fastfood),
                                                   Seafood = max(Seafood),
                                                   Pizza = max(Pizza),
                                                   Breakfast = max(Breakfast),
                                                   Cafe = max(Cafe),
                                                   Diners = max(Diners),
                                                   Bars = max(Bars))

cats$other <- 0
cats$other[cats$Japanese == 0 & cats$Eastern_Asian == 0 & cats$Mexican == 0 &
             cats$Indian == 0 & cats$Southeast_Asian == 0 & cats$American == 0 &
             cats$Middle_Eastern == 0 & cats$Latin_American == 0 & cats$Italian == 0 & 
             cats$Mediterranean == 0 & cats$European == 0] <- 1


#Build Review Words Categories
reviews <- read_csv("/Users/Kris/Box Sync/Kris Penn State/DAAN 888/ReviewsPAwithSentiScores.csv")
reviews$text <- tolower(reviews$text)

service_words = c('service'
                 ,'bartender'
                 ,'waiter'
                 ,'waitress'
                 ,'server'
                 ,'waitperson'
                 ,'carhop'
                 ,'host'
                 ,'hostess'
                 ,'staff'
                 ,'employee'
                 ,'employees'
                 ,'help'
                 ,'mean'
                 ,'rude'
                 ,'abusive'
                 ,'blunt'
                 ,'boorish'
                 ,'coarse'
                 ,'crude'
                 ,'ignorant'
                 ,'impolite'
                 ,'insulting'
                 ,'obscene'
                 ,'vulgar'
                 ,'bad-mannered'
                 ,'badmannered'
                 ,'bad mannered'
                 ,'curt'
                 ,'gruff'
                 ,'inconsiderate'
                 ,'kind'
                 ,'amiable'
                 ,'compassionate'
                 ,'considerate'
                 ,'cordial'
                 ,'courteous'
                 ,'friendly'
                 ,'unfriendly'
                 ,'gracious'
                 ,'kindhearted'
                 ,'kindly'
                 ,'loving'
                 ,'thoughtful'
                 ,'polite'
                 ,'attentive'
                 )
reviews$service <- 0 
reviews$service[grep(paste(service_words,collapse='|'),reviews$text)] <- 1
summary(reviews$service)

speed = c('slow'
          ,'gradual'
          ,'heavy'
          ,'lackadaisical'
          ,'leisurely'
          ,'lethargic'
          ,'passive'
          ,'reluctant'
          ,'sluggish'
          ,'stagnant'
          ,'crawling'
          ,'creeping'
          ,'dawdling'
          ,'delaying'
          ,'deliberate'
          ,'disinclined'
          ,'idle'
          ,'lagging'
          ,'loitering'
          ,'plodding'
          ,'postponing'
          ,'procrastinating'
          ,'slack'
          ,'apathetic'
          ,'dilatory'
          ,'dreamy'
          ,'drowsy'
          ,'imperceptible'
          ,'inactive'
          ,'indolent'
          ,'inert'
          ,'laggard'
          ,'leaden'
          ,'listless'
          ,'phlegmatic'
          ,'ponderous'
          ,'remiss'
          ,'sleepy'
          ,'slothful'
          ,'slow-moving'
          ,'snaillike'
          ,'supine'
          ,'tardy'
          ,'torpid'
          ,'tortoiselike'
          ,'fast'
          ,'agile'
          ,'brisk'
          ,'nimble'
          ,'quick'
          ,'rapid'
          ,'swift'
          ,'accelerated'
          ,'active'
          ,'dashing'
          ,'electric'
          ,'flashing'
          ,'fleet'
          ,'fleeting'
          ,'flying'
          ,'hurried'
          ,'racing'
          ,'snap'
          ,'winged'
          ,'blue streak'
          ,'breakneck'
          ,'chop-chop'
          ,'double-time'
          ,'expeditious'
          ,'expeditive'
          ,'hairtrigger'
          ,'hasty'
          ,'hypersonic'
          ,'in a jiffy'
          ,'in nothing flat'
          ,'lickety split'
          ,'like a bat out of hell'
          ,'like all get out'
          ,'like crazy'
          ,'like mad'
          ,'on the double'
          ,'posthaste'
          ,'presto'
          ,'pronto'
          ,'snappy'
          ,'speedball'
          ,'supersonic'
          ,'velocious')

reviews$speed <- 0
reviews$speed[grep(paste(speed,collapse='|'),reviews$text)] <- 1
summary(reviews$speed)

clean <- c('clean'
           ,'spotless'
           ,'hygienic'
           ,'orderly'
           ,'neat'
           ,'tidy'
           ,'unblemished'
           ,'washed'
           ,'cleaned'
           ,'shining'
           ,'stained'
           ,'dirty'
           ,'disinfected'
           ,'sanitary'
           ,'contaminated'
           ,'dusty'
           ,'filth'
           ,'filthy'
           ,'greasy'
           ,'grimy'
           ,'messy'
           ,'muddy'
           ,'murky'
           ,'nasty'
           ,'polluted'
           ,'sloppy'
           ,'unkempt'
           ,'begrimed'
           ,'smudged'
           ,'smudge'
           ,'sullied'
           ,'unsanitary'
           ,'unsightly'
           ,'sick'
           ,'slimy')

reviews$cleanliness <- 0
reviews$cleanliness[grep(paste(clean,collapse='|'),reviews$text)] <- 1
summary(reviews$cleanliness)

tasty <- c('tasty'
           ,'taste'
           ,'appetizing'
           ,'delectable'
           ,'flavorful'
           ,'luscious'
           ,'pungent'
           ,'savory'
           ,'spicy'
           ,'yummy'
           ,'delish'
           ,'divine'
           ,'flavorsome'
           ,'flavory'
           ,'full-flavored'
           ,'good-tasting'
           ,'heavenly'
           ,'mellow'
           ,'palatable'
           ,'piquant'
           ,'sapid'
           ,'scrumptious'
           ,'sugar-coated'
           ,'sweetened'
           ,'tasteful'
           ,'toothsome'
           ,'toothy'
           ,'zestful'
           ,'bland'
           ,'distasteful'
           ,'dull'
           ,'flavorless'
           ,'offensive'
           ,'tasteless'
           ,'unappetizing'
           ,'unsavory'
           ,'appetizing'
           ,'delectable'
           ,'delightful'
           ,'distinctive'
           ,'enjoyable'
           ,'enticing'
           ,'exquisite'
           ,'heavenly'
           ,'luscious'
           ,'piquant'
           ,'savory'
           ,'spicy'
           ,'sweet'
           ,'tempting'
           ,'yummy'
           ,'yum'
           ,'dainty'
           ,'darling'
           ,'divine'
           ,'adorable'
           ,'ambrosial'
           ,'delish'
           ,'fit for king'
           ,'gratifying'
           ,'mouthwatering'
           ,'nectarous'
           ,'palatable'
           ,'sapid'
           ,'scrumptious'
           ,'tasteful'
           ,'titillating'
           ,'toothsome'
           ,'well-prepared'
           ,'well-seasoned'
           ,'aftertaste'
           ,'bitter'
           ,'sour'
           ,'savor'
           ,'unsavor'
           ,'salty'
           ,'zest'
           ,'flavorless'
           ,'gross'
           ,'delicious'
)

reviews$taste <- 0
reviews$taste[grep(paste(tasty,collapse='|'),reviews$text)] <- 1
summary(reviews$taste)

environment <- c('seating'
                 ,'table'
                 ,'seat'
                 ,'stool'
                 ,'booth'
                 ,'outside'
                 ,'inside'
                 ,'deck'
                 ,'porch'
                 ,'ambiance'
                 ,'music'
                 ,'noise'
                 ,'loud'
                 ,'deafening'
                 ,'rowdy'
                 ,'calm'
                 ,'peaceful'
                 ,'secluded'
                 ,'serene'
                 ,'quiet'
                 ,'private'
                 ,'comfortable'
                 ,'atmosphere'
                 ,'space'
                 ,'environment'
                 ,'climate'
                 ,'setting'
                 ,'cozy'
                 ,'relaxed'
                 ,'serene'
                 ,'relaxing'
                 ,'beautiful'
                 ,'view'
                 ,'charming')

reviews$environment <- 0
reviews$environment[grep(paste(environment,collapse='|'),reviews$text)] <- 1
summary(reviews$environment)

location <- c('parking'
              ,'site'
              ,'garage'
              ,'location'
              ,'convenient'
              ,'neighborhood')

reviews$location <- 0
reviews$location[grep(paste(location,collapse='|'),reviews$text)] <- 1
summary(reviews$location)

new_reviews <- merge(reviews,cats,by='business_id')
summary(new_reviews)

