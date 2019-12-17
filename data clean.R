library(jsonlite)
library(stringr)

business = stream_in(file("business.json"),)
business_flat = flatten(business)


cagu = business_flat[which(str_detect(business_flat$categories,"Restaurants")),]
write.csv(cagu,"Restaurant.csv")

cagu = read.csv("Restaurant.csv")

library(tidyverse)
cagu = cagu %>% filter(review_count>100)

## Missing Value analysis

missing = cagu %>% apply(MARGIN = 2, function(x){round(sum(is.na(x))/length(x)*100,2)}) %>% data.frame()
colnames(missing) = "Prop"
missing = missing %>% mutate(feature = rownames(missing)) %>% filter(Prop > 40)
del = missing$feature
cagu = select(cagu,-del)

write.csv(cagu,"Restaurant_clean.csv")
cagu = select(cagu,-c("hours.Monday","hours.Tuesday","hours.Wednesday","hours.Thursday","hours.Friday","hours.Saturday","hours.Sunday"))
cagu = mutate(cagu,Chinese = ifelse(str_detect(cagu$categories,"Chinese"),1,0))
cagu = mutate(cagu,French = ifelse(str_detect(cagu$categories,"French"),1,0))
cagu = mutate(cagu,Mexican = ifelse(str_detect(cagu$categories,"Mexican"),1,0))
cagu = mutate(cagu,Italian = ifelse(str_detect(cagu$categories,"Italian"),1,0))
cagu = mutate(cagu,Indian = ifelse(str_detect(cagu$categories,"Indian"),1,0))
cagu = mutate(cagu,Japanese = ifelse(str_detect(cagu$categories,"Japanese"),1,0))
cagu = mutate(cagu,American = ifelse(str_detect(cagu$categories,"American"),1,0))

cagu = mutate(cagu,attributes.GoodForMeal = str_count(cagu$attributes.GoodForMeal,"True"))
cagu = mutate(cagu,attributes.BusinessParking = str_count(attributes.BusinessParking,"True"))
cagu = mutate(cagu,attributes.Ambience = str_count(attributes.Ambience,"True"))

write.csv(cagu,"Restaurant_var.csv")

cagu = drop_na(cagu)

cagu

uniq2 = cagu %>% apply(MARGIN = 2,function(x){x %>% unique()})



tip = stream_in(file("user.json"),pagesize = 10000)
