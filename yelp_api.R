client_id <- "CijTUKOxsKUIqWgLQQmo8A"
api <- "kTfxBsvITkcBwuxs9qEg5yXIwPuRSlcEEQeWVbJ45l1IZ6VH9GoRczuqO00p_g_l8w-405oqjbnEq_DBAuIx55G4wuEao9zGVF7ltjEqfqGWw-Cl7u9MpKZbYfHfXXYx"

library(yelpr)
library(jsonlite)

business_ny <- business_search(api_key = api,
                               location = 'Boston',limit = 50)

data = business_ny$businesses

business_d

business.cam = business_search(api_key = api,
                           location = 'Campbidge MA',limit = 50,term = "restaurants",
                           attributes = "GoodForKids,RestaurantsReservations,
                           GoodForMeal,BusinessParking,Caters,NoiseLevel,RestaurantsPriceRange2,
                           OutdoorSeating,BikeParking,Ambience,HasTV,Alcohol,BusinessAcceptsCreditCards")
business.cam = business.cam$businesses
business.all = business
zipcode = seq(02108,02137,1)
zipcode = paste0("0",zipcode)
zipcode = append(zipcode,values = c(02163,02199,02210,02215))