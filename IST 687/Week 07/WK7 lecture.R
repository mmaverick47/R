library(ggplot2)
library(maps)
dummyDF <- data.frame(state.name, stringsAsFactors = FALSE)
dummyDF$state <- tolower(dummyDF$state.name)

us <- map_data("state")

map.simple <- ggplot(dummyDF, aes(map_id=state))

map.simple <- map.simple + 
  geom_map(map = us, fill = "white", + color = "black")

cities <- c("Manhattan, NY", "Boston, MA", "Philadelphia, PA", 
            "Tampa, FL", "Chicago, IL", "Boise, ID", 
            "San Francisco, CA", "Seattle, WA", "Houston, TX")
bus <- c(10,7,6,5,7,3,10,7,5)
weather <- c(5,3,6,7,3,6,10,7,2)
living <- c(7,6,6,7,5,4,6,8,2)
city.df <- data.frame(cities,bus,weather,living)
city.df$state <- statesFake
city.df$geoCode <- geocode(cities)

