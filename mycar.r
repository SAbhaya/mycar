library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)

data <- read_csv("data.csv")


# select only required columns and convert date, add cost column?? required??
# changed date from datetime format.

data2 <- data %>% 
  select(refuelDate, quantity,distance,price,fuel_station,fuel_subtype) %>%
  mutate(refuelDate = date(ymd_hms(refuelDate)), cost = quantity*price/100) %>%
  arrange(refuelDate)


#add dummy fuel station to missing values not very sensitive and can use for calculation


data2$fuel_station[is.na(data2$fuel_station)] <- "noname"

#unit& price


# boxplot for cost
ggplot(data2, aes(x=1, y=cost)) + geom_boxplot() 

#box plot for price

ggplot(data2, aes(x=1, y=price)) + geom_boxplot() 

#pricing~fuel station
ggplot(data2, aes(x=fuel_station, y=price)) + geom_boxplot() + theme(axis.text.x=element_text(angle=90, hjust=1))

#pricing~fuel station > n observation

data2 %>%
  group_by(fuel_station) %>%
  count() %>%
  arrange(desc(n))

#plot

p <- data2 %>%
  group_by(fuel_station) %>%
  count() %>%
  ggplot(aes(x= reorder(fuel_station, -n), y=n)) + geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



top_stations <- data2 %>% 
  count(fuel_station) %>%
  filter(n > 5) %>%
  .$fuel_station

# Box plot for >5 fuel stations

data2 %>% 
  filter(fuel_station %in% top_stations) %>%
  ggplot(aes(x=fuel_station, y=price)) + geom_boxplot() + theme(axis.text.x=element_text(angle=90, hjust=1)) + facet_grid(~fuel_subtype)

ggplot(data2, aes(x=fuel_subtype, y=price, fill = fuel_subtype)) + geom_boxplot(alpha = 0.5) + theme(axis.text.x=element_text(angle=90, hjust=1))


ggplot(data2, aes(x=cost)) + geom_density() 
ggplot(data2, aes(x= price, fill = fuel_subtype)) + geom_density(alpha = 0.3) + facet_grid(~fuel_subtype)

#omit na fuel_subtype data

# density plot
data2 %>% 
  na.omit() %>%
  ggplot(aes(x= price, fill = fuel_subtype)) + geom_density(alpha = 0.3) + facet_grid(~fuel_subtype)   


# summarise
data2 %>% 
  na.omit() %>%
  group_by(fuel_subtype) %>%
  summarise(refuels = n(), mean(price), median(price), sd(price), IQR(price))


# box plot for price per fuel subtype
data2 %>% 
  na.omit() %>%
  ggplot(aes(x= fuel_subtype, y= price)) + geom_boxplot()

#price variation over the time


data2 %>%
  na.omit() %>%
  ggplot(aes(x= refuelDate, y= price, col = fuel_subtype)) + geom_line() + facet_grid(~fuel_subtype)

#with trendline
data2 %>%
  na.omit() %>%
  ggplot(aes(x= refuelDate, y= price, col = fuel_subtype)) + geom_point() + geom_smooth(method = "lm") + facet_grid(~fuel_subtype)



#calculation of fuel efficiency
#need to l/distance*100 for l/100km
#once calculate column to be shifted to make relavent to previous fuel type
#assumptions:
# 1. distance travelled using previous refuel
# 2. refuel amount is equal to the fuel usage for distance travelled = fuel_eff
# 3. fuel_eff is corresponds to the previous refuel, so need to shift
#
#
data_eff <- data2 %>%
  mutate(fuel_eff = lead(quantity/distance*100))


data_eff %>% 
  filter(fuel_eff > 5, fuel_eff < 12) %>%
  na.omit() %>%
  ggplot(aes(x= fuel_subtype, y= fuel_eff)) + geom_boxplot()

data_eff %>% 
  filter(fuel_eff > 5, fuel_eff < 12) %>%
  na.omit() %>%
  ggplot(aes(x= fuel_eff, fill = fuel_subtype)) + geom_density(alpha = .3) +
  facet_grid(~fuel_subtype)

data_eff %>% 
  filter(fuel_eff > 4, fuel_eff < 15) %>%
  na.omit() %>%
  ggplot(aes(x= fuel_eff, fill = fuel_subtype)) + geom_histogram() +
  facet_grid(~fuel_subtype)

data_eff %>% 
  filter(fuel_eff > 4, fuel_eff < 12) %>%
  na.omit() %>%
  ggplot(aes(x= refuelDate, y= fuel_eff, col = fuel_subtype)) + geom_point() +geom_smooth(method = "lm")+ facet_grid(~fuel_subtype)


data_eff %>% 
  filter(fuel_eff > 4, fuel_eff < 15) %>%
  na.omit() %>%
  ggplot(aes(x= refuelDate, y= fuel_eff)) + geom_line() + geom_smooth()

# summarise
data_eff %>% 
  filter(fuel_eff > 5, fuel_eff < 12) %>%
  na.omit() %>%
  group_by(fuel_subtype) %>%
  summarise(refuels = n(), 
            mean(price),median(price), 
            mean(fuel_eff), median(fuel_eff), 
            sd(price),sd(fuel_eff), 
            IQR(price),IQR(fuel_eff))

data_eff %>% 
  filter(fuel_eff > 4, fuel_eff < 15) %>%
  na.omit() %>%
  group_by(fuel_subtype) %>%
  summarise(refuels = n(), 
            min(fuel_eff), mean(fuel_eff), median(fuel_eff), max(fuel_eff), IQR(fuel_eff))



data_eff %>% 
  filter(fuel_eff > 4, fuel_eff < 15) %>%
  na.omit() %>%
  group_by(fuel_subtype) %>%
  summarise(refuels = n(), 
            mean(cost), mean(fuel_eff),
            median(cost), median(fuel_eff), 
            sd(cost),sd(fuel_eff), 
            IQR(cost),IQR(fuel_eff))



#cost per fuel type


data_eff %>% 
  filter(fuel_eff > 4, fuel_eff < 15) %>%
  na.omit() %>%
  ggplot(aes(x= refuelDate, y= cost)) + geom_line() +geom_smooth(method = "lm")+ facet_grid(~fuel_subtype)





#distance travelled overtime

data2 %>%
  mutate(totalDistance = cumsum(distance)) %>%
  ggplot(aes(x= refuelDate, y= totalDistance)) + 
  geom_line() + 
  geom_smooth(method = "lm")

data_distance <- data2 %>%
  mutate(totalDistance = cumsum(distance))

lm(refuelDate ~ totalDistance, data_distance)
