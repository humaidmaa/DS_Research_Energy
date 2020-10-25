# Title Car Fuel Consumption Study
# Name: Mansour Alhumaid
# Date: 15 Oct 2020
# This 
# the data is technical spec of cars showing fuel consumption rate for each car of different manufactureres. The dataset is downloaded from Kaggle

# Data From:
# https://www.kaggle.com/gauravshasae78/car-mpg

# load packages
library(tidyverse)
library(rio)
library(scales)
library(GGally)


# load the data
cars <- read_csv("Cars.csv")

# explore
glimpse(cars)
summary(cars$hwy)

# SPLOM (Scatter plot matrix) but with proper data types:
cars %>% 
  select(-X1, -X1_1, -model, -manufacturer) %>% 
  ggpairs()

# Analysis on highway cars
#qplot(hwy, data=cars, geom="histogram", bins=30)
ggplot()

# Analysis on city cars
summary(cars$cty)
#qplot(cty, data=cars, geom="histogram", bins=30)
ggplot()

# Analysis on cars of different car manufacturers
#qplot(manufacturer, data=cars, geom="bar", fill=manufacturer)
ggplot()

# Analysis on number of cylinders among the cars
table(cars$cyl)
#qplot(cyl, data=cars, geom="bar", fill=factor(cyl))
ggplot()

# Analysis on Wheel drive types
# qplot(drv, data=cars, geom="bar", fill=drv)
ggplot()

# Analysis on Fuel types
# qplot(fl, data=cars, geom="bar", fill=fl)
ggplot()

# Analysis Vehicle Class
table(cars$class)
#qplot(class, data=cars, geom="bar", fill=class)
ggplot()

# Engine_size vs highway efficiency
ggplot(cars, aes(engine_size, hwy)) + 
  geom_point(color = "#cb181d", alpha = 0.4, shape = 16)

# Engine_size vs highway efficiency (bubble chart)
ggplot(cars, aes(engine_size, hwy)) + 
  stat_sum(color = "#cb181d", alpha = 0.4, shape = 16) +
  scale_size_area(limits = c(1,10), breaks = c(1, 5, 10), max_size = 7)

# by color
ggplot(cars, aes(engine_size, hwy, color = class)) + 
  geom_point()

# By plot:
ggplot(cars, aes(engine_size, hwy)) + 
  geom_point() +
  facet_wrap(~ class, nrow = 2)

# Number of cylinders and type of drive
ggplot(data = cars) + 
  geom_point(mapping = aes(x = engine_size, y = hwy, color=drv)) +
  facet_grid(drv ~ cyl)

# Estimating a smooth curve for the relationship between engine_size and highway mileage
ggplot(cars, aes(engine_size, hwy)) + 
  geom_point(alpha = 0.3) +
  geom_smooth()

# Separate curve for each type of drive
ggplot(data = cars) + 
  geom_smooth(mapping = aes(x = engine_size, y = hwy, linetype = , color=drv))

# Is automatic transmission better than manual? according to what measurement? fuel efficient in city

cars2 <- Cars
cars2$is.automatic <- startsWith(cars2$trans, 'auto')
cars2$transmission <- ifelse(cars2$is.automatic, 'auto', 'man')
table(cars2$trans)

table(cars2$is.automatic)
ggplot(cars2, aes(x=transmission, y=cty, fill=transmission)) +
  geom_boxplot()

# 2nd attempt Is automatic transmission better than manual? according to what measurement? fuel efficient in city
cars_m_a <- cars2
manual.cty <- cars2$cty[!cars2$is.automatic]
auto.cty <- cars2$cty[cars2$is.automatic]
ggplot(cars_m_a, aes(x=transmission, y=cty, fill=transmission)) +
  geom_boxplot()

# Analysis by using p-value
manual.cty <- cars2$cty[!cars2$is.automatic]
auto.cty <- cars2$cty[cars2$is.automatic]
t.test(manual.cty, auto.cty, alternative = "two.sided", var.equal = FALSE)
t.test(manual.cty, auto.cty, alternative = "greater", var.equal = FALSE)
t.test(manual.cty, auto.cty, alternative = "less", var.equal = FALSE)


#fit 
fit =  lm(cyl ~ hwy, cars)
summary(fit)

str(cars)

#histogram of MPG in highway
hist(cars$hwy, main = "Data of cars with MPG driving on highways",
     xlab = "Miles per Gallon, m/g")

#histogram of MPG in city
hist(cars$cty, main = "Data of cars with MPG driving on City",
     xlab = "Miles per Gallon, m/g")

#checking B_0 & B_1 for Highway MPG data
cars = lm(hwy ~ cyl + year, data = cars)
coef(cars)

#checking B_0 & B_1 for City MPG data
cars = lm(cty ~ cyl + year, data = cars)
coef(cars)


summary(auto.cty)
summary(manual.cty)

#Analysis of MPG compared to engine size
plot(hwy ~ engine_size, data = cars, col = "dodgerblue", pch = 20, cex = 1.5)
