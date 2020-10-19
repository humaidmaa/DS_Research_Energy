library(tidyverse)
library(rio)
library(tinytex)
library(ggplot2)
library(data.table)
library(scales)
 

Cars <- read_csv ("Cars.csv")
glimpse(Cars)
Cars = as.data.table(Cars) 
#analysis on highway Cars

summary(Cars$hwy)

qplot(hwy, data=Cars, geom="histogram", bins=30)

#analysis on city Cars

summary(Cars$cty)
qplot(cty, data=Cars, geom="histogram", bins=30)

#analysis on Cars of different car manufacturers

qplot(manufacturer, data=Cars, geom="bar", fill=manufacturer)

#analysis on number of cylinders among the cars
table(Cars$cyl)
qplot(cyl, data=Cars, geom="bar", fill=factor(cyl))


#analysis on Wheel drive types
qplot(drv, data=Cars, geom="bar", fill=drv)


#analysis on Fuel types
qplot(fl, data=Cars, geom="bar", fill=fl)


#analysis Vehicle Class
table(Cars$class)
qplot(class, data=Cars, geom="bar", fill=class)


#Engine_size vs highway efficiency
ggplot(data = Cars) + 
  geom_point(mapping = aes(x = engine_size, y = hwy), color='red')

ggplot(data = Cars) + 
  geom_point(mapping = aes(x = hwy, y = hwy, color=class)) +
  facet_wrap(~ class, nrow = 2)


#number of cylinders and type of drive
ggplot(data = Cars) + 
  geom_point(mapping = aes(x = engine_size, y = hwy, color=drv)) +
  facet_grid(drv ~ cyl)

#Estimating a smooth curve for the relationship between engine_size and highway mileage

ggplot(data = Cars) + 
  geom_smooth(mapping = aes(x = engine_size, y = hwy))

#Separate curve for each type of drive
ggplot(data = Cars) + 
  geom_smooth(mapping = aes(x = engine_size, y = hwy, linetype = drv, color=drv))

#Is automatic transmission better?

Cars2 <- Cars
Cars2$is.automatic <- startsWith(Cars2$trans, 'auto')
Cars2$transmission <- ifelse(Cars2$is.automatic, 'auto', 'man')
table(Cars2$trans)

table(Cars2$is.automatic)
qplot(transmission, cty, data=Cars2, geom='boxplot', fill=transmission)

# Analysis by using p-value
manual.cty <- Cars2$cty[!Cars2$is.automatic]
auto.cty <- Cars2$cty[Cars2$is.automatic]
t.test(manual.cty, auto.cty, alternative = "two.sided", var.equal = FALSE)
t.test(manual.cty, auto.cty, alternative = "greater", var.equal = FALSE)
t.test(manual.cty, auto.cty, alternative = "less", var.equal = FALSE)


#fit 
fit =  lm(cyl ~ hwy, Cars)
summary(fit)

str(Cars)

#histogram of MPG in highway
hist(Cars$hwy, main = "Data of cars with MPG driving on highways",
     xlab = "Miles per Gallon, m/g")

#histogram of MPG in city
hist(Cars$cty, main = "Data of cars with MPG driving on City",
     xlab = "Miles per Gallon, m/g")

#checking B_0 & B_1 for Highway MPG data
Cars = lm(hwy ~ cyl + year, data = Cars)
coef(Cars)

#checking B_0 & B_1 for City MPG data
Cars = lm(cty ~ cyl + year, data = Cars)
coef(Cars)


summary(auto.cty)
summary(manual.cty)

#Analysis of MPG compared to engine size
plot(hwy ~ engine_size, data = Cars, col = "dodgerblue", pch = 20, cex = 1.5)
