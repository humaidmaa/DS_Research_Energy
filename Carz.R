
library(tidyverse)

mpg <- read_csv ("mpg.csv")
glimpse(mpg)

table(mpg$manufacturer)
table(mpg$year)

#analysis on highway mpg

summary(mpg$hwy)

qplot(hwy, data=mpg, geom="histogram", bins=30)

#analysis on city mpg

summary(mpg$cty)
qplot(cty, data=mpg, geom="histogram", bins=30)

#analysis on Mpg of different car manufacturers

qplot(manufacturer, data=mpg, geom="bar", fill=manufacturer)

#analysis on number of cylinders among the cars
table(mpg$cyl)
qplot(cyl, data=mpg, geom="bar", fill=factor(cyl))


#analysis on Wheel drive types
qplot(drv, data=mpg, geom="bar", fill=drv)


#analysis on Fuel types
qplot(fl, data=mpg, geom="bar", fill=fl)


#analysis Vehicle Class
table(mpg$class)
qplot(class, data=mpg, geom="bar", fill=class)


#Displacement vs highway efficiency
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color='red')

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = hwy, y = hwy, color=class)) +
  facet_wrap(~ class, nrow = 2)


#number of cylinders and type of drive
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color=drv)) +
  facet_grid(drv ~ cyl)

#Estimating a smooth curve for the relationship between displacement and highway mileage

ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy))

#Separate curve for each type of drive
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv, color=drv))

#Is automatic transmission better?

mpg2 <- mpg
mpg2$is.automatic <- startsWith(mpg2$trans, 'auto')
mpg2$transmission <- ifelse(mpg2$is.automatic, 'auto', 'man')
table(mpg2$trans)

table(mpg2$is.automatic)
qplot(transmission, cty, data=mpg2, geom='boxplot', fill=transmission)

# Analysis by using p-value
manual.cty <- mpg2$cty[!mpg2$is.automatic]
auto.cty <- mpg2$cty[mpg2$is.automatic]
t.test(manual.cty, auto.cty, alternative = "two.sided", var.equal = FALSE)
t.test(manual.cty, auto.cty, alternative = "greater", var.equal = FALSE)
t.test(manual.cty, auto.cty, alternative = "less", var.equal = FALSE)
