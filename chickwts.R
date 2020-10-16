library(tidyverse)
#read.csv(chickwts)
#chickwts <- read_csv ("chickwts.csv")
glimpse(chickwts)
summary(chickwts)
plot(chickwts)

chickwts %>%
  group_by(feed) %>%
  summarise(n = n(), Avg=mean(weight), SD = sd(weight))



ggplot(chickwts,aes(x=feed, y=weight)) + 
  geom_jitter() + 
  stat_summary(fun.data = mean_sdl,
               fun.args = list(mult=1),
               color="red")


#measure and compare feed supplements on the growth rate of chickens

require(stats); require(graphics)
boxplot(weight ~ feed, data = chickwts, col = "lightgray",
        varwidth = TRUE, notch = TRUE, main = "chickwts",
        ylab = "Weight at six weeks (gm)")
anova(lm(weight ~ feed, data = chickwts))


