library(tidyverse)
martian <- read_tsv ("data/martian.txt")

#explore the data
glimpse(martian)

#mean1 wrong caclulations
#martian %>% 
#  group_by(Site == 1) %>% 
#  summarise(avg = mean(Height))

#mean2 correct
martian %>%
  filter(Site == "Site I") %>%
  summarise(avg = sum(Height)/ length(Height))


#mean3
Site1Martin <- martian %>% 
  filter(Site == "Site I")
Avg <- sum(Site1Martin$Height)/nrow(Site1Martin)


#mean4



#Variance


martian %>%
  filter(Site == "Site I") %>%
  summarise(avg = sum(Height)/ length(Height))




#SD

install.packages("writexl") 
library(writexl) 
write_xlsx(x = martian, path = "martian.xlsx", col_names = TRUE)



#the mean is the value that reduce the variance to the smallest   possible value
log(2,8)




