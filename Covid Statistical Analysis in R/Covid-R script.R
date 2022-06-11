rm(list=ls()) #removes all variables stored previously
library(Hmisc)

data <- read.csv("C:/Users/USER/Downloads/Kaggle Covid Dataset/COVID19_line_list_data.csv")
describe(data) # Hmisc command

#cleaned up death column
data$death_dummy <- as.integer(data$death != 0)

#death rate 
sum(data$death_dummy)/nrow(data)

#proving or falsifying claims
#AGE
#First claim: People who die from Covid are older 
dead =  subset(data, death_dummy == 1)
alive = subset(data, death_dummy == 0)
mean(dead$age, na.rm = TRUE)
mean(alive$age, na.rm = TRUE)

# Is our result statistically significant 
t.test(dead$age, alive$age, alternative = "two.sided", conf.level = 0.99)
# Statistically, if p value < 0.05, we reject null hypothesis...
# p ~ 0.05 then this is statistically significant

#GENDER
# Second claim: Men are more subsceptible to Covid Deaths than women
men = subset(data, gender == "male")
women = subset(data, gender == "female")
mean(men$death_dummy, na.rm = TRUE)
mean(women$death_dummy, na.rm = TRUE)

# Is our result statistically significant 
t.test(men$death_dummy, women$death_dummy, alternative = "two.sided", conf.level = 0.99)
# Statistically, if p value < 0.05, we reject null hypothesis...
# 99% confidence is that men have 0.8% to 8.8% higher chance of dying from Covid than women
# p ~  0.05 shows that it is statistically significant 
