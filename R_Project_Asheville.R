#Clears environment and helps you start fresh
rm(list = ls())

#Library Statements
library(tidyverse)
library(ggplot2)

#DATASET 1
#Download AirBnBlistings from the “data for class” folder on Blackboard and save it to your computer.  Read the data into R using a file path. 
AirBnBlistings <-read.csv("~/Desktop/FIN454/AirBnBlistings.csv")
head(AirBnBlistings)

#QUESTION 1
#Use an appropriate file path to read AirBnBlistings.csv into R Studio.  Find the dimensions of the data. 
dim(AirBnBlistings)

#We are interested in explaining variation in the variable price, which is the daily price of the listing.
#Calculate the mean and standard deviation of price and round your answers to two decimal places.  Fill in the blanks:
round(mean(AirBnBlistings$price), digits = 2)
round(sd(AirBnBlistings$price), digits = 2)

#QUESTION 2
#Create a histogram to display the distribution of price.
#Select an appropriate binwidth and label your axes.
#Your title should be: “AirBnB listing prices in Asheville, NC”.
#Your subtitle should be:  “October to December, 2022”.
AirBnB_price_hist <- ggplot(data = AirBnBlistings, aes(x = price)) +
  geom_histogram(color = "black", fill = "lightblue",  binwidth = 25) +
  labs(x = "Price", y = "Count", title = "AirBnB listing prices in Asheville, NC", subtitle = "October to December, 2022") +
  theme_bw() 

AirBnB_price_hist

#Save your figure as a jpg, with a width of 10 inches and a height of 6 inches.  Upload your figure below.
ggsave(filename = "Desktop/AirBnB_price_hist.jpg",
       plot = AirBnB_price_hist,
       height = 6,
       width = 10,
       units = c("in"))

#QUESTION 3
#You first consider review_scores_rating as a possible explanatory variable.
#review_scores_rating is the average review of the listing on a 5-point scale, where 5 points is the highest review.
#Use the summary and standard deviation functions to examine the distribution of this variable.
summary(AirBnBlistings$review_scores_rating)
sd(AirBnBlistings$review_scores_rating)

#Do you believe that review_scores_rating would be a useful explanatory variable for price?  Why or why not?  Use your calculations to defend your answer.

#ANSWER: I do not believe that review_scores_rating would be a useful explanatory variable for price because the variable does not have much variation.
#The minimum rating in the sample is 3 and the maximum is 5, so, technically, the variable has a range of 2.
#However, the 1st quartile is 4.85.  this means that at least 75% of the observations have a rating between 4.85 and 5.
#Or, most observations have ratings that are about the same.
#This means that ratings is not going to be a very effective variable to explain prices.
#Certainly, I expect that other variables, like number of bedrooms or relative privacy of the listing, will be more effective at explaining variation in price. 

#QUESTION 4
#You estimate a first regression in which the only explanatory variable (x-variable) is private_stay, which is a binary variable equal to 1 if the property type indicates that your stay at the AirBnB would be private.  (For example, you would be renting an entire house, entire cottage, entire boathouse, etc.)
#The outcome variable (y-variable) is price.  Fill in the blanks below, and round your answers to two decimal places.
reg_1 <- lm(price ~ private_stay, data = AirBnBlistings)
summary(reg_1)

#QUESTION 5
#This question refers to your first regression results.  Interpret both your intercept and coefficient on private_stay.  What do these numbers mean?

#ANSWER: The intercept is the expected price if the x-variable, private_stay, equals zero.
#So, if a listing is not private, the expected price is $116.83.
#The coefficient on private_stay measures how much the expected price increases if the listing is private, or $94.54 higher, relative to the intercept.
#The two together, $116.83 plus $94.54, or $211.37 is the expected price of a listing that is a private stay.

#QUESTION 6
#What proportion of the variation in listing price does your first regression explain?  How do you know?

#ANSWER: My first regression explains only 11% of the variation in price.
#The R-squared measures the proportion of variation in the  y-variable explained by the x-variable, and R-squared is 11%.

#QUESTION 7
#You estimate a second regression with three explanatory variables:  private_stay, bedrooms, and minimum_nights.
#Bedrooms is an integer variable giving the number of bedrooms a listing has.  minimum_nights is also an integer variable, giving the minimum number of nights for which you can book a listing.
#Fill in the blanks below, and round your answers to two decimal places.
reg_2 <- lm(price ~ private_stay + bedrooms + minimum_nights, data = AirBnBlistings)
summary(reg_2)

#QUESTION 8
#What does your second regression model suggest about price charged for listings with more bedrooms?
#What does your second regression model suggest about price charged for listings with a high number of minimum nights? 

#ANSWER: My second regression model suggests that, holding everything else equal, one additional bedroom increases the expected price by $75.28 per bedroom.
#So, I expect that listings with more bedrooms have higher prices.
#I estimate that every additional night that is added to the minimum number of nights a listing requires you to rent decreases the expected price by $1.26.
#So, I expect that listings with a high number of minimum nights have a slightly lower price than those with a lower number of minimum nights.

#QUESTION 9
#	Next, you consider a third regression model with only two explanatory variables: instant_bookable_binary and private_stay.
#The new variable here, instant_bookable_binary, is a binary variable equal to one if you can click on a listing and immediately book the property.
#If the variable is zero, you must request and be granted permission by the property owner to book the listing.  Fill in the blanks below, and round your answers to two decimal places.
reg_3 <- lm(price ~ instant_bookable_binary + private_stay, data = AirBnBlistings)
summary(reg_3)

#QUESTION 10
#Use your third regression model to predict the selling price of a listing (in dollars) that is instantly bookable and a private stay.  Do not round any intermediate calculations, but round your final solution to two decimal places. 
round(reg_3$coefficients[[1]] + reg_3$coefficients[[2]] + reg_3$coefficients[[3]], digits = 2)

#QUESTION 11
#You re-consider your previous model and decide to estimate a fourth (and final) regression model using this data.
#You use two explanatory variables: instant_bookable_binary and private_stay.  Additionally, you use a natural log of price as your outcome variable (y-variable).
#Fill in the blanks below, and round your answers to two decimal places.
reg_4 <- lm(log(price) ~ instant_bookable_binary + private_stay, data = AirBnBlistings)
summary(reg_4)

#QUESTION 12
#Use your fourth regression model to predict the selling price of a listing (in dollars) that is instantly bookable and a private stay.  Do not round any intermediate calculations, but round your final solution to two decimal places. 
round(exp(reg_4$coefficients[[1]] + reg_4$coefficients[[2]] + reg_4$coefficients[[3]]), digits = 2)

#QUESTION 13
#In regression models 3 and 4, you use the same explanatory variables.
#Which model does a better job of explaining the variation in the y variable?  How can you tell?  Explain fully.

#ANSWER:The fourth regression model does a better job of explaining the variation in the y-variable than the third regression model.
#The R-squared measures the proportion of variation in the  y-variable explained by the x-variable.
#The R-squared for regression 3 is 12.4% and for regression 4 is 20%.
#The R-squared for the fourth regression model is higher; thus, the fourth regression model does a better job of explaining the variation in the y-variable than the third regression model.

########DATASET 2#######
#Download Telco_customer_churn from the “data for class” folder on Blackboard and save it to your computer.  Read the data into R using a file path. 
Telco_cc <-read.csv("~/Desktop/FIN454/Telco_customer_churn.csv")
head(Telco_cc)

#QUESTION 14
#For the remainder of this assignment, you will analyze the second data set, which is from a telecommunications company.  Use an appropriate file path to read Telco_customer_churn.csv into R Studio.
#Find the dimensions of the data. 
dim(Telco_cc)

#You are interested in explaining a binary variable, Churn.Value, equal to one if a customer left the telecommunications company in that period.
#Because you are primarily interested in explaining reasons why customers leave, you decide to drop observations where the customer moved.
#Eliminate any observations from your data in which Churn.Reason is “Moved”.  What are the new dimensions of your data?
Telco_cc <- Telco_cc %>%
  mutate(moved = ifelse(Churn.Reason == "Moved", 1, 0)) %>%
  filter(moved != 1)

dim(Telco_cc)

#QUESTION 15
#Create a variable, internet_binary, equal to one if Internet.Service equals “DSL” or “Fiber optic” and zero otherwise.
#This binary variable indicates which Telco customers have internet service.  (Note:  Telco also offers phone service, so a customer could have internet, phone, or both.)
#Fill in the blanks below, and round your answers to two decimal places.
Telco_cc <- Telco_cc %>%
  mutate(internet_binary = ifelse(Internet.Service == "DSL" |
                                    Internet.Service == "Fiber optic", 1, 0))

#What percent of Telco customers have internet service?
round(mean(Telco_cc$internet_binary), digits = 3)

#What percent of Telco customers leave Telco (Churn.Value equal to 1) in a period?
round(mean(Telco_cc$Churn.Value), digits = 3)

#QUESTION 16
#Use the table function to create a crosstabs table for internet_binary and Churn.Value.  Fill in the blanks in the following table.
internet_churn_xtab <- table(Telco_cc$internet_binary, 	Telco_cc$Churn.Value)
internet_churn_xtab

margin.table(internet_churn_xtab, 1)
margin.table(internet_churn_xtab, 2)

#QUESTION 17
#Calculate the following.  Do not round any intermediate calculations, but round your final solution to two decimal places. 
#What are the odds a non-internet customer leaves Telco? [0.08]
p_noninternet <- 110 / (1413 + 110)
p_noninternet

odds_noninternet <- p_noninternet / (1 - p_noninternet)
odds_noninternet

#What are the odds an internet customer leaves Telco? [0.45]
p_internet <- 1706 / (3761 + 1706)
p_internet

odds_internet <- p_internet / (1 - p_internet)
odds_internet

#What is the odds ratio that an internet customer leaves to a non-internet customer leaves Telco? [5.83] 
odds_internet / odds_noninternet

#RE: question 19
log(odds_internet / odds_noninternet)


#QUESTION 18
#Estimate a logistic model with Churn.Value as the outcome variable (y-variable) and one explanatory variable, internet_binary.  Fill in the blanks below, and round your answers to two decimal places.
reg_5 <- glm(Churn.Value ~ internet_binary, data = Telco_cc, family = binomial(link = "logit"))
summary(reg_5)

#QUESTION 19
#What does the coefficient on internet_binary tell us about Telco’s customer churn?
#Are Telco internet customers more or less likely to leave Telco than customers without internet services?  How can you tell?  Explain.

#ANSWER: The coefficient estimated on internet_binary, 1.76, indicates the log of the odds ratio of churn for internet:non-internet customers.
#This means that exp(1.76) is the odds ratio of churn for internet:non-internet customers, or 5.8267.
#We can stop here because we understand that the odds an internet customer leaves Telco is much higher (approximately 5.8x higher) than the odds a non-internet customer leaves Telco (because the odds ratio is greater than one).

#QUESTION 20
#Estimate one final regression using the Telco data.
#The outcome variable is Churn.Value and there are two explanatory variables:  internet_binary and Tenure.Months.  Tenure.Months is an integer variable equal to the number of months a customer has been with Telco.
#Fill in the blanks below, and round your answers to two decimal places.
reg_6 <- glm(Churn.Value ~ internet_binary + Tenure.Months, data = Telco_cc, family = binomial(link = "logit"))
summary(reg_6)

#QUESTION 21
#Interpret the coefficient on Tenure.Months.
#How does an additional month of tenure affect the log(odds) a customer leaves Telco? 
#How does an additional month of tenure affect the odds a customer leaves Telco?  Explain fully.

#ANSWER: Each additional month of tenure decreases the log(odds) a customer leaves Telco by 0.04.
#Each additional month of tenure decreases the odds a customer leaves Telco multiplicatively by exp(-0.04), or by -4% (solved by calculating:  (exp(-0.04) – 1)*100 = 4%).
#When we estimate a logistic model, log(odds of churn) is linear in the intercept, internet_binary, and Tenure.Months.
#So, log(odds) decreases by the amount of estimated coefficient, 0.04.
#This is an additive relation (like an arithmetic series).
#However, odds and probability of churn are affected nonlinearly (multiplicatively, like a geometric series).  

#QUESTION 22
#Use your regression model to predict the probability a customer leaves Telco who has internet service and has been a customer for 24 months.  Do not round any intermediate calculations, but round your final solution to two decimal places. 
prob_churn <- exp(reg_6$coefficients[[1]] + reg_6$coefficients[[2]] + reg_6$coefficients[[3]] *24 ) / 
  (1 + exp(reg_6$coefficients[[1]] + reg_6$coefficients[[2]] + reg_6$coefficients[[3]] *24 )) 
prob_churn
