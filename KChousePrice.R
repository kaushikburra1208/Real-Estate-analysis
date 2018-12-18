### Function Set Workspace ###
set_workspace <- function(dir_path){
  dir.create(dir_path) # Create Directory
  setwd(dir_path) #Set Working Directory
  print(paste("Working Directory Set to : ",dir_path)) # Print Message
}
### Function To Check/Install/Load Package ###
load_package <- function(package_name) {
  # Check If Package Is Installed
  if(is.element(package_name, installed.packages()[,1])){
    library (package_name, character.only=TRUE)  # If Yes Load Package
  }
  else {
    install.packages(package_name) # Install Package
    library (package_name, character.only=TRUE)  # Load Package
  }
}
### Set Workspace ###
set_workspace("C:/Users/burra/Desktop/data");

#install.packages("rstan", type="source", repos='http://cran.us.r-project.org');

### Load Requied Libraries ###
load_package('dplyr')
load_package('tidyr')
load_package('sqldf')
load_package('ggplot2')
load_package('ggthemes')
load_package('gridExtra')
load_package('grid')
load_package('readr')
load_package('ellipse')
load_package('radarchart')

### Load CSV Files ##
kc_house_data <- read.csv("kc_house_data.csv")
kc_house_data=kc_house_data[,3:21] #selecting all the predictors and response column
kc_house_data

# Finding the summary and structure of the Data
summary(kc_house_data)
str(kc_house_data)

# Finding the relation between Prices and the sqftliving
plot(kc_house_data$sqft_living,kc_house_data$price)

# Finding the relation between Prices and the bathrooms
plot(kc_house_data$bathrooms,kc_house_data$price)


# Finding the relation between Prices and the bedrooms
plot(kc_house_data$bedrooms,kc_house_data$price)


# Plotting Correlation Plots
cr <- cor(kc_house_data)
#install.packages("corrplot")
library(corrplot)
corrplot(cr)





#install.packages("caTools")
library(caTools)
split=sample.split(kc_house_data$price,SplitRatio = 0.8) #80% split ratio
training_set=subset(kc_house_data,split==TRUE) #80% split into training set
test_set=subset(kc_house_data,split==FALSE) #20% split into testing set
dim(training_set)

# Building the Linear Regression Model
model1 <- lm(price~., data = training_set)
model <- lm(price~sqft_living+bathrooms+grade+sqft_above+sqft_living15, data = training_set) ##with only significant predictors





summary(model1)
summary (model) #### for RSE , R^2 (evaluation metrics)



predict <- predict(model, test_set)

predict

plot(test_set$price, type = "l", lty = 1.8, col = "red")
lines(predict,type ="l", col = "blue")

# Visualising the Training set results
library(ggplot2)
ggplot() +
  geom_point(aes(x = training_set$sqft_living, y = training_set$price),
             colour = 'red') +
  geom_line(aes(x = training_set$sqft_living, y = predict(model, newdata = training_set)),
            colour = 'blue') +
  ggtitle('price vs sqft_living (Training set)') +
  xlab('Sqft_living') +
  ylab('Price')

library(ggplot2)
ggplot() +
  geom_point(aes(x = training_set$sqft_living, y = training_set$price),
             colour = 'red') +
  geom_line(aes(x = training_set$sqft_living, y = predict(model, newdata = training_set)),
            colour = 'blue') +
  ggtitle('price vs sqft_living (Training set)') +
  xlab('sqft_living') +
  ylab('Price')



# Visualising the Test set results
ggplot() +
  geom_point(aes(x = test_set$sqft_living, y = test_set$price),
             colour = 'red') +
  geom_line(aes(x = training_set$sqft_living, y = predict(model, newdata = training_set)),
            colour = 'blue') +
  ggtitle('price vs sqft_living (Test set)') +
  xlab('Sqft_living') +
  ylab('Price')


ggplot() +
  geom_point(aes(x = test_set$bathrooms, y = test_set$price),
             colour = 'red') +
  geom_line(aes(x = training_set$bathrooms, y = predict(model, newdata = training_set)),
            colour = 'blue') +
  ggtitle('price vs bathrooms (Test set)') +
  xlab('bathrooms') +
  ylab('Price')


ggplot() +
  geom_point(aes(x = test_set$grade, y = test_set$price),
             colour = 'red') +
  geom_line(aes(x = training_set$grade, y = predict(model, newdata = training_set)),
            colour = 'blue') +
  ggtitle('price vs grade (Test set)') +
  xlab('grade') +
  ylab('Price')



#Hypothesis testing :
  
### we will check whether the average price  is significantly different from 54000  ###

t.test(kc_house_data$price, alternative="two.sided",mu=45000)