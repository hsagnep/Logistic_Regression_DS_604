#Importing data
data <- read.csv('/Users/hervinsagnep/Fixed\ Advertising.csv')
column_names <- c("Clicked.on.Ad","Daily.Time.Spent.on.Site","Area.Income")
data <- data[column_names]
head(data)

#Simple logistic regression
clicked <- data$Clicked.on.Ad
daily_time <- data$Daily.Time.Spent.on.Site
model <- glm(clicked ~ daily_time,family='binomial')
summary(model)

#Plotting simple logistic regression curve
plot(daily_time,clicked,pch=19,main="Simple Logistic Regression",xlab="Daily Time Spent on Site",ylab="Probability of clicking on Ad")
xv <- seq(min(daily_time),max(daily_time),.01)
yv <- predict(model,list(daily_time=xv),type="response")
lines(xv,yv)

####Multivariable Logistic Regression

#Examining correlation, looking for colinearity 
cor(data)

#Splitting data in training and test 
smp_size <- floor(0.75 * nrow(data))
set.seed(123)
train_ind <- sample(seq_len(nrow(data)),size=smp_size)
train <- data[train_ind, ]
test <- data[-train_ind, ]

#Creating logistic model of training data
logit_model <- glm(Clicked.on.Ad ~ Daily.Time.Spent.on.Site + Area.Income,data=train,family = "binomial")
summary(logit_model)

#Predicting probability on testing data
prediction <- predict.glm(logit_model,test,type='response')
test["Predictions"] <- prediction
head(test["Predictions"])

#Testing results
test["Prediction"] <- ifelse(test["Predictions"] <= .5,0,1)
actual_frequency <- table(test$Clicked.on.Ad)
predicted_frequency <- table(test$Prediction)
head(test)

#Creating barplots for frequencies
barplot(actual_frequency,xlab="Clicked on ad?",main="Actual Training Frequency",ylim=c(0,250),col=c("Red","Green"))
legend("topleft", legend = c(actual_frequency["0"],actual_frequency["1"]),col=c("Red","Green"),bty = "n", pch=20 , pt.cex = 2, cex = 0.8, horiz = FALSE, inset = c(0.05, 0.05))
barplot(predicted_frequency,xlab="Clicked on ad?",main="Predicted Training Frequency",ylim=c(0,250),col=c("Red","Green"))
legend("topleft", legend = c(predicted_frequency["0"],predicted_frequency["1"]),col=c("Red","Green"),bty = "n", pch=20 , pt.cex = 2, cex = 0.8, horiz = FALSE, inset = c(0.05, 0.05))

#McFadden's Pseudo R^2 and P-Value
ll.null <- logit_model$null.deviance/-2
ll.proposed <- logit_model$deviance/-2
p_value <- 1 - pchisq(2*(ll.proposed - ll.null),df=(length(logit_model$coefficients)-1))
r_square <- (ll.null - ll.proposed) / ll.null
cat("McFadden's Psuedo R^2 Square:",r_square,"\n")
cat("P-Value:",p_value)

#Values for testing slopes
p_values <- coef(summary(logit_model))[,'Pr(>|z|)']
z_values <- coef(summary(logit_model))[,'z value']
critical_z <- qnorm(.025,lower.tail=FALSE)
cat("Critical-Z:",critical_z)
cat("P-Values:",p_values)
cat("Z-Values:",z_values)

