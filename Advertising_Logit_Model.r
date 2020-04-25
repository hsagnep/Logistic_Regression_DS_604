#Importing data
data <- read.csv('/Users/hervinsagnep/Fixed\ Advertising.csv')
column_names <- c("Clicked.on.Ad","Daily.Time.Spent.on.Site","Area.Income")
data <- data[column_names]
head(data)

#Examining correlation, looking for colinearity 
cor(data)

#Splitting data in training and test 
smp_size <- floor(0.75 * nrow(data))
set.seed(123)
train_ind <- sample(seq_len(nrow(data)),size=smp_size)
train <- data[train_ind, ]
test <- data[-train_ind, ]

#Selecting variables to use in logistic regression
y <- train$Clicked.on.Ad
x1 <- train$Daily.Time.Spent.on.Site
x2 <- train$Area.Income

#Creating logistic model of training data
logit_model <- glm(y ~ x1 + x2,family = "binomial")
summary(logit_model)

#Predicting probability on testing data
prediction <- predict.glm(logit_model,train,type='response')
train["Prediction"] <- prediction

#McFadden's Pseudo R^2 and P-Value
ll.null <- logit_model$null.deviance/-2
ll.proposed <- logit_model$deviance/-2
p_value <- 1 - pchisq(2*(ll.proposed - ll.null),df=(length(logit_model$coefficients)-1))
r_square = (ll.null - ll.proposed) / ll.null
cat("McFadden's Psuedo R^2 Square:",r_square,"\n")
cat("P-Value:",p_value)




