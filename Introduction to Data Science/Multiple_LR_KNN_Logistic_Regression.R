library("datarium")
library("GGally")
library("ggplot2")
library("class")

# 1) 

# The marketing data set in the datarium package contains the impact of three advertising medias (youtube, 
# facebook and newspaper) on sales. Data are the advertising  budget in thousands of dollars along with the sales. The 
# advertising experiment has been repeated 200 times.



# (a) You need to install the package datarium first.

df = marketing

# (b) Make a scatter plot matrix using the ggpairs function in the GGally package.

ggpairs(df)

# (c) Fit a multiple linear regression model using sales as the response and three
# advertising medias as predictor variables.

fit <- lm( sales ~ . , data = df)

# (d) Report the estimated coefficient for each media.

summary(fit)

# (e) Make residual plots to see if model assumptions are violated.

ggplot(fit, aes(x = .fitted, y = .resid)) + geom_point(size = 2, color = "red") + geom_hline(yintercept = 0)
  theme_minimal() +
  labs(title =  "Residual Vs Fitted Values",
       x = " Fitted Values ",
       y = " Residual Values ")
  
# The linearity assumption was not met by this fit.


# (f) Predict the sales for four new cases with

new <- data.frame(youtube = c(125, 108, 103, 114),
                  facebook = c(50, 44, 66, 58),
                  newspaper = c(95, 84, 102, 97)
                  )
new
predict(fit, new)

# 2) 
# The file at http://yangli.us/data/admission.csv contains data of admission into graduate schools. There are three predictor variables: gre (Graduate Record Exam scores), gpa (grade point average), 
# and rank (prestige of the undergraduate institution). The response variable is admit, where 1 means the student was 
# admitted and 0 means not.

# (a) Load the data into R.

df = read.csv("/home/manishakarim/Class/Intro to Data Science/admission.csv")


# (b) Print out the first several lines using head function.

head(df)

# (c) Make side-by-side box plots of gre, one for each level of admit.

ggplot(df, aes(y = gre, x = interaction(admit))) + geom_boxplot() + 
theme_minimal() + 
  labs(title =  "Gre",
       x = "admit ",
       y = "gre")


# (d) Make side-by-side box plots of gpa, one for each level of admit.

ggplot(df, aes(y = gpa, x = interaction(admit))) + geom_boxplot() + 
  theme_minimal() + 
  labs(title =  "GPA",
       x = "admit ",
       y = "gpa")


# (e) Do a feature scaling using min-max normalization.

min_max_norm <- function(v) {
  return((v-min(v))/(max(v)-min(v)))
}

min_maxed <- data.frame(apply(df[c("gre", "gpa", "rank")], 2, min_max_norm))
min_max_df <- cbind(df$admit, min_maxed)
colnames(min_max_df)[1] <- "admit"
head(min_max_df)

# (f) Do a feature scaling using standardization.

standardize <- function(v) {
  return((v-mean(v))/sd(v))
}

standardized <- data.frame(apply(df[c("gre", "gpa", "rank")], 2, standardize))
standardized_df <- cbind(df$admit, standardized)
colnames(standardized_df)[1] <- "admit"
head(standardized_df)

# Let’s take the data from min-max normalization and use it for subsequent analyses.


# (g) Randomly divide the data set into a training set (75%) and a test set (25%).

## 75% of the sample size

set.seed(123)
smp_size <- floor(0.75 * nrow(min_max_df))

## set the seed to make your partition reproducible

train_ind <- sample(seq_len(nrow(min_max_df)), size = smp_size)

train <- min_max_df[train_ind, ]
test <- min_max_df[-train_ind, ]

head(train)
head(test)

# (h) Perform a KNN classification using knn function in the class package. You can choose any K value you want,

train.set <- train[c("gre", "gpa", "rank")]
test.set <- test[c("gre", "gpa", "rank")]

train.label <- train[c("admit")]
test.label <- test[c("admit")]

train.label= train.label[,1]
test.label= test.label[,1]

knn.pred <- knn(train.set, test.set, train.label, k=6, prob=TRUE)


# (i) Report the confusion matrix of the test data set from part (h).

confusion_matrix <- table(knn.pred, test.label)
confusion_matrix

# (j) Report the proportion of correctly predicted observations of the test data set from part (h).

accuracy = ((confusion_matrix[1] + confusion_matrix[4])/(confusion_matrix[1] + confusion_matrix[2] + 
                                                          confusion_matrix[3] + confusion_matrix[4])) * 100
accuracy

# (k) Vary the K value and plot the proportion of correctly predicted observations of the test data set for each K as
# a function of K.

k_values <- c()
acc <- c()

for (i in 1:10){
  
  knn.pred <- knn(train.set, test.set, train.label, k=i, prob=TRUE)
  confusion_matrix <- table(knn.pred, test.label)
  accuracy = ((confusion_matrix[1] + confusion_matrix[4])/(confusion_matrix[1] + confusion_matrix[2] + 
                                                             confusion_matrix[3] + confusion_matrix[4])) * 100
  k_values[i] <- i
  acc[i] <- accuracy
}

k_values
acc
proportion <- as.data.frame(cbind(c(k_values), c(acc)),2)
proportion

ggplot(proportion, aes(x = V1, y = V2))+ geom_line() + geom_point(color = "red", size = 3) +
  theme_minimal() +  scale_x_continuous(breaks=seq(1,10,1)) + 
  labs(title = "Accuracy vs K-values",
       x = "K-Values",
       y = "Accuracy")

# (l) Which K value is the best choice?

## K=9 performed the best with an accuracy of 73%.

# 3) 
# Now let’s use the same data set in (2), and perform a logistic regression analysis. It’s not required to do a 
# feature scaling first. So you can use the original data set.

# (a) Fit a logisitc regression model of admit on all three predictors gre, gpa, rank.

fit <- glm(admit ~ ., data = min_max_df , family = binomial)
summary(fit)

#  (b) Report the regression coefficients.

coef(fit)
## The co-efficients are -1.7489, 1.3305, 1.3520, and -1.6801.

#(c) Predict the responses for three new students (whether they will be admitted or not). Student 1 has GRE 650, GPA 3.4, 
# and rank 2. Student 2 has GRE 720, GPA 3.1, and rank 3. Student 3 has GRE 690, GPA 3.6, and rank 4.

new <- data.frame(gre = c(650, 720, 690),
                  gpa = c(3, 3.1, 3.6),
                  rank = c(2, 3, 4)
)
new

min_maxed_new <- data.frame(apply(new[c("gre", "gpa", "rank")], 2, min_max_norm))
min_maxed_new

predict(fit, min_maxed_new, type="response")

#(4) 
# Finally, let’s compare the performance of KNN and logistic regression using the same data set of admission to 
# graduate schools.

# (a) Randomly divide the whole data set into a training set (75%) and a test set (25%).

## This has been done for KNN in problem 2h. So, I am going to use that train/test dataset so the data points are 
## similar. This will make it more efficient to compare the two models. 

head(train)
head(test)

# (b) Fit the logistic regression model on the training set.

fit <- glm(admit ~ . , data = train, family = binomial)
summary(fit)

# (c) Predict on the test set and use 50% as the threshold value. That is, if the predicted probability is larger 
# than 50%, the student is predicted to be admitted.
# If the predicted probability is less than 50%, the student is predicted to be not admitted.

prob = predict(fit, test, type = "response")

fitted.admit <- rep(0, nrow(test))
fitted.admit[prob > 0.5] <- 1

fitted.admit

# (d) Report the proportion of correctly predicted students using logistic regression.

confusion_matrix = table(fitted.admit, test$admit)
confusion_matrix

accuracy = ((confusion_matrix[1] + confusion_matrix[4])/(confusion_matrix[1] + confusion_matrix[2] + 
                                                           confusion_matrix[3] + confusion_matrix[4])) * 100
accuracy

## The accuracy was 71.

# (e) Use the same training and test sets, and apply a KNN with K value equal to the best value you found in problem 2.

## The same train/test dataset was used in problem-2. So, this was solved in problem-2h and 2k. The best accuracy was
## obtained for k=9.

# (f) Report the proportion of correctly predicted students using KNN.

## The accuracy of KNN with K=9 was 73.

# (g) Which method has a better prediction performance?

## KNN outperformed logistic regression.

  
  

