####### KARIM_MANISHA___HW2__CAP-5768-001 #######

--------------------------------------------------------------------------------------------

install.packages("ggplot2")
install.packages("GGally")

library(GGally)
library(ggplot2)


# 1
# Data set iris in R is a famous data set with the measurements in centimeters of
# the variables sepal length and width and petal length and width, respectively, for
# 50 flowers from each of 3 species of iris. The species are setosa, versicolor, and
# virginica. The variable names are Sepal.Length, Sepal.Width, Petal.Length,
# and Petal.Width. You can access the data set by directly typing iris in R.

df <- iris
head(df)

# (a) Make a scatter plot of Sepal.Length (horizontal) versus Sepal.Width (vertical).

ggplot(df, aes( x = Sepal.Length, y = Sepal.Width)) + geom_point(shape = 16, color = 'red')+
theme_bw() + labs( title = "Sepal Width vs Sepal Length")

# (b) Make a scatter plot of Sepal.Length (horizontal) versus Sepal.Width (vertical)
# with three different colors for three species. Hint: use color in aes.

ggplot(iris, aes( x = Sepal.Length, y = Sepal.Width, color = Species)) + geom_point() +
theme_bw() + labs( title = "Sepal Width vs Sepal Length") + scale_color_viridis_d()

# (c) Make side-by-side box plots of Sepal.Length, one for each Species.

ggplot(iris, aes( y = Sepal.Length, color = Species)) + geom_boxplot() +
theme_bw() + labs( title = "Sepal Width vs Sepal Length") + scale_colour_viridis_d()

# (d) Make the same box plots with horizontal boxes instead of vertical ones.

ggplot(iris, aes( x = Sepal.Length, color = Species)) + geom_boxplot() +
theme_bw() + labs( title = "Sepal Width vs Sepal Length") + scale_colour_viridis_d()

# (e) Make a scatter plot matrix of all four continuous variables. Use ggpairs function
# in GGally package.

ggpairs(iris[1:4], aes(alpha = 0.4))

# (f) Make a scatter plot matrix of all four continuous variables, distinguished by
# Species. That is, each species should have one color on each scatter plot.

ggpairs(iris, columns = c(1:4), aes(colour = Species, alpha = 0.4))


# (2) Fit a simple linear regression model of Sepal.Length (y) on Petal.Length (x).

# (a) Write down the model.

x <- iris$Petal.Length
xbar <- mean(x)
y <- iris$Sepal.Length
ybar <- mean(y)
slope <- sum((x - xbar) * (y - ybar)) / sum(( x - xbar)^2)
intercept <- ybar - slope * xbar

slope
intercept


# (b) Fit the simple linear regression model using lm.

fit <- lm(Sepal.Length ~ Petal.Length, data = iris)
fit

# (c) Report the estimated intercept and slope.

# The intercept and slope are 4.3066 and 0.4089 respectively.

# (d) Report the significance of the regression.

summary(fit)

# The significance of the regression is 2e-16

# (e) Interpret the slope in the context of the problem.

# Each unit change in petal length results in a 0.4089223 change in sepal length.

# (f) Make a scatter plot of these two variables. For the points, use pink-colored
# half-transparent squares with size 5 and red border.

ggplot(df, aes( y = Sepal.Length, x = Petal.Length)) + geom_point(shape = 21, size = 5, color = 'red', fill = 'pink', alpha = 0.5) +
theme_light() + labs( title = "Sepal Length vs Petal Length") 

# (g) Add the fitted regression line in blue on top of the scatter plot in the previous
# step.

ggplot(df, aes( y = Sepal.Length, x = Petal.Length)) + geom_point(shape = 21, size = 5, color = 'red', fill = 'pink', alpha = 0.5) +
theme_light() + labs( title = "Sepal Length vs Petal length")  + stat_smooth(method = "lm")

# (h) Add another smooth line in green using method = loess in stat_smooth.

ggplot(df, aes( x = Sepal.Length, y = Petal.Length)) + geom_point(shape = 21, size = 5, color = 'red', fill = 'pink', alpha = 0.5) +
  theme_light() + labs( title = "Sepal Length vs Petal Length")  + stat_smooth(method = "lm") +
  stat_smooth(method = "loess", color = "green")

# (i) What is the 95% confidence interval on the intercept.

confint( fit, level = 0.9)

# The 95% confidence intercept is 4.4363540

# (j) What is the 99% confidence interval on the slope.

confint( fit, level = 0.98)

# The 95% confidence intercept is 4.4909592

# (k) Predict the sepal length of the first iris flower in the data set whose petal length
# is 1.4.

newx = data.frame(Petal.Length = c(1.4))
newx
predict(fit, newx)
predict(fit, newx)


# (l) Make a confidence interval on the mean sepal length of iris flowers with petal
# length of 1.65.

newx = data.frame(Petal.Length = c(1.65))
predict(fit, newx, interval = "confidence")


# (m) Make a prediction interval on the sepal length of a specific iris flower with petal
# length of 1.65.

predict(fit, newx, interval = "prediction")

# (n) Make a residual plot. Do you observe any pattern that suggests the model is
# not adequate?
  
ggplot(fit, aes( x = .fitted, y = .resid)) + geom_point(color = "red") + geom_hline(yintercept = 0) +
  theme_light() +labs( title = "Residual Plot", x = "Fitted Values", y = "Residuals")

# The residual plot is rectangular in shape indicating that the model is adequate.

# (3)
# The data set Nile in R contains measurements of the annual flow of the river Nile
# at Aswan between 1871 and 1970.

# (a) Make a time series plot using the geom_line function. You may need to create
# a data frame first before plotting.

year <- 1871:1970
df <- data.frame(year, Nile)
df

ggplot(df, aes( x=year , y = Nile )) + geom_line(color = "red") +
  theme_light() + labs( title = "Annual Flow of the river Nile", x = "Year", y = "Annual flow")

# (b) Change the appearance of the line plot in (a) in any way you want.

ggplot(df, aes( x=year , y = Nile )) + geom_line(color = "blue") +
  theme_light() + labs( title = "Annual Flow of the river Nile", x = "Year", y = "Annual flow")

# (4) 

# The data set quakes in R give the locations of 1000 seismic events of MB > 4.0.

df <- quakes
df

# Variable mag represents numeric Richter Magnitude.
# (a) Make a histogram using default settings.

ggplot(df, aes( x = mag)) + geom_histogram()

# (b) Change the number of bins using Sturges’ formula and replot the histogram.

ggplot(df, aes( x = mag)) + geom_histogram( bins = (1 + as.integer(log(length(df$mag), 2))), 
                                            fill = "red", color = "red", alpha = 0.5) + theme_bw()

# (c) Superimpose a blue-colored density plot to the histogram.

ggplot(df, aes( x = mag)) + geom_histogram( bins = (1 + as.integer(log(length(df$mag), 2))), 
                                            fill = "red", color = "red", alpha = 0.5, aes(y = ..density..)) + geom_density(color = "blue") + theme_bw()


# (d) The magnitude is between 4.0 and 6.4 in the data set. Add a new column to the
# data set which is an ordered factor with three levels: “Low” (4.0 ≤ mag < 5.0),
# “Medium” (5.0 ≤ mag < 6.0), and “High” (mag ≥ 6.0).

df = transform(df, level = NA)
df$level <- with(df, ifelse(((4.0 <= mag) &  (mag < 5.0)), "Low", df$level))
df$level <- with(df, ifelse(((5.0 <= mag) &  (mag < 6.0)), "Medium", df$level))
df$level <- with(df, ifelse((mag >= 6.0), "High", df$level))

factor(df$level, levels = c("Low", "Medium", "High"))
df$level <- factor(df$level, levels = c("Low", "Medium", "High"))
head(df)


ggplot(df, aes( x = mag)) + geom_histogram( bins = (1 + as.integer(log(length(df$mag)), 2)), fill = "red", color = "red", alpha = 0.5)+
  facet_grid(df$level) + theme_light()

# (e) Make side-by-side histograms of mag using facet_grid function, one for each
# level in (d).

ggplot(df, aes( x = mag)) + geom_histogram( bins = (1 + as.integer(log(length(df$mag)), 2)), fill = "red", color = "red", alpha = 0.5)+
facet_wrap(df$level, ncol = 3) + theme_light()
