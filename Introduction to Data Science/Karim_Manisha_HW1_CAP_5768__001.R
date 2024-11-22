############################################################################################################################


#                       KARIM_MANISHA________HW_1_________CAP_5768-001 


#############################################################################################


# 1
#Install R and RStudio on your computer.

# I have installed R and R studio in my computer


# 2
#Define a variable x to be the integer of your Z number (without “Z”).

# My ZNumber is 23753021

x <- 23753021

# (a) √x
sqrt_x <- sqrt(x)
print(paste0('The square root of x is', sqrt_x))

# (b) log(x)
log_x <- log(x)
print(paste0('The logarithm of x is', log_x))

# (c) The remainder after dividing x by 49
rem_x <- x %% 49
print(paste0('The remainder after dividing by 49 is', rem_x))

# (d) Convert x to a string
string_x <- as.character(x)
print(paste0(string_x))


# (3) 
#Check if the string “FAU” is equal to “fau”.

if ('FAU' == 'fau'){
  print(paste0('FAU is equal to fau'))
} else{
  print(paste0('FAU is not equal to fau'))
}

# (4) Which ones in the following list are valid R variable names?
  
#(a) .3FAU
# Its not valid since a period(.) can't be followed by a number.

#(b) _F.3AU
# It is invalid because a variable can only start with letter or period(.).

#(c) mYfAu
# It is valid.

#(d) ._F_A_U_
# It is valid.

# (e) 3FAU
# It is invalid because a variable cannot have a numeric start.

# (f) F.A.U
# It is valid

#(g) F@AU
# It is invalid since variables can only have letters, digits, period(.), and underscore(_).

# (5)
# When we define character variables or strings, we can use either single quotes (a <- ’math’)
#or double quotes (a <- "math"). Can they be used interchangeably? Which one is
#preferred? Take a look at the help file on Quotes for more information.

# Double quotations (") are preferable to single quotes ('). 


#(6) 
# Create a numeric variable with your Z number as the value. Convert it to a string,
# and then convert it back to numeric.

x <- 23753021

string__x <- as.character(x)
print(paste0(string__x))

num_x <- as.numeric(x)
print(paste0(num_x))

# (7) 
# Define any variable and add a comment in the end.

x <- 23753021              # My Z-number is 23753021

#(8)
# Create a vector of numeric values.
vec_a <- c(1, 2,3)
vec_a

#(9) 
# Create a vector of strings.
vec_b <- c('apple', 'oranges', 'bananas')
vec_b

#(10)
#Create a vector of logical values.
vec_c <- c(TRUE, FALSE)
vec_c

#(11) 
# Create a vector vec1 <- c(1, 2, 3, 4, 5). Do the following in order.

vec1 <- c(1,2,3,4,5)

#(a) Change the third item to 300.
vec1[3] <- 300
vec1

# (b) Add a new value 10 after 2.
vec1 <- append(vec1, 10, after = 2)
vec1 

# (c) Delete value 4.
vec1 <- vec1[-5]
vec1

# (12)
# Create a numeric vector of length 100 with equally-spaced values starting at 3 and
# ending at 11.

vec_seq <- seq(from =  3, to = 11, length = 100)
vec_seq


# (13) 
# Create a list with four elements: 1 numeric, 1 list, 1 vector, and 1 string.

list1 <- list( 1, list('a', 'b'), c(1, 2, 3), 'abc')
list1

# (14) 
# Let x be a numeric value. Write a if else statement such that if x is larger than
# or equal to 10, print out the sentence It’s a big number. If x is larger than 5 but
# less than 10, print out the sentence It’s a medium-sized number. Otherwise, print
# out the sentence It’s a small number. Check if your code works well for x <- 15,
# x <- 8, and x <- 2.

x <- readline(prompt = "Enter a number: ")

x <- as.numeric(x)

if (x >= 10){
  print(paste0("It's a big number"))
} else if ((x > 5) & (x < 10)) {
  print(paste0("It's a medium number"))
} else {print(paste0("It's a small number"))}
  
# (15) 
# Find out the number of integers between 1 and 10000 which are divisible by 2, 3, and
# 5 at the same time. You can set up a counter to be 0 initially. Then use a for to
# loop through all integers from 1 to 10000. For each integer, if it is divisible by 2, 3,
# and 5, add 1 to the counter.

values <- c(1:10000)

counter <- 0
for (value in values) {
  if ((value %% 2 == 0 ) & (value %% 3 == 0) & (value %% 5 == 0)) {
    counter <- counter +1
  }
}
counter

# (16)
# Write a function with two arguments x and y where the default value of x is 3 and the
# default value of y is 2. The function should return xy.

product <- function ( x = 3, y = 2){
  multiplication = x * y
  return(multiplication)
}

# (a) providing both arguments
xy = product( 3 , 5)
print(paste0("The product of x and y is: ", xy))


# (b) only providing x
xy = product(2)
print(paste0("The product of x and y is: ", xy))

# (c) only providing y
xy = product( y = 5)
print(paste0("The product of x and y is: ", xy))

# (d) providing neither arguments
xy = product()
print(paste0("The product of x and y is: ", xy))

#(17) Write a function to check if a given integer is a prime number or not. A prime number
#is an integer that is divisible only by 1 and itself, such as 3 or 19. On the other hand,
#6 is not a prime number because it is divisible by 1, 2, 3, 6. Your function should
#take an integer n as the argument and print out n is prime number if it is. If n is
#not a prime number, print out n is not a prime number. Here is what you need
#to get.

CheckPrime <- function(n){
  
  if (n == 1){
    return(print(paste0(n, " is a prime number")))
  } else if (n == 2){
    return(print(paste0(n, " is a prime number")))
  } else {
  for (i in (2:(sqrt(n)))) {
      if( (n %% i) == 0){
      return(print(paste0(n, " is not a prime number")))
    } else {return(print(paste0(n, " is a prime number")))
           }
  }
  }
}

CheckPrime(19)                             # It should print out 19 is a prime number
CheckPrime(8)                              # It should print out 8 is not a prime number



#(18) Use the following code to generate a data frame.
#grades <- data.frame (
#  Name = c("Aaron", "Beth", "Charlotte", "David", "Emma", "Frank",
#           "George", "Hannah", "Iris"),
#  Year = c("Freshman", "Senior", "Junior", "Senior", "Sophomore",
#           "Sophomore", "Freshman", "Senior", "Junior"),
#  Score = c(82, 91, 77, 69, 83, 94, 88, 97, 81))

grades <- data.frame (
Name = c("Aaron", "Beth", "Charlotte", "David", "Emma", "Frank",
             "George", "Hannah", "Iris"),
Year = c("Freshman", "Senior", "Junior", "Senior", "Sophomore",
            "Sophomore", "Freshman", "Senior", "Junior"),
Score = c(82, 91, 77, 69, 83, 94, 88, 97, 81)
)

#(a) Add a new student record at the TOP of the original data frame. The student’s
#name is Jon who is a freshman with score 85.
df <- data.frame( Name = c("Jon"), Year = c("Freshman"), Score = c(85)) 

grades <- rbind( df, grades)
grades

#(b) Add a new column named “Grade” which can be “A” (if score is greater than
# or equal to 90), “B” (if score is greater than or equal to 80 but less than 90), and “C” (all the rest).
grades[ , 'Grade'] = NA
grades

grades$Grade[(grades$Score >= 80) & (grades$Score<90)] <- "B"
grades$Grade[(grades$Score >= 90)] <- "A"
grades$Grade[(grades$Score < 80)] <- "C"
 
grades

# (c) Change the “Year” variable to a factor with four ORDERED levels.
grades$Year <- factor(grades$Year, levels=c("Freshman", "Sophomore", "Junior", "Senior"))
grades$Year

# (d) Make a histogram of all scores (including Jon).
hist(grades$Score)

# (e) Make a boxplot of all scores (including Jon).
boxplot(grades$Score)

# (f) Make a frequency table of the years using table. Check its help file if you want
# to know more.

freq_table <- table(grades$Year)
barplot(freq_table)


