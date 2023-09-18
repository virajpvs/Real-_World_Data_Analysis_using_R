
##################################
#Question 1 - Understand the Data
##################################

the.data <- as.matrix(read.table("ENB_2023.txt", header = TRUE))
the.data
summary(the.data)

set.seed(223438779) # using your student ID number for reproducible sampling with the seed function

my.data <- the.data[sample(1:671,340),c(1:6)] 
my.data
summary(my.data)
cor(my.data)
skewness(my.data)

# Use scatter plots and histograms to understand the relationship between each of the 
# variables X1, X2, X3, X4, X5, and your variable of interest Y.

# Create 5 scatterplots function (for each X variable against the variable of interest Y) 
colors <- c("red", "orange", "blue", "darkgreen", "purple","yellow")  # Define different colors
plot(my.data[, 1], my.data[, 6], 
     xlab = expression(paste("X1_Temperature in Kitchen (", degree, "C)")), 
     ylab = expression(paste("Appliances Energy Use (Wh)")), 
     main = "X1_Temperature in Kitchen vs. Appliances Energy Use", 
     col = colors[1])

plot(my.data[, 2], my.data[, 6], 
     xlab = expression(paste("X2_Humidity in Kitchen (%)")), 
     ylab = expression(paste("Appliances Energy Use (Wh)")), 
     main = "X2_Humidity in Kitchen vs. Appliances Energy Use", 
     col = colors[2])

plot(my.data[, 3], my.data[, 6], 
     xlab = expression(paste("X3_Temperature Outside (", degree, "C)")), 
     ylab = expression(paste("Appliances Energy Use (Wh)")), 
     main = "X3_Temperature Outside vs. Appliances Energy Use", 
     col = colors[3])

plot(my.data[, 4], my.data[, 6], 
     xlab = expression(paste("X4_Humidity Outside (%)")), 
     ylab = expression(paste("Appliances Energy Use (Wh)")), 
     main = "X4_Humidity Outside vs. Appliances Energy Use", 
     col = colors[4])

plot(my.data[, 5], my.data[, 6], 
     xlab = expression(paste("X5_Visibility (km)")), 
     ylab = expression(paste("Appliances Energy Use (Wh)")), 
     main = "X5_Visibility vs. Appliances Energy Use", 
     col = colors[5])


# Create 6 histograms for each X variable and Y
hist(my.data[, 1], xlab = "X1_Temperature in Kitchen", main = "Histogram: X1",col = colors[1] )
hist(my.data[, 2], xlab = "X2_Humidity in kitchen", main = "Histogram: X2",col = colors[2] )
hist(my.data[, 3], xlab = "X3_Temperature Outsid", main = "Histogram: X3",col = colors[3] )
hist(my.data[, 4], xlab = "X4_Humidity Outside", main = "Histogram: X4",col = colors[4] )
hist(my.data[, 5], xlab = "X5_Visibility", main = "Histogram: X5",col = colors[5] )

hist(my.data[, 6], xlab = "Y_Appliances, energy use", main = "Histogram: Y",col = colors[6] )


################################
#Question 2 - Transform the Data
################################

# let's take X1, X2, X3, X4 and Y
# I <- c("define your variable index") # Choose any four X variables (from X1, X2, X3, X4, X5) and Y
I <- c(1,2,3,4,6)
variables_to_transform <- my.data[,I]  # obtain a 340 by 5 matrix
variables_to_transform
summary(variables_to_transform)
cor(variables_to_transform)

# for each variable, you need to figure out a good data transformation method, 
# such as Polynomial, log and negation transformation. The k-S test and Skewness 
# calculation may be helpful to select the transformation method

# you need to manually check outliers and impute or remove them with reasonable judgement before applying any transformation

x_1 = variables_to_transform[,1] # Extracting and analyzing x_2
x_2 = variables_to_transform[,2]  # Assuming x_2 is the second column
x_3 = variables_to_transform[,3]  # Assuming x_3 is the third column
x_4 = variables_to_transform[,4] # Assuming x_4 is the fourth column
Y = variables_to_transform[, "Y"]  # Assuming "Y" is the column name for Y

# function for k-s test and skewness
ks_skew <- function(x) {
  k_s <- ks.test(x, "pnorm", mean = mean(x), sd = sd(x))
  s <- skewness(x)
  return(list(KS_Test = k_s, Skewness = s))
}

ks_skew(x_1) # D = 0.06653, p-value = 0.09858, Skewness = 0.2841379
ks_skew(x_2) # D = 0.06653, p-value = 0.09858, Skewness = 0.5122083
ks_skew(x_3) # D = 0.077692, p-value = 0.033, Skewness = 0.04691121
ks_skew(x_4) # D = 0.063724, p-value = 0.1264, Skewness = -0.4225832
ks_skew(Y)  # D = 0.24405, p-value < 2.2e-16,  Skewness = 1.703541

summary(x_1)

# log transformation for X1 variable
t1<-log(x_1)
t1
summary(t1)
ks_skew(t1) # 0.1020754

# log transformation for X2 variable
t2<-log(x_2)
t2
summary(t2)
ks_skew(t2) # 0.2593112


# ltransformation for X3 variable
t3 <- (x_3)^1.1 #P = 1.1
t3
summary(t3)
ks_skew(t3) # 0.1777721
 
# log transformation for X4 variable
t4<-(x_4)^3 #P = 3
t4
summary(t4)
ks_skew(t4)

# log transformation for Y
ty<-log(Y)
ty
summary(ty)
ks_skew(ty) # 0.5161406

# A Min-Max and/or Z-score transformation should then be used to adjust the scale of each variable

# minmax normalization
minmax <- function(x){
  (x - min(x))/(max(x)-min(x))
}

# z-score standardisation and scaling to unit interval
unit.z <- function(x){
  0.15*((x-mean(x))/sd(x)) + 0.5
}

data.transformed = array(0,c(340,5))
data.transformed[,1] <-minmax(t1) 
data.transformed[,2] <-minmax(t2) 
data.transformed[,3] <-minmax(t3) 
data.transformed[,4] <-minmax(t4) 
data.transformed[,5] <-minmax(ty) 
data.transformed
summary(data.transformed) # to check whether all transformation are done same way

current_directory <- getwd() # to find working directory
print(current_directory)

# Save this transformed data to a text file
write.table(data.transformed, "viraj-transformed.txt")  


##########################################
#Question 3 - Build models and investigate
##########################################

source("AggWaFit718.R")

data.transformed_copy <- as.matrix(read.table("Maduki-transformed.txt"))  # import your saved data

# Get weights for Weighted Arithmetic Mean with fit.QAM() 
fit.QAM(data.transformed_copy[,c(1:4,5)])


# Get weights for Power Mean p=0.5 and p=2 with fit.QAM()
fit.QAM(data.transformed_copy[,c(1:4,5)],output.1="QAM5output1.txt",stats.1="QAM_5_stats.txt", g=PM05,g.inv = invPM05)
fit.QAM(data.transformed_copy[,c(1:4,5)],output.1="QAM2output1.txt",stats.1="QAM_2_stats.txt",g=QM,g.inv = invQM)

# Get weights for Ordered Weighted Average with fit.OWA()
fit.OWA(data.transformed_copy[,c(1:4,5)],"OWAoutput1.txt","OWAstats1.txt")


# Get weights for Choquet Integral with fit.choquet() - Optional
fit.choquet(data.transformed_copy[,c(1:4,5)],output.1="Choutput1.txt",stats.1="Chstats1.txt",)



#######################################
#Question 4 - Use Model for Prediction
#######################################

# new_input has X1=22; X2=38; X3=4; X4=88.2; X5=34
new_input_to_transform <- c(22,38,4,88.2)

# transforming the four variables in the same way as in question 2 

# Transformation of X1
min_t1 = 2.720
max_t1 = 3.173
log_X1_value<-log(new_input_to_transform[1])
transformed_X1 <- (log_X1_value - min(min_t1))/(max(max_t1)-min(min_t1)) # 0.8190783

# Transformation of X2
min_t2 = 3.406
max_t2 = 3.980 
log_X2_value<-log(38)
transformed_X2 <- (log_X2_value - min(min_t2))/(max(max_t2)-min(min_t2)) # 0.4034602

# Transformation of X3
min_t3 = 0.2053
max_t3 = 8.7774
polynomial_X3_value <-(4)^1.1
transformed_X3 <- (polynomial_X3_value - min(min_t3))/(max(max_t3)-min(min_t3)) # 0.5120675

# Transformation of X4
min_t4 = 250047
max_t4 = 928878
polynomial_X4_value<-(88.2)^3
transformed_X4 <- (polynomial_X4_value - min(min_t4))/(max(max_t4)-min(min_t4)) # 0.6424014

new_input_tranformation = c(transformed_X1,transformed_X2,transformed_X3,transformed_X4)
write.table(new_input_tranformation,'viraj-y-transformed.txt')

# applying the transformed variables to the best model selected from Q3 for Y prediction

x = c(new_input_tranformation)
v = c(0.335487072928026,0,0.335487073,0.489262522,0.983393457131409,0.796832828282794,
      0.983393457131464,0.309578981561322,0.494869094719374,0.309578981561322,
      0.618205259927693,0.740313237616231,0.999999999999963,0.796832828282744,0.999999999999871)

val = choquet(x,v)
val # value of the weights using Choquet

# Reverse the transformation to convert back the predicted Y to the original scale and then round it to integer

min_ty = 2.996
max_ty = 5.298 

reverse_minmax_scale = (val*(max_ty-min_ty))+min_ty
reverse_minmax_scale

reverse_log_trans <- exp(reverse_minmax_scale)
predicted_val_y = reverse_log_trans
predicted_val_y  # 86.46159

# Comparing the values of Y, Y=100.

"Predicted value of Y = 86"
"Measured value of  Y = 100"

# References:

"Candanedo, L. (2017). "Appliances energy prediction." UCI Machine Learning Repository. [Online] Available at: https://doi.org/10.24432/C5VC8G"

"Berkelaar M, others (2020). _lpSolve: Interface to 'Lp_solve' v. 5.5 to Solve Linear/Integer Programs_. R
  package version 5.6.15, <https://CRAN.R-project.org/package=lpSolve>."

"Mueller, W. (Year). Inverse Power Functions. [Online] Available at: http://wmueller.com/precalculus/newfunc/invpwr.html (Accessed: August 31, 2023)."

"Simon James(2016) An Introduction to Data Analysis using Aggregation Functions in R,Springer, Deakin University Library, Melbourne "





