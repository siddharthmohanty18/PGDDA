####################  Gramener Case Study ####################
#
# Group Members:
# Abhijeet Verma
# Siddharth Mohanty
# Yogesh Deo
# Swaroop Venigalla
#
# Loading of file loan.csv into data frame
#
load <- read.csv("loan.csv",stringsAsFactors = F)
#
#
####################  Checkpoint1: Data Cleaning and preparation ####################
#
#
# Filtering out records which has status as Fully paid. Also fetch only the driving variables from the entire data set.
#
load_no_fully_paid <- subset(load, !(load$loan_status %in% grep("Fully Paid", levels(factor(load$loan_status)), value = T)) & load$loan_status != "")
#
# Create a column named loan_status_1 in load_no_fully_paid
#
loan_status_1 <- vector(mode="character", length = nrow(load_no_fully_paid))
#
# Add the new column to data frame load_no_fully_paid
#
load_no_fully_paid <- cbind(load_no_fully_paid, loan_status_1)
# 
# Convert loan_status_1 to charecter
#
load_no_fully_paid$loan_status_1 <- as.character(load_no_fully_paid$loan_status_1)
#
# rows with loan_status as "current" or "in grace period" are tagged as  "current_new"
#
load_no_fully_paid$loan_status_1[which(as.character(load_no_fully_paid$loan_status) == "Current" | as.character(load_no_fully_paid$loan_status) == "In Grace Period")] <- "current_new"
#
# rows with loan_status as " default" or "charged off" are tagged as "default_new"
#
load_no_fully_paid$loan_status_1[which(as.character(load_no_fully_paid$loan_status) %in% c("Charged Off","Default","Does not meet the credit policy. Status:Charged Off"))] <- "default_new"
#
# rows with loan_status as " late 16- 30 days" "late 31-120 days" are tagged as "late"
#
load_no_fully_paid$loan_status_1[which(as.character(load_no_fully_paid$loan_status) %in% c("Late (16-30 days)","Late (31-120 days)"))] <- "late"
#
# Create a new vector named int_rate_grp 
#
int_rate_grp <- vector(mode="character", length = nrow(load_no_fully_paid))
#
# Adding the column to the table load_no_fully_paid
#
load_no_fully_paid <- cbind(load_no_fully_paid,int_rate_grp)
#
# Converting to character type
#
load_no_fully_paid$int_rate_grp <- as.character(load_no_fully_paid$int_rate_grp)
#
# Create int_rate_grp such that int_rate < 10 is tagged "Low"
#
load_no_fully_paid$int_rate_grp[which(as.numeric(sub("%","",load_no_fully_paid$int_rate)) < 10)] <- "Low"
#
# int_rate (10-18) is tagged "Medium"
#
load_no_fully_paid$int_rate_grp[which(as.numeric(sub("%","",load_no_fully_paid$int_rate)) >= 10 & as.numeric(sub("%","",load_no_fully_paid$int_rate)) <= 18 )] <- "Medium"
#
# int_rate (>18) is tagged "High"
#
load_no_fully_paid$int_rate_grp[which(as.numeric(sub("%","",load_no_fully_paid$int_rate)) > 18)] <- "High"
#
# Imputing the column emp_length with mode value
#
# Function to calculate mode 
#
# Create the function.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
#
# Finding the mode as emp_length is categorical
#
mode.emp.length <- getmode(subset(load_no_fully_paid, load_no_fully_paid$emp_length != "n/a")$emp_length)
#
# Replacing the mode value in place of n/a value
#
load_no_fully_paid$emp_length[which(load_no_fully_paid$emp_length == "n/a")] <- mode.emp.length
#
# Creating a vector emp_len_grp 
#
emp_len_grp  <- vector(mode="character", length = nrow(load_no_fully_paid))
#
load_no_fully_paid <- cbind(load_no_fully_paid, emp_len_grp)
#
# Converting the column emp_len_grp to character
#
load_no_fully_paid$emp_len_grp <- as.character(load_no_fully_paid$emp_len_grp)
#
# emp_length (0-4) is tagged as "Junior"
#
load_no_fully_paid$emp_len_grp[which(load_no_fully_paid$emp_length %in% c("< 1 year","1 year","4 years","3 years","2 years"))] <- "Junior"
#
# emp_length (5-8) is tagged as "Mid-level"
#
load_no_fully_paid$emp_len_grp[which(load_no_fully_paid$emp_length %in% c("8 years","7 years","5 years","6 years"))] <- "Mid-level"
#
# emp_length (>8) is tagged as "Senior".
#
load_no_fully_paid$emp_len_grp[which(load_no_fully_paid$emp_length %in% c("10+ years","9 years"))] <- "Senior"
#
# Checking for presance of NA values in driver variable annual_inc
#
sum(is.na(load_no_fully_paid$annual_inc))
#
# Checking for presance of NA values in driver variable loan_amount
#
sum(is.na(load_no_fully_paid$loan_amnt))
#
# Checking for presance of NA values in driver variable funded_amnt
#
sum(is.na(load_no_fully_paid$funded_amnt))
#
# Checking for presance of NA values in driver variable int_rate
#
sum(is.na(load_no_fully_paid$int_rate))
#
# Checking for presance of NA values in driver variable grade
#
sum(is.na(load_no_fully_paid$grade))
#
# Checking for presance of NA values in driver variable dti
#
sum(is.na(load_no_fully_paid$dti))
#
# Checking for presance of NA values in driver variable emp_length
#
sum(is.na(load_no_fully_paid$emp_length))
#
# Checking for presance of NA values in driver variable Purpose
#
sum(is.na(load_no_fully_paid$purpose))
#
# Checking for presance of NA values in driver variable home_ownership
#
sum(is.na(load_no_fully_paid$home_ownership))
#
# Checking for presance of NA values in driver variable loan_status
#
sum(is.na(load_no_fully_paid$loan_status))
#
# None of the driving variables has any NA values. So we are good to get into descriptive analysis.
#
#
####################  Checkpoint2:  Exploratory Data Analysis ####################
#
#
# Load the required libraries
#
library(ggplot2)
library(moments)
#
# Univariate analysis of annual_inc
#
# Summary for annual_inc
#
summary(load_no_fully_paid$annual_inc)
#
# From the summary it seems there are outliers in the data as the max value is way far from mean and 3rd quartile
#
# Histogram for annual_inc
#
ggplot(load_no_fully_paid,aes(x=annual_inc))+geom_histogram(binwidth = 1000)+ ylim(0, 100) + xlim(0,550000)
#
# Finding skewness of annual_inc
#
skewness(load_no_fully_paid$annual_inc)
#
# As the value is 6.335396 annual_inc is positively skewed.
#
# Finding the kurtosis of annual_inc
#
kurtosis(load_no_fully_paid$annual_inc)
#
# Kurtosis value of 89.81387 means that the tail is heavy and outliers are present.
#
# Lets find out the standard deviation of annual_inc
#
sd(load_no_fully_paid$annual_inc)
#
# Lets check for outliers in the field annual_inc using boxplot and boxplot.stats
#
boxplot(load_no_fully_paid$annual_inc)
#
boxplot.stats(load_no_fully_paid$annual_inc)
#
# Lets replace the outlier value with the median value of annual_inc
#
load_no_fully_paid$annual_inc[which(load_no_fully_paid$annual_inc %in% boxplot.stats(load_no_fully_paid$annual_inc)$out)] <- median(load_no_fully_paid$annual_inc)
#
# Summary of annual_inc is
#
summary(load_no_fully_paid$annual_inc)
#
# Lets check the distribution, boxplot and boxplot.stats again
#
ggplot(load_no_fully_paid,aes(x=annual_inc))+geom_histogram(binwidth = 1000)+ ylim(0, 100) + xlim(0,136000)
#
boxplot(load_no_fully_paid$annual_inc)
#
boxplot.stats(load_no_fully_paid$annual_inc)
#
# Standard deviation of annual_inc after outlier treatment
#
sd(load_no_fully_paid$annual_inc)
#
# Univariate analysis of loan_amnt
#
# Summary for loan_amnt
#
summary(load_no_fully_paid$loan_amnt)
#
# Histogram for loan_amnt
#
ggplot(load_no_fully_paid,aes(x=loan_amnt))+geom_histogram(binwidth = 1000)
#
# Finding skewness of loan_amnt
#
skewness(load_no_fully_paid$loan_amnt)
#
# As the value is 0.8388929 loan_amnt is slightly positively skewed.
#
# Finding the kurtosis of loan_amnt
#
kurtosis(load_no_fully_paid$loan_amnt)
#
# Kurtosis value of 3.069213 means that the tail is slightly heavy and few outliers are present.
#
# Lets find out the standard deviation of annual_inc
#
sd(load_no_fully_paid$loan_amnt)
#
# Lets check for outliers in the field loan_amnt using boxplot and boxplot.stats
#
boxplot(load_no_fully_paid$loan_amnt)
#
boxplot.stats(load_no_fully_paid$loan_amnt)
#
# As per boxplot.stats result for loan_amnt there arnt any outliers
#
# Univariate analysis of funded amount
#
# Summary for funded_amnt
#
summary(load_no_fully_paid$funded_amnt)
#
# Histogram for funded_amnt
#
ggplot(load_no_fully_paid,aes(x=funded_amnt))+geom_histogram(binwidth = 1000)
#
# Finding skewness of funded_amnt
#
skewness(load_no_fully_paid$funded_amnt)
#
# As the value is 0.8630062 funded_amnt is slightly positively skewed.
#
# Finding the kurtosis of funded_amnt
#
kurtosis(load_no_fully_paid$funded_amnt)
#
# Kurtosis value of 3.237569 means that the tail is light and few outliers are present.
#
# Lets find out the standard deviation of annual_inc
#
sd(load_no_fully_paid$funded_amnt)
#
# Lets check for outliers in the field funded_amnt using boxplot and boxplot.stats
#
boxplot(load_no_fully_paid$funded_amnt)
#
boxplot.stats(load_no_fully_paid$funded_amnt)
#
# Lets replace the outlier value with the median value of funded_amnt
#
load_no_fully_paid$funded_amnt[which(load_no_fully_paid$funded_amnt %in% boxplot.stats(load_no_fully_paid$funded_amnt)$out)] <- median(load_no_fully_paid$funded_amnt)
#
# Summary of funded_amnt is
#
summary(load_no_fully_paid$funded_amnt)
#
# Lets check the distribution,boxplot and boxplot.stats again
#
ggplot(load_no_fully_paid,aes(x=funded_amnt))+geom_histogram(binwidth = 1000)
#
boxplot(load_no_fully_paid$funded_amnt)
#
boxplot.stats(load_no_fully_paid$funded_amnt)
#
# Standard deviation of funded_amnt after outlier treatment
#
sd(load_no_fully_paid$funded_amnt)
#
# Univariate analysis of int_rate_grp 
#
# Lets find the count in each percentage categories
#
table(factor(load_no_fully_paid$int_rate_grp))
#
# Lets plot a bar chart for the same
#
ggplot(load_no_fully_paid,aes(x=int_rate_grp)) + geom_bar()
#
# Medium category is the one which has most number of counts
#
# Univariate analysis of grade 
#
# Lets find the count in each grade categories
#
table(factor(load_no_fully_paid$grade))
#
# Lets plot a bar chart for the same
#
ggplot(load_no_fully_paid,aes(x=grade)) + geom_bar()
#
# B,C and D grades are most frequently occuring
#
# Univariate analysis of dti
#
# Summary for dti
#
summary(load_no_fully_paid$dti)
#
# Histogram for dti
#
ggplot(load_no_fully_paid,aes(x=dti))+geom_histogram(binwidth = 1)
#
# Finding skewness of dti
#
skewness(load_no_fully_paid$dti)
#
# As the value is -0.1491589 dti is slightly negatively skewed.
#
# Finding the kurtosis of dti
#
kurtosis(load_no_fully_paid$dti)
#
# Kurtosis value of 2.211636 means that the tail is light and few outliers are present.
#
# Lets find out the standard deviation of dti
#
sd(load_no_fully_paid$dti)
#
# Lets check for outliers in the field dti using boxplot and boxplot.stats
#
boxplot(load_no_fully_paid$dti)
#
boxplot.stats(load_no_fully_paid$dti)
#
# As there are no outliers outlier treatment is not done for this variable
#
# Univariate analysis of emp_len_grp  
#
# Lets find the count in each emp_len_grp  categories
#
table(factor(load_no_fully_paid$emp_len_grp))
#
# Lets plot a bar chart for the same
#
ggplot(load_no_fully_paid,aes(x=emp_len_grp)) + geom_bar()
#
# Junior level has most number of counts
#
# Univariate analysis of purpose  
#
# Lets find the count in each purpose  categories
#
table(factor(load_no_fully_paid$purpose))
#
# Lets plot a bar chart for the same
#
ggplot(load_no_fully_paid,aes(x=purpose)) + geom_bar()
#
# People who take loan for debt consolidation default the most
#
# Univariate analysis of home_ownership  
#
# Lets find the count in each home_ownership  categories
#
table(factor(load_no_fully_paid$home_ownership))
#
# Lets plot a bar chart for the same
#
ggplot(load_no_fully_paid,aes(x=home_ownership)) + geom_bar()
#
# People who have motgaged or rented their home default the most
#
# Univariate analysis of loan_status_1  
#
# Lets find the count in each home_ownership  categories
#
table(factor(load_no_fully_paid$loan_status_1))
#
# Lets plot a bar chart for the same
#
ggplot(load_no_fully_paid,aes(x=loan_status_1)) + geom_bar()
#
# People who fall in default_new category default the most
#
# Multivariate analysis
#
# Finding correlations for all different pairs of continuous variables i.e annual_inc, loan_amnt, funded_amnt and dti
#
# Creating a data frame with only columns annual_inc, loan_amnt, funded_amnt and dti in it
#
cor_data_frame <- data.frame(load_no_fully_paid$annual_inc,load_no_fully_paid$loan_amnt, load_no_fully_paid$funded_amnt, load_no_fully_paid$dti)
#
# Change column names
#
colnames(cor_data_frame) <- c("annual_inc","loan_amnt","funded_amnt","dti")
# 
cor(cor_data_frame)
#
# Looking at the values there is a correlation between the variables annual_inc and loan_amnt. Also there is a strong correlation between funded_amnt and loan_amnt.
#
# Lets check graphically for annual_inc vs loan_amnt
#
ggplot(load_no_fully_paid, aes(annual_inc, loan_amnt)) + geom_point() 
#
# Lets check graphically for annual_inc vs funded_amnt
#
ggplot(load_no_fully_paid, aes(annual_inc, funded_amnt)) + geom_point() 
#
# Lets check graphically for annual_inc vs dti
#
ggplot(load_no_fully_paid, aes(annual_inc, dti)) + geom_point() 
#
# Lets check graphically for loan_amnt vs funded_amnt
#
ggplot(load_no_fully_paid, aes(loan_amnt, funded_amnt)) + geom_point() 
#
# Lets check graphically for loan_amnt vs dti
#
ggplot(load_no_fully_paid, aes(loan_amnt, dti)) + geom_point() 
#
# Lets check graphically for funded_amnt vs dti
#
ggplot(load_no_fully_paid, aes(funded_amnt, dti)) + geom_point() 
#
# Distribution of all the driver variables across different levels of two categorical variables#
#
# Make plots to show how the continuous variables vary across the three levels of loan_status_1
#
ggplot(load_no_fully_paid, aes(x = annual_inc)) + geom_histogram(binwidth = 100) + facet_grid(~loan_status_1)+ ylim(0, 80)
#
ggplot(load_no_fully_paid, aes(x = loan_amnt)) + geom_histogram(binwidth = 100) + facet_grid(~loan_status_1)+ ylim(0, 350)
#
ggplot(load_no_fully_paid, aes(x = funded_amnt)) + geom_histogram(binwidth = 100) + facet_grid(~loan_status_1)+ ylim(0, 100)
#
ggplot(load_no_fully_paid, aes(x = dti)) + geom_histogram(binwidth = 1) + facet_grid(~loan_status_1)
#
# Make plots to show how the continuous variables vary across the three levels of int_rate_grp
#
ggplot(load_no_fully_paid, aes(x = annual_inc)) + geom_histogram(binwidth = 100) + facet_grid(~int_rate_grp)+ ylim(0, 55)
#
ggplot(load_no_fully_paid, aes(x = loan_amnt)) + geom_histogram(binwidth = 100) + facet_grid(~int_rate_grp)
#
ggplot(load_no_fully_paid, aes(x = funded_amnt)) + geom_histogram(binwidth = 100) + facet_grid(~int_rate_grp)
#
ggplot(load_no_fully_paid, aes(x = dti)) + geom_histogram(binwidth = 1) + facet_grid(~int_rate_grp)
#
#
####################  Checkpoint3: Hypothesis Testing ####################
#
# 
# Retrieve annual_inc for loan_status_1 as default_new
#
mean_annual_inc_default_new <- subset(load_no_fully_paid, load_no_fully_paid$loan_status_1 == "default_new")$annual_inc
#
# Retrieve annual_inc for loan_status_1 as current_new
#
mean_annual_inc_current_new <- subset(load_no_fully_paid, load_no_fully_paid$loan_status_1 == "current_new")$annual_inc
#
# As this is a sample data we will be using t statistics to compare the means
#
myTest1 <- t.test(x = mean_annual_inc_default_new, y = mean_annual_inc_current_new, conf.level = 0.95, alternative = "two.sided", var.equal = F)
#
# The test statistics for mean_annual_inc_default_new and mean_annual_inc_current_new are mentioned below  
#
myTest1
#
# Displaying the p-value
#
unname(myTest1$p.value)
#
# Here the t statistic is -10.639 which is less than -1.96 hence the hypothesis that both the 
# means are equal is rejected.
#
# Retrieve loan_amnt for loan_status_1 as default_new
#
mean_loan_amnt_default_new <- subset(load_no_fully_paid, load_no_fully_paid$loan_status_1 == "default_new")$loan_amnt
#
# Retrieve loan_amnt for loan_status_1 as current_new
#
mean_loan_amnt_current_new <- subset(load_no_fully_paid, load_no_fully_paid$loan_status_1 == "current_new")$loan_amnt
#
# As this is a sample data we will be using t statistics to compare the means
#
myTest2 <- t.test(x = mean_loan_amnt_default_new, y = mean_loan_amnt_current_new, conf.level = 0.95, alternative = "two.sided", var.equal = F)
#
# The test statistics for mean_annual_inc_default_new and mean_annual_inc_current_new are mentioned below  
#
myTest2
#
# Displaying the p-value
#
unname(myTest2$p.value)
#
# Here the t statistic is -19.407 which is less than -1.96 hence the hypothesis that both the 
# means are equal is rejected.
#
# Retrieve funded_amnt for loan_status_1 as default_new
#
mean_funded_amnt_default_new <- subset(load_no_fully_paid, load_no_fully_paid$loan_status_1 == "default_new")$funded_amnt
#
# Retrieve loan_amnt for loan_status_1 as current_new
#
mean_funded_amnt_current_new <- subset(load_no_fully_paid, load_no_fully_paid$loan_status_1 == "current_new")$funded_amnt
#
# As this is a sample data we will be using t statistics to compare the means
#
myTest3 <- t.test(x = mean_funded_amnt_default_new, y = mean_funded_amnt_current_new, conf.level = 0.95, alternative = "two.sided", var.equal = F)
#
# The test statistics for mean_annual_inc_default_new and mean_annual_inc_current_new are mentioned below  
#
myTest3 
#
# Displaying the p-value
#
unname(myTest3$p.value)
#
# Here the t statistic is -19.016 which is less than -1.96 hence the hypothesis that both the 
# means are equal is rejected.
#
# Retrieve dti for loan_status_1 as default_new
#
mean_dti_default_new <- subset(load_no_fully_paid, load_no_fully_paid$loan_status_1 == "default_new")$dti
#
# Retrieve dti for loan_status_1 as current_new
#
mean_dti_current_new <- subset(load_no_fully_paid, load_no_fully_paid$loan_status_1 == "current_new")$dti
#
# As this is a sample data we will be using t statistics to compare the means
#
myTest4 <- t.test(x = mean_dti_default_new, y = mean_dti_current_new, conf.level = 0.95, alternative = "two.sided", var.equal = F)
#
# The test statistics for mean_annual_inc_default_new and mean_annual_inc_current_new are mentioned below  
#
myTest4 
#
# Displaying the p-value
#
unname(myTest4$p.value)
#
# Here the t statistic is -3.093 which is less than -1.96 hence the hypothesis that both the 
# means are equal is rejected.
#
# Retrieve annual_inc for int_rate_grp  as High
#
mean_annual_inc_high <- subset(load_no_fully_paid, load_no_fully_paid$int_rate_grp == "High")$annual_inc
#
# Retrieve annual_inc for int_rate_grp as Low
#
mean_annual_inc_low <- subset(load_no_fully_paid, load_no_fully_paid$int_rate_grp == "Low")$annual_inc
#
# As this is a sample data we will be using t statistics to compare the means
#
myTest5 <- t.test(x = mean_annual_inc_high, y = mean_annual_inc_low, conf.level = 0.95, alternative = "two.sided", var.equal = F)
#
# The test statistics for mean_annual_inc_default_new and mean_annual_inc_current_new are mentioned below  
#
myTest5 
#
# Displaying the p-value
#
unname(myTest5$p.value)
#
# Here the t statistic is 12.034 which is more than 1.96 hence the hypothesis that both the 
# means are equal is rejected.
#
# Retrieve loan_amnt for int_rate_grp  as High
#
mean_loan_amnt_high <- subset(load_no_fully_paid, load_no_fully_paid$int_rate_grp == "High")$loan_amnt
#
# Retrieve loan_amnt for int_rate_grp as Low
#
mean_loan_amnt_low <- subset(load_no_fully_paid, load_no_fully_paid$int_rate_grp == "Low")$loan_amnt
#
# As this is a sample data we will be using t statistics to compare the means
#
myTest6 <- t.test(x = mean_loan_amnt_high, y = mean_loan_amnt_low, conf.level = 0.95, alternative = "two.sided", var.equal = F)
#
# The test statistics for mean_annual_inc_default_new and mean_annual_inc_current_new are mentioned below  
#
myTest6 
#
# Displaying the p-value
#
unname(myTest6$p.value)
#
# Here the t statistic is 29.92 which is more than 1.96 hence the hypothesis that both the 
# means are equal is rejected.
#
# Retrieve funded_amnt for int_rate_grp  as High
#
mean_funded_amnt_high <- subset(load_no_fully_paid, load_no_fully_paid$int_rate_grp == "High")$funded_amnt
#
# Retrieve funded_amnt for int_rate_grp as Low
#
mean_funded_amnt_low <- subset(load_no_fully_paid, load_no_fully_paid$int_rate_grp == "Low")$funded_amnt
#
# As this is a sample data we will be using t statistics to compare the means
#
myTest7 <- t.test(x = mean_funded_amnt_high, y = mean_funded_amnt_low, conf.level = 0.95, alternative = "two.sided", var.equal = F)
#
# The test statistics for mean_annual_inc_default_new and mean_annual_inc_current_new are mentioned below  
#
myTest7 
#
# Displaying the p-value
#
unname(myTest7$p.value)
#
# Here the t statistic is 26.684 which is more than 1.96 hence the hypothesis that both the 
# means are equal is rejected.
#
# Retrieve dti for int_rate_grp  as High
#
mean_dti_high <- subset(load_no_fully_paid, load_no_fully_paid$int_rate_grp == "High")$dti
#
# Retrieve dti for int_rate_grp as Low
#
mean_dti_low <- subset(load_no_fully_paid, load_no_fully_paid$int_rate_grp == "Low")$dti
#
# As this is a sample data we will be using t statistics to compare the means
#
myTest8 <- t.test(x = mean_dti_high, y = mean_dti_low, conf.level = 0.95, alternative = "two.sided", var.equal = F)
#
# The test statistics for mean_annual_inc_default_new and mean_annual_inc_current_new are mentioned below  
#
myTest8 
#
# Displaying the p-value
#
unname(myTest8$p.value)
#
# Here the t statistic is 3.9043 which is more than 1.96 hence the hypothesis that both the 
# means are equal is rejected.
