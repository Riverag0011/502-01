###### R Basics ----
# downloading and accessing packages in R
# install.packages("ggplot2", dependencies = T)
library(ggplot2)

# open the bank_marketing_training data set
bank_train <- read.csv(file = "/Users/antran/Google Drive/USD_Teaching/ADS-502/WebsiteDataSets/bank_marketing_training.csv")

# shape of dataset
dim(bank_train)

# names of variables
names(bank_train)

# access the first record in the bank_train data set
bank_train[1, ]

# access the first, third, and fourth record
bank_train[c(1,3,4), ]

# access the records of the first and third variables
bank_train[, c(1, 3)]

# head() shows first 6 rows of dataset by default
head(bank_train)

# access the records of a specific variable by name
bank_train$age

###### Data Preparation ----
# change misleading field values ----
# identify each instance of 999 in the days_since_previous variable,
# and replace it with the R code value for a missing value, NA
bank_train$days_since_previous <- ifelse(test = bank_train$days_since_previous == 999,
                                         yes = NA,
                                         no = bank_train$days_since_previous)

# create a histogram of the variable using the hist() command
hist(bank_train$days_since_previous,
     xlab = "days_since_previous",
     main = "Histogram of days_since_previous - Missing Values replaced by NA")

# create histogram using "ggplot2" 
ggplot(bank_train, aes(x=days_since_previous)) + 
  geom_histogram(binwidth=2, fill='light blue', color='black') +
  ggtitle('Histogram of days_since_previous')

###### Re-express Categorical Field Values
# First, we need to install and load the plyr package
# install.packages("plyr", dependencies = T)
library(plyr)

# specify which values of education go with which numeric values
edu.num <- revalue(x = bank_train$education, replace = c("illiterate" = 0,
                                                         "basic.4y" = 4,
                                                         "basic.6y" = 6,
                                                         "basic.9y" = 9,
                                                         "high.school" = 12,
                                                         "professional.course" = 12,
                                                         "university.degree" = 16, "unknown" = NA))

# The object edu.num is ordinal, so we can convert its values to numbers
bank_train$education_numeric <- as.numeric(edu.num)
bank_train$education_numeric

###### Standardize Numeric Fields
# The command we will use, scale(), is included in R so we don't have to load external libraries
bank_train$age_z <- scale(x = bank_train$age)
bank_train$age_z

# in order to plot graphs side by side, need to install package "egg"
# install.packages("egg", dependencies = T)
library(egg)

# plot original "age" variable vs standardized "age"
age <- ggplot(bank_train, aes(x=age)) + 
  geom_histogram(fill='light blue', color='black') +
  ggtitle('Age')

scaled_age <- ggplot(bank_train, aes(x=age_z)) + 
  geom_histogram(fill='magenta', color='black') +
  ggtitle('Standardized Age')

ggarrange(age, scaled_age, nrow=1, ncol=2)

###### Identify Outliers
# Rule of thumb is that a data value is an outlier if its z-value is either
# greater than 3, or less than -3. For this example, we will continue using the 
# age_z variable that we created in the previous section
bank_outliers <- bank_train[which(bank_train$age_z < -3 | bank_train$age_z > 3), ]
dim(bank_outliers)

# remove the rows that are considered outliers from their "age_z"
outlier_id <- as.numeric(rownames(bank_outliers))
bank_train <- bank_train[-outlier_id, ]
