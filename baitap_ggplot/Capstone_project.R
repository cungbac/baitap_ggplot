
# Library
library(dplyr)
library(readxl)
library(summarytools)
library(ggplot2)
library(labelled)
library(tidyr)
library(broom)
library(ggpubr)


# PART 1

## 1.Import data
data <- read_excel('/Users/cungbac/Documents/Learning/4. R programming/10. Project/1. Capstone_project/Data 1_Bac.xlsx')

## 2. Detect & Remove illogical values
summary(data) # general view data

data$Dum <- data$Dum %>%
  as.integer()

data %>%
  filter(Dum == 30) #Dum at row 51, ID = 50

data$Dum <- data$Dum %>%
  factor(levels = c(1,2,3),labels = c('Nam','Nu','Khac'))
## 3. Imputation missing value

data %>%
  filter(is.na(Dum)|is.na(X1)) #Missing value: X1 - 82; Dum - 73

### Replace missing value of numeric variable by its mean

data_rmv <- data %>%
  mutate(X1 = replace(X1,is.na(X1),mean(X1,na.rm = T)))

summary(data_rmv)

### Replace missing value of categorical variable by its mode

level <- table(data_rmv$Dum)
mode_level <- names(level)[which(level == max(level))]

data_rmv <- data_rmv %>%
  mutate(Dum = replace(Dum,is.na(Dum),mode_level))

summary(data_rmv)

## 4. Assign lables

  #data_rmv$Dum <- data_rmv$Dum %>%
    #factor(levels = c(1,2,3),labels = c('Nam','Nu','Khac'))

str(data_rmv)

# PART 2 - DISCRIPTIVE STATISTICS

## 5. Categorical Variables
  
tab <- as.data.frame(table(data_rmv$Dum))

tab %>%
  mutate(prop = Freq/sum(Freq))

## 6. Numeric Variables
data_rmv %>%
  select(-Dum) %>%
  dfSummary() %>%
  view()

stat_X1 <- data_rmv %>%
  select(X1) %>%
  summarize(mean = mean(X1),
            median = median(X1),
            min = min(X1),
            max = max(X1),
            sd = sd(X1))

##7. Descriptive statistics for multiple variables
stat_all <- data_rmv %>%
  group_by(Dum) %>%
  summarise(mean1 = mean(X1),median1 = median(X1),min1 = min(X1),max1 = max(X1), sd1 = sd(X1),
            mean2 = mean(X2),median2 = median(X2),min2 = min(X2),max2 = max(X2), sd2 = sd(X2),
            mean3 = mean(X3),median3 = median(X3),min3 = min(X3),max3 = max(X3), sd3 = sd(X3),
            mean4 = mean(X4),median4 = median(X4),min4 = min(X4),max4 = max(X4), sd4 = sd(X4),
            mean4 = mean(Y),median4 = median(Y),min4 = min(Y),max4 = max(Y), sd4 = sd(Y))

##8. Quantile of the dependent variable
quantile(data$Y)

##9. Plot data - the dependent variable
### Histogram
data_rmv %>%
  ggplot(aes(x = Y)) +
  geom_histogram()
  
### Density
data_rmv %>%
  ggplot(aes(x = Y)) +
  geom_density()

### Box plot
data_rmv %>%
  ggplot(aes(y = Y)) +
  geom_boxplot()

##10. Plot data - the dependent variable - color = Dum
### Histogram
data_rmv %>%
  ggplot(aes(x = Y)) +
  geom_histogram(aes(color = Dum))

### Density
data_rmv %>%
  ggplot(aes(x = Y)) +
  geom_density(aes(color = Dum))

### Box plot
data_rmv %>%
  ggplot(aes(y = Y,color = Dum)) +
  geom_boxplot()

## 11. Scatter Y vs X1
data_rmv %>%
  ggplot(aes(x = X1, y = Y)) + 
  geom_point()

## 12. Scatter Y vs X1, color = Dum
data_rmv %>%
  ggplot(aes(x = X1, y = Y)) +
  geom_point(aes(color = Dum))

##. 13. Scatter Y vs X1, find linear regression
model <- lm( Y ~ X1, data = data_rmv)
summary(model)

model_td <- tidy(model)

data_rmv %>%
  ggplot(aes(x = X1, y = Y)) +
  geom_point() + 
  geom_smooth(method = 'lm',formula = y  ~ x)

##. 14 Scatter Y vs X1, linear regression, color = Dum
data_rmv %>%
  ggplot(aes(x= X1, y = Y,color = Dum)) +
  geom_point()+
  geom_smooth(method = 'lm', se = T)

# PART 3 - CORRELATION MATRIX ANALYSTIC
## Correlation matrix of dependent variables

cor(data_rmv$X1,data_rmv$X2,method = 'pearson')

mat <- data_rmv %>%
  select(X1,X2,X3,X4)

mat <- as.matrix(mat)

cor(mat,method = 'pearson') # 3 methods to find correlation between 2 vetors: pearson, kendall, spearman





