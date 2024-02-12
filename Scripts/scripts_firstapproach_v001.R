# Load data
data <- read.csv('data/heart_mod_2021-02-04.csv')

dim(data)
colnames(data)

head(data)
summary(data)

# Transform categorical variable to R factors
data$sex <- as.factor(data$sex)
data$cp <- as.factor(data$cp) # Chest pain type (4 types)
data$fbs <- as.factor(data$fbs) # Whether the level of sugar in the blood is higher than 120 mg/dl or not. 
data$restecg <- as.factor(data$restecg) # Results of the electrocardiogram on rest (3 types)
data$exang <- as.factor(data$exang) # Whether the patient had angina during exercise
data$slope <- as.factor(data$slope) # Slope of the ST segment during the most demanding part of the exercise (3 types)
data$thal <- as.factor(data$thal) # Results of the blood flow observed via the radioactive dye. (3 types)
data$target <- as.factor(data$target) # heart attack y/n

# Give a better name to the factor values for the graphs
levels(data$sex) <- c("Female", "Male")
levels(data$cp) <- c("Asymptomatic", "Atypical angina", "No angina", "Typical angina")
levels(data$fbs) <- c("No", "Yes")
levels(data$restecg) <- c("Hypertrophy", "Normal", "Abnormalities")
levels(data$exang) <- c("No", "Yes")
levels(data$slope) <- c("Descending", "Flat", "Ascending")
levels(data$thal) <- c("Fixed defect", "Normal flow", "Reversible defect")
levels(data$target) <- c("Yes", "No")

names_num <- c("X","age","trestbps", "chol", "thalach")
names_fact <- colnames(data)[!colnames(data) %in% names_num]

data_num <- data[,names_num]
summary(data_num)
data_fact <- data[,names_fact]
summary(data_fact)





# hist
hist(data$age, main = "Histograma de la edad", xlab = "Frecuencia",ylab = "Años", col = 2, 
     breaks = 20)

hist(log(data$age), main = "Histograma de la edad", xlab = "Frecuencia",ylab = "Años", col = 2, 
     breaks = 20)

shapiro.test(log(data$age))
shapiro.test(rnorm(100, mean = 5, sd = 3))
shapiro.test(runif(100, min = 2, max = 4))

boxplot(data$age)

x <- c(0.103, 0.528, 0.221, 0.260, 0.091,
       1.314, 1.732, 0.244, 1.981, 0.273,
       0.461, 0.366, 1.407, 0.079, 2.266)

# Histogram of the data
hist(x)

library(MASS)

boxcox(lm(x ~ 1))







# for

sapply(data_num, shapiro.test)
sapply(data_num, function(x) boxplot(x))

par(mfrow=c(2,3))
for(p in 1:length(data_num))
{
  boxplot(data_num[,p], main = names(data_num)[p])
}

par(mfrow=c(2,5))
for(p in 1:length(data_fact))
{
  barplot(table(data_fact[,p]), main = names(data_fact)[p])
}

par(mfrow=c(1,1))


barplot(table(data_fact$sex, data_fact$target))
chisq.test(table(data_fact$sex, data_fact$target))

# var = 0

sapply(data, var)

# missings

summary(data)
hist(data$ca)

#  1. remove
#  2. give closest mean
#  3. multiple imputation

# dummies



# centered

age_mean <- mean(data$age)
data$age_cent <- data$age - age_mean
mean(data$age_cent)
hist(data$age_cent)

# normalized
age_cent_sd <- sd(data$age_cent)
data$age_cent_norm <- data$age_cent / age_cent_sd
hist(data$age_cent_norm)

data$age_scale <- scale(data$age)

plot(data$age_cent_norm, data$age_scale)

# bivariate

mod <- glm(target ~ age, data = data, family = binomial(link = "logit"))
mod
summary(mod)
ss <- summary(mod)
ss$coefficients

# multivariate

# train and test
num_pac <- nrow(data)
pac_train <- sample(1:num_pac,num_pac * 0.8)
train <- data[pac_train,]
test <- data[-pac_train,]

mod <- glm(target ~ age, data = train, family = binomial(link = "logit"))
pred.val <- predict(mod, type = "response", newdata = test)



df55 <- data.frame(test$target, pred.val)


# roc
roc <- pROC::roc(test$target, pred.val, plot = TRUE)

# If p exceeds threshold of 0.5, 1 else 0
hd_or_nohd <- ifelse(pred.val > 0.5, 1, 0)
hd_or_nohd <- as.factor(as.numeric(hd_or_nohd))

# Convert to factor: p_class
# p_class <- factor(hd_or_nohd, levels = levels(test$target))


# Create confusion matrix
library(caret)
# confusionMatrix(hd_or_nohd, test$target)
xt <- table(hd_or_nohd, test$target)
caret::confusionMatrix(xt)





# Carga la librería MASS
library(MASS)

# Genera datos de ejemplo (reemplaza esto con tus datos reales)
mi_variable <- (1:1000)

# Calcula la lambda óptima y la transformación de Box-Cox
boxcox_result <- boxcox(mi_variable ~ 1)
lambda_optima <- boxcox_result$x[which.max(boxcox_result$y)]

x <- rnorm(1000)
shapiro.test(x)






