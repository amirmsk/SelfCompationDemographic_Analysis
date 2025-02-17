# Load necessary libraries
library(haven)
library(dplyr)
library(tidyr)
library(devtools)
library(sjlabelled)
library(stringr)
library(ggplot2)
library(car)
library(corrplot)
library(e1071)
library(caTools)
library(class)
library(fastDummies)
library(party)
library(magrittr)
library(rpart)
library(rpart.plot)
library(lsr)

# Import file
demo <- read_excel('C:\\Users\\user\\Documents\\dr.nofresti\\compation\\1402\\shafaghat2.xlsx')

# Clean the dataset
demo1 <- demo %>% drop_na() %>% distinct()

# Processing the data
devtools::install_github("strengejacke/sjlabelled")
l.demo <- as_character(demo[, c(2,3,4,5,6,7)])

# Replace and subset
demo2 <- subset(demo1, select = 10:35)
shenase <- subset(demo1, select = 1)
demo2 <- cbind(shenase, l.demo, demo2)
table(demo3 == "NA")

# Reverse columns
reversed_values <- list(demo2$q1, demo2$q2, demo2$q4, demo2$q6, demo2$q8, demo2$q11, demo2$q13, demo2$q16, demo2$q18, demo2$q20, demo2$q21, demo2$q24, demo2$q25)
reversing <- function(x) {
  x = as.character(x)
  str_replace_all(x, c("1" = "five" , "2" = "four", "3" = "3", "4" = "two", "5" = "one"))
}
off.demo <- sapply(reversed_values, reversing)
off.demo1 <- data.frame(off.demo)
colnames(off.demo1) <- c('q1', 'q2', 'q4', 'q6', 'q8', 'q11', 'q13', 'q16', 'q18', 'q20', 'q21', 'q24', 'q25')
off.demo1[off.demo1 == 'one'] <- 1
off.demo1[off.demo1 == 'two'] <- 2
off.demo1[off.demo1 == 'three'] <- 3
off.demo1[off.demo1 == 'four'] <- 4
off.demo1[off.demo1 == 'five'] <- 5

# Replace reversed scales
demo2 <- subset(demo2, select = c(-8, -9, -11, -13, -15 , -18, -20, -23, -25, -27, -28, -31, -32))
demo2 <- cbind(demo2, off.demo1)
demo2$q1 <- as.numeric(as.character(demo2$q1))
demo2$q2 <- as.numeric(as.character(demo2$q2))
demo2$q4 <- as.numeric(as.character(demo2$q4))
demo2$q6 <- as.numeric(as.character(demo2$q6))
demo2$q8 <- as.numeric(as.character(demo2$q8))
demo2$q11 <- as.numeric(as.character(demo2$q11))
demo2$q13 <- as.numeric(as.character(demo2$q13))
demo2$q16 <- as.numeric(as.character(demo2$q16))
demo2$q18 <- as.numeric(as.character(demo2$q18))
demo2$q20 <- as.numeric(as.character(demo2$q20))
demo2$q21 <- as.numeric(as.character(demo2$q21))
demo2$q24 <- as.numeric(as.character(demo2$q24))
demo2$q25 <- as.numeric(as.character(demo2$q25))

# Create new factors
demo2$mehrabani.ba.khod <- (demo2$q5 + demo2$q12 + demo2$q19 + demo2$q23 + demo2$q26)
demo2$enzeva <- (demo2$q25 + demo2$q18 + demo2$q13 + demo2$q4)
demo2$ghezavat_nesbat_khod <- ( demo2$q21 + demo2$q16 + demo2$q11 + demo2$q8 + demo2$q1)
demo2$eshterakat_ensani <- (demo2$q15 + demo2$q10 + demo2$q7 + demo2$q3)
demo2$zehn_aggahi <- (demo2$q22 + demo2$q17 + demo2$q14 + demo2$q9)
demo2$hamanandsazi_efrati  <- (demo2$q24  + demo2$q20 + demo2$q6 + demo2$q2)

# Wrangle the data
demo3 <- subset(demo2, select= -(8:33)) 
demo3 <- subset(demo3, select = -6)

# Last column shafaghat
demo3$shafaghatt <- (demo3$enzeva + demo3$mehrabani.ba.khod + demo3$hamanandsazi_efrati + demo3$zehn_aggahi + demo3$enzeva + demo3$ghezavat_nesbat_khod)

# Removing useless variables
demo3$tahsilat[demo3$tahsilat == "tahsilate hozavi"] <- NA
demo3$tahsilat[demo3$tahsilat == "hamsaram fot karde"] <- NA
demo3 <- demo3 %>% na.omit()

# Change age
age <- demo3 %>% select(age)
age <- as_numeric(age)
age <- as_tibble(age)
Age <- as_numeric(age)

# Analyse
demo3 %>% group_by(tahsilat) %>% summarize(mean(shafaghatt))
demo3 %>% group_by(gender) %>% summarize(sd(shafaghatt))
demo3 %>% group_by(vaziat.taahol) %>% summarize(mean(shafaghatt))
demo3 %>% group_by(vaziat.eshteghal) %>% summarize(sd(shafaghatt))

# Descriptive Statistics
demo3 %>% group_by(gender) %>% summarize(n())
mean(demo3$shafaghatt)
sd(demo3$shafaghatt)
table(demo3$shafaghatt)

# Plotting
boxplot(data = demo3, mehrabani.ba.khod~gender, xlab = "جنسیت", ylab = "مهربانی.با.خود")
boxplot(data = demo3, mehrabani.ba.khod~vaziat.shoghl, xlab ="وضعیت_شغل",  ylab = "مهربانی.با.خود")
boxplot(data = demo3, mehrabani.ba.khod~vaziat.taahol, xlab = "وضعیت_تاهل", ylab = "مهربانی.با.خود")
boxplot(data = demo3, zehn_aggahi~vaziat.taahol)
boxplot(data = demo3, zehn_agahi~vaziat.shoghl)
boxplot(data = demo3, zehn_aggahi~gender)
boxplot(data = demo3, zehn_aggahi~vaziat.shoghl)
boxplot(data = demo3, zehn_aggahi~shoghl)
boxplot(data = demo3, shafaghatt~vaziat.taahol)
boxplot(data = demo3, shafaghatt~tahsilat)
boxplot(data = demo3, shafaghatt~shoghl)
boxplot(data = demo3, enzeva~vaziat.taahol)
boxplot(data = demo3, enzeva~tahsilat)
boxplot(data = demo3, ghezavat_nesbat_khod~tahsilat)

# Analysis for different variables
leveneTest(mehrabani.ba.khod~vaziat.taahol)
one_way <- aov(mehrabani.ba.khod~vaziat.taahol, data = demo3)
summary(one_way)
TukeyHSD(one_way)

# Continue with further analysis as needed...

# Dummy Variables and Regression
dumdum <- dummy_columns(demo3, select_columns = c("vaziat.taahol", "vaziat.eshteghal", "tahsilat"), remove_selected_columns = TRUE)
dum <- dumdum %>% subset(select = 11:22)
colnames(dum)[10] <- "tasli_arsh_doc"
colnames(dum)[12] <- "tahsilat_zir"
dumplot <- cor(dum)
corrplot(dumplot)

lm_model <- lm(data = dum, shafaghatt~vaziat.taahol_mojarad + vaziat.eshteghal_bikar + tahsilat_diplom + tahsilat_karshenasi + tasli_arsh_doc)
plot(lm_model)
summary(lm_model)

# KNN
split <- sample.split(dum, SplitRatio = 0.7)
train_cl <- subset(dum, split == "TRUE")
test_cl <- subset(dum, split == "FALSE")
classifier_knn <- knn(train = train_cl, test = test_cl, cl = train_cl$shafaghatt, k = 1)
misClassError <- mean(classifier_knn != test_cl$shafaghatt)
print(paste('Accuracy =', 1-misClassError))

# Decision Tree
split <- sample.split(dum, SplitRatio = 0.7)
train_cl <- subset(dum, split == "TRUE")
test_cl <- subset(dum, split == "FALSE")
model <- ctree(shafaghatt~., train_cl)
plot(model)
predict_model <- predict(model, test_cl)
table_mat <- table(test_cl$shafaghatt, predict_model)
ac_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test is found to be', ac_Test))

# Rpart
model <- rpart(shafaghatt~tahsilat_zir + tasli_arsh_doc + tahsilat_karshenasi + tahsilat_diplom + vaziat.eshteghal_shaghel + vaziat.eshteghal_bikar + vaziat.taahol_talagh + vaziat.taahol_moteahel + vaziat.taahol_mojarad, data = dum)
prp(model)
