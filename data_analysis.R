
###############################################clean#####################################################
###############################################clean###################################################


##########################################importfile###############################################

library(haven)

demo <- read_excel('C:\\Users\\user\\Documents\\dr.nofresti\\compation\\1402\\shafaghat2.xlsx')


dim(demo)

#########################################clean the dataset##########################################
library(dplyr)

library(tidyr)

demo1 <- demo %>% drop_na() %>% distinct()


#########################################processing the data ######################################
library(devtools)

devtools::install_github("strengejacke/sjlabelled")

library(sjlabelled)

l.demo <- as_character(demo[, c(2,3,4,5,6,7)])


#########################################process phase 2 replacing and subsetting####################
demo2 <- subset(demo1, select = 10:35)

shenase <- subset(demo1, select = 1)

demo2 <- cbind(shenase, l.demo, demo2)

table(demo3 == "NA")


#########################################reversed columns ################################
reversed_values <- list(demo2$q1, demo2$q2, demo2$q4, demo2$q6, demo2$q8, demo2$q11, demo2$q13, demo2$q16, demo2$q18, demo2$q20, demo2$q21, demo2$q24, demo2$q25)

library(stringr)

reversing <- function(x){
  x = as.character(x)
  str_replace_all(x, c("1" = "five" , "2" = "four", "3" = "3", "4" = "two", "5" = "one"))
}

off.demo <- sapply(reversed_values, reversing )

off.demo1 <- data.frame(off.demo)

class(off.demo)

colnames(off.demo1) <- c('q1', 'q2', 'q4', 'q6', 'q8', 'q11', 'q13', 'q16', 'q18', 'q20', 'q21', 'q24', 'q25')

off.demo1[off.demo1 == 'one'] <- 1

off.demo1[off.demo1 == 'two'] <- 2

off.demo1[off.demo1 == 'three'] <- 3

off.demo1[off.demo1 == 'four'] <- 4

off.demo1[off.demo1 == 'five'] <- 5


#########################################replacing the reversed scales###############################

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

#########################################creat new factors##########################################
demo2$mehrabani.ba.khod <- (demo2$q5 + demo2$q12 + demo2$q19 + demo2$q23 + demo2$q26)

demo2$enzeva <- (demo2$q25 + demo2$q18 + demo2$q13 + demo2$q4)

demo2$ghezavat_nesbat_khod <- ( demo2$q21 + demo2$q16 + demo2$q11 + demo2$q8 + demo2$q1)

as.numeric(demo2$q10)
class(demo2$q10)

demo2$eshterakat_ensani <- (demo2$q15 + demo2$q10 + demo2$q7 + demo2$q3)

demo2$zehn_aggahi <- (demo2$q22 + demo2$q17 + demo2$q14 + demo2$q9)

demo2$hamanandsazi_efrati  <- (demo2$q24  + demo2$q20 + demo2$q6 + demo2$q2)

#########################################wrangle the data ###########################################
demo3 <- subset(demo2, select= -(8:33)) 

demo3 <- subset(demo3, select = -6)

#########################################last column shafaghat########################################
demo3$shafaghatt <- (demo3$enzeva + demo3$mehrabani.ba.khod + demo3$hamanandsazi_efrati + demo3$zehn_aggahi + demo3$enzeva + demo3$ghezavat_nesbat_khod)

#########################################removing the useless varaiables###############################
demo3$tahsilat[demo3$tahsilat == "tahsilate hozavi"] <- NA

demo3$tahsilat[demo3$tahsilat == "hamsaram fot karde"] <- NA

demo3 <- demo3 %>% na.omit()

View(demo3)
###################################change age
age <- demo3 %>% select(age)

age <- as_numeric(age)

age <- as_tibble(age)

Age <- as_numeric(agge)

View(age)







################################################analyse###############################################
################################################analyse###############################################
demo3 %>% group_by(tahsilat) %>% summarize(mean(shafaghatt))

demo3 %>% group_by(gender) %>% summarize(sd(shafaghatt))

demo3 %>% group_by(vaziat.taahol) %>% summarize(mean(shafaghatt))

demo3 %>% group_by(vaziat.eshteghal) %>% summarize(sd(shafaghatt))

################################################dscriptive############################################
demo3 %>% group_by(gender) %>% summarize(n())

mean(demo3$shafaghatt)
sd(demo3$shafaghatt)
table(demo3$shafaghatt)
########################################plotting#####################################################
boxplot(data = demo3, mehrabani.ba.khod~gender, xlab = "جنسیت", ylab = "مهربانی.با.خود")

boxplot(data = demo3, mehrabani.ba.khod~vaziat.shoghl, xlab ="وضعیت_شغل",  ylab = "مهربانی.با.خود")

boxplot(data = demo3, mehrabani.ba.khod~vaziat.taahol, xlab = "وضعیت_تاهل", ylab = "مهربانی.با.خود" )

####################################################zehn
boxplot(data = demo3, zehn_aggahi~vaziat.taahol)

boxplot(data = demo3, zehn_agahi~vaziat.shoghl)

boxplot(data = demo3, zehn_aggahi~gender)

boxplot(data = demo3, zehn_aggahi~vaziat.shoghl)

boxplot(data = demo3, zehn_aggahi~shoghl)

#####################################################shaf
boxplot(data = demo3, shafaghatt~vaziat.taahol)

boxplot(data = demo3, shafaghatt~tahsilat)

boxplot(data = demo3, shafaghatt~shoghl)

####################################################enz
boxplot(data = demo3, enzeva~vaziat.taahol)

boxplot(data = demo3, enzeva~tahsilat)

####################################################ghez
boxplot(data = demo3, ghezavat_nesbat_khod~tahsilat)

vars <- c(vaziat.eshteghal,gender, vaziat.taahol, tahsilat)

vartable <- table(vars)

install.packages("ggpubr")

library(gg)

library(ggplot2)

demo3 %>% ggplot(aes(x = vaziat.taahol)) + geom_bar(position = "stack") 

########################################################age
as.list(age$age[age$age %in% 0:18])

sd(age$age[age$age %in% 0:18])

as.list(age$age[age$age %in% 19:30])

sd(age$age[age$age %in% 19:30])

mean(age$age[age$age %in% 19:30])

mean(age$age[age$age %in% 31:50])

sd(age$age[age$age %in% 31:50])

as.list(age$age[age$age %in% 31:50])

mean(age$age[age$age %in% 51:100])

as.list(age$age[age$age %in% 51:100])

sd(age$age[age$age %in% 51:100])

######################################mehrabani_ba_khod
######vaziat_taahol~mehrabani_khod
install.packages("car")

library(car)

leveneTest(mehrabani.ba.khod~vaziat.taahol) #p =0.02

one_way <- aov(mehrabani.ba.khod~vaziat.taahol, data = demo3)

summary(one_way)

TukeyHSD(one_way) # there is a slightly significant mean_difference between 
moteahel-mojarad p_value = 0.02

age <- demo4 %>% group_by(vaziat.taahol) %>% summarise(age = mean(age))


###different method
pra2 <- demo3 %>% spread(vaziat.taahol, mehrabani.ba.khod) %>% select(mojarad) 
%>% drop_na()  %>% mutate()

pra3 <- demo3 %>% spread(vaziat.taahol, mehrabani.ba.khod) %>% select(moteahel) 
%>% drop_na() %>% mutate()

t.test(pra2, pra3) #  p-value = 0.003987 there is meaningful difference 


######mehrabani_ba_khod~gender
t.test(data=demo3, mehrabani.ba.khod~gender) # p_value = 0.38

t.test(data = demo3, )


######mehrabani~vaziat_eshteghal
t.test(data = demo3, mehrabani.ba.khod~vaziat.eshteghal)

boxplot(data = demo3, mehrabani.ba.khod~vaziat.eshteghal)


#####mehrabani_tahsilat
boxplot(data = demo3, mehrabani.ba.khod~tahsilat)
mehr_tahs <- demo3 %>% select(tahsilat, mehrabani.ba.khod) %>% filter(tahsilat != "tahsilate hozavi") %>% mutate()
View(mehr_tahs)

aov_mehr_tahs <- aov(data = demo3, mehrabani.ba.khod~tahsilat) # p = 9.05e-05

str_aov_mehr_tah <- summary(aov_mehr_tahs)

str(str_aov_mehr_tah)

print(str_aov_mehr_tah)


TukeyHSD(aov_mehr_tahs) # between zire diplom va others

demo3 <- as.numeric(demo3$age)

demo3 %>% group_by(tahsilat) %>%  summarise(age = mean(age))


######mehrabani_shoghl
aov_mehr_shoghl <- aov(data = demo3, mehrabani.ba.khod~shoghl)
summary(aov_mehr_shoghl) #p =  0.00117

TukeyHSD(aov_mehr_shoghl)# danesh_bazn, khanedar~baznesh


###############################################zehn_agahih
######vaziat_taahol~ zehn agahi
boxplot(data = demo3, zehn_agahi~vaziat.taahol)

one_way_zehn <- aov(data = demo3, zehn_agahi~vaziat.taahol)

summary(one_way_zehn) #p = 0.8 not significant

leveneTest(y = zehn_agahi, group = vaziat.taahol) #pr = 0.319


######zehn_gahi~gender
t.test(data = demo3, zehn_agahi~gender) #p_value = 0.0094 significant


######zehn_agahi~shoghl
leveneTest(y = zehn_agahi, group = shoghl) #signifanct

ggplot(demo3, aes(x = zehn_agahi, fill = shoghl)) + geom_density(alpha = 0.2)

aov_zehn_shoghl <- aov(data = demo3, zehn_aggahi~shoghl) # p = 0.0001

summary(aov_zehn_shoghl)

tukey_zehn_shoghl <- TukeyHSD(aov_zehn_shoghl)#significance danesh~bazneshaste,
#khanedar~bazneshaste, karmand~bazneshaste, kargar~bazneshate

######zehn_vaziat.eshtegh
t.test(data = demo3, zehn_aggahi~vaziat.eshteghal) #significant, p = 0.001329

######zeh~tahsilat
tahsil_zehn <- demo3 %>% select(tahsilat, zehn_aggahi) %>% filter(tahsilat != "tahsilate hozavi" ) %>% mutate()  
View(tahsil_zehn)

tahs_zehn_aov <- aov(data = tahsil_zehn, zehn_aggahi~tahsilat)
summary(tahs_zehn_aov) # not significant

################################################enzeva
library(ggplot2)
ggplot(data = demo3, aes(sample = shafaghatt)) + stat_qq() + stat_qq_line()

ggplot(data = demo3, aes(y = enzeva, x = vaziat.eshteghal )) + geom_boxplot(aes(color = "red",
                                                                                fill = "TRUE", alpha = 'TRUE'))

ggplot(data = demo3, aes(y = enzeva, x = gender )) + geom_boxplot()

ggplot(data = demo3, aes(y = enzeva, x = tahsilat )) + geom_boxplot()

######enz~vaziat_tahol
leveneTest(y = demo3$enzeva, group = demo3$vaziat.taahol) #not significant

boxplot(data = demo3, enzeva~vaziat.taahol)

demo3 %>% group_by(vaziat.taahol) %>% summarize(n())

aov_enz_vaztah <- aov(data = demo3 , enzeva~vaziat.taahol)
summary(aov_enz_vaztah)# significant p = 2.42e-09

TukeyHSD(aov_enz_vaztah) # moteahel~ mojarad , talagh~mojarad

######enz~vaziat_eshteghal
t.test(data = demo3, enzeva~vaziat.eshteghal)
demo3 %>% group_by(vaziat.eshteghal) %>% summarize(n())

######enz~shoghl
leveneTest(data = demo3, enzeva~shoghl)

aov_enz_shoghl <- aov(data = demo3, enzeva~shoghl)
summary(aov_enz_shoghl)

TukeyHSD(aov_enz_shoghl)

demo3 %>% group_by(shoghl) %>% summarise(mean(enzeva))

demo3 %>% group_by(shoghl) %>% summarise(n())

######enz~gender
t.test(data = demo3, enzeva~ gender)# not significant

######enz~tahsilat
demo3$AGE <- as.numeric(demo3$age)

demo3 %>% group_by(tahsilat) %>% summarise(mean(enzeva)) 

demo3 %>% group_by(tahsilat) %>% summarise(mean(AGE)) 

demo3 <- subset( demo3, select =  -15)

leveneTest(data = demo3, enzeva~tahsilat)

aov_enz_tahsilat <- aov(data = demo3, enzeva~tahsilat)
summary(aov_enz_tahsilat) #significant p = 0.000988

TukeyHSD(aov_enz_tahsilat)#zire diplom-karshenasi , zire diplom-diplom,
#karshenasi arshad va doktora-karshenasi 

########################################################eshterakate_ensani
######eshter~gender
ggplot(data = demo3, aes(sample = eshterakat_ensani)) + geom_qq_line() + stat_qq()

t.test(data = demo3, eshterakat_ensani~gender )#significant female

######esshter~vaziat_taahol
leveneTest(data = demo3, eshterakat_ensani~vaziat.taahol)

aov_eeshte_vaziat.tah <- aov(data = demo3, eshterakat_ensani~vaziat.taahol)
summary(aov_eeshte_vaziat.tah)# significant

TukeyHSD(aov_eeshte_vaziat.tah)#moteahel-mojarad 

######eshter~vaziat.eshtegh
t.test(data = demo3, eshterakat_ensani~vaziat.eshteghal)#significant shaghel

######eshter~shoghl
leveneTest(data = demo3, eshterakat_ensani~shoghl)

aov_eshter_shogh <- aov(data = demo3, eshterakat_ensani~shoghl)
summary(aov_eshter_shogh)#sig

TukeyHSD(aov_eshter_shogh)#mohandesi-moallem ya ostad, moallem ya ostad-mashaghele azad 
#, moallem ya ostad-daneshjoo ya daneshamooz, 
#mashaghele azad  va ranandigi-bazneshaste , daneshjoo ya daneshamooz-bazneshaste 

demo3$AGE <- as.numeric(demo3$age) 

demo3 %>% group_by(shoghl) %>% summarize(mean(AGE))# herer it seems like the career matters

######eshter_tahsil
aov_eshte_tah <- aov(data = demo3, eshterakat_ensani~tahsilat)
summary(aov_eshte_tah)

#####################################################################ghezavat
######ghez~gender
t.test(data = demo3, ghezavat_nesbat_khod~gender)
class(demo3$ghezavat_nesbat_khod)

######ghez~vaz_taah
leveneTest(data = demo3, ghezavat_nesbat_khod~vaziat.taahol)

aov_ghez_taah <- aov(data = demo3, ghezavat_nesbat_khod~vaziat.taahol)
summary(aov_ghez_taah)#significant

TukeyHSD(aov_ghez_taah)#moteahel-mojarad 

######ghez~vaz_esht
t.test(data = demo3, ghezavat_nesbat_khod~vaziat.eshteghal)

######ghez~shoghl
leveneTest(data = demo3, ghezavat_nesbat_khod~shoghl)

aov_ghez_shogh <- aov(data = demo3, ghezavat_nesbat_khod~shoghl)
summary(aov_ghez_shogh)

######ghez~tahsilat
leveneTest(data = demo3, shafaghatt~tahsilat) #sig 

krus <- aov(data = demo3, ghezavat_nesbat_khod~tahsilat)# zire diplom-karshenasi arshad va doktora,
#zire diplom-karshenasi zire diplom-diplom 
summary(krus)

TukeyHSD(krus)

demo3 %>% group_by(tahsilat) %>% summarize(mean(ghezavat_nesbat_khod))

########################################################################hamanandsazi
######haman_gender 
t.test(data=demo3, hamanandsazi_efrati~gender) #sig women p = p-value = 0.0001887

######haman_vaz_taah
aov_haman_taah <- aov(data = demo3, hamanandsazi_efrati~vaziat.taahol)
summary(aov_haman_gen)#  sig  , p = 0.000842

TukeyHSD(aov_haman_taah)#moteahel-mojarad,talagh-mojarad,talagh-moteahel

######haman_esht
t.test(data = demo3, hamanandsazi_efrati~vaziat.eshteghal)# sig p=0.01586

######haman_shoghl
aov_haman_shoghl <- aov(data = demo3, hamanandsazi_efrati~shoghl)
aas <- summary(aov_haman_shoghl)

######haman_tahsil
aov_hama_tahs <- aov(data = demo3, hamanandsazi_efrati~tahsilat)
summary(aov_hama_tahs)# sig , p=4.72e-09

library(car)
leveneTest(data= demo3, hamanandsazi_efrati~tahsilat)

TukeyHSD(aov_hama_tahs)# zire diplom-diplom , karshenasi arshad va doktora-karshenasi
#zire diplom-karshenasi, zire diplom-karshenasi arshad va doktora 

demo3 %>% group_by(tahsilat) %>% summarize(mean(hamanandsazi_efrati))

##########################################################shafaghat
######shaf~gender
t.test(demo, shafaghatt~gender)
 
library(dplyr)

newgender <- demo3 %>% spread(gender, shafaghatt) 

newmale <- newgender %>% select(male) %>% drop_na() %>% unlist(as.numeric())

newfemale <- newgender %>% select(female) %>% drop_na() %>% unlist(as.numeric())


cohen.d(newmale,newfemale)

######shaf~taah 

shaf_vazi.tah <- aov(data =demo3, shafaghatt~vaziat.taahol)# sig
summary(shaf_vazi.tah)

TukeyHSD(shaf_vazi.tah)

shaf_vazi.taah_aov <- aov(demo3$shafaghatt~demo3$vaziat.taahol)

etaSquared(shaf_vazi.taah_aov)

######shaf~esht
t.test(data = demo3, shafaghatt~vaziat.eshteghal)# significant


newvazesh <- demo3 %>% spread(vaziat.eshteghal, shafaghatt) 

newbikar <- newvazesh %>% select(bikar) %>% drop_na() %>% unlist(as.numeric())

newshaghel <- newvazesh %>% select(shaghel) %>% drop_na() %>% unlist(as.numeric())


cohen.d(newshaghel, newbikar)


######shaf~shoghl
leveneTest(data= demo3, shafaghatt~shoghl)

aov_shaf_shogh <- aov(data = demo3, shafaghatt~shoghl)

summary(aov_shaf_shogh)# sig

TukeyHSD(aov_shaf_shogh)


######shaf_tahs
aov_shaf_tahsi <-aov(data = demo3, shafaghatt~tahsilat)

summary(aov_shaf_tahsi) #sig p = 1.35e-05

ggplot(data = demo3, aes(sample = shafaghatt)) + geom_qq_line() + stat_qq()

TukeyHSD(aov_shaf_tahsi)

shaf_vazi.tah_aov <- aov(demo3$shafaghatt~demo3$tahsilat)

install.packages("lsr")

library(lsr)

etaSquared(shaf_vazi.tah_aov)

##############################################################age
AGE <- as.numeric(demo3$age)

shafaghat <- demo3$shafaghatt

cor(shafaghat, AGE)

#shafaghatt~age
demo3 %>% ggplot(aes(x =shafaghatt, y = age, color = vaziat.taahol)) + geom_point()

demo3 %>% ggplot(aes(x = age, y = age)) + geom_col()

table(demo3$age)

cor(shafaghat, AGE)

##############################################################algo###############################

############################################################## dummy
library(fastDummies) 

dumdum <- dummy_columns(demo3, select_columns = c( "vaziat.taahol", "vaziat.eshteghal", "tahsilat") , remove_selected_columns
                        = TRUE)

View(dumdum)

############################################################## regression_shafaghat
boxplot(demo3$shafaghat)

corrplot()

library(dplyr)

dum <- dumdum %>% subset(select = 11:22)

colnames(dum)[10] <- "tasli_arsh_doc"

colnames(dum)[12] <- "tahsilat_zir"

View(dum)        

dumplot <- cor(dum)

library(corrplot)

corrplot(dumplot)

lm_model <- lm(data = dum, shafaghatt~vaziat.taahol_mojarad + vaziat.eshteghal_bikar + tahsilat_diplom + tahsilat_karshenasi + tasli_arsh_doc)

plot(lm_model)

summary(lm_model) #Adjusted R-squared:  0.03436 

lm_predict <- predict(lm_model, dum)

plot(lm_predict)

plot(lm_predict~dum$shafaghatt)

##################################################knn
library(e1071)
library(caTools)
library(class)

split <- sample.split(dum, SplitRatio = 0.7)

train_cl <- subset(dum, split == "TRUE")

test_cl <- subset(dum, split == "FALSE")

######k = 1,  "Accuracy = 0.756505576208178"
classifier_knn <- knn(train = train_cl,
                      test = test_cl,
                      cl = train_cl$shafaghatt,
                      k = 1)
classifier_knn

cm <- table(test_cl$shafaghatt, classifier_knn)
cm

misClassError <- mean(classifier_knn != test_cl$shafaghatt)

print(paste('Accuracy =', 1-misClassError))



dim(train_cl)

k <- sqrt(1075) / 2
k


######k = 16 , "Accuracy = 0.527881040892193"
classifier_knn<- knn(train = train_cl,
                     test = test_cl,
                     cl = train_cl$shafaghatt,
                     k = 16)
classifier_knn

misClassError <- mean(classifier_knn != test_cl$shafaghatt)

print(paste('Accuracy =', 1-misClassError))


######k = 10 ,"Accuracy = 0.5817843866171"
classifier_knn <- knn(train = train_cl,
                      test = test_cl,
                      cl = test_cl$shafaghatt,
                      k = 10)

misClassError <- mean(classifier_knn != test_cl$shafaghatt)

print(paste('Accuracy =', 1-misClassError))


######k = 7, "Accuracy = 0.609665427509294"
classifier_knn <- knn(train = train_cl,
                      test = test_cl,
                      cl = train_cl$shafaghatt,
                      k = 7)

misClassError <- mean(classifier_knn != test_cl$shafaghatt) 

print(paste('Accuracy =', 1-misClassError))

######k = 3 , "Accuracy = 0.618959107806691"
classifier_knn <- knn(train = train_cl,
                      test = test_cl,
                      cl = train_cl$shafaghatt,
                      k = 3)
misClassError <- mean(classifier_knn != test_cl$shafaghatt)         

classifier_knn

print(paste('Accuracy =', 1-misClassError))

######k = 5, "Accuracy = 0.596654275092937"
classifier_knn <- knn(train = train_cl,
                      test = test_cl,
                      cl = train_cl$shafaghatt,
                      k = 5)

misClassError <- mean(classifier_knn != test_cl$shafaghatt)

print(paste('Accuracy =', 1-misClassError))

####### k= 18, "Accuracy = 0.546468401486989"
classifier_knn <- knn(train = train_cl,
                      test = test_cl,
                      cl = train_cl$shafaghatt,
                      k = 18)

misClassError <- mean(classifier_knn != test_cl$shafaghatt)

print(paste('Accuracy =', 1-misClassError))

######k = 2,
classifier_knn <- knn(train = train_cl,
                      test = test_cl,
                      cl = train_cl$shafaghatt,
                      k = 2)

misClassError <- mean(classifier_knn != test_cl$shafaghatt)

print(paste('Accuracy =', 1-misClassError))

###########################################decisoin tree
library(datasets)
library(caTools)
library(party)
library(dplyr)
library(magrittr)

split <- sample.split(dum, SplitRatio = 0.7)

train_cl <- subset(dum, split == "TRUE")

test_cl <- subset(dum, split == "FALSE")

model<- ctree(shafaghatt~., train_cl)

plot(model)

predict_model<-predict(model, test_cl)

table_mat <- table(test_cl$shafaghatt, predict_model)

ac_Test <- sum(diag(table_mat)) / sum(table_mat)

print(paste('Accuracy for test is found to be', ac_Test))

#######rpart
library(rpart)
library(rpart.plot)

model<- rpart(shafaghatt~tahsilat_zir + tasli_arsh_doc + tahsilat_karshenasi + tahsilat_diplom
              vaziat.eshteghal_shaghel+ vaziat.eshteghal_bikar + vaziat.taahol_talagh + vaziat.taahol_moteahel
              + vaziat.taahol_mojarad ,  data = dum)

prp(model)

