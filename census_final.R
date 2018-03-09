#PQHS471 Midterm
#Rachel Sabol
#March 8, 2018

path1="C:/Users/Rachel Sabol/Documents/Graduate Coursework/PQHS 471/census_train.csv"
path2="C:/Users/Rachel Sabol/Documents/Graduate Coursework/PQHS 471/census_test.csv"

census_train_val=read.csv(path1)
census_test=read.csv(path2)

# Sample code from exploratory analysis --------------------------------------
dim(census_train_val)
names(census_train_val)
summary(census_train_val)
cor(census_train_val[sapply(census_train_val, is.numeric)]) 

plot(census_train_val$age, census_train_val$income)
plot(census_train_val$education, census_train_val$income) 

#note there is missing data for: workclass, occupation, native.country

# Recoding categoricals ---------------------------------------------------
new.levels=c(0,1,2,3,4,5,6,7,3)
census_train_val$workclass=factor(new.levels[census_train_val$workclass])
census_train_val$workclass[census_train_val$workclass == 0]=NA
census_train_val$education=factor(census_train_val$education, levels=c(" Preschool", " 1st-4th", " 5th-6th", " 7th-8th", " 9th", " 10th", " 11th", " 12th", " HS-grad", " Assoc-voc", " Assoc-acdm", " Some-college", " Bachelors", " Masters", " Prof-school", " Doctorate"), labels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16))
census_train_val$marital.status=factor(census_train_val$marital.status, levels=c(" Never-married", " Married-AF-spouse", " Widowed", " Separated", " Married-spouse-absent", " Divorced", " Married-civ-spouse"), labels=c(1,2,3,4,5,6,7))
census_train_val$occupation=factor(census_train_val$occupation, levels=c(" ?", " Adm-clerical", " Armed-Forces", " Craft-repair", " Exec-managerial"," Farming-fishing"," Handlers-cleaners", " Machine-op-inspct", " Other-service", " Priv-house-serv", " Prof-specialty", " Protective-serv", " Sales", " Tech-support", " Transport-moving"), labels=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14))
census_train_val$occupation[census_train_val$occupation == 0]=NA
census_train_val$relationship=factor(census_train_val$relationship, levels=c(" Own-child", " Other-relative", " Unmarried", " Not-in-family", " Husband", " Wife"), labels=c(1,2,3,4,5,6))
census_train_val$race=factor(census_train_val$race, levels=c(" Black", " Amer-Indian-Eskimo", " Other", " Asian-Pac-Islander", " White"), labels=c(1,2,3,4,5))
census_train_val$sex=factor(census_train_val$sex, levels=c(" Male", " Female"), labels=c(1,2))
new.levels=c(0,8,10,5,3,2,2,3,3,9,9,9,9,2,2,9,2,5,4,6,6,9,9,2,5,8,2,3,2,3,8,4,9,2,9,3,5,8,2,10,8,4)
census_train_val$native.country=factor(new.levels[census_train_val$native.country])
census_train_val$native.country[census_train_val$native.country == 0]=NA
census_train_val$income=factor(census_train_val$income, levels=c(" <=50K", " >50K"), labels=c(0,1))

#missing data is less than 10 percent 

# Split the data ----------------------------------------------------------
sample_size=floor(0.60 * nrow(census_train_val))
set.seed(2)
train_ind=sample(seq_len(nrow(census_train_val)), size = sample_size)
census_train=census_train_val[train_ind, ]
census_val=census_train_val[-train_ind, ]
dim(census_train); dim(census_val)

for (i in 1:ncol(census_train)){
  x=sum(is.na(census_train[,i]))
  cat(colnames(census_train)[i], x, "\n")
}

# Model testing -----------------------------------------------------------
new.levels=c(0,1,2,3,4,5,6,7,3)
census_test$workclass=factor(new.levels[census_test$workclass])
census_test$workclass[census_test$workclass == 0]=NA
census_test$education=factor(census_test$education, levels=c(" Preschool", " 1st-4th", " 5th-6th", " 7th-8th", " 9th", " 10th", " 11th", " 12th", " HS-grad", " Assoc-voc", " Assoc-acdm", " Some-college", " Bachelors", " Masters", " Prof-school", " Doctorate"), labels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16))
census_test$marital.status=factor(census_test$marital.status, levels=c(" Never-married", " Married-AF-spouse", " Widowed", " Separated", " Married-spouse-absent", " Divorced", " Married-civ-spouse"), labels=c(1,2,3,4,5,6,7))
census_test$occupation=factor(census_test$occupation, levels=c(" ?", " Adm-clerical", " Armed-Forces", " Craft-repair", " Exec-managerial"," Farming-fishing"," Handlers-cleaners", " Machine-op-inspct", " Other-service", " Priv-house-serv", " Prof-specialty", " Protective-serv", " Sales", " Tech-support", " Transport-moving"), labels=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14))
census_test$occupation[census_test$occupation == 0]=NA
census_test$relationship=factor(census_test$relationship, levels=c(" Own-child", " Other-relative", " Unmarried", " Not-in-family", " Husband", " Wife"), labels=c(1,2,3,4,5,6))
census_test$race=factor(census_test$race, levels=c(" Black", " Amer-Indian-Eskimo", " Other", " Asian-Pac-Islander", " White"), labels=c(1,2,3,4,5))
census_test$sex=factor(census_test$sex, levels=c(" Male", " Female"), labels=c(1,2))
new.levels=c(0,8,10,5,3,2,2,3,3,9,9,9,9,2,2,9,2,5,4,6,6,9,9,2,5,8,2,3,2,3,8,4,9,2,9,3,5,8,2,10,8,4)
census_test$native.country=factor(new.levels[census_test$native.country])
census_test$native.country[census_test$native.country == 0]=NA
census_test$income=factor(census_test$income, levels=c(" <=50K", " >50K"), labels=c(0,1))

library(gam)

gam.mod=gam(income~s(age,4)+s(hours.per.week,6)+education.num+lo(capital.gain)+fnlwgt+
              capital.loss+workclass+marital.status+occupation+relationship+
              race+sex+native.country, family=binomial,data=census_train)
gam.probs=predict(gam.mod, type="response")
gam.pred=rep(0,length(census_train$income))
gam.pred[gam.probs>.5]=1
table(gam.pred, census_train$income)
mean(gam.pred==census_train$income)

gam.probs=predict(gam.mod, newdata=census_val, type="response")
gam.pred=rep(0,length(census_val$income))
gam.pred[gam.probs>.5]=1
table(gam.pred, census_val$income)
mean(gam.pred==census_val$income)

gam.probs=predict(gam.mod, newdata=census_test, type="response")
gam.pred=rep(0,length(census_test$income))
gam.pred[gam.probs>.5]=1
table(gam.pred, census_test$income)
mean(gam.pred==census_test$income)



