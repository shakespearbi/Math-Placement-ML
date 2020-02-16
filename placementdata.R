#Analysis
placement <- read.csv("PlacementDataNoID.csv", stringsAsFactors = FALSE)
str(placement)
attach(placement)
library(dplyr)
library(gmodels)
library(caTools)
library(e1071)

grade<-factor(grade, levels = c("A+","A","A-","B+","B",
"B-","C+","C","C-","D+","D","D-","F"))

placement<-mutate(placement,gradegpa=case_when(grade =="A+"~ 4.0,
grade =="A"~ 4.0,grade =="A-"~ 3.7,grade =="B+"~ 3.3,grade =="B"~ 3.0,
grade =="B-"~ 2.7,grade =="C+"~ 2.3,grade =="C"~ 2.0,grade =="C-"~ 1.7,
grade =="D+"~ 1.3,grade =="D"~ 1.0, grade =="D-"~ 0.7,grade =="F"~ 0.0, 
grade == "W"~ 0.0))


# math145 <- subset(placement,course == "M145")
# math131 <- subset(placement,course == "M131")


math151 <- subset(placement,course == "M151")

math121 <- subset(placement,course == "M121")


mod1a = lm(math151$gradegpa~math151$hsgpa)
plot(math151$hsgpa,math151$gradegpa)
summary(mod1a)#0.12 variation explained
cor(math151$hsgpa,math151$gradegpa, method ="pearson",use ="complete.obs") # 0.350724

mod1b = lm(math121$gradegpa~math121$hsgpa)
plot(math121$hsgpa,math121$gradegpa)
summary(mod1b)#0.099 variation explained
cor(math121$hsgpa,math121$gradegpa, method ="pearson",use ="complete.obs") #0.3188458


modact = lm(actma~satm)
plot(satm,actma)
summary(modact)
#correlation of 0.7304357
cor(satm,actma, method ="pearson",use ="complete.obs")
#regression line: 1.773941 + 0.040874 x
#if else(assign)

#function that predict new act math scores
createactma <- function(actma,satm)
{
  results <-vector(mode="numeric",length=1052)
for (i in 1:1052)
{
if (is.na(actma[i]) && is.na(satm[i])==FALSE) {
  results[i]=round(1.773941 + 0.0408748*satm[i])
} 
  else {
  results[i] = actma[i]
  }
}
  return (results)
}
#add newact scores to dataframe
placement<-mutate(placement,newactm=createactma(actma,satm))
# test relationship between actc and grade in math class

mod2a = lm(math151$gradegpa~math151$actc)
summary(mod2a) #there's a relationship -> keep actc 0.044 variation explained
cor(math151$actc,math151$gradegpa, method ="pearson",use ="complete.obs") #0.2226114

mod2b = lm(math121$gradegpa~math121$actc)
summary(mod2b) #0.1251 explained variation
cor(math121$actc,math121$gradegpa, method ="pearson",use ="complete.obs") #0.357

mod3a = lm(math151$gradegpa~math151$actsc)
summary(mod3a) #there's no relationship, only 0.01499  of variation is explained
plot(math151$actsc,math151$gradegpa)
cor(math151$actsc,math151$gradegpa, method ="pearson",use ="complete.obs") #0.1421173

mod3b = lm(math121$gradegpa~math121$actsc)
summary(mod3b) #there's a relationship, only  0.08834  of variation is explained
plot(math121$actsc,math121$gradegpa)
cor(math121$actsc,math121$gradegpa, method ="pearson",use ="complete.obs") #0.3020741

mod4a = lm(math151$gradegpa~math151$satv)
summary(mod4a) #0.002301 variation explained and there's no relationship 
cor(math151$satv,math151$gradegpa, method ="pearson",use ="complete.obs") #0.1051304

mod4b = lm(math121$gradegpa~math121$satv)
summary(mod5) #0.001052 variation explained and there's no relationship
cor(math121$satv,math121$gradegpa, method ="pearson",use ="complete.obs") #0.1046084

mod5a = lm(math151$gradegpa~math151$acten)
summary(mod5b)#0.02704  variation explained
cor(math151$acten,math151$gradegpa, method ="pearson",use ="complete.obs") #0.1794012 correlation

mod5b = lm(math121$gradegpa~math121$acten)
summary(mod5b)#0.02704  variation explained
cor(math121$acten,math121$gradegpa, method ="pearson",use ="complete.obs") #0.2624768 correlation

mod6 = lm(actc~satv + satm)
summary(mod6) 


createactc <- function(actc,satv,satm)
{
  results <-vector(mode="numeric",length=1052)
  for (i in 1:1052)
  {
    if (is.na(actc[i]) && is.na(satv[i])==FALSE && is.na(satm[i])==FALSE) {
     results[i]=round(2.235279 +0.025336*satv[i] + 0.014926*satm[i])
    } 
    else {
      results[i] = actc[i]
    }
  }
  return (results)
}

placement<-mutate(placement,newactc=createactc(actc,satv,satm))

math151 <- subset(placement,course == "M151")

math121 <- subset(placement,course == "M121")

mod7a = lm(math151$gradegpa~math151$place2)  
summary(mod7a) #0.1313 variation explained
cor(math151$place2,math151$gradegpa, method ="pearson",use ="complete.obs") #0.3671207

mod7b = lm(math121$gradegpa~math121$place2)  
summary(mod7b) #0.01932 variation explained
cor(math121$place2,math121$gradegpa, method ="pearson",use ="complete.obs") #0.1480405

mod8a = lm(math121$gradegpa~math121$place1)  
summary(mod8) #0.09  variation explained
cor(math121$place1,math121$gradegpa, method ="pearson",use ="complete.obs") #0.3039853
#look at academic rating, placement 1,2, and total

mod8b = lm(math151$gradegpa~math151$place1)  
summary(mod8b) #0.1325   variation explained
cor(math151$place1,math151$gradegpa, method ="pearson",use ="complete.obs") #0.3687728


mod9a = lm(math151$gradegpa~math151$hspercen)  
summary(mod9a) #0.08991 variation explained
cor(math151$hspercen,math151$gradegpa, method ="pearson",use ="complete.obs") #0.3091967

mod9b = lm(math121$gradegpa~math121$hspercen)  
summary(mod9b) #0.05446 variation explained
cor(math121$hspercen,math121$gradegpa, method ="pearson",use ="complete.obs") #0.2408925

mod10a = lm(math151$gradegpa~math151$acadrate)  
summary(mod10a) #0.1487  variation explained
cor(math151$acadrate,math151$gradegpa, method ="pearson",use ="complete.obs") #-0.389499

mod10b = lm(math121$gradegpa~math121$acadrate)  
summary(mod10b) # 0.12  variation explained
cor(math121$acadrate,math121$gradegpa, method ="pearson",use ="complete.obs") #-0.3488608
plot(math121$acadrate,math121$gradegpa)

mod11a = lm(math151$gradegpa~math151$actr)  
summary(mod11a) # 0.02842    variation explained
cor(math151$actr,math151$gradegpa, method ="pearson",use ="complete.obs") #0.1831997
plot(math151$actr,math151$gradegpa)

mod11b = lm(math121$gradegpa~math121$actr)  
summary(mod11b) # 0.0734   variation explained
cor(math121$actr,math121$gradegpa, method ="pearson",use ="complete.obs") #0.2763349
plot(math121$actr,math121$gradegpa)

mod12a = lm(math151$gradegpa~math151$newactc)  
summary(mod12a) # 0.01719   variation explained
cor(math151$newactc,math151$gradegpa, method ="pearson",use ="complete.obs") #0.1437607
plot(math151$newactc,math151$gradegpa)

mod12b = lm(math121$gradegpa~math121$newactc)  
summary(mod12b) # 0.0998    variation explained
cor(math121$newactc,math121$gradegpa, method ="pearson",use ="complete.obs") #0.3195381
plot(math121$newactc,math121$gradegpa)

mod13a = lm(math151$gradegpa~math151$newactm)  
summary(mod13a) #0.0244  variation explained
cor(math151$newactc,math151$gradegpa, method ="pearson",use ="complete.obs") #0.1647298

mod13b = lm(math121$gradegpa~math121$reclevel)  
summary(mod13b) #0.1024    variation explained
cor(math121$reclevel,math121$gradegpa, method ="pearson",use ="complete.obs") #0.3226322
plot(math121$reclevel,math121$gradegpa)

mod14a = lm(math151$gradegpa~math151$newactm)  
summary(mod14a) #0.05593     variation explained
cor(math151$newactm,math151$gradegpa, method ="pearson",use ="complete.obs") #0.2434441
plot(math151$newactm,math151$gradegpa)

mod14b = lm(math121$gradegpa~math121$newactm)  
summary(mod14b) #0.1148     variation explained
cor(math121$newactm,math121$gradegpa, method ="pearson",use ="complete.obs") #0.3421434
plot(math151$newactm,math151$gradegpa)

mod15a = lm(math151$gradegpa~math151$newactm + math151$newactc)  
summary(mod15a) #0.05263 variation explained
cor(math151$newactm,math151$gradegpa, method ="pearson",use ="complete.obs") #0.3421434

mod15b = lm(math121$gradegpa~math121$newactm + math121$newactc)  
summary(mod15b) #0.1302 variation explained, both actc and actma are sig
cor(math121$newactm,math121$gradegpa, method ="pearson",use ="complete.obs") #0.3421434

mod16a = lm(math151$gradegpa~math151$place1 + math151$place2)  
summary(mod16a) #0.1568 variation explained
cor(math151$place1,math151$place2,math151$gradegpa, method ="pearson",use ="complete.obs") #0.3421434

mod16b = lm(math121$gradegpa~math121$place1 + math121$place2) #place1 is statistically sig 
summary(mod16b) #0.08916  variation explained 

mod17a = lm(math121$gradegpa~math121$actr + math121$acten) #actr is statistically sig 
summary(mod17a) #0.07918  variation explained 

#logistic regression

codedsuccess  <- function(gradegpa)
{
  results <-vector(mode="numeric",length=1052)
  for (i in 1:1052)
  {
    if (is.na(placement$gradegpa[i])==FALSE && placement$gradegpa[i] >= 2.3){
      results[i] = 1
    } 
    else{
      results[i] = 0
    }
  }
  return (results)
}

placement<-mutate(placement,success=codedsuccess(placement$gradegpa)) 
 

smp_size <- floor(0.75*nrow(math151))
set.seed(123)
train_ind<-sample(seq_len(nrow(math151)),size=smp_size)

math151_train<-math151[train_ind,]
math151_test<-math151[-train_ind,]


logitmod151 = glm(success ~ hsgpa + place2,
family=binomial(link=logit),data=math151_train )

predmodLgt = predict(logitmod151, newdata=math151_test)
predmod = logistic(predmodLgt) 


classifysuccess <- function(predmod){
  results <-vector(mode="numeric",length=length(predmod))
  for (i in 1:length(predmod)){
    if (is.na(predmod[i])==FALSE && predmod[i]>=0.75){
      results[i] = 1
    }
    else if(is.na(predmod[i])==TRUE){
      results[i] = NA
    }
    else{
      results[i] = 0
    }
  }
  return (results)
}
math151_test_pred = classifysuccess(predmod)

CrossTable(math151_test_pred,math151_test$success, prop.chisq = FALSE,prop.t=FALSE,dnn = c('predicted','actual'))


prop.table(table(math151_train$success))

#Math121
smp_size <- floor(0.75*nrow(math121))
set.seed(123)
train_ind<-sample(seq_len(nrow(math121)),size=smp_size)

math121_train<-math121[train_ind,]
math121_test<-math121[-train_ind,]

logitmod121 = glm(success ~ newactc + hsgpa + place1,
family=binomial(link=logit),data=math121_train)
summary(logitmod121)

predmodLgt121 = predict(logitmod121,newdata=math121_test)
predmod121 = logistic(predmodLgt121)

math121_train_pred = classifysuccess(predmod121)
CrossTable(math121_train_pred,math121_test$success, prop.chisq = FALSE,prop.t=FALSE,dnn = c('predicted','actual'))


###**** Naive Bayes
### 0-2,2-3,3-4 gpa
### 0-5,6-10,11-15 

placement$success<-factor(placement$success, levels = c(0,1),labels = c("Failure","Success"))

placement<-mutate(placement,hsgpacat=case_when((hsgpa <= 4.5 & hsgpa>3.4)~3, (hsgpa <= 3.4 & hsgpa>2.7)~2, (hsgpa <= 2.7 & hsgpa>=0.0)~1))

placement<-mutate(placement,place2cat=case_when((place2 <= 4 & place2>=0)~1, (place2 <= 7 & place2>=5)~2, (place2 <= 15 & place2>=8)~3))

prop.table(table(math151_train$success))
prop.table(table(math151_test$success))

math151 <- subset(placement,course == "M151")

math121 <- subset(placement,course == "M121")

smp_size <- floor(0.75*nrow(math151))
set.seed(123)
train_ind<-sample(seq_len(nrow(math151)),size=smp_size)

math151_train<-math151[train_ind,]
math151_test<-math151[-train_ind,]

hist(hsgpa,xlab = "High school GPA")
hist(place2,xlab = "Placement 2 scores")

omitmath151_train = subset(math151_train,select=c(hsgpacat,place2cat))

math151_classifier <- naiveBayes(omitmath151_train,
math151_train$success,laplace=0)
math151_test_pred<-predict(math151_classifier,
newdata=math151_test,type="raw")

classifysuccessBayes <- function(math151_test_pred){
results <-vector(mode="numeric",length=length(math151_test_pred)/2)
for (i in 2:length(math151_test_pred)/2){
  if (is.na(math151_test_pred[i,2])==FALSE && math151_test_pred[i,2]>=0.75){
    results[i] = 1
  }
  else if(is.na(math151_test_pred[i,2])==TRUE){
    results[i] = NA
  }
  else{
    results[i] = 0
  }
}
return (results)
}
math151_test_pred2 = classifysuccessBayes(math151_test_pred)

library(gmodels)
CrossTable(math151_test_pred2,math151_test$success, prop.chisq = FALSE,prop.t=FALSE,dnn = c('predicted','actual'))

### MATH 121 newactm + actr + newactc + newactm +  hsgpa + place1 + acadrate


smp_size <- floor(0.75*nrow(math121))
set.seed(123)
train_ind<-sample(seq_len(nrow(math121)),size=smp_size)

math121_train<-math121[train_ind,]
math121_test<-math121[-train_ind,]

hist(placement$newactc,xlab = "ACT composites")
hist(place1,xlab = "Placement 1 scores")

placement<-mutate(placement,actccat=case_when((placement$newactc <= 21 & placement$newactc >=0)~1, (placement$newactc  <= 27 & placement$newactc >=22)~2, (placement$newactc  <= 36 & placement$newactc >=28)~3))

placement<-mutate(placement,place1cat=case_when((place1 <= 5 & place1>=0)~1, (place1 <= 9 & place1>=6)~2, (place1 <= 15 & place1>=10)~3))


math151 <- subset(placement,course == "M151")

math121 <- subset(placement,course == "M121")

omitmath121_train = subset(math121_train,select=c(hsgpacat,place1cat,actccat))
math121_classifier<- naiveBayes(omitmath121_train,math121_train$success,laplace=0)
math121_test_pred<-predict(math121_classifier,newdata=math121_test)#*
library(gmodels)
CrossTable(math121_test_pred,math121_test$success, prop.chisq = FALSE,prop.t=FALSE,dnn = c('predicted','actual'))

