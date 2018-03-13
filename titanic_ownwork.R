###################################
#  사용한 패키지 
library(ggplot2)
library(dplyr)
library(rpart)
library(rpart.plot)
library(Hmisc)
library(DMwR)
library(stringr)
library(caret)
set.seed(137)
###################################

###################################
# 작업경로 설정 및 데이터 불러오기 
###################################

setwd("C:\\dataanalysis")

test<-read.csv("test.csv",header=T)
train<-read.csv("train_titanic.csv",header=T)

trainsurv<-train[,c(1,2)] # survived and Passsenger ID 따로 빼기 
train_<-train[,-2] # test 와 train을 함께 하기 위해서 
mer<-rbind(train_,test)

##################################
# 결측치 확인 Fare 1개 , Age 263개
# 데이터 구조 확인 및 class변경 
##################################
apply(is.na(mer),2,sum)

str(mer)
#'data.frame':	891 obs. of  12 variables:
#$ PassengerId: int  1 2 3 4 5 6 7 8 9 10 ...
#$ Survived   : int  0 1 1 1 0 0 0 0 1 1 ...
#$ Pclass     : int  3 1 3 1 3 3 1 3 3 2 ...
#$ Name       : Factor w/ 891 levels "Abbing, Mr. Anthony",..: 109 191 358 277 16 559 520 629 417 581 ...
#$ Sex        : Factor w/ 2 levels "female","male": 2 1 1 1 2 2 2 2 1 1 ...
#$ Age        : num  22 38 26 35 35 NA 54 2 27 14 ...
#$ SibSp      : int  1 1 0 1 0 0 0 3 0 1 ...
#$ Parch      : int  0 0 0 0 0 0 0 1 2 0 ...
#$ Ticket     : Factor w/ 681 levels "110152","110413",..: 524 597 670 50 473 276 86 396 345 133 ...
#$ Fare       : num  7.25 71.28 7.92 53.1 8.05 ...
#$ Cabin      : Factor w/ 148 levels "","A10","A14",..: 1 83 1 57 1 1 131 1 1 1 ...
#$ Embarked   : Factor w/ 4 levels "","C","Q","S": 4 2 4 4 4 3 4 4 4 2 ...

# 문제 정의 분류 문제 Survived 0 : dead 1 : survived
summary(mer)
#Sex female : 314 male : 577
#Age NA : 17 Min 0 Max 80
#Embarked NA : 2 Cherbourg 영국 : 168 Queenstown 뉴질랜드 : 77 Southampton 프랑스 : 644
#Name : Mrs 기혼 Miss 미혼
#table(data, useNA = "always")
describe(result)


table(mer$Pclass, useNA = 'always')
# 1 216 2 184 3 491 
#   323   277   709    0 
table(mer$Embarked, useNA = 'always')
# C    Q    S <NA> 
# 270  123  914    2 


# Embarked에 분명히 NA값이 있는데, 이게 제대로 보여지지 않는 점이 있어서 이런 방안으로 처리 
levels(mer$Embarked)
levels(mer$Embarked)[1]<-NA
levels(mer$Embarked)
table(mer$Embarked, useNA = "always")

#View(mer[!complete.cases(mer),])# 265행이  관찰 됨. 
#mer<-mer[,-c(3,8,10)] # Ticket Name Cabin 제거 

#결측치 하나 있는Fare 변수 중앙값 처리
mer$Fare[which(is.na(mer$Fare))]<-median(mer$Fare,na.rm=T)
mer$Embarked[which(is.na(mer$Embarked))]<-"S"
class(mer$Embarked)

# NA인 Age를 knn으로 채워넣기 
#mer<-knnImputation(mer, k = 3)
#class(mer$Age)
#mer$Age<- as.integer(mer$Age)

preage<-rpart(Age~Pclass+Sex+SibSp+Parch+Fare+Embarked,data=mer[!is.na(mer$Age),],method="anova")
mer$Age[is.na(mer$Age)]<-predict(preage,mer[is.na(mer$Age),])
mer$Term<-0
mer$Term[which(mer$Age<14)]<-1
mer$Term[which(mer$Age<18 & mer$Age>=14)]<-2
mer$Term[which(mer$Age<60 & mer$Age>=18)]<-3
mer$Term[which(mer$Age>=60)]<-4


###########################################
# 데이터 형 변환
###########################################
# 얘는 integer인데 사실 사칙 연산 관계가 존재하지 않아 
# 범주형으로 바꾸는게 낳을 듯 해서 바꾼다.
mer$Term <- as.factor(mer$Term)
mer$Pclass<-as.factor(mer$Pclass)
mer$Name <- as.character(mer$Name)
mer$Ticket<-as.character(mer$Ticket)
mer$Cabin<-as.character(mer$Cabin)


#################################################
# Cabin 데이터 살려보쟈 나는 이게 층마다 뭔가를 뜻하는거라 생각하게 됨.
#################################################
mer$isCabin<-0
mer$isCabin<-apply(mer, 1, FUN= function(x){ifelse(x["Cabin"]=="", 0,1)} )
class(mer$isCabin)
mer$isCabin <- as.factor(mer$isCabin)

############################################
# 이름 변수로 가족 한번 해보기 
############################################
first<-str_split(mer$Name, ",")
mer$SurName<-lapply(first, FUN = function(x){unlist(x)[1]})

locate<- which(duplicated(mer$SurName)) # duplicated but not include own duplicated state 
locate_names<-mer$SurName[locate]

locate_<-c()
count = 1 
for (i in 1:24){
  if(mer$SurName[i] %in% locate_names)
  {
    locate_[count]<-i
    count = count  + 1
  }
}

# 같은 이름 가진 사람들 idx 가지고 있는 변수 
really_du<-c(locate_, locate)
really_du 

#더효율적 방안 찾으려했으나. 실패;; 
#library(dplyr)
#(distinct(mer,SurName))

mer$dupliated<-0
mer$duplicated<-apply(mer, 1, FUN=function(x){
  ifelse(x$SurName %in% mer$SurName[really_du], 1, 0)
})

#이렇게 끝내면 안되.... 다시 새롭게 해줘야해 
#Ticket 번호까지 같은걸 확인해야 한닷!!!!
mer[which(mer$duplicated==1),]$Ticket

locate_t<- which(duplicated(mer$Ticket)) 
locateT<-c()
count = 1 
for (i in 1:16){
  if(mer$Ticket[i] %in% mer[locate_t,]$Ticket)
  {
    locateT[count]<-i
    count = count  + 1
  }
}
really_ti_du<-c(locateT, locate_t)

mer$ticketdu<-apply(mer, 1, FUN=function(x){
  ifelse(x$Ticket %in% mer$Ticket[really_ti_du], 1, 0)
})



#mer<- mer %>% mutate(Title = as.factor(str_sub(Name, str_locate(Name,",")[,1]+2, str_locate(Name,'\\.')[,1] -1)))



# 가족 데이터 만들기 
#영국 South Shampton 에서 출발 ~ 프랑스 Cherbourg ~ 그리고 아일랜드 Qeenstown 거쳐 미국의 뉴욕 항구..  
# 1등급 329 2등급 285 3등급 710 
# 여자들 대부분 뱃머리 남자는 뒷머리 쪽에 위치해있었음
# 선원이 885 명정도에서 여성은 23명 .. 
# 가족이면 같이 승선..? 
# 여자와 아이들을 우선적으로 구하려 했으며, 3등급 객실을 분리(당시 전염병)으로 인해 구하는 과정이 늦어짐 
# 거리 개념을 조금 도입해보기로 하자 
# 가격과 pClass와 female등으로 예측해보기 Age를 / Survived됬는지도 중요함.
str(mer)
names(mer)
mer$isFamily<-as.factor(mer$isFamily)
# 테스트 데이터를 분리. 검증에 사용하기 위해서 


mer_<-mer
#mer<-mer_


mer<-mer[,c("Pclass","Sex","Embarked","Term","isCabin","isFamily")]
#mer<-mer[,-c(3,5,8,10,13,14,15,16)]
testsurv<-read.csv("gender_submission.csv",header=T)
##############################################
mersurv<-rbind(trainsurv,testsurv) 

mery<-cbind(mer,mersurv)

tests<-mery[which(mery$PassengerId>891),]
trains<-mery[which(mery$PassengerId<=891),]

#tests<-mer[which(mer$PassengerId>891),]
#trains<-mer[which(mer$PassengerId<=891),]

View(mery)

# passengerID 제외하고... train과정진행해보기 
#mer<-mer[,-1]
tests<-tests[,-7]
trains<-trains[,-7]
############################################
# train으로 검증 데이터 해보기 
#############################################
trains_train<-trains[1:600,]
trains_test<-trains[601:nrow(trains),]
fit<-glm(Survived~.,data=trains_train)
p<-predict(fit,newdata=trains_test[,-7],type="response")
p<-round(p)

CrossTable(p,trains_test[,7],prop.chisq=FALSE,prop.t=FALSE,prop.r=FALSE,
           dnn=c('predicted','actual'))




View(train[train$Survived==1,])


#trains$Survived<-as.factor(trains$Survived)
str(trains)
fit<-glm(Survived~.,data=trains)



#install.packages("randomForest")
library(randomForest)
ntree <- c(400,500,600)
mtry <-c(2:4)
param <- data.frame(n = ntree, m = mtry)
for(i in param$n){
  cat('ntree=',i,'\n')
  for (j in param$m){
    cat('mtry')
    model_titanic<-randomForest(Survived~., data=trains, ntree=i, mtry=j)
    print(model_titanic)
  }
}
fit<-randomForest(Survived~., data=trains, ntree=500, ntrt=4)
View(trains)

p<-predict(fit,newdata=tests,type="response")

p<-round(p)


table(p)
table(testsurv$Survived)

library(gmodels)

CrossTable(p,testsurv$Survived,prop.chisq=FALSE,prop.t=FALSE,prop.r=FALSE,
           dnn=c('predicted','actual'))

a<-data.frame(PassengerId=testsurv$PassengerId,Survived=p)

write.csv(a,"hyu.csv")


