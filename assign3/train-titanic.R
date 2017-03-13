library(data.table)

train=fread("C:/data/train-titanic.csv", sep=",", header=TRUE, stringsAsFactors=TRUE)
setkey(train)
summary(train)
#Age Na 177, Cabin 687, Embarked 2
train[Embarked==''] # two ladies with same ticket no. fare and cabin no.,but no embark port info
train[Ticket=='113572']
train[Cabin=='B28'] 
train[Fare==80]
plot(train$Embarked,train$Fare) #still no idea, ,,
train$Embarked=as.character(train$Embarked)
train[Embarked=='', Embarked:=NA]
summary(train$Cabin)
train[Cabin=='B96 B98']
train[Cabin=='B71']
train[Cabin!=''] #some survived, some not; not all 1st pclass,, not sure what it means

#list single variable distribution except name and ticket no.
library(ggplot2)
library(gridExtra)
psur=qplot(train$Survived,geom='bar',fill=I('#DB9933'), 
      ylab='Numbers of People', main='Survived or Not')
ppclass=qplot(train$Pclass,geom='bar',fill=I('#F00350'),main='Social-Economic Status', 
      ylab='Numbers of People')
psex=qplot(train$Sex,geom='bar',fill=I('#DBFF33'),main='Sex', 
      ylab='Numbers of People')
page=qplot(train$Age,geom='bar',fill=I('#FFBD33'),main='Age', 
      ylab='Numbers of People')
pss=qplot(train$SibSp,geom='bar',fill=I('#FFBD66'),main='People with x Siblings and Spouse', 
      ylab='Numbers of People')
ppc=qplot(train$Parch,geom='bar',fill=I('#FF8833'),main='People with x Parents and Children', 
      ylab='Numbers of People')
pf=qplot(train$Fare, fill=I('#FF5733'),main='Price of Ticket', 
      ylab='Numbers of People')
pe=qplot(train$Embarked, fill=I('#FF5700'),main='Embarked Port', 
      ylab='Numbers of People')
grid.arrange(psur, ppclass,psex,page,pss,ppc,pf,pe, ncol=3)


train$NewName=grepl('(', train$Name,fixed=TRUE)
ggplot(train,aes(x=NewName, fill=Survived))+geom_bar() 
#kind of correlation, what does it mean? 

train[Ticket=='1601']
#same ticket no, different last name, friends?
train[Ticket=='347082']
#Seems a big family, may be a ticket are used by a group of connected people

# to do correlation
install.packages("GGally")
library(GGally)
ggcorr(train)
#Only shows numeric variables
train[Sex=='male',nSex:=1]
train[Sex=='female',nSex:=2]
train[Embarked=='C',Port:=1]
train[Embarked=='Q',Port:=2]
train[Embarked=='S',Port:=3]
ggcorr(train,label=TRUE, label_alpha=TRUE) #fixed, looks fine


train$Survived=as.factor(train$Survived)
SurPc=ggplot(train,aes(x=Pclass, fill=Survived))+geom_bar()
SurFa=ggplot(train,aes(x=Fare, fill=Survived))+geom_histogram()
SurFa=ggplot(train,aes(x=Sex, fill=Survived))+geom_bar()             
grid.arrange(SurPc,SurFa,SurFa, ncol=3) 
# correlation >0.3

# to show factor importance
library(randomForest)
rf <- randomForest(Survived ~ Pclass+Age+Sex+Parch+SibSp+Fare+Embarked, data=train,
                   na.action = na.omit,ntree=100, importance=TRUE)

plot(rf, ylim=c(0,1))
legend('topright', colnames(rf$err.rate), col=1:3, fill=1:3)
varImpPlot(rf, sort=TRUE) #not factorized,...
#data.table biggest disadvantage, will learn how to use dplyr
train$Pclass=as.factor(train$Pclass)
train$Sex=as.factor(train$Sex) 
train$Age=as.numeric(train$Age)
train$Parch=as.factor(train$Parch)
train$SibSp=as.factor(train$SibSp) 
train$Fare=as.numeric(train$Fare) 
train$Embarked=as.factor(train$Embarked) 
rf2 = randomForest(Survived ~ Pclass+Age+Sex+Parch+SibSp+Fare+Embarked, data=train,
                   na.action = na.omit, ntree=101, importance=TRUE)
plot(rf2, ylim=c(0,1))
legend('topright', colnames(rf2$err.rate), col=1:3, fill=1:3)
varImpPlot(rf2, sort=TRUE)
#alright now
