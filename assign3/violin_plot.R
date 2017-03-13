#This is the violin plot part edited by Saixiong Han

-------------------------------------------------------------
#prepration part
#import training data
train<-read.csv('F:/GraduateStudy/2016Fall/DatAna/Assignment/3/train_s.csv',stringsAsFactors=F)
str(train)

#check and deal with the missing data
sum(is.na(train))
train1=train[complete.cases(train),]

#get a overview from training data
summary(train1)

--------------------------------------------------------------
#analysis part
#calculate single factor importance in female surviving

#Pclass
table(train1$Sex,train1$Pclass,train1$Survived)
#The female survived in the Titanic most from first and second class, 
#the number of female in the first and second class died in Titanic is less than 10. 
#This is a very important factor.

#Embarked
table(train1$Sex,train1$Embarked,train1$Survived)
#Cabin
table(train1$Sex,train1$Cabin,train1$Survived)
#Parch
table(train1$Sex,train1$Parch,train1$Survived)
#Women without any children or parents are more likely to survive 
#While the more parch they have, the less probability they can survive. 

#SibSp
table(train1$Sex,train1$SibSp,train1$Survived)
#Women without any siblings or spouses are more likely to survive.
#While the more siblings they have, the less probability they can survive. 

#title
train1$Title<-gsub('(.*,)|(\\..*)','',train1$Name)
table(train1$Sex,train1$Title,train1$Survived)
#The survived unmarried women is more than married women, which is accor1dance with the result about SibSp. Generally, the number of women survived is more than the women died.


------------------------------------------------------------------
#data visualization part
require(ggplot2)
data(train1)
head(train1)

#use scatter plot to show the relation among pclass,survived,age and fare
p<-ggplot(data=train1,mapping = aes(x=Age,y=Fare,shape=Pclass_s,color=Survived_s))
p+geom_point()
#This is scatter plot shows there is no linear relation between age and fare. 
#From the color scatter we can see, blue points are mostly on the top wihle the red points are maily on the bottom. 
#That means the higher fare is, the more chances for surviving. It also can be shown in the shape of different classes. The first and second class get more chances for surviving compared to the third class.

#use scatter plot to show the relation among sex,survived,age and fare
p<-ggplot(data=train1,mapping = aes(x=Age,y=Fare,shape=Sex,color=Survived_s))
p+geom_point()
#From this scatter plot we can know, 
#the blue round points is more than blue squre points. 
#That shows females have more chances of surviving compared to males. 
#In addition, the higher fare of females bought, the higher chances for surviving.


#the distribution of age and fare(histogram)
ggplot(train1)+geom_histogram(aes(x=Age,fill=Survived_s))
#the age histogram is left-skewed, whhich means the mean age is less than the median.
#Most of the people is between 20 to 40 years old.
#Most of the survived people also are around 20 to 30 years old.
#Other ages seldom people survived and there is an outlier around 30 age.
##I don't know how to descibe and analyze histogram, so I'm not sure if this descriprion will be ok.
ggplot(train1)+geom_histogram(aes(x=Fare,fill=Survived_s))
#the fare histigram is left skewed, which means the mean fare is less than the median.
#Most of people get the fare less than 50 and most of them didn't survived.
#few people get the fare above 100, but most of this part of people survived.


#the distribution of SibSp and Parch(bar-plot)
ggplot(train1)+geom_bar(aes(x=SibSp,fill=Survived_s))+coord_flip()
#most people don't have any siblings or spouses and nearly quarter of them survived.
#few people have more than 1 siblings and spouses but most of them didn't survived.
ggplot(train1)+geom_bar(aes(x=Parch,fill=Survived_s))+coord_flip()
#most people don't have any parents or children and nearly one thirds of them survived.
#nearly half of people having 1 or 2 parents or chidren survived.
#few people have more than 2 siblings and spouses but most of them didn't survived.
ggplot(train1)+geom_bar(aes(x=SibSp+Parch,fill=Survived_s))+coord_flip()
#most people don't have any family members and a quarter of them survived.
#survived people are more than died people during the range of 1 to 3 family members
#people having more than 3 family members get lower chance for surviving.

#the relation between fare,sex and survived(box-plot)
ggplot(train1)+geom_boxplot(aes(x=Sex,y=Fare,fill=Survived_s))
#From the charts we can see, the lower whisker and lower quartile for all the four box are the same
#the survived female have higher median and upper quartier than the died female
#that shows female with higher fare or higher pclass have more chance for surviving
#it's the same with male's surviving, but the amount of surviving men is smaller than women
#the distribution of survived female is very sparse while the distribution of died male is very intensive.

#the relation between fare,pclass and survived(box-plot)
ggplot(train1)+geom_boxplot(aes(x=Pclass_s,y=Fare,fill=Survived_s))
#The charts shows that the fare in the first class has larger range than the second and third class
#the median of survived people in first class is equal to the upper quartier of dead people
#Generally, people in the first class with higher fare have more chance to survive
#the upper whisker of second and third class are almost equal to the lower quartier of first class
#the fare distribution of second and third class are more intensive than the first class

#the relation among age,pclass and survived(violin_plot)
p<-ggplot(train1,aes(factor(Pclass),Age))
p+geom_violin(aes(fill=Survived_s))

#the relation among age,sex and survived(violin_plot)
p<-ggplot(train1,aes(factor(Sex),Age))
p+geom_violin(aes(fill=Survived_s))



