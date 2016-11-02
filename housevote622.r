
#split 60/20/20 of the data as the training/validation/test sets
#using knn model

df <- read.table("https://raw.githubusercontent.com/chirayukong/infsci2725-fall-2016/master/class-7/house-votes-84.txt",header = TRUE,stringsAsFactors = TRUE);


# convert Party data to factor and the other issue data to numeric
# this is for the application of KNN
df$Party<-as.factor(df$Party)
df$handicapped_infants<-as.numeric(df$handicapped_infants)
df$water_project_cost_sharing<-as.numeric(df$water_project_cost_sharing)
df$adoption_of_the_budget_resolution<-as.numeric(df$adoption_of_the_budget_resolution)
df$physician_fee_freeze<-as.numeric(df$physician_fee_freeze)
df$el_salvador_aid<-as.numeric(df$el_salvador_aid)
df$religious_groups_in_schools<-as.numeric(df$religious_groups_in_schools)
df$nti_satellite_test_ban<-as.numeric(df$nti_satellite_test_ban)
df$aid_to_nicaraguan_contras<-as.numeric(df$aid_to_nicaraguan_contras)
df$mx_missile<-as.numeric(df$mx_missile)
df$immigration<-as.numeric(df$immigration)
df$synfuels_corporation_cutback<-as.numeric(df$synfuels_corporation_cutback)
df$education_spending<-as.numeric(df$education_spending)
df$superfund_right_to_sue<-as.numeric(df$superfund_right_to_sue)
df$crime<-as.numeric(df$crime)
df$duty_free_exports<-as.numeric(df$duty_free_exports)
df$export_administration_act_sa<-as.numeric(df$export_administration_act_sa)


#divide the whole data into train and test on the ratio of 80/20
#data(df)
smp_size<-floor(0.80*nrow(df))
set.seed(123)
train_ind<-sample(seq_len(nrow(df)),size=smp_size)
train1<-df[train_ind,]
train<-df[train_ind,2:17]
test<-df[-train_ind,2:17]

#divide the train data into training data and validation data according to 75/25
#so that the whole data will be divided into training, validation and test on the ratio of 60/20/20
#data(train)
smp_size1<-floor(0.75*nrow(train))
set.seed(123)
ind<-sample(seq_len(nrow(train)),size = smp_size1)
training<-train[ind,]
validation<-train[-ind,]

#use knn model to train data
#use validation data to test the appropriate K value
#install.packages("class",repos='http://cran.us.r-project.org/')
library(class)
traininglabels<-train1[ind,1]
validalabels<-train1[-ind,1]

#df_pred_i<-knn(train=training,test=validation,cl=traininglabels,k=2)
df_pred_i<-knn(train=training,test=validation,cl=traininglabels,k=3)
#df_pred_i<-knn(train=training,test=validation,cl=traininglabels,k=4)
#df_pred_i<-knn(train=training,test=validation,cl=traininglabels,k=5)

#evaluate knn model with validation data and find the appropriate k=3
install.packages("gmodels",repos='http://cran.us.r-project.org/')
library(gmodels)
CrossTable(x=validalabels,y=df_pred_i,prop.chisq=FALSE)

#initial positive and negative value for each of two Parties
#caculate the initial accuracy, specificity and sensitivity of training data
df_pred_i <- as.data.frame(df_pred_i)
validalabels <- as.data.frame(validalabels)
cm_i <- table(df_pred_i$df_pred_i,validalabels$validalabels)
accuracy_i=(cm_i[1,1]+cm_i[2,2])/(sum(cm_i))
sensitivity_i=(cm_i[1,1])/(cm_i[1,1]+cm_i[1,2])
specificity_i=(cm_i[2,2])/(cm_i[2,1]+cm_i[2,2])
cm_i
accuracy_i
sensitivity_i
specificity_i

#use test data to test the accuracy of train data
library(class)
trainlabels<-df[train_ind,1]
testlabels<-df[-train_ind,1]
df_pred_f<-knn(train=train,test=test,cl=trainlabels,k=3)

#evaluate the knn model with test data
library(gmodels)
CrossTable(x=testlabels,y=df_pred_f,prop.chisq=FALSE)

#final positive and negative value for each of two Parties
#caculate the final accuracy, specificity and sensitivity of train data
df_pred_f <- as.data.frame(df_pred_f)
testlabels <- as.data.frame(testlabels)
cm_f <- table(df_pred_f$df_pred_f,testlabels$testlabels)
accuracy_f=(cm_f[1,1]+cm_f[2,2])/(sum(cm_f))
sensitivity_f=(cm_f[1,1])/(cm_f[1,1]+cm_f[1,2])
specificity_f=(cm_f[2,2])/(cm_f[2,1]+cm_f[2,2])
cm_f
accuracy_f
sensitivity_f
specificity_f

#make a histogram of the final result
install.packages('ggplot2',repos='http://cran.us.r-project.org/')
library(ggplot2)
new_df <- df_pred_f
new_df$testlabels <- as.vector(testlabels)
ggplot(new_df)+geom_bar(aes(x=df_pred_f))
ggplot(new_df)+geom_bar(aes(testlabels))


