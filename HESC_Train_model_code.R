library(caret)
library(e1071)
library(randomForest)
#####################10倍交叉验证，标准化训练数据以及训练模型
Training_Test_yes<-as.matrix(read.table("Input_yes.txt",header=T)[,c(1:6,8,11:63)])
Training_Test_no<-as.matrix(read.table("Input_NO.txt",header=T)[,c(1:6,8,11:63)])
Training_Test_set_all<-as.matrix(rbind(Training_Test_yes,Training_Test_no))
l<-length(Training_Test_set_all[1,])-1
l2<-length(Training_Test_set_all[1,])
newdata1_all<-Training_Test_set_all[,2:l]
newdata1_all=apply(newdata1_all,2,as.numeric)
rownames(newdata1_all)<-Training_Test_set_all[,1]
mdrrClass_all<-matrix(Training_Test_set_all[,l2],ncol=1)
Fold_lable<-matrix(Training_Test_set_all[,length(Training_Test_set_all[1,])],ncol=1)
Train_list<-createFolds(Fold_lable,k = 10,list = TRUE,returnTrain = TRUE)
models<-list()
Var<-list()
VarImp<-list()
form<-list()
Pred1_rf_list<-list()
for(j in 1:10)
{
newdata2_Train<-newdata1_all[Train_list[[j]],]
newdata2_Test<-newdata1_all[-Train_list[[j]],]
mdrrClass_Train<-mdrrClass_all[Train_list[[j]]]
mdrrClass_Test<-mdrrClass_all[-Train_list[[j]]]
preProcValues=preProcess(newdata2_Train,method=c("center","scale"))
trainTransformed=predict(preProcValues,newdata2_Train)
testTransformed=predict(preProcValues,newdata2_Test)
form[[j]]= list(preProcValues=preProcValues,trainTrans=trainTransformed,testTrans=testTransformed)
subsets=c(10,15,20,25,30,35,40,45,50,55,60)
ctrl= rfeControl(functions = rfFuncs, method = "cv")
Profile = rfe(as.data.frame(trainTransformed),factor(mdrrClass_Train),metric ="Accuracy", sizes = subsets, rfeControl = ctrl)
print(Profile)
Profile$optVariables
newdata3_Train<-trainTransformed[,Profile$optVariables]
newdata3_Test<-testTransformed[,Profile$optVariables]
Var[j]= list(Var=Profile$optVariables)
fitControl = trainControl(method ="repeatedcv", number = 10, repeats = 3,returnResamp = "all")
rfFit1= train(as.data.frame(newdata3_Train),factor(mdrrClass_Train),method = "rf",trControl = fitControl,tuneLength = 10,verbose = F)
models[[j]]= list(rfFit1=rfFit1)
VarrfFit1=varImp(models[[j]]$rfFit1,scale=FALSE)
VarImp[j]=list(VarrfFit1)
probValues = extractProb(models[[j]],testX = as.data.frame(newdata3_Test),testY = factor(mdrrClass_Test))
test_probValues = subset(probValues, dataType == "Test")
Pred1_rf= subset(test_probValues, model =="rf")
Pred1_rf_list[[j]]= list(Pred1_rf=Pred1_rf)
Pred1_rf$lable=ifelse(Pred1_rf$obs=='Yes',yes=1,0)
Pred2_rf<-prediction(Pred1_rf$Yes,Pred1_rf$lable)
as.numeric(performance(Pred2_rf,'auc')@y.values)
precrec_Pred1_rf <- evalmod(scores=Pred1_rf$Yes,labels=Pred1_rf$lable)
auc(precrec_Pred1_rf)$aucs[2]
print(j) 
}


