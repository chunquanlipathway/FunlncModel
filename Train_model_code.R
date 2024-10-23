#####################需要加载的R包
library(caret)
library(e1071)
library(randomForest)
#####################10倍交叉验证，标准化训练数据以及训练模型过程
############第一步：准备训练模型的特征矩阵，矩阵中最后一列为已知的标签，详见"Feature_matrix(HESC).csv"等例子文件
Training_Test_yes<-as.matrix(read.table("Input_yes.txt",header=T)[,c(1:6,8,11:63)])
Training_Test_no<-as.matrix(read.table("Input_NO.txt",header=T)[,c(1:6,8,11:63)])
Training_Test_set_all<-as.matrix(rbind(Training_Test_yes,Training_Test_no))
l<-length(Training_Test_set_all[1,])-1
l2<-length(Training_Test_set_all[1,])
newdata1_all<-Training_Test_set_all[,2:l]
newdata1_all=apply(newdata1_all,2,as.numeric)####将特征值转为数字格式
rownames(newdata1_all)<-Training_Test_set_all[,1]
mdrrClass_all<-matrix(Training_Test_set_all[,l2],ncol=1)
Fold_lable<-matrix(Training_Test_set_all[,length(Training_Test_set_all[1,])],ncol=1)
############第二步：构建10倍交叉验证的数据
Train_list<-createFolds(Fold_lable,k = 10,list = TRUE,returnTrain = TRUE)
models<-list()
Var<-list()
VarImp<-list()
form<-list()
Pred1_rf_list<-list()
for(j in 1:10)
{
newdata2_Train<-newdata1_all[Train_list[[j]],]##基于10倍交叉验证分割，提取训练集
newdata2_Test<-newdata1_all[-Train_list[[j]],]##基于10倍交叉验证分割，提取测试集
mdrrClass_Train<-mdrrClass_all[Train_list[[j]]]
mdrrClass_Test<-mdrrClass_all[-Train_list[[j]]]
############第三步：特征矩阵标准化
preProcValues=preProcess(newdata2_Train,method=c("center","scale"))
trainTransformed=predict(preProcValues,newdata2_Train)
testTransformed=predict(preProcValues,newdata2_Test)
form[[j]]= list(preProcValues=preProcValues,trainTrans=trainTransformed,testTrans=testTransformed)
############第四步：rfe函数进行特征选择，设定感兴趣的特征数量，以筛选出最优的集合数量
subsets=c(10,15,20,25,30,35,40,45,50,55)
ctrl= rfeControl(functions = rfFuncs, method = "cv")
Profile = rfe(as.data.frame(trainTransformed),factor(mdrrClass_Train),metric ="Accuracy", sizes = subsets, rfeControl = ctrl)
print(Profile)
Profile$optVariables
newdata3_Train<-trainTransformed[,Profile$optVariables]
newdata3_Test<-testTransformed[,Profile$optVariables]
Var[j]= list(Var=Profile$optVariables)
############第五步：利用caret包训练模型过程，参数method可以选择感兴趣的机器学习方法
fitControl = trainControl(method ="repeatedcv", number = 10, repeats = 3,returnResamp = "all")
rfFit1= train(as.data.frame(newdata3_Train),factor(mdrrClass_Train),method = "rf",trControl = fitControl,tuneLength = 10,verbose = F)
models[[j]]= list(rfFit1=rfFit1)###将训练好的模型存储到list，方便调用
VarrfFit1=varImp(models[[j]]$rfFit1,scale=FALSE)###由于训练中参数method设定为随机森林，因此可根据节点纯度获得特征节点的重要性得分
VarImp[j]=list(VarrfFit1)
############第六步：以上五步是为了训练好相应的分类模型，接下来是利用extractProb将模型应用到测试集，以及用户自己的测试数据，更详细的可见"Prediction_code.R"
probValues = extractProb(models[[j]],testX = as.data.frame(newdata3_Test),testY = factor(mdrrClass_Test))
test_probValues = subset(probValues, dataType == "Test")
Pred1_rf= subset(test_probValues, model =="rf")#提取随机森林方法的预测结果
Pred1_rf_list[[j]]= list(Pred1_rf=Pred1_rf)
print(j) 
}