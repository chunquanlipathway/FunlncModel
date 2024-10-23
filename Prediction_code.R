#####################需要加载的R包
library(caret)
library(e1071)
library(randomForest)
load("HESC_Train_model.Rdata")
############第一步：加载已经训练好的分类模型，例如HESC_Train_model和Combiner_Train_model等文件（可从https://bio.liclab.net/FunlncModel/download.php获取）；
load("HESC_Train_model.RData")

############第二步：准备自己的特征矩阵，矩阵中最后一列为已知的标签，详见"Feature_matrix(HESC).csv"等例子文件；
file_unknow<-as.matrix(read.table("Input_unknow.txt",header=T))
newdata1_unknow<-file_unknow[,2:l]
newdata1_unknow=apply(newdata1_unknow,2,as.numeric)
rownames(newdata1_unknow)<-file_unknow[,1]
mdrrClass_unknow<-matrix(file_unknow[,l2],ncol=1)

############第三步：将训练好的分类模型应用到用户自己的测试数据中，由于例子模型是基于10倍交叉验证，因此这里采用了10次循环，用户可视情况更改此步骤；
for(j in 1:10)
{
preProcValues=form[[j]]$preProcValues###提取训练好的数据缩放尺度；
unknowformed=predict(preProcValues,newdata1_unknow)###将测试数据进行标准化；
unknowValues = extractProb(models[[j]],testX = as.data.frame(unknowformed[,which(colnames(unknowformed)%in%as.matrix(Var[j][[1]]))]),testY = factor(mdrrClass_unknow))###预测过程：其中Var[j][[1]]代表训练过程中最优的特征集合；
test_unknowValues = subset(unknowValues, dataType == "Test")
unknow_rf= subset(test_unknowValues, model =="rf")###提取随机森林的预测结果
write.table(unknow_rf,paste0("Unknow_preresult",j,".txt"),row.names=T,quote=F,sep="\t")###结果输出包括：模型预测的概率值以及标签
}