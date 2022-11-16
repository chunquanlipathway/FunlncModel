library(caret)
library(e1071)
library(randomForest)
load("HESC_Train_model.Rdata")
file_unknow<-as.matrix(read.table("Input_unknow.txt",header=T))
newdata1_unknow<-file_unknow[,2:l]
newdata1_unknow=apply(newdata1_unknow,2,as.numeric)
rownames(newdata1_unknow)<-file_unknow[,1]
mdrrClass_unknow<-matrix(file_unknow[,l2],ncol=1)
for(j in 1:10)
{
preProcValues=form[[j]]$preProcValues
unknowformed=predict(preProcValues,newdata1_unknow)
unknowValues = extractProb(models[[j]],testX = as.data.frame(unknowformed[,which(colnames(unknowformed)%in%as.matrix(Var[j][[1]]))]),testY = factor(mdrrClass_unknow))
test_unknowValues = subset(unknowValues, dataType == "Test")
unknow_rf= subset(test_unknowValues, model =="rf")
write.table(unknow_rf,paste0("Unknow_preresult",j,".txt"),row.names=T,quote=F,sep="\t")
}