#x is hidden layer type; k is k folds;
cv.err<-function(x,data=data3,k=10,seed.val=1)
{
  MSE<-ave.RMSE<-va.MSE<-NULL
  set.seed(seed.val)
  #randomly shuffle the data
  data.rd<-data[sample(nrow(data)),]
  #Create k equally size folds
  folds<-cut(seq(1,nrow(data.rd)),breaks=k,labels=FALSE)
  layer.val<-x[1] # #layers
  nodes.val<-x[2] # #nodes
  #Parallel computing, return type is "vector"
  MSE<-foreach(i=1:k,.combine="c") %dopar%
  {
    set.seed(seed.val)
    test_ind<-which(folds==i,arr.ind=TRUE)
    testData<-data.rd[test_ind,]
    trainData<-data.rd[-test_ind,]
    #Estimated model of trainData
    nn.model<-neuralnet(minref+maxtrans~a+b+tBCB+r_MDA,data=trainData,
                        hidden=rep(nodes.val,layer.val),stepmax=1e+05,
                        algorithm="rprop+",err.fct="sse",
                        act.fct="logistic",linear.output=FALSE)
    #Predictions of testData
    nn.pred<-compute(nn.model,testData[1:4])
    #Mean square errors in each fold
    MSE<-(1/nrow(testData))*(sum((testData[,5]-nn.pred$net.result[,1])^2)+
                                       sum((testData[,8]-nn.pred$net.result[,2])^2))
  }
  #Overall Root mean square error for each network structure
  ave.RMSE<-sqrt(sum(MSE)/k)
  #variation of RMSE
  va.RMSE<-var(sqrt(MSE))
  return(list(MSE,ave.RMSE,va.RMSE))
}


model.rep<-function(x,data=data3,rep=10,seed.val=1)
{
  set.seed(seed.val)
  layer.val<-x[1] # #layers
  nodes.val<-x[2] # #nodes
  #Repeat training and save model list
  nn.model<-foreach(i=1:rep) %dopar%
  {
    neuralnet(minref+maxtrans~a+b+tBCB+r_MDA,data=data,
              hidden=rep(nodes.val,layer.val),stepmax=1e+05,
              algorithm="rprop+",err.fct="sse",
              act.fct="logistic",linear.output=FALSE)
  }
  return(nn.model)
}













