# Function preparation
nn<-function(x)
{
  set.seed(11)
  time9<-Sys.time()
  nn<-neuralnet(minref+maxtrans~a+b+tBCB+r_MDA,data=data3,
                hidden=c(4,4,4,4,4),stepmax=1e+05,
                algorithm="rprop+",err.fct="sse",
                act.fct="logistic",linear.output=FALSE)
  time10<-Sys.time()
  time10-time9
  return(nn)
}
# Constrast among “for” “apply” “lapply” and “foreach”
model<-list()
time1<-Sys.time()
for(i in 1:10)
{
  model[[i]]<-nn(x)
}
time2<-Sys.time()
time2-time1

time5<-Sys.time()
model<-lapply(model,nn)
time6<-Sys.time()
time6-time5

m<-matrix(0,10,1)
time7<-Sys.time()
model<-apply(m,1,nn)
time8<-Sys.time()
time8-time7

cl<-makeCluster(4)
registerDoParallel(cl)
time3<-Sys.time()
model<-foreach(i=1:10,.packages="neuralnet") %dopar%
{
  nn(x)
}
time4<-Sys.time()
time4-time3

stopCluster(cl)