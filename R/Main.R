library(tidyverse)
library(neuralnet)
library(doParallel) #Parallel computing
library(plotly)
library(wesanderson) #Wes Anderson color palettes
if (!require("processx")) install.packages("processx") #contains orca() to export graph locally
data3<-read.csv("TrainSet3.csv")

#Brief summary of data3
str(data3)
summary(data3)
range(data3$a) #range of a=(0.0,0.9)
range(data3$b) #range of b=(0.0,0.9)
range(data3$tBCB) #range of tBCB=(0.1,0.9)
range(data3$r_MDA) #range of r_MDA=(0.61111,0.77778)

#Data visualization
attach(data3)
#2D relationship between minref & maxtrans;
#Red represents bad values while green represents good values
plot_ly(mpg,x=~minref,y=~maxtrans) %>%
  add_markers(color=~maxtrans,colors=c("red","green"))
#3D relationships between a & b & minref|maxtrans
plot_ly(mpg,x=~a,y=~b,z=~minref) %>%
  add_markers(color=~minref,colors=c("green","red"))
plot_ly(mpg,x=~a,y=~b,z=~maxtrans) %>%
  add_markers(color=~maxtrans,colors=c("red","green"))
#3D relationships between tBCB & r_MDA & minref|maxtrans
plot_ly(mpg,x=~tBCB,y=~r_MDA,z=~minref) %>%
  add_markers(color=~minref,colors=c("green","red"))
plot_ly(mpg,x=~tBCB,y=~r_MDA,z=~maxtrans) %>%
  add_markers(color=~maxtrans,colors=c("red","green"))
detach(data3)

#hidden layer types
#totally 11 types of hidden layer(s)
hid<-data.frame(layers=c(1,1,2,2,3,3,3,4,4,5,5),
                nodes=c(4,6,3,5,2,4,6,3,5,3,4))
#2D scatterplot of #nodes & #layers
#wes_palette represents color palette type
ggplot(data=hid)+
  geom_point(mapping=aes(x=layers,y=nodes,color=as.factor(nodes)),size=4)+
  scale_color_manual(values=wes_palette(n=5,name="Darjeeling1"),
                     name="nodes")

#setup ThreadProc
cl<-makeCluster(4)
registerDoParallel(cl)

time1<-Sys.time()
#Parallel computing MSE & overall RMSE & RMSE variation
#Assign given package(s) to each core
cv.info<-foreach(i=1:11,.packages=c("neuralnet","doParallel")) %dopar%
{
  cv.err(as.numeric(hid[i,]),seed.val=2)
}
time2<-Sys.time()
time2-time1

#extract overall RMSE for each structure
ave.RMSE<-foreach(i=1:11,.combine="c") %dopar%
{
  cv.info[[i]][[2]]
}
#extract variation of RMSE for each structure
va.RMSE<-foreach(i=1:11,.combine="c") %dopar%
{
  cv.info[[i]][[3]]
}

#RMSE info table ordered by overall RMSE
cv.table<-hid %>%
  mutate(ave.RMSE=ave.RMSE,va.RMSE=va.RMSE) %>%
  arrange(ave.RMSE)
#Visualization
ggplot(data = cv.table)+
  geom_point(aes(x = layers,y = nodes,size = va.RMSE,color = ave.RMSE))+
  scale_color_gradient(low = "green",high= "red")+
  geom_text(aes(label = paste('mean=',round(ave.RMSE,6),sep = ""),
                x = layers,y = nodes),vjust = 2.0 )+
  geom_text(aes(label = paste('variance=',round(va.RMSE,6),sep = ""),
                x = layers,y = nodes),vjust = 4.0 )+
  xlim(0,6)+
  ylim(1,6)+
  ggtitle("Mean and variance of Cross-validation errors")+
  theme(plot.title = element_text(hjust = 0.5))



#Select the structure where the results of RMSE and variation are
#both ranked in the 6 lowest values.
cv.table1<-cv.table[rank(cv.table$ave.RMSE)<=6&rank(cv.table$va.RMSE)<=6,]

#Repeat model 10 times for each hidden layer structure and save as list
#nn.model contains 5 hidden layer structure where 
#each structure has 10 models
#i.e nn.model[[i]][[j]] represents jth repetition for ith structure
nn.model<-foreach(i=1:5) %dopar%
{
  model.rep(as.numeric(cv.table1[i,]),seed.val=2) # 50 models
}


#Extract error vector for each structure,save as data.frame
#ith row represents ith structure,jth column represents jth repetition
nn.res<-foreach(i=1:length(nn.model),.combine="rbind") %dopar%
{
  #Extract errors for each repetition,save as vector
  foreach(j=1:length(nn.model[[i]]),.combine="c") %dopar%
  {
    nn.model[[i]][[j]]$result.matrix[1,]
  }
}
# write.csv(nn.res,"nn_res.csv")

# error mean & variance & nonconvergence rate
# include nonconverged situation
error_mean <- apply(nn.res,1,mean)
error_var <- apply(nn.res,1,var)
nonconv_rate <- apply(nn.res,1,function(x) mean(x>1))
# visualize error mean & variance & nonconvergence_rate
ggplot(data = cv.table1,aes(x = layers, y = nodes))+
  geom_point(aes(color = error_mean, size = error_var))+
  scale_color_gradient(low = "green",high= "red",limits = c(0,3))+
  xlim(0,6)+
  ylim(1,6)+
  geom_text(aes(label = paste('mean=',round(error_mean,6),sep = ""),
                x = layers,y = nodes),vjust = 2.0 )+
  geom_text(aes(label = paste('variance=',round(error_var,6),sep = ""),
                x = layers,y = nodes),vjust = 4.0 )+
  geom_text(aes(label = paste('nonconvergence rate = ',nonconv_rate,sep = ""),
                x = layers ,y = nodes),vjust = 6.0)+
  ggtitle("Mean & Variance & nonconvergence rate within 10 repetitions")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(color = "error mean")+
  labs(size = "error variance")

# exclude nonconvergence situation
conv_error_mean <- apply(nn.res,1,function(x) mean(x[which(x<1)])) # error mean in converged situation
conv_error_var <- apply(nn.res,1,function(x) var(x[which(x<1)]))
conv_size <- apply(nn.res,1,function(x) sum(x<1))
ggplot(data = cv.table1,aes(x = layers, y = nodes))+
  geom_point(aes(color = conv_error_mean, size = conv_error_var))+
  scale_color_gradient(low = "green",high= "red",limits = c(0.35,0.65))+
  xlim(0,6)+
  ylim(1,6)+
  geom_text(aes(label = paste('mean=',round(conv_error_mean,6),sep = ""),
                x = layers,y = nodes),vjust = 2.0 )+
  geom_text(aes(label = paste('variance=',round(conv_error_var,6),sep = ""),
                x = layers,y = nodes),vjust = 4.0 )+
  geom_text(aes(label = paste('converged sample size = ',conv_size,sep = ""),
                x = layers ,y = nodes),vjust = 6.0)+
  ggtitle("Mean & Variance & Efficient sample size of converged samples among 10 repetitions ")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(color = "error mean")+
  labs(size = "error variance")


#Grid prediction part
#Set up digits
grid<-expand.grid(a=round(seq(0.0,0.9,0.045),3),b=round(seq(0,0.9,0.045),3),
                  tBCB=round(seq(0.1,0.9,0.04),3),r_MDA=seq(0.61111,0.77778,0.0083335))

#grid prediction
grid.pred<-foreach(i=1:length(nn.model),.packages=c("doParallel","neuralnet")) %dopar%
{
  #Extract errors for each repetition,save as vector
  foreach(j=1:length(nn.model[[i]]),.verbose=TRUE,.packages=c("doParallel","neuralnet")) %dopar%
  {
    cbind(grid,
          minref=neuralnet::compute(nn.model[[i]][[j]],grid[1:4])$net.result[,1],
          maxtrans=neuralnet::compute(nn.model[[i]][[j]],grid[1:4])$net.result[,2])
  }
}

#subset correponding to 5 lowest minref and 5 highest maxtrans
min.5ref<-foreach(i=1:length(grid.pred),.packages="doParallel") %dopar%
{
  #Assign package "dplyr" to each core
  foreach(j=1:length(grid.pred[[i]]),.packages="dplyr") %dopar%
  {
    grid.pred[[i]][[j]] %>%
      #5 lowest minref values' subset
      filter(rank(minref)<=5)
  }
}
max.5tra<-foreach(i=1:length(grid.pred),.packages="doParallel") %dopar%
{
  #Assign package "dplyr" to each core
  foreach(j=1:length(grid.pred[[i]]),.packages="dplyr") %dopar%
  {
    grid.pred[[i]][[j]] %>%
      #5 highest maxtrans values' subset
      filter(rank(-maxtrans)<=5)
  }
}

#combine subset to see consistency
#combine row values as data.frame
grid.comb<-foreach(i=1:length(grid.pred),.combine="rbind") %dopar%
{
  #combine column features to compare consistency
  foreach(j=1:length(grid.pred[[i]]),.packages="dplyr",.combine="rbind") %dopar%
  {
    #structure represents hidden layer structure,rep represents jth repetition
    cbind(min.5ref[[i]][[j]],max.5tra[[i]][[j]],
          structure=paste(cv.table1[i,]$layers,"layers",cv.table1[i,]$nodes,"nodes",sep=""),
          rep=j)
  }
}
# write.csv(grid.comb,"grid_comb.csv")

stopCluster(cl) # stop parallel computing









