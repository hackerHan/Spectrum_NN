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
# error mean and variance
# contain nonconverged situation
error_mean <- apply(nn.res,1,mean)
error_var <- apply(nn.res,1,var)
# visualize error mean and variance
ggplot(data = cv.table1,aes(x = layers, y = nodes))+
  geom_point(aes(colour = error_var,alpha = error_mean))+
  scale_color_gradient(low = "green",high= "red")


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

# contour plot part
# set up specific combinations to generate contour plot
a.val<-b.val<-c(0,0.045,0.09)
t.val<-c(0.26,0.30,0.34,0.38,0.42)
r.val<-c(unique(grid$r_MDA)[12],unique(grid$r_MDA)[18],unique(grid$r_MDA)[21])
#contour plot of minref with fixed a and b
system.time({minref.abfixed<-foreach(i=1:length(grid.pred)) %dopar%
{
  foreach(j=1:length(grid.pred[[i]])) %dopar%
  {
    foreach(k=1:length(a.val)) %dopar%
    {
      foreach(t=1:length(b.val),.packages="plotly") %dopar%
      {
        minref.abfixed<-grid.pred[[i]][[j]] %>%
          filter(a==a.val[k]&b==b.val[t])
        plot_ly(data=minref.abfixed,type="contour",
                   x=~tBCB,y=~r_MDA,z=~minref,colors=c("green","red"),
                   contours=list(showlabels=TRUE),
                   hoverinfo="all",name="minref") %>%
          colorbar(title="minref") %>%
          layout(title=paste("a=",a.val[k],",","b=",b.val[t],sep=""))
      }
    }
  }
}
})

#contour plot of maxtrans with fixed a and b
system.time({maxtra.abfixed<-foreach(i=1:length(grid.pred),.packages="doParallel") %dopar%
{
  foreach(j=1:length(grid.pred[[i]])) %dopar%
  {
    foreach(k=1:length(a.val)) %dopar%
    {
      foreach(t=1:length(b.val),.packages="plotly") %dopar%
      {
        maxtra.abfixed<-grid.pred[[i]][[j]] %>%
          filter(a==a.val[k]&b==b.val[t])
        plot_ly(data=maxtra.abfixed,type="contour",
                x=~tBCB,y=~r_MDA,z=~maxtrans,colors=c("red","green"),
                contours=list(showlabels=TRUE),
                hoverinfo="all",name="maxtrans") %>%
          colorbar(title="maxtrans") %>%
          layout(title=paste("a=",a.val[k],",","b=",b.val[t],sep=""))
      }
    }
  }
}
})

#contour plot of minref with fixed tBCB and r_MDA
system.time({minref.trfixed<-foreach(i=1:length(grid.pred)) %dopar%
{
  foreach(j=1:length(grid.pred[[i]])) %dopar%
  {
    foreach(k=1:length(t.val)) %dopar%
    {
      foreach(t=1:length(r.val),.packages="plotly") %dopar%
      {
        minref.trfixed<-grid.pred[[i]][[j]] %>%
          filter(tBCB==t.val[k]&r_MDA==r.val[t])
        plot_ly(data=minref.trfixed,type="contour",
                x=~a,y=~b,z=~minref,colors=c("green","red"),
                contours=list(showlabels=TRUE),
                hoverinfo="all",name="minref") %>%
          colorbar(title="minref") %>%
          layout(title=paste("tBCB=",t.val[k],",","r_MDA=",r.val[t],sep=""))
      }
    }
  }
}
})

#contour plot of maxtrans with fixed tBCB and r_MDA
system.time({maxtra.trfixed<-foreach(i=1:length(grid.pred),.packages="doParallel") %dopar%
{
  foreach(j=1:length(grid.pred[[i]])) %dopar%
  {
    foreach(k=1:length(t.val)) %dopar%
    {
      foreach(t=1:length(r.val),.packages="plotly") %dopar%
      {
        maxtra.trfixed<-grid.pred[[i]][[j]] %>%
          filter(tBCB==t.val[k]&r_MDA==r.val[t])
        plot_ly(data=maxtra.trfixed,type="contour",
                x=~a,y=~b,z=~maxtrans,colors=c("red","green"),
                contours=list(showlabels=TRUE),
                hoverinfo="all",name="maxtrans") %>%
          colorbar(title="maxtrans") %>%
          layout(title=paste("tBCB=",t.val[k],",","r_MDA=",r.val[t],sep=""))
      }
    }
  }
}
})

# Add Frequency as input
# Choose 6 * 3 structure as NN model
data3.minref<-select(data3,a:freq1)%>%
  mutate(norm.freq1 = (freq1-mean(freq1))/sqrt(var(freq1)))
data3.maxtra<-select(data3,a:r_MDA,maxtrans:freq2) %>%
  mutate(norm.freq2 = (freq2-mean(freq2))/sqrt(var(freq2)))


# nn_1<-neuralnet(minref~a+b+tBCB+r_MDA+norm.freq1,data=data3.minref,
#               hidden=rep(6,3),stepmax=1e+05,
#                algorithm="rprop+",err.fct="sse",
#                act.fct="logistic",linear.output=FALSE)
# saveRDS(nn_1,"nn_1.rds")
nn_1<-readRDS("nn_1.rds")
# nn_2<-neuralnet(maxtrans~a+b+tBCB+r_MDA+norm.freq2,data=data3.maxtra,
#               hidden=rep(6,3),stepmax=1e+05,
#                algorithm="rprop+",err.fct="sse",
#                act.fct="logistic",linear.output=FALSE)
# saveRDS(nn_2,"nn_2.rds")
nn_2<-readRDS("nn_2.rds")
# grid prediction
grid.freq <- expand.grid(a=round(seq(0.0,0.9,0.045),3),b=round(seq(0,0.9,0.045),3),
                         tBCB=round(seq(0.1,0.9,0.04),3),r_MDA=seq(0.61111,0.77778,0.0083335),
                    freq = seq(47.0,47.8,0.2))%>%
  mutate(norm.freq = (freq-mean(freq))/sqrt(var(freq)),
         pred.minref=neuralnet::compute(nn_1,cbind(a,b,tBCB,r_MDA,norm.freq))$net.result[,1],
         pred.maxtra=neuralnet::compute(nn_2,cbind(a,b,tBCB,r_MDA,norm.freq))$net.result[,1])



# contour plot ShinyApp
library(shiny)
library(shinythemes)
shinyApp(
  
  ui = fluidPage(
    theme = shinytheme("spacelab"),
    # Feature selection
    fixedRow(
      # Input: a
      column(4,sliderInput(inputId = "featureInput1", label = "a",min = 0, max = 0.9,
                           step = 0.09, value = 0)),
      # Input: b
      column(4,sliderInput(inputId = "featureInput2", label = "b",min = 0, max = 0.9,
                           step = 0.09, value = 0)),
      # Input: frequency
      column(4,sliderInput(inputId = "featureInput3", label = "frequency",min = 47,
                           max = 47.8, step = 0.2, value = 47))),
    # First row
    fixedRow(
      column(6, plotlyOutput("Plot1",height = "600px")),
      column(6, plotlyOutput("Plot2",height = "600px"))),
    
    # Feature selection
    fixedRow(
      # Input: tBCB
      column(4,sliderInput(inputId = "featureInput4", label = "tBCB",min = 0.1, max = 0.9,
                           step = 0.08, value = 0.5)),
      # Input: r_MDA
      column(4,sliderInput(inputId = "featureInput5", label = "r_MDA",min = 0.61111, max = 0.77778,
                           step = 0.016667, value = 0.77778)),
      # Input: frequency
      column(4,sliderInput(inputId = "featureInput6", label = "frequency",min = 47,
                           max = 47.8, step = 0.2, value = 47))
    ),
    # Second row
    fixedRow(
      column(6, plotlyOutput("Plot3",height = "600px")),
      column(6, plotlyOutput("Plot4",height = "600px"))
    )
  ),
  
  server = function(input, output) {
    grid.abfixed <- reactive({
      filter(grid.freq,round(a,2) == input$featureInput1 &
               round(b,2) == input$featureInput2 &
               round(freq,2) == input$featureInput3)
    })
    grid.trfixed <- reactive({
      filter(grid.freq,round(tBCB,2) == input$featureInput4 &
              round(r_MDA,6) == input$featureInput5 &
               round(freq,2) == input$featureInput6)
    })
    
    output$Plot1 <- renderPlotly({plot_ly(data=grid.abfixed(),type="contour",
                            x=~tBCB,y=~r_MDA,z=~pred.minref,colors=c("red","green"),
                            contours=list(showlabels=TRUE),
                            hoverinfo="all",name="minref") %>%
        colorbar(title="minref")
    
  })
    output$Plot2 <- renderPlotly({plot_ly(data=grid.abfixed(),type="contour",
                                          x=~tBCB,y=~r_MDA,z=~pred.maxtra,
                                          colors=c("green","red"),
                                          contours=list(showlabels=TRUE),
                                          hoverinfo="all",name="maxtrans") %>%
        colorbar(title="maxtrans")
      
    })
    output$Plot3 <- renderPlotly({plot_ly(data=grid.trfixed(),type="contour",
                                          x=~a,y=~b,z=~pred.minref,colors=c("red","green"),
                                          contours=list(showlabels=TRUE),
                                          hoverinfo="all",name="minref") %>%
        colorbar(title="minref")
      
    })
    output$Plot4 <- renderPlotly({plot_ly(data=grid.trfixed(),type="contour",
                                          x=~a,y=~b,z=~pred.maxtra,colors=c("green","red"),
                                          contours=list(showlabels=TRUE),
                                          hoverinfo="all",name="maxtrans") %>%
        colorbar(title="maxtrans")
      
    })
    
    
},
  options = list(height = 500)
)






