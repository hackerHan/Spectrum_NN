# Neural network training comparison

hid_struc <- list(c(2,3,4),c(4,3,2),c(3,4,5),c(5,4,3),c(3,3,3),c(4,4,4),c(5,5,5),c(6,6,6),
                  c(2,3,4,5),c(5,4,3,2),c(3,4,5,6),c(6,5,4,3),c(3,3,3,3),c(4,4,4,4),c(5,5,5,5),c(6,6,6,6))

Model_16 <- list() # 16 structures
system.time({
  for(i in 1:16)
  {
    Model_16[[i]]<-list() # Assign list to model_16
    set.seed(2)
    for(j in 1:50)
    {
      Model_16[[i]][[j]] <- neuralnet(minref+maxtrans~a+b+tBCB+r_MDA,data=data3,
                                 hidden=hid_struc[[i]],stepmax=1e+05,
                                 algorithm="rprop+",err.fct="sse",
                                 act.fct="logistic",linear.output=FALSE) # 50 repetitions
    }
  }
})

# error & convergence step
error_mat <- matrix(NA, nrow = 16,ncol = 50) # error matrix
step_mat <- matrix(NA, nrow = 16,ncol =50) # convergence step matrix
for(i in 1:16)
{
  for(j in 1:50)
  {
    error_mat[i,j] <- ifelse(length(Model_16[[i]][[j]]$result.matrix[1,])==1,
                             Model_16[[i]][[j]]$result.matrix[1,],Inf) # set up error to infinity for nonconverged models
    step_mat[i,j] <- ifelse(length(Model_16[[i]][[j]]$result.matrix[3,])==1,
                            Model_16[[i]][[j]]$result.matrix[3,],0) # set up step to infinity for nonconverged models
  }
}

# nonconvergence rate
non.conv_rate <- apply(error_mat,1,function(x) mean(x>1)) # nonconvergence rate for 16 NN structures within 50 repetitions

# Mean and variance of convergence steps (exclude nonconvergence situation)
idx <- apply(error_mat,1,function(x) which(x<1)) # converged index
ave_step<-NULL
var_step<-NULL
for(i in 1:16)
{
  ave_step[i] <- mean(step_mat[i,idx[[i]]])
  var_step[i] <- var(step_mat[i,idx[[i]]])
}
# Mean and variance of error (exclude nonconvergence situations)
ave_error<-NULL
var_error<-NULL
for(i in 1:16)
{
  ave_error[i] <- mean(error_mat[i,idx[[i]]])
  var_error[i] <- var(error_mat[i,idx[[i]]])
}

# visualization
Model_info <- data.frame(hidden.layer = unlist(lapply(hid_struc,function(x) paste(unlist(x),collapse = ""))),
                         nonconvergence.rate = non.conv_rate,
                         average.converged.step = ave_step,
                         average.error = ave_error,
                         variance.error = var_error)
# visualize non-convergence rate
p1 <- ggplot(data = Model_info,aes(x = hidden.layer, y = nonconvergence.rate))+
  geom_bar(stat='identity',fill = "#9e0000")+
  ggtitle(label = "nonconvergence rate")+
  theme(plot.title = element_text(hjust = 0.5))

# visualize average convergence step
p2 <- ggplot(data = Model_info,aes(x = hidden.layer, y = average.converged.step))+
  geom_bar(stat='identity',fill = "#009e73")+
  ggtitle(label = "average convergence step")+
  theme(plot.title = element_text(hjust = 0.5))

# visualize average error
p3 <- ggplot(data = Model_info,aes(x = hidden.layer, y = average.error))+
  geom_bar(stat='identity',fill = "#663300")+
  ggtitle(label = "average error")+
  theme(plot.title = element_text(hjust = 0.5))

# visualize error variance
p4 <- ggplot(data = Model_info,aes(x = hidden.layer, y = variance.error))+
  geom_bar(stat='identity',fill = "#004d66")+
  ggtitle(label = "error variance")+
  theme(plot.title = element_text(hjust = 0.5))


 


