# Neural network comparison among different structures

# Model structure options
hid_struc <- list(c(2,3,4),c(4,3,2),c(3,4,5),c(5,4,3),c(3,3,3),c(4,4,4),c(5,5,5),
                  c(6,6,6),c(2,3,4,5),c(5,4,3,2),c(3,4,5,6),c(6,5,4,3),c(3,3,3,3),c(4,4,4,4),
                  c(5,5,5,5),c(6,6,6,6))

# Shuffle the data
data.random <- data3[sample(nrow(data3)),]
# Write.csv(data.random,"rd_data.csv")

# Create 10 folds
N_folds = 10 # number of folds
folds <-cut(seq(1,nrow(data.random)),breaks=N_folds,labels=FALSE)

# training repetition
N_rep <- 10

# initialize model
comp_model <- rep(list(rep(list(rep(list(NA),N_folds)),N_rep)),length(hid_struc))

# training
for(i in 1:length(hid_struc))
{
  for(j in 1:N_rep)
  {
    for(k in 1:N_folds)
    {
      test_idx<-which(folds==k,arr.ind=TRUE)
      test_data<-data.random[test_idx,]
      train_data<-data.random[-test_idx,]
      comp_model[[i]][[j]][[k]] <- 
        neuralnet(minref+maxtrans~a+b+tBCB+r_MDA,data=train_data,
                  hidden=hid_struc[[i]],stepmax=1e+05,
                  algorithm="rprop+",err.fct="sse",
                  act.fct="logistic",linear.output=FALSE)
    }
  }
}

# error matrix and step matrix initialization
error_mat<-rep(list(matrix(NA,nrow = N_rep,ncol = N_folds)),length(hid_struc))
step_mat <-rep(list(matrix(NA,nrow = N_rep,ncol = N_folds)),length(hid_struc))

for(i in 1:length(hid_struc))
{
  for(j in 1:N_rep)
  {
    for(k in 1:N_folds)
    {
      tryCatch({
        test_idx<-which(folds==k,arr.ind=TRUE) # validation set index
        testData<-data.random[test_idx,] # validation set
        test_pred <-neuralnet::compute(comp_model[[i]][[j]][[k]],testData[1:4])
        error_mat[[i]][j,k]<-(1/nrow(testData))*(sum((testData[,5]-test_pred$net.result[,1])^2)+
                                                   sum((testData[,8]-test_pred$net.result[,2])^2))
        step_mat[[i]][j,k]<-comp_model[[i]][[j]][[k]]$result.matrix[3,] # convergence step
      },error=function(e){})
    }
  }
}

# data preprocessing
for(i in 1:length(hid_struc))
{
  for(j in 1:N_rep)
  {
    for(k in 1:N_folds)
    {
      # skip if there's error message
      tryCatch({
        error_mat[[i]][j,k]<-ifelse(comp_model[[i]][[j]][[k]]$result.matrix[1,]>1,0,error_mat[[i]][j,k])
        step_mat[[i]][j,k]<-ifelse(comp_model[[i]][[j]][[k]]$result.matrix[1,]>1,0,step_mat[[i]][j,k])
      },error=function(e){})
      error_mat[[i]][j,k]<-ifelse(is.na(error_mat[[i]][j,k]),0,error_mat[[i]][j,k])
      step_mat[[i]][j,k]<-ifelse(is.na(step_mat[[i]][j,k]),0,step_mat[[i]][j,k])
    }
  }
}




# Exclude non-convergence situation
cross.error <- rep(list(NA),length(hid_struc)) # k folds cross validation error
grand.error <- NULL # grand validation error corresponding to each model structure
var_cross.error <- NULL # variance of k folds cross validationn error during training
var.error <- NULL # variance of validation error corresponding to each model structure
grand_mean.step <- NULL # grand mean of convergence step
var_step <- NULL # variance of convergence step corresponding to each model structure
non_convergence_rate <- NULL # percentage of nonconvergence situation
for(i in 1:length(hid_struc))
{
  cross.error[[i]] <- apply(error_mat[[i]],1,function(x) sum(x)/sum(x != 0))
  grand.error[i] <- sum(error_mat[[i]])/sum(error_mat[[i]] != 0)
  var_cross.error[i] <- var(cross.error[[i]])
  var.error[i] <- var(error_mat[[i]][which(error_mat[[i]] != 0)])
  grand_mean.step[i] <- sum(step_mat[[i]])/sum(step_mat[[i]] != 0 )
  var_step[i] <- var(step_mat[[i]][which(step_mat[[i]] != 0 )])
  non_convergence_rate[i] <- sum(error_mat[[i]] == 0)/length(error_mat[[i]])
}



# information summary
Model_info <- data.frame(hidden.layer = unlist(lapply(hid_struc,function(x) paste(unlist(x),collapse = ""))),
                         nonconvergence.rate = non_convergence_rate,
                         average.converged.step = grand_mean.step,
                         var.converged.step = var_step,
                         average.error = grand.error,
                         var.cverror = var_cross.error,
                         var.grand.error = var.error)


# hidden layer vs nonconvergence.rate
p1 <- ggplot(Model_info)+
  geom_bar(mapping = aes(x = hidden.layer, y = nonconvergence.rate),stat = "identity")

# hidden layer vs average convergence steps
p2 <- ggplot(Model_info)+
  geom_bar(mapping = aes(x = hidden.layer, y = average.converged.step),stat = "identity")

# hidden layer vs variance of convergence steps
p3 <- ggplot(Model_info)+
  geom_bar(mapping = aes(x = hidden.layer, y = var.converged.step),stat = "identity")

# hidden layer vs grand mean error
p4 <- ggplot(Model_info)+
  geom_bar(mapping = aes(x = hidden.layer, y = average.error),stat = "identity")

# hidden layer vs variance of cross validation error
p5 <- ggplot(Model_info)+
  geom_bar(mapping = aes(x = hidden.layer, y = var.cverror),stat = "identity")

# hidden layer vs variance of grand error
p6 <- ggplot(Model_info)+
  geom_bar(mapping = aes(x = hidden.layer, y = var.grand.error),stat = "identity")


grid.arrange(p1,p2,p3,p4,p5,p6,nrow = 3,ncol = 2)




















