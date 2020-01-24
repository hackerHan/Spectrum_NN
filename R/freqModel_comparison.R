library(neuralnet)
library(plotly)
library(gridExtra)


# Neural network comparison among different structures
# Add frequency as input; Check performance for minref and maxtrans independently

# Model structure options
hid_struc <- list(c(2,3,4),c(4,3,2),c(3,4,5),c(5,4,3),c(3,3,3),c(4,4,4),c(5,5,5),
                  c(6,6,6),c(2,3,4,5),c(5,4,3,2),c(3,4,5,6),c(6,5,4,3),c(3,3,3,3),c(4,4,4,4),
                  c(5,5,5,5),c(6,6,6,6))

# Shuffle the data
# data.random <- data3[sample(nrow(data3)),]

data.random <- data.random %>%
  mutate(norm.freq1 = (freq1-mean(freq1))/sqrt(var(freq1)),
         norm.freq2 = (freq2-mean(freq2))/sqrt(var(freq2)))

# Create 10 folds
N_folds = 10 # number of folds
folds <-cut(seq(1,nrow(data.random)),breaks=N_folds,labels=FALSE)

# training repetition
N_rep <- 10

# initialize model
freq2minref_model <- rep(list(rep(list(rep(list(NA),N_folds)),N_rep)),length(hid_struc))
freq2maxtra_model <- rep(list(rep(list(rep(list(NA),N_folds)),N_rep)),length(hid_struc))

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
      freq2minref_model[[i]][[j]][[k]] <- 
        neuralnet(minref~a+b+tBCB+r_MDA+norm.freq1,data=train_data,
                  hidden=hid_struc[[i]],stepmax=1e+05,
                  algorithm="rprop+",err.fct="sse",
                  act.fct="logistic",linear.output=FALSE)
    }
  }
}

for(i in 1:length(hid_struc))
{
  for(j in 1:N_rep)
  {
    for(k in 1:N_folds)
    {
      test_idx<-which(folds==k,arr.ind=TRUE)
      test_data<-data.random[test_idx,]
      train_data<-data.random[-test_idx,]
      freq2maxtra_model[[i]][[j]][[k]] <- 
        neuralnet(maxtrans~a+b+tBCB+r_MDA+norm.freq2,data=train_data,
                  hidden=hid_struc[[i]],stepmax=1e+05,
                  algorithm="rprop+",err.fct="sse",
                  act.fct="logistic",linear.output=FALSE)
    }
  }
}


# ====
# error matrix and step matrix initialization
minref_error_mat<-rep(list(matrix(NA,nrow = N_rep,ncol = N_folds)),length(hid_struc))
maxtra_error_mat<-rep(list(matrix(NA,nrow = N_rep,ncol = N_folds)),length(hid_struc))

minref_step_mat <-rep(list(matrix(NA,nrow = N_rep,ncol = N_folds)),length(hid_struc))
maxtra_step_mat <-rep(list(matrix(NA,nrow = N_rep,ncol = N_folds)),length(hid_struc))

for(i in 1:length(hid_struc))
{
  for(j in 1:N_rep)
  {
    for(k in 1:N_folds)
    {
      tryCatch({
        test_idx<-which(folds==k,arr.ind=TRUE) # validation set index
        testData<-data.random[test_idx,] # validation set
        minref_pred <-neuralnet::compute(freq2minref_model[[i]][[j]][[k]],testData[c(1:4,11)])
        maxtra_pred <-neuralnet::compute(freq2maxtra_model[[i]][[j]][[k]],testData[c(1:4,12)])
        
        minref_error_mat[[i]][j,k]<-(1/nrow(testData))*(sum((testData[,5]-minref_pred$net.result[,1])^2))
        maxtra_error_mat[[i]][j,k]<-(1/nrow(testData))*(sum((testData[,8]-maxtra_pred$net.result[,1])^2))
        
        minref_step_mat[[i]][j,k]<-freq2minref_model[[i]][[j]][[k]]$result.matrix[3,] # convergence step
        maxtra_step_mat[[i]][j,k]<-freq2maxtra_model[[i]][[j]][[k]]$result.matrix[3,] # convergence step
        
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
        minref_error_mat[[i]][j,k]<-ifelse(freq2minref_model[[i]][[j]][[k]]$result.matrix[1,]>1,0,minref_error_mat[[i]][j,k])
        maxtra_error_mat[[i]][j,k]<-ifelse(freq2maxtra_model[[i]][[j]][[k]]$result.matrix[1,]>1,0,maxtra_error_mat[[i]][j,k])
        
        minref_step_mat[[i]][j,k]<-ifelse(freq2minref_model[[i]][[j]][[k]]$result.matrix[1,]>1,0,minref_step_mat[[i]][j,k])
        maxtra_step_mat[[i]][j,k]<-ifelse(freq2maxtra_model[[i]][[j]][[k]]$result.matrix[1,]>1,0,maxtra_step_mat[[i]][j,k])
      },error=function(e){})

      minref_error_mat[[i]][j,k]<-ifelse(is.na(minref_error_mat[[i]][j,k]),0,minref_error_mat[[i]][j,k])
      maxtra_error_mat[[i]][j,k]<-ifelse(is.na(maxtra_error_mat[[i]][j,k]),0,maxtra_error_mat[[i]][j,k])
      
      minref_step_mat[[i]][j,k]<-ifelse(is.na(minref_step_mat[[i]][j,k]),0,minref_step_mat[[i]][j,k])
      maxtra_step_mat[[i]][j,k]<-ifelse(is.na(maxtra_step_mat[[i]][j,k]),0,maxtra_step_mat[[i]][j,k])
    }
  }
}





# ================================================================================
# Freq2Minref
# Exclude non-convergence situation
minref_cross.error <- rep(list(NA),length(hid_struc)) # k folds cross validation error
minref_grand.error <- NULL # grand validation error corresponding to each model structure
minref_var_cross.error <- NULL # variance of k folds cross validationn error during training
minref_var.error <- NULL # variance of validation error corresponding to each model structure
minref_grand_mean.step <- NULL # grand mean of convergence step
minref_var_step <- NULL # variance of convergence step corresponding to each model structure
minref_non_convergence_rate <- NULL # percentage of nonconvergence situation
for(i in 1:length(hid_struc))
{
  minref_cross.error[[i]] <- apply(minref_error_mat[[i]],1,function(x) sum(x)/sum(x != 0))
  minref_grand.error[i] <- sum(minref_error_mat[[i]])/sum(minref_error_mat[[i]] != 0)
  minref_var_cross.error[i] <- var(minref_cross.error[[i]])
  minref_var.error[i] <- var(minref_error_mat[[i]][which(minref_error_mat[[i]] != 0)])
  minref_grand_mean.step[i] <- sum(minref_step_mat[[i]])/sum(minref_step_mat[[i]] != 0 )
  minref_var_step[i] <- var(minref_step_mat[[i]][which(minref_step_mat[[i]] != 0 )])
  minref_non_convergence_rate[i] <- sum(minref_error_mat[[i]] == 0)/length(minref_error_mat[[i]])
}



# information summary
minref_Model_info <- data.frame(hidden.layer = unlist(lapply(hid_struc,function(x) paste(unlist(x),collapse = ""))),
                         nonconvergence.rate = minref_non_convergence_rate,
                         average.converged.step = minref_grand_mean.step,
                         var.converged.step = minref_var_step,
                         average.error = minref_grand.error,
                         var.cverror = minref_var_cross.error,
                         var.grand.error = minref_var.error)


# hidden layer vs nonconvergence.rate
p7 <- ggplot(minref_Model_info)+
  geom_bar(mapping = aes(x = hidden.layer, y = nonconvergence.rate),stat = "identity")

# hidden layer vs average convergence steps
p8 <- ggplot(minref_Model_info)+
  geom_bar(mapping = aes(x = hidden.layer, y = average.converged.step),stat = "identity")

# hidden layer vs variance of convergence steps
p9 <- ggplot(minref_Model_info)+
  geom_bar(mapping = aes(x = hidden.layer, y = var.converged.step),stat = "identity")

# hidden layer vs grand mean error
p10 <- ggplot(minref_Model_info)+
  geom_bar(mapping = aes(x = hidden.layer, y = average.error),stat = "identity")

# hidden layer vs variance of cross validation error
p11 <- ggplot(minref_Model_info)+
  geom_bar(mapping = aes(x = hidden.layer, y = var.cverror),stat = "identity")

# hidden layer vs variance of grand error
p12 <- ggplot(minref_Model_info)+
  geom_bar(mapping = aes(x = hidden.layer, y = var.grand.error),stat = "identity")


grid.arrange(p7,p8,p9,p10,p11,p12,nrow = 3,ncol = 2)





# ================================================================================
# Freq2Maxtra
# Exclude non-convergence situation
maxtra_cross.error <- rep(list(NA),length(hid_struc)) # k folds cross validation error
maxtra_grand.error <- NULL # grand validation error corresponding to each model structure
maxtra_var_cross.error <- NULL # variance of k folds cross validationn error during training
maxtra_var.error <- NULL # variance of validation error corresponding to each model structure
maxtra_grand_mean.step <- NULL # grand mean of convergence step
maxtra_var_step <- NULL # variance of convergence step corresponding to each model structure
maxtra_non_convergence_rate <- NULL # percentage of nonconvergence situation
for(i in 1:length(hid_struc))
{
  maxtra_cross.error[[i]] <- apply(maxtra_error_mat[[i]],1,function(x) sum(x)/sum(x != 0))
  maxtra_grand.error[i] <- sum(maxtra_error_mat[[i]])/sum(maxtra_error_mat[[i]] != 0)
  maxtra_var_cross.error[i] <- var(maxtra_cross.error[[i]])
  maxtra_var.error[i] <- var(maxtra_error_mat[[i]][which(maxtra_error_mat[[i]] != 0)])
  maxtra_grand_mean.step[i] <- sum(maxtra_step_mat[[i]])/sum(maxtra_step_mat[[i]] != 0 )
  maxtra_var_step[i] <- var(maxtra_step_mat[[i]][which(maxtra_step_mat[[i]] != 0 )])
  maxtra_non_convergence_rate[i] <- sum(maxtra_error_mat[[i]] == 0)/length(maxtra_error_mat[[i]])
}


# information summary
maxtra_Model_info <- data.frame(hidden.layer = unlist(lapply(hid_struc,function(x) paste(unlist(x),collapse = ""))),
                                nonconvergence.rate = maxtra_non_convergence_rate,
                                average.converged.step = maxtra_grand_mean.step,
                                var.converged.step = maxtra_var_step,
                                average.error = maxtra_grand.error,
                                var.cverror = maxtra_var_cross.error,
                                var.grand.error = maxtra_var.error)


# hidden layer vs nonconvergence.rate
p13 <- ggplot(maxtra_Model_info)+
  geom_bar(mapping = aes(x = hidden.layer, y = nonconvergence.rate),stat = "identity")

# hidden layer vs average convergence steps
p14 <- ggplot(maxtra_Model_info)+
  geom_bar(mapping = aes(x = hidden.layer, y = average.converged.step),stat = "identity")

# hidden layer vs variance of convergence steps
p15 <- ggplot(maxtra_Model_info)+
  geom_bar(mapping = aes(x = hidden.layer, y = var.converged.step),stat = "identity")

# hidden layer vs grand mean error
p16 <- ggplot(maxtra_Model_info)+
  geom_bar(mapping = aes(x = hidden.layer, y = average.error),stat = "identity")

# hidden layer vs variance of cross validation error
p17 <- ggplot(maxtra_Model_info)+
  geom_bar(mapping = aes(x = hidden.layer, y = var.cverror),stat = "identity")

# hidden layer vs variance of grand error
p18 <- ggplot(maxtra_Model_info)+
  geom_bar(mapping = aes(x = hidden.layer, y = var.grand.error),stat = "identity")


grid.arrange(p13,p14,p15,p16,p17,p18,nrow = 3,ncol = 2)

































