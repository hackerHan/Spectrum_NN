# Neural network training comparison

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

# define model type
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



