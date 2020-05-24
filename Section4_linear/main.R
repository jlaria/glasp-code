# sc6

# Connect cluster --------------------------------------------------------
library(sparklyr)
library(dplyr)

# conf <- spark_config()
# conf$spark.executor.memory <- "2GB"
# conf$spark.memory.fraction <- 0.9
# conf$spark.yarn.maxAppAttempts <- 20
# conf$sparklyr.apply.schema.infer <- 1

# sc <- spark_connect(master="spark://master:7077", 
#                     version = "2.4.4",
#                     config = conf,
#                     spark_home = "~/spark-2.4.4-bin-hadoop2.7/")

sc <- spark_connect(master = "local[*]")


# Functions ---------------------------------------------------------------


parallel_function <- function(iter){
  library(yardstick)

  rho <- as.data.frame(iter)$rho[1]
  iter <- as.data.frame(iter)$iter[1]
  

  # generate data -----------------------------------------------------------
  set.seed(iter)
  # Set-up #
  n <- 200
  p <- 1000
  beta <- c(rep(1,25)+runif(25)/5-0.1,rep(0,25),rep(-1,25),rep(0,25),rep(0,p-100))
  
  Sigma <- matrix(0, nrow=p, ncol=p)
  for(j in 1:2){
    Sigma[((j-1)*50+1):(j*50), ((j-1)*50+1):(j*50)] <- rho 
  }
  diag(Sigma) <- 1
  noiseSD <- 2.5
  # Set-up #
  
  
  
  ntun <- 25
  mseste.oscar <- mseste.en <- mseste.cen <- mseste.cenKnownCs <- array(NA, dim=c(ntun,ntun))
  epesval.oscar <- epesval.en <- epesval.cen <- epesval.cenKnownCs <- array(NA, dim=c(ntun,ntun))
  numnonzeros.oscar <- numnonzeros.en <- numnonzeros.cen <-numnonzeros.cenKnownCs <-  array(NA, dim=c(ntun,ntun))
  correctzeros.oscar <- correctzeros.en <- correctzeros.cen <- correctzeros.cenKnownCs <- array(NA, dim=c(ntun,ntun))
  RIs.grouplasso <- correctzeros.grouplasso <-  mseste.grouplasso <- epesval.grouplasso <- numnonzeros.grouplasso <- rep(NA, ntun)
  RIs.grouplasso3 <- correctzeros.grouplasso3 <-  mseste.grouplasso3 <- epesval.grouplasso3 <- numnonzeros.grouplasso3 <- rep(NA, ntun)
  #RIs.grouplasso20 <- correctzeros.grouplasso20 <-  mseste.grouplasso20 <- epesval.grouplasso20 <- numnonzeros.grouplasso20 <- rep(NA, ntun)
  RIs.oscar <-  RIs.en <- RIs.cen <- RIs.cenKnownCs <- array(NA, dim=c(ntun,ntun))
  
  # Using this as "true Cs" because despite correlation structure in X, the 0 coefficients should all be clustered together by CEN
  trueCs <- c(rep(1,25),rep(2,25),rep(3,25),rep(2,25),rep(2,900)) 
  
  
  # Generating a particular instance of data #
  x <- mvtnorm::rmvnorm(n*6, sigma=Sigma)
  y <- as.numeric(x%*%beta+noiseSD*rnorm(n*6))  
  xtr <- x[1:n,]; ytr <- y[1:n]; xval <- x[1:n+n,]; yval <- y[1:n+n]; xte <- x[(2*n+1):(6*n),]; yte <- y[(2*n+1):(6*n)]
  rm(x); rm(y)  
  # Now standardizing the data as needed to perform CEN/glmnet/etc. w/o hiccup #
  # Really, I'm just centering the response and the features to have mean 0 in the training set
  yte <- (yte-mean(ytr))
  yval <- (yval-mean(ytr))
  ytr <- (ytr-mean(ytr))
  meanx <- apply(xtr,2,mean)
  xtr <- scale(xtr, center=meanx, scale=FALSE)
  xte <- scale(xte, center=meanx, scale=FALSE)
  xval <- scale(xval, center=meanx, scale=FALSE)   
  
  trueCs <- c(rep(1,50),rep(2,50),rep(3,900)) 
  # compute glasp -----------------------------------------------------------
  
  train_data <- data.frame(y =  ytr,  xtr)
  valid_data <- data.frame(y =  yval,  xval)
  test_data <- data.frame(y =  yte,  xte)
  
  glasp_mod <- glasp::glasp_model(
    l1 = tune::tune(),
    l2 = tune::tune(),
    frob = tune::tune(),
    num_comp = 4
  )
  glasp_mod <- parsnip::set_mode(glasp_mod, "regression")
  glasp_mod <- parsnip::set_engine(glasp_mod, "glasp")
  
  data_rs <- rsample::vfold_cv(rbind(train_data, valid_data), v = 2)
  data_rs$splits$`1`$in_id <- 1:200
  data_rs$splits$`2`$in_id <- 201:400
  metric_vals <- yardstick::metric_set(rmse)
  ctrl <- tune::control_grid(verbose = T)
  
  rec_form <-
    tune::tune_grid(
      y~.,
      model = glasp_mod,
      resamples = data_rs,
      metrics = metric_vals,
      control = ctrl,
      grid = 1000
    )
  
  best_config <- tune::show_best(rec_form[1,], metric = "rmse")
  model <- glasp::linear_regression(
    y ~ . ,
    train_data,
    l1 = best_config$l1[1],
    l2 = best_config$l2[1],
    frob = best_config$frob[1],
    num_comp = 4
  )
  betas_est <- model$beta
  
  store.rmseste <- sqrt(sum(( xte%*%beta- xte%*%betas_est)^2))
  store.RIs <- clues::adjustedRand(trueCs, model$clusters, randMethod="Rand")
  store.numnonzeros <- sum(betas_est!=0)
  store.correctzeros <- mean(((betas_est==0)&beta==0) | (betas_est!=0)&(beta!=0)) # Fraction of features for which estimated and true sparsity are the same
  
  return(
    dplyr::tibble(
      rho = rho,
      iter = iter,
      store.rmseste = store.rmseste,
      store.correctzeros = store.correctzeros,
      store.numnonzeros = store.numnonzeros,
      store.RIs = store.RIs)
    )
}


# Loop --------------------------------------------------------------------
rho <- c(0, 0.1, 0.2, 0.5, 0.8)
num_iter <- 30

iter <- expand.grid(iter = 1:num_iter, rho = rho) %>% 
  copy_to(sc, ., memory = TRUE, repartition = 150)

# result <- spark_apply(iter, function(x){x+1}) %>% as_tibble()

result <- spark_apply(iter, parallel_function, 
                      columns = list(
                        rho = "double",
                        iter = "integer",
                        store.rmseste = "double",
                        store.correctzeros = "double",
                        store.numnonzeros = "integer",
                        store.RIs = "double"
                      )) %>%
  as_tibble()


# Summary -----------------------------------------------------------------

summary <- 
  result %>% group_by(rho) %>% summarise(
    Method = "glasp",
    RMSE = paste(round(mean(store.rmseste),3), "(", round(sd(store.rmseste)/sqrt(num_iter),3), ")",  sep=""),
    `Correct Zeros` = paste(round(mean(store.correctzeros),3), "(", round(sd(store.correctzeros)/sqrt(num_iter),3), ")",  sep=""),
    `Num. Non-Zeros` = paste(round(mean(store.numnonzeros),3), "(", round(sd(store.numnonzeros)/sqrt(num_iter),3), ")",  sep=""),
    RI = paste(round(mean(store.RIs),3), "(", round(sd(store.RIs)/sqrt(num_iter),3), ")",  sep="")
  )

write.table(file="Section4_linear/results.glasp", summary[,-1], sep=" & ", quote=FALSE, row.names=FALSE,col.names=FALSE, eol="\\\\ \n")  
