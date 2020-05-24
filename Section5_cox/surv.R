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

sc  <- spark_connect("local[*]")

# Functions ---------------------------------------------------------------


parallel_function <- function(iter){
  result <- NULL
  
  library(yardstick)
  iter <- as.data.frame(iter)$iter[1]
  set.seed(iter)
  # generate data -----------------------------------------------------------
  data <- glasp::simulate_CEN_surv_data(N = 100, p = 20, p_group = 5, true_p_group = 2, 
                                        rho = 0.5)
  new_data <- data
  dataset <- data$data
  part <-rsample::initial_split(dataset, prop = 0.5)
  data$data <- rsample::training(part)
  new_data$data <- rsample::testing(part)
  
  
  # compute glasp -----------------------------------------------------------
  glasp_mod <- glasp::glasp_model(mode = "classification",
                                  l1 = tune::tune(),
                                  l2 = tune::tune(),
                                  frob = tune::tune(),
                                  num_comp = 3)
  
  glasp_mod <- parsnip::set_engine(glasp_mod, "glasp")
  metric_vals <- metric_set(roc_auc)
  data_rs <- rsample::vfold_cv(data$data, v = 10)
  ctrl <- tune::control_grid(verbose = T)
  
  rec_form <- tune::tune_grid( event~., model = glasp_mod, resamples = data_rs, metrics = metric_vals,
                              control = ctrl, grid = 1000)
  
  # ctrl <- tune::control_bayes(verbose = T, no_improve = 50, uncertain = 2)
  # rec_form <- tune::tune_bayes( event~., model = glasp_mod, resamples = data_rs, metrics = metric_vals,
  #                             control = ctrl, iter = 1000)

  
  best_config <- tune::show_best(rec_form, n = 50, metric = "roc_auc")
  
  model <- glasp::linear_classification(
    event ~ .,
    data$data,
    l1 = best_config$l1[1],
    l2 = best_config$l2[1],
    frob = best_config$frob[1],
    num_comp = 3 )
  
  error <- glasp::error_summary_CEN_surv(model, new_data)
  result <- rbind(result, data.frame(iter = iter, model = "glasp", error))

  # survival ----------------------------------------------------------------
  datos <- data$data
  datos$event <- as.numeric(datos$event) - 1
  library(survival)
  fit <- coxph(Surv(time, event, type = "right")~., datos)
  
  # compute clusters 
  X <- datos[, -c(1:2)]
  Eta_t <- apply(X, 1, function(x){x*fit$coefficients})
  km <- kmeans(Eta_t, centers = 3)
  #km$cluster
  
  # compute baseline survival
  bs <- survfit(fit)
  t <- c(0, bs$time, Inf)
  sv <- c(1, bs$surv, 0)
  
  # Overwrite model
  model$beta <- fit$coefficients
  model$intercept <- 0
  model$clusters <- km$cluster
  model$info$blin_S <- data.frame(time = t, S0 = sv)
  
  error <- glasp::error_summary_CEN_surv(model, new_data)
  result <- rbind(result, data.frame(iter = iter, model = "coxph", error))
  
  return(
    dplyr::as_tibble(result)
  )
}


# Loop --------------------------------------------------------------------

num_iter <- 30

iter <- expand.grid(iter = 1:num_iter) %>% 
  copy_to(sc, ., memory = TRUE, repartition = 30)

# result <- spark_apply(iter, function(x){x+1}) %>% as_tibble()

result <- spark_apply(iter, parallel_function, 
                      columns = list(
                        iter = "integer",
                        model = "character",
                        beta_rmse = "double",
                        beta_correctzeros = "double",
                        beta_numnonzeros = "integer",
                        vi.dist = "double",
                        vi.dist_sup = "double",
                        mean_abs_S_error = "double",
                        beta_tpr = "double",
                        beta_tnr = "double",
                        RI = "double",
                        RI_sup = "double"
                      )) %>%
  as_tibble()
save.image("Section5_cox/result.RData")


# summarise ---------------------------------------------------------

result %>% group_by(model) %>% summarise_all(list(mean)) %>% as.data.frame()
result %>% group_by(model) %>% summarise_all(list(function(x){sd(x)/sqrt(30)})) %>% as.data.frame()
