load("Section7_real/dlbcl_processed.RData")
set.seed(0)
# model -------------------------------------------------------------------
library(glasp)
library(tune)
library(yardstick)
library(parsnip)
library(dplyr)
library(survival)
library(rsample)

glasp_mod <- glasp_model(mode = "classification",
                         l1 = tune(),
                         l2 = tune(),
                         frob = tune(),
                         num_comp = tune()) %>%
  set_engine("glasp")

metric_vals <- metric_set(mn_log_loss, pr_auc, roc_auc)
data_rs <- vfold_cv(datos, v = 2)
ctrl <- control_grid(verbose = F)

rec_form <- tune_grid( Status~., model = glasp_mod, resamples = data_rs, metrics = metric_vals,
                             control = ctrl, grid = 100)
best_config <- show_best(rec_form, "pr_auc", n = 10)

model <- linear_classification(
  Status ~ .,
  datos,
  l1 = best_config$l1[9],
  l2 = best_config$l2[9],
  frob = best_config$frob[9],
  num_comp = best_config$num_comp[9] )
model$clusters
model$beta

save.image("dlbcl_result_seed0.RData")


cl <- model$clusters[model$beta != 0]
vars <- gsub("`",  "", names(cl))
vars <- sapply(strsplit(vars, "\\("), function(x){x[1]})
type <- ifelse(1:80 >= 79, "clinical", "genetic")[model$beta != 0]
betas <- round(model$beta[model$beta!=0], digits = 3)
vars <- paste0(paste0(vars, "\n"), betas)

df <- data.frame(cl = cl, vars = vars, y = 1, type = type)
library(ggplot2)

ggplot(df) + aes(x = cl, y = y, label = vars, fill = type) +
  geom_bar(stat = "identity", color = "black") +
  coord_flip()+
  geom_text(size = 2, position = position_stack(vjust = 0.5))+
  scale_x_continuous(labels = 12:0, breaks = 12:0)+
  scale_fill_manual(values = c("gray", "white"))+
  theme_classic()+
  theme(legend.position = "bottom",
        legend.key.size = unit(3, "mm"),
        axis.line.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.x = element_blank())+
  labs(x = "Cluster", fill = "")
