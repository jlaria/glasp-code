# Fig1
library(ggpubr)

# Regularization
Phi_WT_fixed = function(X, beta, T_mat, W, lambda1=0.2, lambda2=0.01, lambda3=0.1){
  K = ncol(W)
  p = ncol(X)
  N = nrow(X)

  lasso = sum(abs(beta))
  glasso = sum(sqrt((t(W!=0)%*%beta^2)*(apply(t(W!=0), 1, sum))))
  pca = norm(X%*%diag(beta) - T_mat%*%t(W), "F")

  return(lasso*lambda1 + lambda2*glasso + lambda3/2*pca)
}

set.seed(0)
# Generate X
N = 100
X = MASS::mvrnorm(N, mu = c(0, 0, 0),
                  Sigma = matrix(c(1, 0, 0.5, # ... not really important in this example
                                   0, 1, 0,
                                   0.5, 0, 1), nrow = 3, byrow = T))
X <- scale(X)
beta_optim = c(0.5, 0.25, 0.1)

M <- X%*%diag(beta_optim)

svd <- svd(M)
svd$d
T_mat <- svd$u[,1:2]
W <- svd$v[,1:2]%*%diag(svd$d[1:2])
W <- t(apply(W, 1, function(x){x*(abs(x)==max(abs(x)))}))
t(W)%*%W #diagonal
t(T_mat)%*%T_mat # identity

# Create the grid
betas = seq(-1, 1.5, length.out = 100)
grid = expand.grid(beta1 = betas,
                   beta2 = betas, KEEP.OUT.ATTRS = F)
for (i in 1:nrow(grid)) {
  beta = c(grid$beta1[i], grid$beta2[i], 0.1)
  grid$z[i] = Phi_WT_fixed(X, beta, T_mat, W, 2, 1, 1)
}

# plot
gg1 = ggplot(grid) +
  geom_contour(aes(x = beta1, y = beta2, z = z))+
  geom_point(aes(x=0.5, y=0.25))+
  geom_point(aes(x=0, y=0))+
  theme_classic2()+
  labs(x = expression(beta[1]),
       y = expression(beta[2]))+
  geom_hline(aes(yintercept=0), linetype = "dashed", size = 0.1)+
  geom_vline(aes(xintercept=0), linetype = "dashed", size = 0.1)

# Same for lasso
for (i in 1:nrow(grid)) {
  beta = c(grid$beta1[i], grid$beta2[i], 0.1)
  grid$z2[i] = Phi_WT_fixed(X, beta, T_mat, W, 2, 0, 0)
}

# plot
gg2 = ggplot(grid) +
  geom_contour(aes(x = beta1, y = beta2, z = z2))+
  geom_point(aes(x=0, y=0))+
  theme_classic2()+
  labs(x = expression(beta[1]),
       y = expression(beta[2]))+
  geom_hline(aes(yintercept=0), linetype = "dashed", size = 0.1)+
  geom_vline(aes(xintercept=0), linetype = "dashed", size = 0.1)

# Same for sparse_group_lasso
for (i in 1:nrow(grid)) {
  beta = c(grid$beta1[i], grid$beta2[i], 0.1)
  grid$z3[i] = Phi_WT_fixed(X, beta, T_mat, W, 2, 1, 0)
}

# plot
gg3 = ggplot(grid) +
  geom_contour(aes(x = beta1, y = beta2, z = z3))+
  geom_point(aes(x=0, y=0))+
  theme_classic2()+
  labs(x = expression(beta[1]),
       y = expression(beta[2]))+
  geom_hline(aes(yintercept=0), linetype = "dashed", size = 0.1)+
  geom_vline(aes(xintercept=0), linetype = "dashed", size = 0.1)

# Same for Phi
for (i in 1:nrow(grid)) {
  beta = c(grid$beta1[i], grid$beta2[i], 0.1)
  grid$z4[i] = Phi_WT_fixed(X, beta, T_mat, W, 0, 0, 1)
}

# plot
gg4 = ggplot(grid) +
  geom_contour(aes(x = beta1, y = beta2, z = z4))+
  geom_point(aes(x=0.5, y=0.25))+
  theme_classic2()+
  labs(x = expression(beta[1]),
       y = expression(beta[2]))+
  geom_hline(aes(yintercept=0), linetype = "dashed", size = 0.1)+
  geom_vline(aes(xintercept=0), linetype = "dashed", size = 0.1)

ggarrange(gg1, gg2, gg3, gg4, ncol = 4, nrow = 1,
          labels = c("a)", "b)", "c)", "d)"))


