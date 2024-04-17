RPV <- function(Design, k, order) {
  pv <- rep(0, 10000)
  f <- model_matrix(Design, order)
  for (i in seq_len(10000)) {
    X_samp <- matrix(runif(k, min = -1, max = 1), nrow = 1)
    F_samp <- model_matrix(X_samp, order)
    predVar <- F_samp%*%(solve(t(f)%*%f))%*%t(F_samp)
    pv[i] <- predVar
  }
  return (pv)
}

FDS_helper <- function(Des_list, N, k) {
  RPV_df <- data.frame()
  for (i in seq_len(length(Des_list))) {
    RPV_df[,i] <- RPV(Des_list[[i]])
    colnames(RPV_df)[i] <- paste0("Design", i)
  }
  return (RPV_df)
}


ju <- matrix(c(1,0,-1,0.2), nrow = 1)
ju
