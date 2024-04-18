RPV <- function(Design, order) {
  pv <- rep(0, 10000)
  k <- ncol(Design)
  f <- model_matrix(Design, order)
  for (i in seq_len(10000)) {
    X_samp <- matrix(runif(k, min = -1, max = 1), nrow = 1)
    F_samp <- model_matrix(X_samp, order)
    predVar <- F_samp%*%(solve(t(f)%*%f))%*%t(F_samp)
    pv[i] <- predVar
  }
  return (pv)
}

FDS_helper <- function(Des_list, order) {
  RPV_df <- matrix(nrow = 10000, ncol = length(Des_list))
  for (i in seq_len(length(Des_list))) {
    RPV_df[,i] <- sort(RPV(Des_list[[i]], order))
  }
  RPV_df <- data.frame(RPV_df)
  colnames(RPV_df) <- paste0("Design", seq_along(Des_list))
  return (RPV_df)
}


# des_list_check <- lapply(head(sorted, 5), `[[`, 1)

FDS_plot <- function(data) {
  y_vars <- names(data)

  # Create a ggplot object
  p <- ggplot(data, aes(x=c(1:10000)/10000))

  # Add a geom_line for each y variable
  colors <- rainbow(length(y_vars))
  for (i in seq_along(y_vars)) {
    p <- p + geom_line(aes(y = !!sym(y_vars[i])), color = colors[i])
  }

  # Return the plot
  return(p)
}
