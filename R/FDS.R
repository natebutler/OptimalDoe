# Calculate Relative Prediction Variance for a design matrix
RPV <- function(Design, order) {
  pv <- rep(0, 10000)
  k <- ncol(Design)
  f <- model_matrix(Design, order)
  for (i in seq_len(10000)) {
    X_samp <- matrix(stats::runif(k, min = -1, max = 1), nrow = 1)
    F_samp <- model_matrix(X_samp, order)
    predVar <- F_samp%*%(solve(t(f)%*%f))%*%t(F_samp)
    pv[i] <- predVar
  }
  return (pv)
}

# Calculate the RPV vectors for a list of design matrices
FDS_helper <- function(Des_list, order) {
  RPV_df <- matrix(nrow = 10000, ncol = length(Des_list))
  for (i in seq_len(length(Des_list))) {
    RPV_df[,i] <- sort(RPV(Des_list[[i]], order))
  }
  RPV_df <- data.frame(RPV_df)
  colnames(RPV_df) <- paste0("Design", seq_along(Des_list))
  return (RPV_df)
}

#' Fraction of Design Space Plots
#'
#' Generate Fraction of Design Space plots for the design matrices
#'
#' @param Des_list a list of design matrices that you want to compare
#' @param order order of the model you want.
#'   Enter 0 for a first order main effects model,
#'   1 for a first order model with 2 way interactions,
#'   2 for a second order model with 2 way interactions and squared main effects
#'
#' @import ggplot2
#' @import tidyr
#' @import stats
#'
#' @returns a plot of relative prediction variances over the design space
#'
#' @export
FDS_plot <- function(Des_list, order) {
  if(length(Des_list) > 4){
    message <- paste0("You are trying to plot more than 4 lines on one graph,",
                      " there is a potential of overplotting")
    warning(message)
  }

  data <- FDS_helper(Des_list, order)
  y_vars <- names(data)

  data$x <- c(1:10000)/10000

  df_long <- tidyr::gather(data, key = "line", value = "value", -x)

  p <- ggplot(df_long, aes(x = x, y = value, color = line)) +
    geom_line(linewidth = 0.75) +
    labs(x= "Fraction of Design Space", y = "Relative Prediction Variance",
                title= "Relative Prediction Variance over Design Space") +
    scale_color_brewer(palette = "Set1", labels = y_vars, name = "Designs") +
    theme(legend.position = "right")

  # Return the plot
  return(p)
}
