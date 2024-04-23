# Calculate Relative Prediction Variance for a design matrix
rpv <- function(design, order) {
  pv <- rep(0, 10000)
  k <- ncol(design)
  f <- OptimalDoe::model_matrix(design, order)
  for (i in seq_len(10000)) {
    x_samp <- matrix(stats::runif(k, min = -1, max = 1), nrow = 1)
    f_samp <- OptimalDoe::model_matrix(x_samp, order)
    pred_var <- f_samp %*% (solve(t(f) %*% f)) %*% t(f_samp)
    pv[i] <- pred_var
  }
  return(pv)
}

# Calculate the RPV vectors for a list of design matrices
fds_helper <- function(des_list, order) {
  rpv_df <- matrix(nrow = 10000, ncol = length(des_list))
  for (i in seq_len(length(des_list))) {
    rpv_df[, i] <- sort(rpv(des_list[[i]], order))
  }
  rpv_df <- data.frame(rpv_df)
  colnames(rpv_df) <- paste0("Design", seq_along(des_list))
  return(rpv_df)
}

#' Fraction of Design Space Plots
#'
#' Generate Fraction of Design Space plots for the design matrices
#'
#' @param des_list a list of design matrices that you want to compare
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
#' @examples
#' # Generate Optimal Designs
#' optimal_designs <- generate_designs(N = 15, k = 3, order = 2, criteria = "A",
#'  iterations = 10)
#'
#' # Pull out the 4 lowest A-Scored designs
#' result <- lapply(head(optimal_designs, 4), `[[`, 1)
#'
#' # Plot those designs
#' fds_plot(result, order = 2)
#'
#' @export
fds_plot <- function(des_list, order) {
  if (length(des_list) > 4) {
    message <- paste0(
      "You are trying to plot more than 4 lines on one graph,",
      " there is a potential of overplotting"
    )
    warning(message)
  }

  data <- fds_helper(des_list, order)
  y_vars <- names(data)

  data$x <- c(1:10000) / 10000

  df_long <- tidyr::pivot_longer(
    data,
    cols = -all_of("x"),
    names_to = "line",
    values_to = "value"
  )



  p <- ggplot(df_long, aes(x = .data$x, y = .data$value, color = .data$line)) +
    geom_line(linewidth = 0.75) +
    labs(
      x = "Fraction of Design Space", y = "Relative Prediction Variance",
      title = "Relative Prediction Variance over Design Space"
    ) +
    scale_color_brewer(palette = "Set1", labels = y_vars, name = "Designs") +
    theme(legend.position = "right")

  # Return the plot
  return(p)
}
