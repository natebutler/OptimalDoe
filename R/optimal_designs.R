#' Generate Optimal Designs by Coordinate Exchange
#'
#' This function generates optimal designs using the coordinate exchange
#'   algorithm. It initializes a matrix of desired size based on trials and
#'   factors and then makes single entry swaps on a -1 to 1 scale.
#'   The designs will be scored by the given criteria and the algorithm will
#'   stop once it has found the minimum score. It will then repeat that process
#'   a specified number of times to try and find the global optimization.
#'
#' @param n number of trials in your experiment
#' @param k the number of factors in your experiment
#' @param order order of the model you want.
#'   Enter 0 for a first order main effects model,
#'   1 for a first order model with 2 way interactions,
#'   2 for a second order model with 2 way interactions and squared main effects
#' @param criteria the scoring criteria used to find the optimal design. Enter
#'   either 'A' or 'D' to score on A-criteria or D-criteria
#' @param iterations the number of iterations you want the coordinate exchange
#'   algorithm to run. It is suggested that you run this algorithm 1000 times to
#'   best find the global optimization.
#'
#' @returns a list which is the length of iterations of
#'   outputs that tells you the optimized design and score
#'
#' @import matrixcalc
#'
#' @export
generate_designs <- function(n, k, order = 1, criteria = "D",
                             iterations = 1000) {
  check_inputs(n, k, order, criteria, iterations)
  designs <- list(iterations)

  if (criteria == "D") {
    for (i in seq_len(iterations)) {
      designs[[i]] <- coordinate_D(n, k, order)
    }
  } else if (criteria == "A") {
    for (i in seq_len(iterations)) {
      designs[[i]] <- coordinate_A(n, k, order)
    }
  }
  sorted_designs <- designs[order(sapply(designs, function(x) x[[2]]))]
  return(sorted_designs)
}

# Checking inputs of generate_designs function
check_inputs <- function(n, k, order, criteria, iterations) {
  if (n %% 1 != 0 || k %% 1 != 0) {
    stop("N and k must be integer values")
  }

  if (!(order %in% c(0, 1, 2))) {
    stop("Order must be either 0, 1, or 2")
  }

  if (order == 0) {
    if (n <= k + 1) {
      stop("N must be greater than number of parameters to estimate")
    }
  }

  if (order == 1) {
    if (n <= k + 1 + (k * (k - 1)) / 2) {
      stop("N must be greater than number of parameters to estimate")
    }
  }

  if (order == 2) {
    if (n <= 2 * k + 1 + (k * (k - 1)) / 2) {
      stop("N must be greater than number of parameters to estimate")
    }
  }

  if (!(criteria %in% c("D", "A"))) {
    stop("Criteria must be either 'D' or 'A'")
  }

  if (iterations %% 1 != 0 || iterations < 1) {
    stop("Iterations must be an integer greater than 1")
  }
}
