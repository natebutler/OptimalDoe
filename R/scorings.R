# This function generates a model matrix of the given design
model_matrix <- function(design, order) {
  dim_x <- dim(design)
  order1 <- cbind(rep(1, dim_x[1]), design)
  if (order == 0) {
    return(order1)
  }

  if (order == 1 || order == 2) {
    order1int <- order1
    for (col in seq_len(dim(design)[2] - 1)) {
      for (j in seq_len(dim(design)[2] - col)) {
        interactions <- matrixcalc::hadamard.prod(design[, col],
                                                  design[, col + j])
        order1int <- cbind(order1int, interactions)
      }
    }
    if (order == 1) {
      return(order1int)
    }
    order2 <- order1int
    for (i in seq_len(dim_x[2])) {
      squares <- matrixcalc::hadamard.prod(design[, i], design[, i])
      order2 <- cbind(order2, squares)
    }
    return(order2)
  }
}

#' D-Criteria Score
#'
#' This function takes in a design matrix and a desired model as input. From the
#' inputs, the design matrix is expanded into the model matrix and then the
#' information matrix is computed. From that, inverse of the information matrix
#' is computed and the determinant of the inverse of the information matrix is
#' calculated.
#'
#' @param design the design matrix you want to score
#' @param order order of the model you want.
#'   Enter 0 for a first order main effects model,
#'   1 for a first order model with 2 way interactions,
#'   2 for a second order model with 2 way interactions and squared main effects
#' @import matrixcalc
#'
#' @returns This function returns the determinant of the inverse of the
#' information matrix.
#'
#' @export
d_crit <- function(design, order) {
  f <- model_matrix(design, order)
  singular <- singular_check(design, order)
  if (singular == TRUE) {
    crit <- Inf
  } else {
    crit <- 1 / (det(t(f) %*% f))
  }
  return(crit)
}

#' A-Criteria Score
#'
#' This function takes in a design matrix and a desired model as input. From the
#' inputs, the design matrix is expanded into the model matrix and then the
#' information matrix is computed. From that, inverse of the information matrix
#' is computed and the trace of the inverse of the information matrix is
#' calculated.
#'
#' @param design the design matrix you want to score
#' @param order order of the model you want.
#'   Enter 0 for a first order main effects model,
#'   1 for a first order model with 2 way interactions,
#'   2 for a second order model with 2 way interactions and squared main effects
#' @import matrixcalc
#'
#' @returns This function returns the trace of the inverse of the information
#' matrix.
#'
#' @export
a_crit <- function(design, order) {
  f <- model_matrix(design, order)
  singular <- singular_check(design, order)
  if (singular == TRUE) {
    crit <- Inf
  } else {
    crit <- sum(diag(solve(t(f) %*% f)))
  }
  return(crit)
}
# this function checks if the design matrix is a singular matrix
#' Check Singularity of an information matrix from a design matrix.
#'
#' This function takes in a design matrix and a model desired to be fit. From
#' this, we expand the design out into the model matrix and then compute the
#' information matrix. This function then checks if the matrix is invertible.
#'
#' @param x A design matrix with n rows and k columns.
#' @param order Model you want to fit. 0 fits a first order model. 1 fits a
#' first order model with two-way interactions. 2 fits a second order model.
#'
#' @returns This function returns a TRUE if the information matrix created from
#'   the design matrix is singular, FALSE if not.
#' @export
singular_check <- function(x, order) {
  f <- model_matrix(x, order)
  info <- t(f) %*% f
  check <- matrixcalc::is.singular.matrix(info, .Machine$double.eps)
  return(check)
}
