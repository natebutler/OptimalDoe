# This function generates a model matrix of the given design
model_matrix <- function(design, order){
  dimX <- dim(design)
  order1 <- cbind(rep(1, dimX[1]), design)
  if (order == 0){
    return(order1)
  }

  if (order == 1 || order == 2){
    order1int <- order1
    for (col in seq_len(dim(design)[2]-1)){
      for (j in seq_len(dim(design)[2]-col)){
        interactions <- matrixcalc::hadamard.prod(design[,col],design[,col+j])
        order1int <- cbind(order1int, interactions)
      }
    }
    if(order == 1){
      return(order1int)
    }
    order2 <- order1int
    for (i in seq_len(dimX[2])){
      squares <- matrixcalc::hadamard.prod(design[,i],design[,i])
      order2 <- cbind(order2, squares)
    }
    return(order2)
  }

}

#' D-Criteria Score
#'
#' Score any design matrix with the D-Criteria
#'
#' @param design the design matrix you want to score
#' @param order order of the model you want.
#'   Enter 0 for a first order main effects model,
#'   1 for a first order model with 2 way interactions,
#'   2 for a second order model with 2 way interactions and squared main effects
#' @import matrixcalc
#'
#' @returns the score of the D-Criteria
#'
#' @export
D_crit <- function(design, order){
  f <- model_matrix(design, order)
  singular <- singular_check(design, order)
  if (singular == TRUE){
    crit <- Inf
  }
  else{
    crit <- 1 / (det(t(f)%*%f))
  }
  return(crit)
}

#' A-Criteria Score
#'
#' Score any design matrix with the A-Criteria
#'
#' @param design the design matrix you want to score
#' @param order order of the model you want.
#'   Enter 0 for a first order main effects model,
#'   1 for a first order model with 2 way interactions,
#'   2 for a second order model with 2 way interactions and squared main effects
#' @import matrixcalc
#'
#' @returns the score of the A-Criteria
#'
#' @export
A_crit <- function(design, order){
  f <- model_matrix(design, order)
  singular <- singular_check(design, order)
  if (singular == TRUE){
    crit <- Inf
  }
  else{
    crit <- sum(diag(solve(t(f)%*%f)))
  }
  return(crit)
}

# this function checks if the design matrix is a singular matrix
#' @export
singular_check <- function(X, order){
  f <- model_matrix(X, order)
  info <- t(f)%*%f
  check <- matrixcalc::is.singular.matrix(info, .Machine$double.eps)
  return(check)
}
