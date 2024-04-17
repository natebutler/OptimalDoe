#' Model Matrix
#'
#' @param design the design matrix
#' @param order order of the model you want.
#'   Enter 0 for a first order main effects model,
#'   1 for a first order model with 2 way interactions,
#'   2 for a second order model with 2 way interactions and squared main effects
#'
#' @import matrixcalc
#'
#' @returns a matrix of the desired order
#' @export
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

#' D-Criteria
D_crit <- function(X, order){
  f <- model_matrix(X, order)
  # if (det(t(f)%*%f) == 0){
  #   crit <- Inf
  # }
  # else{
  #   crit <- 1 / (det(t(f)%*%f))
  # }
  # return(crit)

  crit <- 1 / (det(t(f)%*%f))
  return(crit)
}

#' A-Criteria
A_crit <- function(X, order){
  f <- model_matrix(X, order)
  if (det(t(f)%*%f) == 0){
    crit <- Inf
  }
  else{
    crit <- sum(diag(solve(t(f)%*%f)))
  }
  return(crit)
}

singular_check <- function(X, order){
  f <- model_matrix(X, order)
  info <- t(f)%*%f
  check <- matrixcalc::is.singular.matrix(info)
  return(check)
}
