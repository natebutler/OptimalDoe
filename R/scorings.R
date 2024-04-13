#' Model Matrix
#'
#' @param design the design matrix
#' @param order order of the model you want
#'
#' @import matrixcalc
#'
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
  if (abs(det(t(f)%*%f)) < (.Machine$double.eps)){
    crit <- Inf
  }
  else{
    crit <- 1 / (det(t(f)%*%f))
  }

  return(crit)
}

#' A-Criteria
A_crit <- function(X){
  f <- MM2.11(X)
  if (det(t(f)%*%f) < (.Machine$double.eps)){
    crit <- Inf
  }
  else{
    crit <- sum(diag(solve(t(f)%*%f)))
  }
  return(crit)
}
