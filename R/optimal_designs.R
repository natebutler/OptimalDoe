#' Generate Optimal Designs by Coordinate Exchange
#'
#'
#'
#'
#' @export
generate_designs <- function(N, k, order = 1, criteria = "D",
                             iterations = 1000){

  check_inputs(N, k, order, criteria, iterations)
  designs <- list(iterations)

  if(criteria == "D"){
    for (i in seq_len(iterations)){
      designs[[i]] <- coordinate_D(N, k, order)
    }
  } else if(criteria == "A"){
    for (i in seq_len(iterations)){
      designs[[i]] <- coordinate_A(N, k, order)
    }
  }
  sorted_designs <- designs[order(sapply(designs, function(x) x[[2]]))]
  return(sorted_designs)
}

# Checking inputs of generate_designs function
check_inputs <- function(N, k, order, criteria, iterations){
  if(N %% 1 != 0 || k %% 1 != 0){
    stop("N and k must be integer values")
  }

  if(!(order %in% c(0,1,2))){
    stop("Order must be either 0, 1, or 2")
  }

  if(order == 0){
    if(N <= k + 1){
      stop("N must be greater than number of parameters to estimate")
    }
  }

  if(order == 1){
    if(N <= k + 1 + (k*(k-1))/2){
      stop("N must be greater than number of parameters to estimate")
    }
  }

  if(order == 2){
    if(N <= 2*k + 1 + (k*(k-1))/2){
      stop("N must be greater than number of parameters to estimate")
    }
  }

  if(!(criteria %in% c("D","A"))){
    stop("Criteria must be either 'D' or 'A'")
  }

  if(iterations %% 1 != 0 || iterations < 1){
    stop("Iterations must be an integer greater than 1")
  }
}
