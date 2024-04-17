mMatrix <- function(X, model) {
  dimX <- dim(X)
  oneVector <- c(1)
  for (i in 1:(length(X[,1]) - 1)) {
    oneVector <- c(oneVector, 1)
  }
  if (length(X[1,]) == 1) {
    if (model == 1) {
      modelMatrix = cbind(oneVector, X)
    }
    else if (model == 2) {
      modelMatrix = cbind(oneVector, X)
    }
    else if (model == 3) {
      modelMatrix = cbind(oneVector, X,  X^2)
    }
  }
  else {
    if (model == 1) {
      modelMatrix = cbind(oneVector, X)
    }
    else if (model == 2) {
      int <- matrix(1,nrow=dimX[1],ncol=choose(dimX[2],2),byrow=TRUE)
      h = 1
      c = 2
      for (i in 1:(dimX[2]-1)) {
        for (j in i:(dimX[2]-1)) {
          inter = X[,i]*X[,c]
          int[,h] = inter
          h = h + 1
          if (c < dimX[2]) {
            c = c + 1
          }
          else{
            c = dimX[2]
          }
        }
        i = i + 1
        c = c - c + i + 1
      }
      modelMatrix <- cbind(oneVector, X, int)
    }
    else if (model == 3) {
      int <- matrix(1,nrow=dimX[1],ncol=choose(dimX[2],2),byrow=TRUE)
      h = 1
      c = 2
      for (i in 1:(dimX[2]-1)) {
        for (j in i:(dimX[2]-1)) {
          inter = X[,i]*X[,c]
          int[,h] <- inter
          h = h + 1
          if (c < dimX[2]) {
            c = c + 1
          }
          else{
            c = dimX[2]
          }
        }
        i = i + 1
        c = c - c + i + 1
      }
      modelMatrix <- cbind(oneVector, X, int, X^2)
    }
  }
  return(modelMatrix)
}

RPV <- function(X) { 
  pv <-c()
  f <- mMatrix(X, 3)
  for (x in 1:10000) {
    X1_new <-runif(1, min = -1, max = 1)
    X2_new <-runif(1, min = -1, max = 1)
    X3_new <-runif(1, min = -1, max = 1)
    X_new <- cbind(X1_new, X2_new, X3_new)
    F_new <- mMatrix(X_new, 3)
    predVar <- F_new%*%(solve(t(f)%*%f))%*%t(F_new)
    pv <- cbind(pv, predVar)
  }
  return (pv)
}
X1As <- c(0.059889624,	1,	-1,
          0.059889624,	1,	-1,
          1,	1,	-0.081687919,
          1,	-1,	1,
          1,	-1,	1,
          1,	1,	-1,
          1,	-1,	-1,
          1,	-1,	-1,
          -1,	-1,	1,
          -1,	-1,	1,
          -0.050160736,	-1,	-0.074919811,
          -0.050160736,	-1,	-0.074919811,
          0.058643552,	0.209629595,	1,
          0.058643552,	0.209629595,	1,
          -1,	1,	-0.079487217,
          1,	-0.012338577,	0.017572639,
          -1,	1,	1,
          -1,	1,	1,
          -1,	1,	-1,
          -1,	0.104859182,	-1,
          1,	1,	1,
          1,	1,	1,
          -1,	-0.155258365,	1,
          1,	0.079503096,	-1,
          -1,	-1,	-1,
          -1,	-1,	-1)
X1 <- matrix(X1As, nrow=26, ncol=3, byrow=TRUE)
randPredVar1WB <- RPV(X1)
SrandPredVar1WB <- sort(randPredVar1WB)
X1VGT <- c(-1, -1, -1, -1, -1, -1, -1, -1, 1,
           -1, 1, -1, -1, 1, -1, -1, 1, 1, 1, -1, -1,
           1, -1, -1, 1, -1, 1, 1, -1, 1, 1, 1, -1,
           1, 1, -1, 1, 1, 1, 1, 1, 1, -1, -1, 0,
           -1, 1, 0, -1, 0, 1, -1, 0, 1, 0, -1, 1,
           0, 1, 1, 1, 0, 0, 1, 0, 0, 0, -1, 0,
           0, 1, 0, 0, 0, -1, 0, 0, -1)
X1GT <- matrix(X1VGT, nrow=26, ncol=3, byrow=TRUE)
randPredVar1GT <- RPV(X1GT)
SrandPredVar1GT <- sort(randPredVar1GT)
plot(x=c(1:10000)/10000, y=SrandPredVar1WB, main = "Design Scenario: second order model, K=3, N=26", xlab="Fraction of Design Space",
     ylab = "Relative Prediction Variance", type = 'l', col='blue', cex.main=.75)
lines(x=c(1:10000)/10000, y=SrandPredVar1GT,col="red")
legend('topleft',inset=0,c("Gilmour & Trinca (2012): K=3, N=26, As Optimal Design via CEXCH", "Walsh & Bolton (2022): K=3, N=26, As−Optimal Design via PSO"),lty=1,col=c("red","blue"), cex = .45)
