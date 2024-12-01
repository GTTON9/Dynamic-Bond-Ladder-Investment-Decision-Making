CovReg <- setRefClass(
  "CovReg",
  fields = list(
    X1 = "matrix",
    X2 = "matrix",
    Y = "matrix",
    meanBasis = "matrix",
    n = "numeric",
    p = "numeric",
    q1 = "numeric",
    q2 = "numeric",
    guess = "logical",
    A = "matrix",
    B = "matrix",
    Psi = "matrix",
    invPsi = "matrix",
    gamma = "matrix",
    EX = 'matrix'
  ),
  
  methods = list(
    initialize = function(X1, X2, Y, meanBasis = matrix()) {
      # Initialize fields
      .self$X1 <- X1
      .self$X2 <- X2
      .self$Y <- Y
      .self$n <- ncol(Y)
      .self$p <- nrow(Y)
      .self$q1 <- nrow(X1)
      .self$q2 <- nrow(X2)
      .self$guess <- FALSE
      
      if (sum(is.na(meanBasis)) > 0) {
        .self$meanBasis <- diag(rep(1, .self$q1))
      } else {
        .self$meanBasis <- meanBasis
      }
      

      .self$EX <- .self$meanBasis %*% .self$X1
      .self$A <- matrix(nrow = .self$q1,ncol = .self$n)
      .self$B <- matrix(nrow = .self$p,ncol = .self$q2)
      .self$Psi <- matrix(nrow = .self$p,ncol = .self$p)
      .self$invPsi <- matrix(nrow = .self$p, ncol = .self$p)
      .self$gamma <- matrix(nrow = .self$n,ncol = 2)
    },
    
    # Get functions
    getA = function() {
      return(.self$A)
    },
    
    getB = function() {
      return(.self$B)
    },
    
    getPsi = function() {
      return(.self$Psi)
    },
    
    getGamma = function() {
      return(.self$gamma)
    },
    
    isGuessActive = function() {
      return(.self$guess)
    },
    
    # Set functions
    lsetA = function(newA) {
      .self$A <- newA
    },
    
    lsetB = function(newB) {
      .self$B <- newB
    },
    
    lsetPsi = function(newPsi) {
      .self$Psi <- newPsi
    },
    
    lsetInvPsi = function() {
      .self$invPsi <- .self$lfindInvPsi()
    },
    
    lsetGamma = function(newGamma) {
      .self$gamma <- newGamma
    },
    
    lsetBool = function(newBool) {
      .self$guess <- newBool
    },
    
    lsetGuess = function(A = matrix(), B = matrix(), Psi = matrix()) {
      if (sum(is.na(A)) == 0 && sum(is.na(B)) == 0 && sum(is.na(Psi)) == 0) {
        .self$lsetA(A)
        .self$lsetB(B)
        .self$lsetPsi(Psi)
      } else {
        .self$lsetA(matrix(1, nrow = .self$p, ncol = .self$q1))
        .self$lsetB(matrix(1, nrow = .self$p, ncol = .self$q2))
        .self$lsetPsi(diag(rep(1, .self$p)))
      }
      
      .self$lsetBool(TRUE)
    },
    
    # Find functions
    lfindInvPsi = function() {
      if (length(.self$Psi) != 0) {
        return(solve(.self$Psi))
      } else {
        stop("Psi undefined.")
      }
    },
    
    lfindGamma = function() {
      if (.self$isGuessActive()) {
        gamMat <- matrix(NA, nrow = .self$n, ncol = 2)
        
        for (a in 1:.self$n) {
          
          symComp <- t(.self$X2[,a]) %*% t(.self$B)
          mainComp <- symComp %*% .self$invPsi
          v <- (1 + mainComp %*% t(symComp))^(-1)
          m <- v * t(.self$Y[,a] - .self$A %*% .self$EX[,a]) %*% t(mainComp)
          
          gamMat[a, ] <- c(m, v)
        }
        
        .self$lsetGamma(gamMat)
      } else {
        stop("Please make a guess first.")
      }
    },
    
    lfindNextGuess = function() {
      s <- sqrt(.self$gamma[,2])
      X_Tilde_1 <- cbind(t(.self$X1), t(.self$gamma[,1] * .self$X2))
      X_Tilde_2 <- cbind(matrix(0, nrow = .self$n, ncol = nrow(.self$X1)), t(s * .self$X2))
      
      X_Tilde <- rbind(X_Tilde_1, X_Tilde_2)
      Y_Tilde <- rbind(t(.self$Y), matrix(0, nrow = .self$n, ncol = .self$p))
      
      invComp <- solve(t(X_Tilde) %*% X_Tilde)
      
      C <- t(Y_Tilde) %*% X_Tilde %*% invComp
      

      resComp <- Y_Tilde - X_Tilde %*% t(C)
      newPsi <- (1 / .self$n) * t(resComp) %*% resComp
      newA <- C[,1:.self$q1]
      newB <- C[,(.self$q1+1):ncol(C)]
      
      if ((ncol(newA) + ncol(newB)) != (.self$q1 + .self$q2)) {
        stop("I wrote the notation wrong.")
      }
      
      .self$lsetPsi(newPsi)
      .self$lsetA(newA)
      .self$lsetB(newB)
    },
    
    # EM algorithm for finding optimal parameters
    findParameters = function(A_Guess = matrix(), B_Guess = matrix(), Psi_Guess = matrix(), tol = 0.05) {
      .self$lsetGuess(A_Guess, B_Guess, Psi_Guess)
      
      init <- TRUE
      currTol <- 1
      count <- 0
      
      while (init || currTol > tol) {
        
        print(currTol)
        if (!init) {
          currA <- .self$getA()
          currB <- .self$getB()
          currPsi <- .self$getPsi()
        }
        
        .self$lsetInvPsi()
        .self$lfindGamma()
        .self$lfindNextGuess()
        
        if (!init) {
          nextA <- .self$getA()
          nextB <- .self$getB()
          nextPsi <- .self$getPsi()
        }
        
        if (!init) {
          A_Norm <- norm(currA, "F")
          B_Norm <- norm(currB, "F")
          
          if (A_Norm == 0) stop("A matrix guess not adjusting from zero.")
          if (B_Norm == 0) stop("B matrix guess not adjusting from zero.")
          
          diffList <- c(norm(nextA - currA, "F") / A_Norm,
                        norm(nextB - currB, "F") / B_Norm,
                        norm(nextPsi - currPsi, "F") / norm(currPsi, "F"))
          currTol <- max(diffList)
        } else {
          init <- FALSE
        }
        count <- count + 1
      }
      
      cat("Convergence successfully achieved after", count, "iterations.\n")
    }
  )
)

# Example of usage:
# Create some dummy data to test the class
set.seed(10)

X1 <- matrix(runif(50), nrow = 5, ncol = 10)
X2 <- matrix(runif(30), nrow = 3, ncol = 10)
Y <- matrix(runif(20), nrow = 2, ncol = 10)

# Create instance and run method
model <- CovReg$new(X1 = X1, X2 = X2, Y = Y)
model$findParameters(tol = 0.01)
