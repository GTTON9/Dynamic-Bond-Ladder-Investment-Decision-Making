# Avery KF Test
library(MTS)
source("syntheticDataFuncs.R")

############################################### Tony Prep

# Fix get_C

KF_likelihood <-function(A, B, C, D, Q, R, last_Sig, y_t, cur_x){
  
  F_t <- C %*% (A %*% last_Sig %*% t(A) + B %*% Q %*% t(B)) %*% t(C) + D %*% R %*% t(D)
  # print(last_Sig[1,1])
  
  while(kappa(F_t) ^ -1 > 10 ^ -2){
    F_t = F_t + 10 ^ -2
    print(kappa(F_t))
  }
  
  e_t <- as.matrix(y_t - C %*% cur_x)
  # print(e_t[1])
  # print(cat("a",as.numeric(t(e_t) %*% ginv(F_t, 1e-4) %*% e_t)))
  log_likelihood_t <- -0.5 * (log(det(F_t)) + t(e_t) %*% solve(F_t) %*% e_t )
  
  return(log_likelihood_t)
} 


#get_partial_R(last_x, last_Sig, cur_x, cur_Sig, last_partial_x_R, last_partial_Sig_R, F_t, e_t, A, B, C, D, Q, R)
partial_G_approx <- function(A, B, C, D, Q, R, last_Sig, y_t, cur_x, h=1e-3){
  log_likelihood_0 <- KF_likelihood(A, B, C, D, Q, R, last_Sig, y_t, cur_x)
  grad_A <- matrix(0, nrow=nrow(A), ncol=ncol(A))
  
  for (i in 1:nrow(A)) {
    for (j in 1:ncol(A)) {
      # Create perturbed matrices
      A_plus <- A
      A_minus <- A

      A_plus[i, j] <- A_plus[i, j] + h
      A_minus[i, j] <- A_minus[i, j] - h

      log_likelihood_plus <- KF_likelihood(A_plus, B, C, D, Q, R, last_Sig, y_t, cur_x)
      log_likelihood_minus <- KF_likelihood(A_minus, B, C, D, Q, R, last_Sig, y_t, cur_x)

      grad_A[i, j] <- (log_likelihood_plus - log_likelihood_minus) / (2 * h)
    }
  }
  
  
  return(list(grad_A = grad_A, log_likelihood_0 = log_likelihood_0))
}

partial_W_approx <- function(A, B, C, D, Q, R, last_Sig, y_t, cur_x, h=1e-5){
  log_likelihood_0 <- KF_likelihood(A, B, C, D, Q, R, last_Sig, y_t, cur_x)
  
  grad_Q <- matrix(0, nrow=nrow(Q), ncol=ncol(Q))
  
  for (i in 1:nrow(Q)) {
    for (j in 1:ncol(Q)) {
      # Create perturbed matrices
      Q_plus <- Q
      Q_minus <- Q
      
      Q_plus[i, j] <- Q_plus[i, j] + h
      Q_minus[i, j] <- Q_minus[i, j] - h
      
      log_likelihood_plus <- KF_likelihood(A, B, C, D, Q_plus, R, last_Sig, y_t, cur_x)
      log_likelihood_minus <- KF_likelihood(A, B, C, D, Q_minus, R, last_Sig, y_t, cur_x)
      
      grad_Q[i, j] <- (log_likelihood_plus - log_likelihood_minus) / (2 * h)
    }
  }
  return(grad_Q)
}

partial_U_approx <- function(A, B, C, D, Q, R, last_Sig, y_t, cur_x, h=1e-5){
  log_likelihood_0 <- KF_likelihood(A, B, C, D, Q, R, last_Sig, y_t, cur_x)
  
  grad_R <- matrix(0, nrow=nrow(R), ncol=ncol(R))
  
  for (i in 1:nrow(R)) {
    for (j in 1:ncol(R)) {
      # Create perturbed matrices
      R_plus <- R
      R_minus <- R
      
      R_plus[i, j] <- R_plus[i, j] + h
      R_minus[i, j] <- R_minus[i, j] - h
      
      log_likelihood_plus <- KF_likelihood(A, B, C, D, Q, R_plus, last_Sig, y_t, cur_x)
      log_likelihood_minus <- KF_likelihood(A, B, C, D, Q, R_minus, last_Sig, y_t, cur_x)
      
      # print(c(log_likelihood_plus,log_likelihood_minus))
      
      grad_R[i, j] <- (log_likelihood_plus - log_likelihood_minus) / (2 * h)
    }
  }
  return(grad_R)
}


myFunc <- function(yieldMat,tenors,lam){
  
  C <- NS(tenors,lam)
  B = diag(rep(1,3))


  n = ncol(yieldMat)
  tenorLen = nrow(yieldMat)
  D = diag(rep(1,tenorLen))
  
  U = diag(rep(1,nrow(yieldMat))) # will be updated
  
  # For derivatives.
  
  partial_log_l_G <- diag(3)
  partial_log_l_W <- diag(3)
  partial_log_l_U <- diag(tenorLen)
  
  # Help with initialization.
  
  myVAR = VARMA(betaMat,p = 1)
  G = myVAR$Phi # will be updated
  W = myVAR$Sigma # will be updated
  
  
  
  while(TRUE){
    m_KF=matrix(NA,nrow = 3,ncol = n) 
    a_KF=matrix(NA,nrow = 3,ncol = n)  
    f_KF=matrix(NA,nrow = tenorLen,ncol = n)
    
    C_KF=matrix(NA,ncol = 3 * n,nrow = 3)
    
    R_KF = matrix(NA,ncol = 3 * n,nrow = 3)
    Q_KF = matrix(NA,ncol = tenorLen * n,nrow = tenorLen)
    
    a_KF[,1]= c(0,0,0)
    f_KF[,1]= C %*% a_KF[,1]
    R_KF[1:3,1:3] <- diag(rep(1,3))
    
    Q_KF[1:tenorLen,1:tenorLen] = C %*% R_KF[1:3,1:3] %*% t(C) + U
    Q_KF_inv=solve(Q_KF[1:tenorLen,1:tenorLen])    ##1d ##solve(Q_KF[1])
    m_KF[,1]=a_KF[,1]+ R_KF[1:3,1:3] %*% t(C) %*% Q_KF_inv %*% (yieldMat[,1]-f_KF[,1])
    C_KF[1:3,1:3]= R_KF[1:3,1:3] - R_KF[1:3,1:3] %*% t(C) %*% Q_KF_inv %*% C %*% R_KF[1:3,1:3]
    

    for(i in 2:n){
      a_KF[,i] = G %*% m_KF[,i-1]
      
      f_KF[,i] = C %*% a_KF[,i]
      
      
      R_KF[1:3,(1 + 3 * (i-2)):(3 * (i-1))] = G %*% C_KF[,(1 + 3 * (i-2)):(3 * 
                               (i-1))] %*% t(G) + W
      

      Q_KF[(1:tenorLen),(1 + tenorLen * (i-2)):(tenorLen * (i-1))] = 
        C %*% R_KF[1:3,(1 + 3 * (i-2)):(3 * 
                               (i-1))] %*% t(C) + U
      
      Q_KF_inv = solve(Q_KF[(1:tenorLen),
                           (1 + tenorLen * (i-2)):(tenorLen * (i-1))])
      
      m_KF[,i]= a_KF[,i]+ R_KF[1:3,(1 + 3 * (i-2)):(3 * (i-1))] %*% 
        t(C) %*% Q_KF_inv %*% (yieldMat[,i] - f_KF[,i])
      
      
      
      C_KF[1:3,(1 + 3 * (i-1)):(3 * i)] = 
        R_KF[1:3,(1 + 3 * (i-2)):(3 * (i-1))] - 
        R_KF[1:3,(1 + 3 * (i-2)):(3 * (i-1))] %*% 
        t(C) %*% Q_KF_inv %*% C %*% R_KF[1:3,(1 + 3 * (i-2)):(3 * (i-1))]

      
      last_Sig = C_KF[1:3,(1 + 3 * (i-2)):(3 * (i-1))]
      cur_x = a_KF[,i]

      partial_log_G_t <- partial_G_approx(G, B, C, D, W, U, last_Sig = last_Sig, y_t = yieldMat[,i], cur_x = cur_x, h=1e-5)
      partial_log_U_t <- partial_U_approx(G, B, C, D, W, U, last_Sig = last_Sig, y_t = yieldMat[,i], cur_x = cur_x, h=1e-5)
      partial_log_W_t <- partial_W_approx(G, B, C, D, W, U, last_Sig = last_Sig, y_t = yieldMat[,i], cur_x = cur_x, h=1e-5)
      # add the current time partial_log_likelihood to the summation of partial_log_likelihood over time 1:T_
      partial_log_l_G <- partial_log_l_G + partial_log_G_t$grad_A
      partial_log_l_U <- partial_log_l_U + partial_log_U_t
      partial_log_l_W <- partial_log_l_W + partial_log_W_t
      
    }
    lastG = G
    lastW = W
    lastU = U
    
    alpha <- 10 ^ -5
    # update the 
    
    G <- G + alpha * partial_log_l_G
    W <- W + alpha * partial_log_l_W
    
    U <- U + alpha * partial_log_l_U
    
    
    # Compute the convergece condition by the ratio of difference of paramters, using Euclidean norm
    # num <- norm(A - lastA, type = "F") + norm(Q - lastQ, type = "F") + norm(R - lastR, type = "F") # 
    # denom <- norm(A, type = "F") + norm(Q, type = "F") + norm(R, type = "F")
    # ratio <- num/denom
    ratio_G <- norm(G - lastG, type = "F")/norm(G, type = "F")
    ratio_W <- norm(W - lastW, type = "F")/norm(W, type = "F")
    ratio_U <- norm(U - lastU, type = "F")/norm(U, type = "F")
    ratio <- max(c(ratio_G, ratio_W, ratio_U))

    if(ratio < 0.01){
      break
    }
    
    # parameter update for the next iteration
  }
  return(m_KF) 
}


###############################################



# smoother

# s_KF=matrix(NA,nrow = n,ncol = 2) 
# S_KF=list() 
# 
# s_KF[n,] = m_KF[n]
# S_KF[[n]] = C_KF[[n]]
# 
# for(i in (n-1):1){
#   R_KF_inv = solve(R_KF[[i+1]])
#   s_KF[i,] = t(t(m_KF[i,])) + C_KF[[i]] %*% t(G) %*% R_KF_inv %*% t(t(s_KF[i+1,] - a_KF[i+1,]))
#   S_KF[[i]] = C_KF[[i]] + C_KF[[i]] %*% t(G) %*% R_KF_inv %*% (S_KF[[i+1]] - R_KF[[i+1]]) %*%
#     R_KF_inv %*% G * C_KF[[i]]
# }
# 
# sampleVar = c()
# for(i in 1:100){
#   sampleVar[i] = sum(diag(C_KF[[i]]) ^ 2)


n = 100
set.seed(20)
betaMat <- getBetas()
yieldMat <- getYields(betaMat)

lam = 0.6915
tenors <- c(1/12,3/12,6/12,1,2,3,5,7,10,20)

y = t(yieldMat) # |R| / |Q|


estMat <- myFunc(y,tenors,lam = lam)
C <- NS(tenors,lam)

v = 50
plot(tenors,y[,ncol(y) - v])
lines(tenors,C %*% t(betaMat)[,ncol(t(betaMat)) - v])
lines(tenors,C %*% estMat[,ncol(estMat) - v], col = 'red')



# Extra: KF_Estimate_approx(data, 30, tenors = c(1/12,3/12,6/12,1,2,3,5,7,10,20), lambda = 0.5)