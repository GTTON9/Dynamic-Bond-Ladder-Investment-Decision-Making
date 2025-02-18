# Avery KF Test
library(MTS)
source("syntheticDataFuncs.R")

n = 1000
set.seed(20)
betaMat <- getBetas()
yieldMat <- getYields(betaMat)

lam = 0.6915
tenors <- c(1/12,3/12,6/12,1,2,3,5,7,10,20)
C <- NS(tenors,lam)


y = yieldMat
m_KF=matrix(NA,nrow = n,ncol = 3) 
a_KF=matrix(NA,nrow = n,ncol = 3)  
f_KF=list() 
C_KF=list()
R_KF=list() 
Q_KF=list() 

myVAR = VARMA(betaMat,p = 1)
G = myVAR$Phi
W = myVAR$Sigma

#marginal distribution for first one


time_record_KF_in_R=system.time(
  for(ii in 1:1){
    
    a_KF[1,]= c(0,0,0)
    R_KF[[1]]= diag(rep(1,3))
    f_KF[[1]]= C %*% t(t(a_KF[1,]))
    Q_KF[[1]]= round(C %*% R_KF[[1]] %*% t(C) + <theCovMat>,4)
    Q_KF_inv=solve(Q_KF[[1]])    ##1d ##solve(Q_KF[1])
    m_KF[1,]=t(a_KF[1,]+R_KF[[1]] %*% t(featVectors[[1]]) %*% Q_KF_inv %*% (y[[1]]-f_KF[[1]]))
    C_KF[[1]]=R_KF[[1]]-R_KF[[1]] %*% t(featVectors[[1]]) %*% Q_KF_inv %*% C %*% t(R_KF[[1]])
    
    
    
    for(i in 2:n){
      a_KF[i,]=G %*% m_KF[i-1,]
      R_KF[[i]]=G %*% C_KF[[i-1]] %*% t(G) + W
      
      f_KF[[i]]=C %*% a_KF[i,]
      Q_KF[[i]]=round(C %*% R_KF[[i]] %*% t(C) + obsCovList[[i]],4)
      
      Q_KF_inv= solve(Q_KF[[i]])
      m_KF[i,]=a_KF[i,]+ R_KF[[i]] %*% t(C) %*% Q_KF_inv %*% (y[[i]]-f_KF[[i]])
      C_KF[[i]]=R_KF[[i]]-R_KF[[i]] %*% t(C) %*% Q_KF_inv %*% C %*% R_KF[[i]]
      
    }
    
  }
)


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
}