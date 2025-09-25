# Functions to compute beta and theta parameters 

###----------------------------------------------
Design1ComputeBeta_IPW <- function(dat, n){
  n <- nrow(dat)
  dat$ID<-1:n
  
  dat$w=rep(0)
  
  dat$w[dat$r==1]<-2 
  dat$w[dat$r==0]<-4
  
  dat.R1 <- dat.R2 <- dat[dat$r==1,];
  dat.R1$a2 <- 1; dat.R2$a2 <- -1;
  
  rep.dat <- rbind(dat.R1, dat.R2, dat[dat$r==0,])
  
  rep.dat<-rep.dat[order(rep.dat$ID),]
  
  
  beta <- coef(geeglm(formula = y ~ a1+ a2, id=ID,
                      weights = w, data= rep.dat, family= gaussian,
                      corstr = "independence") )
  
  return(beta)
}


###----------------------------------------------

Design1ComputeTheta <- function(beta){
  #(1,1)
  e1 <- t(beta)%*%(c(1,1,1))
  
  #(1,-1)
  e2 <- t(beta)%*%(c(1,1,-1))
  
  #(-1,1)
  e3 <- t(beta)%*%(c(1,-1,1))
  
  #(-1,-1)
  e4 <- t(beta)%*%(c(1,-1,-1))
  
  # best txt regime: (1,1,-1)
  theta.hat <- cbind(e1,e2,e3,e4)
}