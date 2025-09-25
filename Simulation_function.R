#Simulation code from 
#https://github.com/wilart/SMART-Sizer-Paper/blob/master/R/Simulation/design-1/1-design-1-simulation.R

# Simulation design 1: Simulate 1000 datasets
delta = 0.1
#Set number of datasets
n_sim = 1000

n=400

###----------------------------------------------
Design1Simulate <- function(n, delta, n_sim){
  # Source: ADHD simulation code from Prof. Ashkan Ertefaie
  # Simulates n_sim design 1 SMART trials with n individuals with delta difference between best and non-best (confirm)
  #
  # Args: 
  #  n: Number of individuals in each SMART study simulation
  #  delta: Effect size
  #  n_sim: Number of simulations
  #
  # Returns: 
  # A list of n_sim SMART trial simulations each consisting of a data frame of the simulated variables, and outcomes
  ADHD.sim.list <- vector("list", length = n_sim)
  for (i in 1:n_sim){
    o11 <- rnorm(n,0,1)
    
    a1 <- 2*rbinom(n,1,0.5)-1 
    
    o21<-rnorm(n,0.5*o11 + 0.5*I(a1==-1),1)
    
    r<- (o21<0) 
    
    s<-rep(0,n)
    s[r==0]<-1 #Indicator of non-response
    
    a2<-2*rbinom(n,1,0.5)-1
    
    
    #####################
    y1 <- (1+o11+o21+a1*(delta+o11) +s*(a2*delta/2 ))
    
    y <- y1 + rnorm(n,0,1)
    
    ADHD.sim.list[[i]]<-data.frame(o11,a1,o21,r,s,a2,y)
  }
  ADHD.sim.list
}