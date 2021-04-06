BestHome2 <- read.csv("BestHomePart2.csv",
                      header = T, as.is = T)


# function: MIN abs (p*q -12900) + abs(p*q - 6000) + abs(p*q - 3900) + abs(p*q - 2500) + abs(p*q - 7000)
n <- length(BestHome2$ProductID)
cost <- BestHome2$Purchasing_cost
capacity <- BestHome2$Required_capacity
class <- BestHome2$Class
furniture_capacity <- 12900
decor_capacity <- 6000
bedding_and_bath_capacity <- 3900
rug_capacity <- 2500
appliances_capacity <- 7000

variation_func <- function(stock){
  for (i in 1: n){
    if(stock[i] < 0){
      return(Inf)}
  } ## non negativity constraint
  if(sum(stock*capacity) > 32300 | sum(stock*cost) > 2300000 ){
    
    
    ##  NEED CONSTRAINT sum((cost*stock)[which(class == "Funiture")]) > 920000 | 
    ##  sum((cost*stock)[which(class == "Funiture")]) <345000 ##
    
    
    return(Inf)
  } else { ## total inventory constraint
    variation <- c()
    
    for(i in 1:n){
      variation[i] <- capacity[i]*stock[i]
    }
    
    furniture_variation <- sum(variation[which(class == "Furniture")])
    decor_variation <- sum(variation[which(class == "Decor")])
    bedding_variation <- sum(variation[which(class == "Bedding and bath")])
    rug_variation <- sum(variation[which(class == "Rugs")])
    appliance_variation <- sum(variation[which(class == "Appliances")])
    
    objective <- abs(furniture_variation - furniture_capacity) + abs(decor_variation - decor_capacity)
      + abs(bedding_variation - bedding_and_bath_capacity)  + abs(rug_variation - rug_capacity) + abs(appliance_variation - appliances_capacity)
  }
  return(objective)
}


#testing functions
sample_stock <- rep(600, n)
variation_func(stock = sample_stock) # returns a number, and therefore will act as a feasible sol

sample_stock2 <- rep(-1,n)
variation_func(stock = sample_stock2) #returns Inf hence infeasible

sample_stock3 <- rep(100,n)
variation_func(stock = sample_stock3) #returns Inf hence infeasible

## Optimization
feasible_sol <- sample_stock
opt <- optim(par = feasible_sol, fn = variation_func)
opt$value
sum(opt$par)

opt2 <- optim(par = opt$par, fn = variation_func, control = list(maxit=50000), method = "SANN")
opt2$value
sum(opt2$par)
opt2$par






#Simulation

# B
nsim <- 1000
final_cost <- c()
set.seed(0)

f_mean <- mean(BestHome2$Purchasing_cost[which(class == "Furniture")])
d_mean <- mean(BestHome2$Purchasing_cost[which(class == "Decor")])
b_mean <- mean(BestHome2$Purchasing_cost[which(class == "Bedding and bath")])
r_mean <- mean(BestHome2$Purchasing_cost[which(class == "Rugs")])
a_mean <- mean(BestHome2$Purchasing_cost[which(class == "Appliances")])

for (i in 1:nsim) {
f_cost <- max(rnorm(1, mean = f_mean, sd = 100),0)
d_cost <- max(rnorm(1, mean = d_mean, sd = 100),0)
b_cost <- max(rnorm(1, mean = b_mean, sd = 100),0)
r_cost <- max(rnorm(1, mean = r_mean, sd = 100),0)
a_cost <- max(rnorm(1, mean = a_mean, sd = 100),0)


avg_cost <- mean(f_cost,d_cost,b_cost,r_cost,a_cost)

final_cost[i] <- avg_cost*sum(opt$par)
}
final_cost

# C
mean_final_cost <- mean(final_cost)

# D
sd_c <- sd(final_cost)

MSE <- sd_c/sqrt(nsim)

UB <- mean_final_cost + 1.96*MSE
LB <- mean_final_cost - 1.96*MSE

