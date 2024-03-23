set.seed(50)

#parameters
T <- 500   #time steps
N <- 1000  #realizations
mu <- 0.2
p0 <- r0 <- 15
epsilon_0 <- 0

#matrices to store the simulations
pt_simulations <- matrix(0, nrow = N, ncol = T)
rt_simulations <- matrix(0, nrow = N, ncol = T)

for (i in 1:N) {
  pt <- rep(0, T)
  rt <- rep(0, T)
  pt[1] <- p0
  rt[1] <- r0
  
  
  epsilon2 <- rep(0, T)

  eta <- rnorm(T, mean = 0, sd = 1)

  epsilon1 <- rnorm(T, mean = 0, sd = 2)


  epsilon1[1] <- epsilon_0
  epsilon2[1] <- epsilon_0

  for (t in 2:T) {
    #update epsilon for process 2

    
    epsilon2[t] <- -epsilon[t-1] + eta[t]


    
    #process 1
    pt[t] <- mu + pt[t-1] + epsilon1[t] + epsilon1[t-1]
    
    #process 2
    rt[t] <- rt[t-1] + epsilon2[t]
  }
  
  #store the simulation
  pt_simulations[i, ] <- pt
  rt_simulations[i, ] <- rt
}

#first 10 steps of the first simulation of pt and rt)
pt_simulations_example <- pt_simulations[1, 1:10]
rt_simulations_example <- rt_simulations[1, 1:10]

print(pt_simulations_example)
print(rt_simulations_example)




#process pt
plot(pt_simulations[1, ], type = 'l', col = 'blue', xlab = 'Time', ylab = 'Value', main = 'Process pt')
# Adding a legend
legend("topright", legend=c("pt"), col=c("blue"), lty=1, cex=0.8)

#process rt
plot(rt_simulations[1, ], type = 'l', col = 'red', xlab = 'Time', ylab = 'Value', main = 'Process rt')

legend("topright", legend=c("rt"), col=c("red"), lty=1, cex=0.8)



#plot the first time series to set up the plot
#plot(ts(rws[, 1]), ylim = c(min(rws, na.rm = TRUE), max(rws, na.rm = TRUE)), ylab = 'Log Returns', xlab = 'Time', type = 'l')

plot(ts(pt_simulations[1, ]), ylim = c(min(pt_simulations, na.rm = TRUE), max(pt_simulations, na.rm = TRUE)), type = 'l', col = 'blue', xlab = 'Time', ylab = 'Value', main = 'Process pt')



#add the other time series
for(j in 2:nrow(pt_simulations)) {
  lines(ts(pt_simulations[j, ]), col = colors()[j])
}

#legend("topright", legend = names(pt_simulations), col = colors()[2:(nrow(pt_simulations) + 1)], lty = 1)



plot(ts(rt_simulations[1, ]), ylim = c(min(rt_simulations, na.rm = TRUE), max(rt_simulations, na.rm = TRUE)), type = 'l', col = 'blue', xlab = 'Time', ylab = 'Value', main = 'Process rt')



#add the other time series
for(j in 2:nrow(rt_simulations)) {
  lines(ts(rt_simulations[j, ]), col = colors()[j])
}

#legend("topright", legend = names(rt_simulations), col = colors()[2:(nrow(rt_simulations) + 1)], lty = 1)




#parameters
mu <- 0.2
p0 <- r0 <- 15

#E(p200) and E(r400)
E_p200 <- p0 + 200 * mu
E_r400 <- r0  #since the expected change in r_t is 0 at each step

cat("E(p200) =", E_p200, "\n")
cat("E(r400) =", E_r400, "\n")




#check empirically (with the generated matrices)


mean(pt_simulations[,200])
mean(rt_simulations[,400])