#Drawing a power curve for the mean:
#For a group test being lower than the population test:
pop_mn <- 120; alpha <- 0.05; n <- 100; sigma <- 24; smp_mn <- pop_mn
power <- 0
means_list <- c(); power_list <- c()

while (power < 1){
  power <- pnorm(-qnorm(1 - alpha) + ((abs(pop_mn - smp_mn) * sqrt(n))/sigma))
  means_list <- c(means_list, smp_mn)
  power_list <- c(power_list, power)
  smp_mn <- smp_mn - 1
}
plot(means_list, power_list, type = "l", xlab = "Sample Mean", ylab = "Power")
abline(h = 0.8, col = "red")
abline(v = abs((((qnorm(0.8) + qnorm(1-alpha))*sigma)/sqrt(n))-pop_mn), 
       col = "blue")

#for a group test being larger than the population:
pop_mn <- 120; alpha <- 0.05; n <- 100; sigma <- 24; smp_mn <- pop_mn
power <- 0
means_list <- c(); power_list <- c()
while (power < 1){
  power <- pnorm(-qnorm(1 - alpha) + ((abs(pop_mn - smp_mn) * sqrt(n))/sigma))
  means_list <- c(means_list, smp_mn)
  power_list <- c(power_list, power)
  smp_mn <- smp_mn + 1
}
plot(means_list, power_list, type = "l", xlab = "Sample Mean", ylab = "Power")
abline(h = 0.8, col = "red")
abline(v = abs((((qnorm(0.8) + qnorm(1-alpha))*sigma)/sqrt(n))+pop_mn), 
      col = "blue")

##Calculating the sample size that gives the good power:
pop_mn <- 120; alpha <- 0.05; n <- 1; sigma <- 24; smp_mn <- 115
power <- 0
n_list <- c(); power_list <- c()
while (power < 1){
  power <- pnorm(-qnorm(1 - (alpha/2)) + ((abs(pop_mn - smp_mn) * sqrt(n))/sigma))
  n_list <- c(n_list, n)
  power_list <- c(power_list, power)
  n <- n + 1
}
plot(n_list, power_list, type = "l", xlab = "Sample Size", ylab = "Power")
abline(h = 0.95, col = "red")
abline(v = ((qnorm(0.95) + qnorm(1-(alpha/2))) ** 2) * (sigma**2)/(abs(pop_mn - smp_mn))**2,
       col = "blue")
