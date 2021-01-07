##First, we create a population from a normal distribution:
mean <- sample(1:1000, size = 1); sd <- sample(1:50, size = 1)
mean; sd
set.seed(647); pop <- rnorm(1e7, mean = mean, sd = sd)

#Then, we establish the number and size of the samples:
sp_size <- 10; sp_num <- 10

#A vector and df to storage the data:

tbl <- as.data.frame(matrix(ncol = 3)); colnames(tbl) <- c("Mean", "N. of Samples", "Difference")
tbl <- rbind(tbl, c(mean, "Population", (mean-mean))); tbl <- tbl[-1,]
#Sample taking:
sp_means <- c()
for (i in 1:sp_num){
        xbar <- mean(sample(pop, size = sp_size))
        sp_means <- c(sp_means, xbar)
}

X_bar <- mean(sp_means)
tbl <- rbind(tbl, c(X_bar, sp_num, (X_bar - mean)))

hist(sp_means, xlab = "Sample Means", main = "Sampling distribuition 50000 sample means") 
abline(v = mean, col = "blue", lwd = 2); abline(v = X_bar, col = "red", lwd = 2)

