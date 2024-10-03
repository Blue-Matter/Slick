# Generate dummy values
nsim <- 10
nOM <- 2
nMP <- 4
nPI <- 3

values <- array(NA, dim=c(nsim, nOM, nMP, nPI))
pi_means <- runif(nPI, 5, 50)
for (om in 1:nOM) {
  for (mp in 1:nMP) {
    for (pi in 1:nPI) {
      values[,om, mp, pi] <- rlnorm(nsim, log(pi_means[pi]), 0.4)
    }
  }
}

# Create and populate Object
boxplot <- Boxplot(Code=c('PI1', 'PI2', 'PI3'),
                   Label=c('Performance Indicator 1',
                           'Performance Indicator 2',
                           'Performance Indicator 3'),
                   Description = c('This is the description for PI 1',
                                   'This is the description for PI 2',
                                   'This is the description for PI 3'),
                   Value=values)

# Check
Check(boxplot)

# Add to `Slick` object
slick <- Slick()
Boxplot(slick) <- boxplot

# Plots
plotBoxplot(slick)

plotBoxplot(slick, type='violin')

plotBoxplot(slick, type='both')
