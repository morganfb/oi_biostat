#####################################################################
# Solutions to Lab4A_Sampling_Distributions 'On Your Own' exercises #
#####################################################################

# Question 1
# Take a random sample of size 50 from \hlstd{young\$wtdesire}. Using this sample, what
# is your best point estimate of the population mean?

# Solution 1
samp1 <- sample(young$wtdesire, 50)
mean(samp1)

# Question 2
# Since you have access to the population, simulate the sampling distribution for 
# $\bar{x}_{young\$wtdesire}$ by taking 5000 samples from the population of size 50 and 
# computing 5000 sample means.  Store these means in a vector called 
# \hlstd{sample\_means50}. Plot the data, then describe the shape of this sampling 
# distribution. Based on this sampling distribution, what would you guess the mean 
# desired weight of this population is? Finally, calculate and report the population mean.

# Solution 2
# Simulate Sampling Distribution
for(i in 1:5000){
  samp <- sample(young$wtdesire, 50)
  sample_means50[i] <- mean(samp)
}
# Plot the data
hist(sample_means50)
# The sampling distribution is bell shaped and symmetrical around the a mean of about 155
# and appears to be normal.
mean(sample_means50) # mean based on sampling dist
mean(young$wtdesire) # mean based on population

# Question 3
# Change your sample size from 50 to 300, then compute the sampling distribution using
# the same method as above, and store these means in a new vector called 
# \hlstd{sample\_means300}. Describe the shape of this sampling distribution, and compare
# it to the sampling distribution for a sample size of 50.  Based on this sampling 
# distribution, what would you guess to be the mean desired weight of individuals in this
# population?

# Solution 3
# For loop for Sampling distribution
sample_means300 <- rep(0, 5000)
for(i in 1:5000){
  samp <- sample(young$wtdesire, 300)
  sample_means300[i] <- mean(samp)
}
# Plot the sampling distribution
hist(sample_means300)
# This distribution also appears to be normally distributed. It is centered a little
# under 155. Slightly more symmetrical than the previous sampling distribution
mean(sample_means300)

# Question 4
# Of the sampling distributions from 2 and 3, which has a smaller spread?  If we're 
# concerned with making estimates that are more often close to the true value, would we
# prefer a distribution with a large or small spread?

# Solution 4
sd(sample_means50)
sd(sample_means300)
var(sample_means50)
var(sample_means300)
# the sampling distribution with a larger sample size (size 300) has a smaller spread.
# if we are concerned with estimates that are more often close to the true value, we
# would prefer a distribution with a smaller spread.

# Question 5
# Create a new variable \hlstd{wdiff} that calculates the difference between actual 
# weight and desired weight (weight - wtdesire). Repeat questions 2 and 3 for this new 
# variable to estimate the mean difference in weight and desired weight in the young 
# adult population. What can you conclude about how this population feels about their
# weight (on average)?

# Solution 5
# new variable
wtdiff <- young$weight - young$wtdesire
# question 2:
for(i in 1:5000){
  samp <- sample(wtdiff, 50)
  sample_means50[i] <- mean(samp)
}
hist(sample_means50)
mean(sample_means50)
# question 3:
for(i in 1:5000){
  samp <- sample(wtdiff, 300)
  sample_means300[i] <- mean(samp)
}
hist(sample_means300)
mean(sample_means300)
# Young adults living in the U.S. in 2002 want to lose about nine and a half pounds, on
# average. The two distributions are similar in that they appear to be normal and centered
# around approximately 9.5.