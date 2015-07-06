################################################################
# Solutions to Lab4B_Confidence_Levels 'On Your Own' exercises #
################################################################

# Question 1
# Using the package \hlkwd{plotrix}, you will be able to plot the 50 confidence intervals
# you just created and stored. First, install the package and then run the code below.
# How do your confidence levels look? Do approximately 95\% of them contain the true 
# population mean?

# Solution 2
require(plotrix)
mean50 <- rep(mean(population),50) # Vector of 'true' mean repeated 50 times
plotCI(1:50, mean50, ui=upper_vector, li=lower_vector)
# Approximately 95% of the intervals contain the true mean. There are about 2 intervals
# that are really close to not containing the true mean.

# Question 2
# Pick a confidence level of your choosing, provided it is not 95\%. What is the 
# appropriate critical value?

# Solution 2
Example repsonse: # A 90% confidence interval has a z critical value of:
qnorm(.05,lower.tail=FALSE)

# Question 3
# Calculate 50 confidence intervals at the confidence level you chose in the previous 
# question. You do not need to obtain new samples, simply calculate new intervals based
# on the sample means and standard deviations you have already collected. Using the 
# \hlkwd{plotCI} function, plot all intervals and calculate the proportion of intervals
# that include the true population mean. How does this percentage compare to the 
# confidence level selected for the intervals?

# Solution 3
lower_vector <- samp_mean - 1.64 * samp_sd / sqrt(n) 
upper_vector <- samp_mean + 1.64 * samp_sd / sqrt(n)
plotCI(1:50, mean50, ui=upper_vector, li=lower_vector)
# There are about 5 intervals that don't contain the true mean which implies that about 
# 90% do. This is equal to the .90 confidence level chosen.