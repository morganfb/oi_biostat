###################################################################
# Solutions to Lab6_Inference_Categorical 'On Your Own' exercises #
###################################################################

# Question 1(a)
# Is there convincing evidence that the proportion of smokers who had babies with low birth 
# weights differs from the proportion of non-smokers who had babies with low birth weights?

# Solution 1(a)
inference(y = nc$lowbirthweight, x= nc$habit, est = "proportion", type = "ht",null = 0, 
          alternative = "twosided", method = "theoretical", success = "low")
# The data suggests that there proportions of low birth weights for smokers and non-smokers
# are equal.

# Question 1(b)
# Is there convincing evidence that the proportion of married mothers with babies with low 
# birth weights differs from that of single mothers?

# Solution 1(b)
inference(y = nc$lowbirthweight, x= nc$marital, est = "proportion", type = "ht",null = 0, 
          alternative = "twosided", method = "theoretical", success = "low")
# The data suggests that there is a significant difference in proportions. In fact, there
# is evidence that the proportion of married mothers with low birth weight babies is higher
# than that of single mothers.

# Question 2
# Suppose you're hired by the local government to estimate the proportion of mothers that
# visit the the doctor's office at least 12 times before they give birth. According to the 
# guidelines, the estimate must have a margin of error no greater than 1\% with 95\% 
# confidence. You have no idea what to expect for $p$. How many people would you have 
# to sample to ensure that you are within the guidelines?

# Solution 2
# Since ME = 2*sqrt(p*(1 - p)/n), and ME can be at most .01, referring to the plot p would
# need to be .03 or less. n (sample size) can be calculated as follows:
#        .01 = 2*sqrt(.03*(1 - .03)/n)
#        ((.01/2)^2)/(.03*(1-.03)) = 1/n
#        (.03*(1-.03))/((.01/2)^2) = n
(.03*(1-.03))/((.01/2)^2)
# n would have to be at least 1164.

