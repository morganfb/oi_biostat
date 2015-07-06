#########################################################
# Solutions to Lab3_Normal_Dist 'On Your Own' exercises #
#########################################################

# Question 1
# Subset the data into two groups: Individuals with 1 or more blood vessels with 
# $>50$\% diameter narrowing, and individuals with 0. Look at the mean cholestrol 
# levels for each group. Is there a difference? Which group on average has higher 
# cholestrol? Does this suprise you?

# Solution 1
morethan1 <- subset(heart, heart$num >= 1)
none <- subset(heart, heart$num == 0)
mean(morethan1$chol)
mean(none$chol)
# People with 1 or more blood vessels with $>50$\% diameter narrowing have a higher
# cholestrol levels which is not suprising.

# Question 2
# What is the range of \emph{num}? What is the maxmum number of blood vessels with
# $>50$\% diameter narrowing? What is the mean cholestrol level for this group? 
# Compare to the groups above.

# Solution 2
range(heart$num)
most <- subset(heart,heart$num == 4)
mean(most$chol)
# The mean cholestrol level for people with 4 or more blood vessels with $>50$\%
# diameter narrowing is higher than the other two groups.

# Question 3
# You now should have three subsets of the data based on the variable \emph{num}.
# Using the methods you learned above, determine which (if any) are approximately
# normally distributed. 

# Solution 3
hist(none$chol, breaks = 15)
hist(morethan1$chol, breaks = 15)
hist(most$chol)
qqnorm(none$chol)
qqline(none$chol)
qqnorm(morethan1$chol)
qqline(morethan1$chol)
qqnorm(most$chol)
qqline(most$chol)
qqnormSim(none$chol)
qqnormSim(morethan1$chol)
qqnormSim(most$chol)
# It appears that the distribution of cholestrol for people with 1 or more blood
# blood vessels or with the maximum 4 blood vessels with $>50\%$ diameter narrowing
# are normally distributed. The distribution of people with 0 blood vessles with 
# narrowing look almost normal, but should be considered with caution since there is
# skewness outside of the simulation bands around the upper quantiles.

# Question 4
# What happens to the histograms and normal probability plots as the sample size 
# gets smaller? Is it easier or more difficult to judge if the data is normally 
# distributed with a smaller sample size or a larger one?

# Solution 4
# As the sample size gets smaller, it becomes more difficult to judge if data is 
# normal. A larger sample size is preferable.

# Question 5
# What is the probability that a randomly chosen adult with more than 2 blood 
# vessels with $>50$\% diameter narrowing has a cholestrol of above 300? 
# What about for an adult with 4 narrowing blood vessels?

# Solution 5
1 - pnorm(q=300, mean = mean(morethan2$chol), sd = sd(morethan2$chol))
1 - pnorm(q=300, mean = mean(most$chol), sd = sd(most$chol))

# Question 6
# Find the above probabilites empirically. Which probabilites are in closer 
# agreement? What does this tell you?

# Solution 6
sum(morethan2$chol > 300) / length(morethan2$chol)
sum(most$chol > 300) / length(most$chol)
# The probabilities for people 4 blood vessels narrowing are closer in agreement
# which tells me that this distribution is more normally distributed than the other. 


