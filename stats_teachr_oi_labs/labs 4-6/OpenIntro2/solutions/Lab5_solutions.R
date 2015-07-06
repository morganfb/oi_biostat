#################################################################
# Solutions to Lab5_Inference_numerical 'On Your Own' exercises #
#################################################################

# Question 1
# Calculate a 95\% confidence interval for the average length of pregnancies (\hlstd{weeks}) 
# and interpret it in context. Note that since you're doing inference on a single population
# parameter, there is no explanatory variable, so you can omit the \hlkwc{x} variable from 
# the function.

# Solution 1
inference(y = nc$weeks, est = "mean", type = "ci", method = "theoretical")
# [38.1528 , 38.5165]

# Question 2
# Calculate a new confidence interval for the same parameter at the 90\% confidence level. 
# You can change the confidence level by adding a new argument to the function: 
# \hlkwc{conflevel =}\hlnum{0.90}.

# Solution 2
inference(y = nc$weeks, est = "mean", type = "ci", method = "theoretical", conflevel = 0.90)
# [38.182 , 38.4873]

# Question 3
# Conduct a hypothesis test evaluating whether the average weight gained by younger mothers 
# is different than the average weight gained by mature mothers.

# Solution 3
inference(y = nc$gained, x = nc$mature, est = "mean", type = "ht", null = 0, 
             alternative = "twosided", method = "theoretical")
# The data suggests that there is no significant difference between younger and mature mothers
# average weight gain during pregnancy.

# Question 4
# Now, a non-inference task: Determine the age cutoff for younger and mature mothers. 
# Use a method of your choice, and explain how your method works.

# Solution 4
range(nc$mage[nc$mature == "younger mom"])
range(nc$mage[nc$mature == "mature mom"])
# It is clear from these ranges that the cutoff for a mature mom is >= 35 years old.

# Question 5
# Pick a pair of numerical and categorical variables and come up with a research question 
# evaluating the relationship between these variables. Formulate the question in a way that
# it can be answered using a hypothesis test and/or a confidence interval. Answer your 
# question using the \hlkwd{inference} function, report the statistical results, and 
# also provide an explanation in plain language.

# Solution 5
# Example question: Is the average weight of babies born with married parents different than
# the average weight of babies with parents whom are not married?
inference(y = nc$weight, x = nc$marital, est = "mean", type = "ht", null = 0, 
          alternative = "twosided", method = "theoretical")
# Since the p-value is virtually 0, the data suggests that the average weight of babies born
# to married parents is different than that of babies born to parents whom are not married.
# In fact, there is evidence that babies born to married parents have a lower birth weight,
# on average. 

