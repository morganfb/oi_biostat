#########################################################
# Solutions to Lab2_Probability 'On Your Own' exercises #
#########################################################

# Question 1
# Describe the distribution of streak lengths. What is the typical streak length for this
# simulated independent shooter with a 45\% shooting percentage? How long is the player's
# longest streak of baskets in 133 shots?

# Solution 1
sim_streak <- calc_streak(sim_basket)
barplot(table(sim_streak))
# This distribution is not symmetric or bell-shaped. Typically, the independent shooter
# has a streak of length 0. This player's longest streak is 4 baskets in a row.

# Question 2
# If you were to run the simulation of the independent shooter a second time, how would
# you expect its streak distribution to compare to the distribution from the question
# above? Exactly the same? Somewhat similar? Totally different? Explain your reasoning.

# Solution 2
# The distribution would be somewhat similar in shape. The probabilities did not change
# so while the streaks might vary more or less than the first sample, the distribution
# will still have a typical streak of 0. 

# Question 3
# How does Kobe Bryant's distribution of streak lengths from page~\pageref{kobeStreak}
# compare to the distribution of streak lengths for the simulated shooter? Using this
# comparison, do you have evidence that the hot hand model fits Kobe's shooting patterns?
# Explain.

# Solution 3
barplot(table(kobe_streak))
table(kobe_streak)
table(sim_streak)
# The distribution of Kobe's streaks is very similar to that of the independent model.
# Kobe has more streaks of 1. There is not enough evidence that Kobe fits the hot hand
# model. Kobe's distribution is similar to if you were to run the independent shooter 
# simulation again, suggesting his shots are in fact independent of one another.


