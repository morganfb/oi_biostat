#######################################################
# Solutions to Lab7_Intro_SLR 'On Your Own' exercises #
#######################################################

# Question 1
# Produce a scatterplot of \hlkwd{CO} and \hlkwd{nicotene} and fit a linear model.  At a 
# glance, does there seem to be a linear relationship?

# Solution 1
plot(cigs$CO ~ cigs$nicotene)
# At a glance, there does appear to be a strong positive linear relationship with 1 large
# outlier.

# Question 2
# How does this relationship compare to the relationship between \hlkwd{CO} and \hlkwd{tar}?
# Use the R$^2$ values from the two model summaries to compare.  Does \hlkwd{nicotene} seem
# to predict \hlkwd{CO} better than \hlkwd{tar}?  How can you tell? 

# Solution 2
mNicotene <- lm(CO ~ nicotene, data=cigs)
summary(mNicotene)
mTar <- lm(CO ~ tar, data=cigs)
summary(mTar)
# Tar appears to better predict CO content. First off, the R^2 value is larger for mTar, 
# implying that the model with tar as the predictor explains more of the variability.
# In addition, although this was not discussed in this lab, the intercept for the mNicotene
# model does not appear to be significant, while the intercept in mTar is significant.

# Question 3
# Which variable best predicts \hlkwd{CO} out of the three in this data set? Support your
# conclusion using the graphical and numerical methods we've discussed.

# Solution 3
# example graphics for model with tar as predictor of CO content (best fit)
summary(mTar)
plot(cigs$CO ~ cigs$tar)
abline(mTar)

# Question 4
# Check the model diagnostics for the regression model with the variable you decided was the
# best predictor for CO content.

# Solution 4
# Linearity and constant variability
plot(mTar$residuals ~ cigs$tar)
abline(h=0,lty=3)
# Nearly normal residuals
hist(mTar$residuals)
qqnorm(mTar$residuals)
qqline(mTar$residuals)

