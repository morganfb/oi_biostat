########################################################
# Solutions to Lab1_Intro_Data 'On Your Own' exercises #
########################################################

# Question 1
# Make a scatterplot of the number of labs and the number of medications 
# someone is on for the first 100 patients. Describe the relationship between 
# these two variables.

# Solution 1
plot(diab$labs[1:100]~diab$meds[1:100])
# There appears to be a positive relationship between the number of medications
# someone is on and then number of labs someone has recieved. On average, the
# more medications someone is on, the more labs they have undergone.

# Question 2
# Create a new categorical variable that splits the patient population into two 
# groups, patients who are on 16 or less medications, and those who are on more 
# than 16 medications. Create a mosaic plot of these two groups as a function 
# of gender. Describe the plot. Are you surpised by the results? Why or why not.
diab$meds16[diab$meds > 16] <- "more than 16"
diab$meds16[diab$meds <= 16] <- "16 or less"
mosaicplot(table(diab$gender,diab$meds16))
# The result is not suprising. There are slightly more individuals (both male and
# female) who take 16 or less medications. If you look at a boxplot, this is evident
# in the population as a whole. Large outliers pull the mean towards more medications
# which explains the way the mosaic plot looks since the mean number of meds is
# approximately 16. If the number of medications taken was more symmetrical around
# the mean, the mosaic plot would also be more symmetrical.

# Question 3
# Are African Americans more represented in the 16 or less medications population 
# or in the more than 16 medications population? Create a visual or figure that
# best supports your answer.
mosaicplot(table(diab$race,diab$meds16))
# African Americans are represented in the 16 or less medications population more

# Question 4
# Create a subset of the data African American females. Describe the shape and 
# center of the distribution of the number of labs done in this population. 
# Compare this distribution to African American males. Is there any difference? 
# Can you compare these two distributions easily with side-by-side boxplots? Why
# or why not.
females <- subset(diab,diab$gender == "Female")
males <- subset(diab,diab$gender == "Male")
hist(females$labs)
hist(males$labs)
# The two distributions seem to be centered between 40 and 60 labs and are 
# practically bell shaped. Both have a relatively large frequency of 0-10 labs 
# which makes the distributions less symmetrical. While you can make seperate
# boxplots for the two distributions, you can not compare them side-by-side easily
# because there are more females than males. If you try and run the code:
# boxplot(females$labs ~ males$labs)
# you will get an error that the variable lengths differ.
