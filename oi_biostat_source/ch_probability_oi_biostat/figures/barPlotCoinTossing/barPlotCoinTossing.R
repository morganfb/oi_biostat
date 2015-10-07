# bar graph of distribution of number heads, 3 coin tosses

x.values = c(0, 1, 2, 3)
x.probs = c(1/8, 3/8, 3/8, 1/8)

x.dist.table = as.table(cbind(x.values, x.probs))
x.dist.table
barplot(x.probs, names.arg = c("0", "1", "2", "3"), xlab = "Values of X", ylab = "Probabilities")