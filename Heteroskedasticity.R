# Clearing my workspace
rm(list = ls())

# for creating errors on either sides
# to generate heteroskedasticity problem
m = rep(1,50)
i =0
for(i in 1:50){
  if((i%%2) == 0){ m[i] = -1}
}

# Synthesizing the data
x = 1:50
y = 2*x + exp(m*x/10)
# error term exponentially raising

# Plots
plot(x,y)
abline(lm(y~x))

plot(log(x),log(y))
abline(lm(log(y)~log(x)))

# Summaries of the models
summary(lm(y~x))
summary(lm(log(y)~log(x)))
