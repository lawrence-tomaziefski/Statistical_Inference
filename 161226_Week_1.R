
##Normal Distributions
#Again, we can use R's qnorm function and simply specify the mean and standard deviation (the square root of the variance). Do this
# now. Find the 97.5th percentile of a normal distribution with mean 3 and standard deviation 2.

qnorm(.975, mean = 3, sd = 2)

#qnorm returns the number or the quantile

#Let's check it using the formula above, X = mu + sigma*Z. Here we'll use the 97.5th percentile for the standard normal as the value
# Z in the formula. Recall that we previously calculated this to be 1.96. Let's multiply this by the standard deviation of the given
#| normal distribution (2) and add in its mean (3) to see if we get a result close to the one qnorm gave us.

3 + (2*1.96)


# Suppose you have a normal distribution with mean 1020 and standard deviation of 50 and you want to compute the probability that the
# associated random variable X > 1200. The easiest way to do this is to use R's pnorm function in which you specify the quantile
# (1200), the mean (1020) and standard deviation (50). You also must specify that the lower.tail is FALSE since we're asking for a
# probability that the random variable is greater than our quantile. Do this now.

pnorm(1200, mean = 1020, sd = 50, lower.tail = FALSE)

#Alternatively, we could use the formula above to transform the given distribution to a standard normal. We compute the number of
#standard deviations the specified number (1200) is from the mean with Z = (X -mu)/sigma. This is our new quantile. We can then use
#the standard normal distribution and the default values of pnorm. Remember to specify that lower.tail is FALSE.  Do this now.

pnorm((1200-1020)/50, lower.tail = FALSE)



#find the 75% percentile
qnorm(.75, mean = 1020, sd = 50, lower.tail = TRUE)


#Poison Distributions

#For example, suppose the number of people that show up at a bus stop is Poisson with a mean of 2.5 per hour, and we want to know the
#| probability that at most 3 people show up in a 4 hour period. We use the R function ppois which returns a probability that the
#| random variable is less than or equal to 3. We only need to specify the quantile (3) and the mean (2.5 * 4). We can use the default
#parameters, lower.tail=TRUE and log.p=FALSE. Try it now.

ppois(3, lambda = (2.5 *4))


#To see this, use the R function pbinom to estimate the probability that you'll see at most 5 successes out of 1000 trials each of
#| which has probability .01. As before, you can use the default parameter values (lower.tail=TRUE and log.p=FALSE) and just specify
# the quantile, size, and probability.

pbinom(5, 1000, .01)

ppois(5, 1000*.01)


##### Asymptotics #####

6 + c(-1,1)*qnorm(.975)*sqrt(.6*.4/100)

lamb + c(-1,1)*qnorm(.975)*sqrt(lamb)



1100 + c(-1,1)*qt(.975, 8)* 30/sqrt(9)


mn= -2
s= sqrt(.6) - sqrt(.68)
n = 10

mn+ c(-1,1) *qt(.975, n-1) * s/sqrt(n)

#Example problems

#Load the data set mtcars in the datasets R package. Calculate a 95% confidence interval to
#the nearest MPG for the variable mpg.

data("mtcars")
t.test(mtcars$mpg, mu = mean(mtcars$mpg), conf.level = .95)
round(t.test(mtcars$mpg)$conf.int)


#Suppose that standard deviation of 9 paired differences is 1. 
#What value would the average difference have to be so that the 
#lower endpoint of a 95% students t confidence interval touches zero?

#Use t = Xbar +/- t_(.975,8) * s/sqrt(n)

s = 1 
n = 9 
qt(.975,8) * s/sqrt(n)

#find the standard deviation so that lower endpoint of a 
#95% students t confidence interval touches zero
xbar = -2 
n = 9 
(-xbar * sqrt(n))/qt(.975,8)


#Consider the mtcars dataset. 
#Construct a 95% T interval for MPG comparing 4 to 6 cylinder cars 
#(subtracting in the order of 4 - 6) assume a constant variance.

mpg4 = mtcars %>%
        filter(cyl == 4) %>%
        select(mpg)

mpg6 = mtcars %>%
        filter(cyl == 6) %>%
        select(mpg)

difference = mpg4$mpg[1:7] - mpg6$mpg[1:7]

t.test(mpg4, mpg6, var.equal = TRUE)

#pooled variance estimate

n_x = 9
n_y = 9 

s2_x = 1.5^2
s2_y = 1.8^2

pooled_var = ((n_x-1)*s2_x+(n_y-1)*s2_y)/(n_x+n_y-2)
pooled_var


n = 9
ss_new = 1.5^2
ss_old = 1.8^2
mean_new = -3
mean_old = 1
sp <- sqrt(((n-1) * ss_new + (n-1) * ss_old)/ (n + n-2))
md <- mean_new - mean_old
semd <- sp * sqrt(1/n + 1/n)

md + c(-1, 1) * qt(.95, n + n - 2) * semd



hist(rexp(1000, rate = .2))
mns = NULL
for (i in 1 : 1000) mns = c(mns, mean(rexp(40,rate = .2)))
mns = data.frame(mns = mns)

hist(mns)

mean(mns)

require(graphics)

##-log(dpois(0:7, lambda = 1) * gamma(1+ 0:7)) # == 1
Ni <- rexp(40, rate = .2); table(factor(Ni, 0:max(Ni)))



qqnorm(mns,main = "Normal Q-Q Plot",
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
       plot.it = TRUE, datax = FALSE)

qq = ggplot(mns) + stat_qq(aes(sample = mns), distribution = stats :: qnorm)
qq


tooth_data = data.frame(datasets::ToothGrowth)

mns = mutate(mns , variance = var(mns))

mns = NULL
variance = NULL
for (i in 1 : 1000){
mean = c(mns, mean(rexp(40,rate = .2)))        
variance = c(variance, var(rexp(40,rate = .2)))
}

exp = data.frame(means = mean, variance = variance)


simulation_output = data.frame()
set.seed(10)
for (i in 1 :1000){
        output = data.frame(output = rexp(40, rate = .2))
        output = summarize(output, means = mean(output), variance = var(output))
        simulation_output = rbind(simulation_output, output)
}

sample_stats = summarize(simulation_output, mean = mean(means), variance = mean(variance))
sample_stats

require(ggthemes)
hist(simulation_output$means)
hist(simulation_output$variance)

qq = ggplot(simulation_output) + 
        stat_qq(aes(sample = means, color = "Mean"), distribution = stats :: qnorm) +
        stat_qq(aes(sample = variance , color = "Variance"), distribution = stats :: qnorm) +
        scale_color_stata()
qq



data("mtcars")

test_data = mtcars %>%
        filter(cyl == 4 | cyl == 6) %>%
        select(1,2) 
        
t.test(mpg ~ cyl, paired = FALSE, alternative = "two.sided", var.equal = FALSE, data = test_data)


xbar = 1100
SE= 30/3
TS = c(qt(.025,8),qt(.975,8))
mu0 = round(-(SE * TS) + xbar,2)
mu0 


pbinom(54,100,prob = .5, lower.tail = FALSE)

ppois(15800-1, 520*30, lower.tail = FALSE)
pv <- ppois(15800 - 1, lambda = 520 * 30, lower.tail = FALSE)



#Test for power
alpha = .05
z = qnorm(1-alpha) #for a one sided test
mu0 = .01
mua = 0
sigma = .04
sample_n = 100
standard_error = sigma/sqrt(sample_n)

pnorm(mu0 + z * standard_error, mean = mua, sd = standard_error, lower.tail = FALSE)

#find sample size using power.t.test
desired_power = .9
mu0 = .01
mua = 0
delta = mu0 - mua 
sigma = .04

power.t.test(power = desired_power, delta = delta, sd = sigma, type = "one.sample", alternative = "one.sided")$n

power.t.test(n = 100, delta = delta, sd = sigma, type = "one.sample", alternative = "one.sided")$power


pbinom(2,4, prob = .5, lower.tail = FALSE)


baseline = c(140,138,150,148,135)
week2 = c(132,135,151,146,130)

t.test(baseline - week2)

pv <- ppois(.01, lambda = 1, lower.tail = FALSE)
pv

qt(.95, 9 + 9 - 2)

pt(1.74,8, lower.tail = TRUE)
