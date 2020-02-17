library(ggplot2)
library(dplyr)
library(carData)
library(RColorBrewer)
library(microbenchmark)
library(grid)
library(ggthemes)
library(ggplot2movies)
library(latex2exp)
#------------------------------------------------------

#Data sets
data("mtcars")
data("GSSvocab")
data("msleep")
data("iris")
data("diamonds")
movies
#------------------------------------------------------

#Practice
glimpse(movies)

movies <- movies %>% sample_n(., size=1000) %>% mutate(., rating=factor(round(rating)))

ggplot(movies, aes(x = rating, y = votes)) +
  geom_point() +
  stat_summary(fun.data = "mean_cl_normal", geom ="crossbar",
               width = .2, color = 'red') +
               scale_y_log10()

#scale_y_log10() and scale_x_log10() perform log10 transformations on our data


# Reproduce the plot
ggplot(diamonds, aes(x = carat, y = price, col = color)) +
  geom_point(alpha=.5, size=1,shape=16) +
  
  scale_x_log10(expression(log[10](Carat)), limits = c(0.1,10)) +
  
  scale_y_log10(expression(log[10](Price)), limits = c(100,100000)) +
  
  scale_color_brewer(palette = "YlOrRd") +
  
  coord_equal() 

print(expression(x[2]))
plotmath(a^2)


TeX("\\alpha")

#Specialized Plots - Well-Suited for Academics



#If you only have continuous variables, you can convert them into ordinal variables:

diamonds <- diamonds %>% sample_n(., size=1000)

ggplot(diamonds, aes(x=carat, y=price)) + geom_point(alpha=.3)

#So if you want to do multiple boxplots, you can split or "cut" the data 

#cut interval splits by range
ggplot(diamonds, aes(x=carat, y=price)) + geom_boxplot(aes(group=cut_interval(carat, n=20)))

#cut number splits by number of observations so that each cut has an approximately equal number of observations
ggplot(diamonds, aes(x=carat, y=price)) + geom_boxplot(aes(group=cut_number(carat, n=20)))


#cut width makes group with a given width 
ggplot(diamonds, aes(x=carat, y=price)) + geom_boxplot(aes(group=cut_width(carat, width=.5)))


#creates a normal distribution based on integer value
rnorm(10)

#IQR becomes more consistent as sample size increases


#DENSITY PLOTS - theoretical vs empirical (probability density function - pdf)

#Kernel Density Estimate - KDE
# A sum of 'bumps' placed at the observations.
















