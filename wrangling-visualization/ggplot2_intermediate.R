library(ggplot2)
library(dplyr)
library(carData)
library(RColorBrewer)
#library(Hmisc)
library(microbenchmark)
library(grid)
library(ggthemes)
#------------------------------------------------------

#Data sets
data("mtcars")
data("GSSvocab")
data("msleep")
data("iris")

#------------------------------------------------------
#Using grouping
ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE) +
  stat_smooth(aes(group=1),method = "lm", se = FALSE)


#
ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE) +
  # Change method and add span below
  stat_smooth(method = "loess",span=.7, aes(group = 1),
              se = FALSE, col = "black")

#--------------------------------------------------------------
#Default span = .9 for LOESS
#Lower span means risk of overfit
#higher span means risk of underfit
ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE) +
  stat_smooth(method = "loess",aes(group = 1, col = "All"),
              se = FALSE,  span = .3)
                                #****

ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE) +
  stat_smooth(method = "loess",aes(group = 1, col = "All"),
              se = FALSE,  span = 20)
                                 #****
#--------------------------------------------------------------
myColors <- c(brewer.pal(3, "Dark2"), "black")
danny_colors <- c(brewer.pal(4, "Set3"))
ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE, span = 0.7) +
  stat_smooth(method = "loess", 
              aes(group = 1, col="All"), 
              se = FALSE, span = 0.7) +
  scale_color_manual("Cylinders",values=danny_colors)

#Qualitative Palette Options:
# Accent	8
# Dark2	8
# Paired	12
# Pastel1	9
# Pastel2	8
# Set1	9
# Set2	8
# Set3	12

#--------------------------------------------------------------


glimpse(GSSvocab)

years <- c(1980, 1984, 1988,1990, 1994, 1998, 2000, 2004, 2008, 2010)
 
GSSvocab <- GSSvocab %>% mutate(., year=as.numeric(as.character(year))) %>% 
            filter(., year %in% years)

count(GSSvocab, vars=year)


#Both effectively the same:
#geom_smooth - for smoothing standard geoms
#stat_smooth - for smoothing non-standard geoms

# Plot 1: Jittered scatter plot, add a linear model (lm) smooth
ggplot(GSSvocab, aes(x = educ, y = vocab)) +
  geom_jitter(alpha = 0.2) +
  stat_smooth(method="lm", se=FALSE) # smooth


ggplot(GSSvocab, aes(x = educ, y = vocab, color=year)) +
  geom_jitter(alpha = 0.2) +
  stat_smooth(method="lm", se=FALSE) # smooth


# microbenchmark(ggplot(GSSvocab, aes(x = educ, y = vocab, color=year)) +
#                  geom_jitter(alpha = 0.2) +
#                  stat_smooth(method="lm", se=FALSE), times = 15)


#Group by year
ggplot(GSSvocab, aes(x = educ, y = vocab, color=factor(year))) +
  geom_jitter(alpha = 0.2) +
  stat_smooth(method="lm", se=FALSE) # smooth


#Get rid of the dots
ggplot(GSSvocab, aes(x = educ, y = vocab, color=factor(year))) +
 # geom_jitter(alpha = 0.2) +
  stat_smooth(method="lm", se=FALSE) # smooth



ggplot(GSSvocab, aes(x = educ, y = vocab, color=factor(year))) +
  #geom_jitter(alpha = 0.2) +
  stat_smooth(method="lm", se=FALSE) +
  scale_color_brewer() +
  scale_x_continuous(expand=c(0,0)) + 
  scale_y_continuous(limit=c(-5,10),expand=c(0,0)) + 
  theme_minimal()


ggplot(GSSvocab, aes(x = educ, y = vocab, col = year, group= factor(year))) +
  stat_smooth(method = "lm", se = FALSE, alpha = .6, size = 3) +
  scale_color_gradientn(colors = brewer.pal(9, "YlOrRd"))


ggplot(GSSvocab, aes(x = educ, y = vocab, col = year, group= factor(year))) +
  stat_smooth(method = "lm", se = FALSE, alpha = .6, size = 3, geom="path") +
  scale_color_gradientn(colors = brewer.pal(9, "YlOrRd"))


#------------------------------------------------------------------
#rq - quantile regression method (rather than conditional mean, using conditional median)

ggplot(GSSvocab, aes(x = educ, y = vocab, col = year, group= factor(year))) +
  stat_quantile(alpha = .6, size = 3) +
  scale_color_gradientn(colors = brewer.pal(9, "YlOrRd"))

#Set quantiles=.5 to just get the median
ggplot(GSSvocab, aes(x = educ, y = vocab, col = year, group= factor(year))) +
  stat_quantile(quantile=.5, alpha = .6, size = 3) +
  scale_color_gradientn(colors = brewer.pal(9, "YlOrRd"))


#------------------------------------------------------------------
#stat_sum() - calculates total number of overlapping observations
p <- ggplot(GSSvocab, aes(x = educ, y = vocab)) + geom_jitter(alpha=.2)
p
                   #sets min and max size of dots 
p + stat_sum() + scale_size(range=c(1,10))

#------------------------------------------------------------------

glimpse(msleep)
log10(msleep$bodywt)
#dnorm() - for getting normal distr. values


msleep <- msleep %>% mutate(., bodywt=log10(msleep$bodywt))


ggplot(msleep, aes(x=bodywt)) + 
    geom_histogram(aes(y=..density..)) + 
    geom_rug() +
    stat_function(fun=dnorm, color="lightgreen", size=2
                  )

#Not working for some reason 
#arg=list(mean=mean(msleep$bodywt), sd=sd(msleep$bodywt))

#-----------Base R for QQ Plots------------------------
#QQ Plot helps us determine if a distribution is normally distributed
# (Should be close to a positive line)
# qnorm(seq(0.01,0.99,0.01)) - to generate values
qqnorm(qnorm(seq(0.01,0.99,0.01)))
qqnorm(msleep$bodywt)
#-------------------------------------------------------
#Diff measures the value needed to get from the first value to the second value
quantile(msleep$bodywt, c(.25,.75))
qnorm(c(.25,.75))

diff(quantile(msleep$bodywt, c(.25,.75)))
diff(qnorm(c(.25,.75)))

msleep <- msleep %>% mutate(., slope=diff(quantile(msleep$bodywt, c(.25,.75)))/
                            diff(qnorm(c(.25,.75))))

msleep <- msleep %>% mutate(., intercept=quantile(msleep$bodywt, .25) - 
                                           msleep$slope * qnorm(.25))

                  #Have to use sample rather than x

ggplot(msleep, aes(sample=bodywt)) + 
        stat_qq() +
        geom_abline(aes(slope=slope, intercept=intercept), color='orange')

#------------------------------------------------------------------------
glimpse(mtcars)

#Converting cyl and am to factors 
mtcars <- mtcars %>% mutate(., cyl= as.factor(cyl), am=as.factor(am))

#Presetting some positions for our graphs
posn_d <- position_dodge(width=.1)
posn_jd <- position_jitterdodge(jitter.width = .1, dodge.width = .2)
posn_j <- position_jitter(width=.2)


#These plots use geom_pointrange() as default
wt_cyl_am <- ggplot(mtcars,aes(x=cyl, y=wt, color=am, fill=am, group=am)) 

wt_cyl_am + geom_point(position = posn_d, alpha=.6)

wt_cyl_am + geom_point(position = posn_jd, alpha = 0.6) + 
  stat_summary(fun.data=mean_sdl, fun.args=list(mult=1))

wt_cyl_am + geom_point(position = posn_jd, alpha = 0.6) + 
  stat_summary(fun.data=mean_cl_normal)

#This one relies more on stat_summary - need Hmisc for it to actually plot everything
wt_cyl_am + geom_point(position = posn_jd, alpha = 0.6) + 
  stat_summary(geom = "point", fun.y = mean,
               position = posn_d) +
  stat_summary(geom = "errorbar", fun.data = mean_sdl,
               position = posn_d, fun.args = list(mult = 1), width = 0.1)



#How to zoom in - use coord_cartesian()
#scale_x_continuous
#xlim() - not as flexible

#Best zooming as we see the data continue indicating an incomplete picture
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) +
      geom_point(alpha=.7) + 
      geom_smooth(method='loess') + 
      coord_cartesian(xlim=c(4,8))

#Aspect Ratio (height:width)
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) +
  geom_point(alpha=.7) + 
  geom_smooth(method='lm') +
  coord_fixed(ratio = 1) #can set it easily
  
  #coord_equal() #Forces graph to 1:1 Aspect Ratio



#How to make a pie chart using a stacked bar and polar coordinates
ggplot(mtcars, aes(x = 1, fill = cyl)) + 
    geom_bar() +
    coord_polar(theta = "y")


#How to make a donut chart
ggplot(mtcars, aes(x = 1, fill = cyl)) +
    geom_bar(width = .5) +
    scale_x_continuous(limits = c(0,2)) +
    coord_polar(theta = "y")


#-------------FACETS---------------------
p <- ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) +
            geom_point(alpha=.7) + 
            coord_fixed(ratio = 1) #can set it easily

p + facet_grid(. ~ Species)


# dot represents nothing (rows to left annd item to right represents columns)
#Splits be levels (categorical data)
p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point()

# 1 - Separate rows according to transmission type, am
p + facet_grid(am ~ .)

# 2 - Separate columns according to cylinders, cyl
p + facet_grid(. ~ cyl)

# 3 - Separate by both columns and rows 
p + facet_grid(am ~ cyl)


#NOT A GOOD WAY TO USE FACET GRID - TOO MUCH GOING ON
#-----------------------------------------------------------------
# Code to create the cyl_am col and myCol vector
mtcars <- mtcars %>% mutate(., cyl_am = paste(mtcars$cyl, mtcars$am, sep = "_"))
my_cols <- rbind(brewer.pal(9, "Blues")[c(3,6,8)],
               brewer.pal(9, "Reds")[c(3,6,8)])

ggplot(mtcars, aes(x = wt, y = mpg, color = cyl_am, size=disp)) +
  geom_point() +
  # Add a manual colour scale
  scale_color_manual(values = my_cols) +
  facet_grid(gear~vs)
#-----------------------------------------------------------------
#To see the color options: colors()
#-----------------------------------------------------------------
#THEMES = text, line, rectangle control everything, and then broken down to sub-aspect

#element_ + 

#element_blank() can remove elements

datacamp_blues <- brewer.pal(9, "Blues")[c(4,6,8)]


#Recreating datacamp example as practice to then modify theme layer
p <- ggplot(mtcars, aes(x=wt, y=mpg, color=factor(cyl))) + geom_point(alpha=.5, size=4) +
      stat_smooth(method="lm", se=FALSE) +
      facet_grid(.~cyl, scales="free_y") +
      scale_color_manual(values = datacamp_blues) + 
      scale_x_discrete(name="Weight (lb/1000)", limits=c(2,4,6)) + 
      coord_cartesian(xlim=c(1,6)) +
      labs(y="MPG", color="Cylinders") + #color used for legends
                                         #For histograms and boxplots, n
                                         #Need to use "fill"
      theme( #RECT-------------------
              rect= element_blank(),
                  plot.background = element_rect(fill="wheat1", color="black", size=4),
             
             #LINE------------------
             #panel.grid = element_blank(),
                  axis.line = element_line(color='red'),
                  axis.ticks = element_line(color='red'),
             
             #TEXT------------------
                strip.text = element_text(size=16, color='orange'),
                axis.title = element_text(color='black', hjust=.5, face='italic'),
                axis.text = element_text(color='gray'),
          
             
             #LEGEND----------------
                legend.position = c(.2,.2),
                legend.direction= 'horizontal',
               # legend.position = 'bottom',
                #legend.position = 'none'
            
             
             #SPACING--------------
                panel.spacing.x = unit(2, "cm"),
                        #spacing for top, right, bottom, and left margins)
                plot.margin = unit(c(1,2,1,1), "cm")
      )
p + theme_light()
p + theme_tufte()

#Set default theme
theme_set(theme_bw())
