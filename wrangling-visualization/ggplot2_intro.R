library(ggplot2)
library(dplyr)

#This shows all of the pre-installed data sets from a few packages (gapminder, MASS,, datasets, ggplot2, dplyr)
data()

#Loads in mtcars data set
data(mtcars)

#Cylinder is actually categorical (only 4,6,8) but without factor() is plotted as numeric (4,5,6,7,8)
ggplot(mtcars, aes(x=factor(cyl), y=mpg)) + geom_point()
#------------------------------------------------------------------------------------------------------
ggplot(mtcars, aes(x=wt, y=mpg, color=disp, size=disp)) + geom_point(shape=3)

#Loads in iris data set
data(iris)                                                                        #LAYER 1 - Data
names(iris)

#Jitter adds a small amount of random variation to the location of each point

ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width)) + 
        geom_point(shape=20, alpha=.5, size=10) 
#------------------------------------------------------------------------------------------------------
#se = standard error

ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width)) + 
        geom_point(shape=20, alpha=.5, size=10) +
        facet_grid(.~ Species) +                                                  
        stat_smooth(method='lm', se=FALSE, color='blue')
#------------------------------------------------------------------------------------------------------
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width)) +                                #LAYER 2 - Aesthetics
       geom_point(shape=20, alpha=.5, size=5) +                                   #LAYER 3 - Geometries
       facet_grid(.~Species) +                                                    #LAYER 4 - Facets
       geom_smooth(method="lm", se=FALSE, color='blue') +                         #LAYER 5 - Statistics
       scale_y_continuous("Sepal Width (cm)", limits=c(1.8,4.5), expand=c(0,0)) + #LAYER 6 - Coordinates
       scale_x_continuous("Sepal Length (cm)", limits=c(4,8)) +                   #LAYER 7 - Theme
       coord_equal() +
       theme( axis.line = element_line(color="purple"),
              axis.text = element_text(color="darkblue"),
              axis.ticks = element_line(color = "red"),
              
              legend.background = element_blank(),
              legend.key = element_blank(),
              
              panel.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.spacing = unit(1, "lines"),
             
              plot.background = element_blank(),
             
              strip.background = element_blank(),
              strip.text = element_blank()
             )                        
#------------------------------------------------------------------------------------------------------
data("diamonds")

glimpse(diamonds)
names(diamonds)

#Randomly sampling 1000 rows to make data quicker to plot and analyze
diamonds <- diamonds %>% sample_n(., size=1000)

ggplot(diamonds, aes(x=carat, y=price, color=clarity)) + 
        geom_point() + 
        geom_smooth(method='loess') #Locally estimated scatterplot smoothing
#------------------------------------------------------------------------------------------------------

ggplot(diamonds, aes(x=carat, y=price, color=clarity)) + geom_point()
#------------------------------------------------------------------------------------------------------
ggplot(diamonds, aes(x=carat, y=price)) + 
      geom_point(aes(color=clarity)) +
      geom_smooth(aes(color=clarity), se=FALSE)

#------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------
#Base R Plotting
plot(iris$Sepal.Length, iris$Sepal.Width)


#Having tidy data will enable better visualizations

             #Aesthetics in aesthetic layer         #Attributes in geometric layer
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width)) + 
        geom_point(aes(color=Species))


#Color - changes outline of point
#Fill - changes inside of point

#Shape: 19 - Solid Dot
#       1 - Hollow Dot
#       16 - Solid Dot no outline
#       21 - both fill and outline

#Shapes 1-20 only accept color
#Shapres 21-25
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width)) + 
        geom_point(aes(color=Species), shape=21, size=10, fill='lightblue') +
        geom_text(label=Sepal.Length)

#------------------------------------------------------------------------------------------------------
#Mapping (variable = variable) requires an aesthetic "aes()"
ggplot(mtcars, aes(x=wt, y=mpg, size=cyl)) + geom_point()
ggplot(mtcars, aes(x=wt, y=mpg, alpha=cyl)) + geom_point()
ggplot(mtcars, aes(x=wt, y=mpg, shape=cyl)) + geom_point()
ggplot(mtcars, aes(x=wt, y=mpg, label=cyl)) + geom_point() + geom_text()

#This version won't work since it was not correctly mapped:
#ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point(size=cyl)

ggplot(mtcars, aes(x=wt, y=mpg)) + 
    geom_point(size=20, color="yellow", fill='black') + 
    geom_text(aes(label=cyl))

#------------------------------------------------------------------------------------------------------
#Here the attribute color is overwriting the aesthetic color WRONG WAY
my_color <- "#4ABEFF"
ggplot(mtcars, aes(x=wt, y=mpg, color=cyl)) + 
        geom_point(color=my_color)

#Here there is no overwriting, but independent setting of outline and insider RIGHT WAY
ggplot(mtcars, aes(x=wt, y=mpg, fill=factor(cyl))) + 
      geom_point(color=my_color, size=10, shape=23)

#------------------------------------------------------------------------------------------------------
ggplot(mtcars, aes(x = wt, y = mpg, fill = cyl)) +
        geom_point() + 
        geom_text(aes(label=rownames(mtcars)),color='red')

#------------------------------------------------------------------------------------------------------
ggplot(mtcars, aes(x=mpg, y=qsec, color=factor(cyl), shape=factor(am))) +
       geom_point()


ggplot(mtcars, aes(x=mpg, y=qsec, color=factor(cyl), shape=factor(am), size=(hp/wt))) + 
       geom_point()
#------------------------------------------------------------------------------------------------------
#position = "identity" i s the default

pos_jit <- position_jitter(width=.1)

#OKAY WAY
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) +
      geom_point(position='jitter')

#OR 
#BEST WAY-------------------------------------------
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) +
  geom_point(position=pos_jit) +
  scale_x_continuous("Sepal Length",breaks = seq(2,3,8)) +
  scale_color_discrete(labels=c("Toast", "Butter", "Flamenco"))

#To change labels, can also use "labs":
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) +
  geom_point(position=pos_jit) +
  scale_x_continuous("Sepal Length",breaks = seq(2,3,8)) +
  labs(x='Yinzers', y="Taquitos", color="Caraca velho!")

#Categorical factors = {factor, discrete, categorical} SAME THING

#SCALE Functions          +  TYPE of DATA
# scale_x_                    continuous / discrete
# scale_y_
# scale_color
# scale_fill
# scale_shape
# scale_linetype

------------------------------------------------------------------------------------------------------
cyl_am <- ggplot(mtcars, aes(x = factor(cyl), fill = factor(am)))
cyl_am

#default position="stack"
cyl_am + geom_bar() 
#------------------------------------------------------------------------------------------------------
#Shows proportion
cyl_am + geom_bar(position ="fill") 
#------------------------------------------------------------------------------------------------------
#Shows normal bars side-by-side
cyl_am + geom_bar(position="dodge") #default position="stack"
#------------------------------------------------------------------------------------------------------

val = c("#E41A1C", "#377EB8")
lab = c("Manual", "Automatic")

#Scale in this case just has to do with labels
cyl_am + geom_bar(position = "dodge") +
    scale_x_discrete("Cylinders") + 
    scale_y_continuous("Number") +
    scale_fill_manual("Transmission", values = val, labels = lab) 
#------------------------------------------------------------------------------------------------------
#Graphs of univariate data - no y axis, but also not a histogram
ggplot(mtcars, aes(x = mpg, y=0)) + geom_jitter()

# 2 - Add function to change y axis limits
ggplot(mtcars, aes(x = mpg, y=0)) + 
    geom_jitter() + 
    scale_y_continuous(limits=c(-2,2))

#------------------------------------------------------------------------------------------------------
#Base R Way #pch is equivalent to shape in ggplot2
stripchart(mtcars$mpg, xlim = c(5,49), col="blue", pch=5)

#------------------------------------------------------------------------------------------------------

#MORE GEOMS 
#------------------------------------------------------------------------------------------------------
#Lines

ggplot(mtcars, aes(x = mpg, y=0)) + 
        geom_jitter(shape=1, width=.01) + 
        geom_vline(aes(xintercept = 20), color="red", linetype = 2) +
        geom_hline(aes(yintercept = .23), color="blue", size=10, alpha=.4, linetype=5) +
      geom_abline(intercept=5, slope=20)

ggplot(iris, aes(x = Sepal.Length, y=Sepal.Width)) + 
  geom_jitter(shape=1, width=.01) + 
  geom_vline(aes(xintercept = 20), color="red", linetype = 2) +
  geom_hline(aes(yintercept = .23), color="blue", size=10, alpha=.4, linetype=5) +
  geom_abline(intercept=1, slope=.2)
#------------------------------------------------------------------------------------------------------
#Histograms

ggplot(iris, aes(x=Sepal.Width, fill=Species)) + 
      geom_histogram(bins = 50, binwidth = .05)

#Using the .. before and after to indicate internal data frame
#For density histograms = proportional frequency 
# Example 1
ggplot(iris, aes(x=Sepal.Width, fill=Species)) + 
      geom_histogram(aes(y=..density..), bins = 50, binwidth = .25, position="dodge") +
      scale_fill_brewer()

#Example 2
ggplot(mtcars, aes(x = mpg)) +
  geom_histogram(aes(y=..density.., ), binwidth=1, fill='#377EB8', color='black')


#Frequency polygon plots
ggplot(mtcars, aes(mpg,color=factor(cyl))) +
  geom_freqpoly( binwidth=1)

#Bars - for absolute counts or distributions)

# ggplot(msleep, aes(x=genus, fill=vore)) +
#   geom_bar(position = "fill") +
#   scale_fill_brewer()










