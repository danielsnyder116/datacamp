library(ggplot2)
library(scales)

#Sets the path so you can read in the file name separately
setwd("C:/Users/602770/Downloads/")

benefits <- read.csv("Benefits.csv", col.names = c("State", "Size"))

#Thankfully, since the State column is already in the order of greatest to least based on size,
# you can simply use that to set the levels, rather than having to manually type everything out.

benefits$State <- factor(benefits$State, levels=benefits$State)

#This checks the levels, now they're right, yippee! 
levels(benefits$State) 

#We then need to use the reorder function which helps us sort the values from 
# largest to smallest, and so we prioritize state first and then size.
p <- ggplot(data = benefits, aes(x=reorder(State, Size), y=Size)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ggtitle("Maximum Monthly TANF Benefits for a Family of 3") 

p

#This code would sort by smallest value first:
# q <- ggplot(data = benefits, aes(x=reorder(State, -Size), y=Size)) +
#   geom_bar(stat = "identity") +
#   coord_flip() +
#   ggtitle("Maximum Monthly TANF Benefits for a Family of 3") 

