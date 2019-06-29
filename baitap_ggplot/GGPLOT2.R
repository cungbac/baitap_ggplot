### GGPLOT2 ###
# library
library(ggplot2)
# dataset:
data=data.frame(value=rnorm(10000))
# 220 Basic histogram
ggplot(data, aes(x=value)) + geom_histogram()
# Change size Custom Binning. I can just give the size of the bin
ggplot(data, aes(x=value)) + geom_histogram(binwidth = 0.05)
# Uniform color
ggplot(data, aes(x=value)) + 
  geom_histogram(binwidth = 0.2, color="white", fill=rgb(0.2,0.7,0.1,0.4) ) 
# Proportional color
ggplot(data, aes(x=value)) + 
  geom_histogram(binwidth = 0.2, aes(fill = ..count..) )

library(ggplot2)
library(gridExtra)
# create data
set.seed(123)
var=rnorm(1000)
# Without theme
plot1 <- qplot(var , fill=I(rgb(0.1,0.2,0.4,0.6)) )
# With themes
plot2 = plot1+theme_bw()+annotate("text", x = -1.9, y = 75, label = "bw()" , col="red" , size=4)
plot3 = plot1+theme_classic()+annotate("text", x = -1.9, y = 75, label = "classic()" , col="orange" , size=4)
plot4 = plot1+theme_gray()+annotate("text", x = -1.9, y = 75, label = "gray()" , col="orange" , size=4)
plot5 = plot1+theme_linedraw()+annotate("text", x = -1.9, y = 75, label = "linedraw()" , col="orange" , size=4)
plot6 = plot1+theme_dark()+annotate("text", x = -1.9, y = 75, label = "dark()" , col="orange" , size=4)
plot7 = plot1+theme_get()+annotate("text", x = -1.9, y = 75, label = "get()" , col="orange" , size=4)
plot8 = plot1+theme_minimal()+annotate("text", x = -1.9, y = 75, label = "minimal()" , col="orange" , size=4)
# Arrange and display the plots into a 2x1 grid
grid.arrange(plot1,plot2,plot3,plot4, ncol=2)
grid.arrange(plot5,plot6,plot7,plot8, ncol=2)
grid.arrange(plot1,plot2,plot3,plot4,plot5,plot6,plot7,plot8,ncol=4)



library(tidyverse)
library(plotly)
# A classic histogram for the iris data set (left)
ggplot(iris, aes(x=Sepal.Length)) +
  geom_histogram()
# Transform a litte bit the dataset to make dots
don = iris %>% 
  arrange(Sepal.Length) %>% # sort using the numeric variable that interest you
  mutate(var_rounded = (Sepal.Length+1) - ( (Sepal.Length+1) %% 0.2 ) ) %>% # This attributes a bin to each observation. Here 0.2 is the size of the bin.
  mutate(y=ave(var_rounded, var_rounded, FUN=seq_along)) # This calculates the position on the Y axis: 1, 2, 3, 4...
# Make the plot (middle)
ggplot(don, aes(x=var_rounded, y=y) ) +
  geom_point( size=6, color="skyblue" ) 

# Improve the plot, and make it interactive (right)
don=don %>% mutate(text=paste("ID: ", rownames(iris), "\n", "Sepal Length: ", 
                            Sepal.Length, "\n", "Species:: ", Species, sep="" ))

p=ggplot(don, aes(x=var_rounded, y=y) ) +
  geom_point( aes(text=text), size=6, color="skyblue" ) +
  xlab('Sepal Length') +
  ylab('# of individual') +
  theme_classic() +
  theme(
    legend.position="none",
    axis.line.y = element_blank(),
    axis.text=element_text(size=15)
  )
p
# Use the magic of ggplotly to have an interactive version
ggplotly(p, tooltip="text")



### BASE R ###
# 2 hidtogram with melt colors
#Create data
Ixos=rnorm(40 , 10 , 3)     
Primadur=rnorm(40 , 20 , 3) 

#Represent separately first
par(mfrow=c(1,2))
hist(Ixos, breaks=3 , xlim=c(0,30) , 
     col=rgb(1,0,0,0.5) , xlab="height" ,
     ylab="nbr of plants" , main="" )

hist(Primadur, breaks=3 , xlim=c(0,30) ,
     col=rgb(0,0,1,0.5) , xlab="height" , 
     ylab="nbr of plants" , main="")

#But it is hard to visualize differences in the distribution. 
#It would be more interesting to have both distribution on the same graph, 
#with transparency permitting to see the whole distribution :

hist(Ixos, breaks=30, xlim=c(0,300), 
     col=rgb(1,0,0,0.5), xlab="height", 
     ylab="nbr of plants",
     main="distribution of height of 2
     durum wheat varieties" )

hist(Primadur, breaks=30, 
     xlim=c(0,300), 
     col=rgb(0,0,1,0.5), add=T)
legend("topright", legend=c("Ixos","Primadur"), 
       col=c(rgb(1,0,0,0.5), 
       rgb(0,0,1,0.5)), pt.cex=2, pch=15 )



