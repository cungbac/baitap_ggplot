#Histogram hplm 
test

# ggplot2
## 220 Basic ggplot2 histogram
### library
library(ggplot2)

### dataset
data= data.frame(value= rnorm(10000))

### Basic histogram
ggplot(data, aes(x= value)) +
  geom_histogram()

### Custom Binning. I can just give the size of the bin
ggplot(data, aes(x= value)) + 
  geom_histogram(binwidth = 0.05)

### Uniform color
ggplot(data, aes(x= value))+
  geom_histogram(binwidth = 0.2, color= "white",fill= rgb(0.2, 0.7, 0.1, 0.4))

### Proportional color
ggplot(data, aes(x= value))+
  geom_histogram(binwidth = 0.2, aes(fill= ..count..))



## 192 ggplot themes
## library
library(ggplot2)
library(gridExtra)

### create data
set.seed(123)
var=rnorm(1000)

### without theme
plot1= qplot(var, fill=I(rgb(0.1, 0.2, 0.4, 0.6)))

### With theme
plot2= plot1 + theme_bw()+ annotate("text", x= -1.9, y= 75, label= "bw()", col= "orange", size= 4)
plot3= plot1+ theme_classic()+ annotate("text", x= -1.9, y= 75, label= "classic()", col= "orange", size= 4)
plot4= plot1 + theme_gray()+ annotate("text", x= -1.9, y= 75, label= "gray()", col= "orange", size= 4)
plot5= plot1 + theme_linedraw()+ annotate("text", x= -1.9, y= 75, label= "linedraw()", col= "orange", size= 4)
plot6= plot1 + theme_dark()+ annotate("text", x= -1.9, y= 75, label= "dark()", col= "orange", size= 4)
plot7= plot1 + theme_get()+ annotate("text", x= -1.9, y= 75, label= "get()", col= "orange", size= 4)
plot8= plot1 + theme_minimal()+ annotate("text", x= -1.9, y= 75, label= "mininal()", col= "orange", size= 4)

### Arrange and display the plots into a 2X1 grid
grid.arrange(plot1, plot2, plot3, plot4, ncol= 2)
grid.arrange(plot5, plot6, plot7, plot8, ncol=2)


## 337 interactive dotplot histogram
### library
library(tidyverse)
library(plotly)

### A classic histogram for the iris data set (left)
ggplot(iris, aes(x= Sepal.Length))+
  geom_histogram()

### Transform a litte bit the dataset to make dots
don= iris %>%
  arrange(Sepal.Length)%>% #sort using the numeric variable that interest you
  mutate(var_rounded= (Sepal.Length+1)- ((Sepal.Length +1)%% 0.2)) %>% #this attributes a bin to each observation. Here 0.2 is the size of the bin.
  mutate(y= ave(var_rounded, var_rounded, FUN= seq_along)) #this calculates the position on the Y axis 1,2,3,4

### Make the plot (middle)
ggplot(don, aes(x= var_rounded, y=y))+
  geom_point(size= 6, color= "skyblue")

### Improve the plot, and make it interactive (right)
don= don %>% mutate(text= paste("ID:", rownames(iris), "\n", "Sepal Length:", Sepal.Length, "\n", "Species::", Species, seq=""))
p= ggplot(don, aes(x= var_rounded, y= y))+
  geom_point(aes(text= text), size=6, color= "skyblue")+
  xlab('Sepal. Length')+
  ylab('# of individual')+
  theme_classic()+
  theme(
    legend.position = "none",
    axis.line = element_blank(),
    axis.text = element_text(size=15)
  )
p

### use the magic of ggplotly to have an interactive version
ggplotly(p, tooltip = "text")



# Base R
## 190 Mirrored histogram
### Create Data
x1= rnorm(100)
x2= rnorm(100)+ rep(2, 100)
par(mfrow= c(2,1))

### Make the plot
par(mar=c(0.5,3,3))
hist(x1, main= "", xlim= c(-2, 5), ylab= " Frequency for x1", xlab="", ylim = c(0, 25), xaxt= "n", las= 1, col= "slateblue1", breaks=10)
par(mar= c(5,5,0,3))
hist(x2, main = "", xlim = c(-2,5), ylab= "Frequency for x2", xlab = "Value of my variable", ylim = c(25,0), las=1, col= "tomato3", breaks = 10)


## 82 boxplot on top of histogram
### create data
my_variable= c(rnorm(1000,0,2), rnorm(1000,9,2))

### Layout to split the screen
layout(mat= matrix(c(1,2),2,1, byrow = TRUE), height= c(1,8))

### Draw the boxplot and the histogram
par(mar= c(0, 3.1, 1.1, 2.1))
boxplot(my_variable, horizontal = TRUE, ylim= c(-10,20), xaxt="n", col= rgb(0.8, 0.8, 0, 0.5))
par(mar= c(4,3.1,1.1,2.1))
hist(my_variable,breaks = 40, col= rgb(0.2,0.8,0.5,0.5), border = F, main="", xlab = "value of the variable", xlim= c(-10,20))


## 2 two histogram with melt colors
### Create data
Ixos= rnorm(4000,100,30)
Primadur= rnorm(4000, 200, 30)

### Represent separately first
par(mfrow= c(1,2))
hist(Ixos, breaks = 30, xlim= c(0,300), col= rgb(1,0,0,0.5), xlab = "height", ylab = "nbr of plants", main = "")
hist(Primadur, breaks=30 , xlim=c(0,300) , col=rgb(0,0,1,0.5) , xlab="height" , ylab="nbr of plants" , main="")

hist(Ixos, breaks=30, xlim=c(0,300), col=rgb(1,0,0,0.5), xlab="height", 
     ylab="nbr of plants", main="distribution of height of 2 durum wheat varieties" )
hist(Primadur, breaks=30, xlim=c(0,300), col=rgb(0,0,1,0.5), add=T)
legend("topright", legend = c("Ixos", "Primadur"), col= c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)), pt.cex = 2, pch=5)


## 25 histogram without border
### Create data
my_variable= c(rnorm(1000,0,2), rnorm(1000, 9,2))

### Draw the histogram with border= F
hist(my_variable, breaks = 40, col = rgb(0.2,0.8,0.5,0.5), border = F, main="")


## 83 Histogram with colored tail
### Create data
my_variable1= rnorm(2000, 0, 10)

### Calculate histogram but do not draw it
my_hist= hist(my_variable1, breaks = 40, plot = F)

### Color vector
my_color= ifelse(my_hist$breaks < -10, rgb(0.2,0.8,0.5,0.5), ifelse(my_hist$breaks >=10, "purple", rgb(0.2,0.2,0.2,0.2)))

### Final plot
plot(my_hist, col=my_color, border = F, main="", xlab= "value of the variable", xlim = c(-40,40))




# Interactive version
## 112 basic histogram | plotly
### library
library(plotly)

### basic histogram of one vecotr only
graph1= plot_ly(x= rnorm(500), type = "histogram")
graph1


## 113 overlaid interactive histogram | plotly
###library
library(plotly)

### Overlaid histogram of 2 vectors
graph= plot_ly(x= rnorm(500), opacity= 0.6, type = "histogram")%>%
  add_trace(x= rnorm(500)+1) %>%
  layout(barmode="overlay")
graph
              
## 111 interactive 3D plot| plotly
###Plotly library
library(plotly)

### Let's use the volcano dataset
head(volcano)
              
### 3D plot
p= plot_ly(z= volcano, type = "surface")
p             
