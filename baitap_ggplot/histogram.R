

library(ggplot2)

#1. Nhu Y
#-----------------HISTOGRAM----------------#
#====GGPLOT2====#
## Basic ggplot2 histogram
library(ggplot2)
data = data.frame(value=rnorm(10000))
data
head(data)
ggplot(data,aes(x=value)) +
  geom_histogram() #bins = 0.2
ggplot(data,aes(x=value)) +
  geom_histogram(binwidth = 0.1)

##change color
ggplot(data,aes(x=value)) +
  geom_histogram(binwidth = 0.2, aes(fill = ..count..))
ggplot(data,aes(x=value)) +
  geom_histogram(binwidth = 0.2,col = "white",fill = rgb(0.7,0.2,0.2,0.4)) #rgb la ham cho phep tao mau theo cuong do tu 0 den 1
?rgb

## DOTPLOT HISTOGRAM (day la mot loai dac trung,rieng biet cua histogram) thay vi su dung thanh bar thi bieu do ay sd dot
## 1 dot dai dien cho 1 quan sat rieng le va 1 point se cho ta biet them thong tin de nhan dang du lieu do
library(ggplot2)
library(plotly)
install.packages("plotly")

# Classic histogram
ggplot(iris,aes(x=Sepal.Length)) + 
  geom_histogram()

# Transform the dataset to make dots
don = iris %>%
  arrange(Sepal.Length) %>%
  mutate(var_rounded = (Sepal.Length+1) - ((Sepal.Length+1) %% 0.2)) %>%
  mutate(y=ave(var_rounded,var_rounded,FUN = seq_along)) #de tinh toan vi tri tren truc Y
?ave

# Make the plot 
ggplot(don, aes(x=var_rounded, y=y)) +
  geom_point(size=6, color="skyblue") 

# Improve the plot, and make it interactive 
don=don %>% 
  mutate(text=paste("ID: ",rownames(iris), "\n", "Sepal Length: ", Sepal.Length, "\n", "Species:: ", Species, sep="" )) 
p=ggplot(don, aes(x=var_rounded, y=y) ) +
  geom_point(aes(text=text),size=6, color="skyblue") +
  xlab('Sepal Length') +
  ylab('# of individual') +
  theme_classic() +
  theme(
    legend.position="none",
    axis.line.y = element_blank(), #xoa duong truc y
    axis.text=element_text(size=15) #ten truc
  )
p
# Use the magic of ggplotly to have an interactive version
ggplotly(p, tooltip="text")
?ggplotly

#192 GGPLOT THEME
?I #inhibit 
## create data
set.seed(123) #random number generation
?set.seed
var1=rnorm(1000) #data de ve qplot
var=data.frame(var=var1) # data de ve bang ggplot
head(var1)
head(var)

## Without theme
?qplot #quick plot
plot1 <- qplot(var1 , fill=I(rgb(0.1,0.2,0.4,0.6))) #qplot la quickplot thi data k can la 1 data.frame 
plot1
plot<- ggplot(var,aes(x=var))+
  geom_histogram(binwidth = 0.2,fill=rgb(0.1,0.4,0.1,0.6))
plot #muon ve bang ham ggplot thi data phai la data.frame

# With themes
plot2 = plot1+
  theme_bw()+
  annotate("text", x = -2, y = 75, label = "bw()" , col="orange" , size=5)
plot2
?theme_bw # co vien den
?annotate #giup them geom vao plot, nhung ma k giong geom function, no se bien data thanh vector thay vi dung geom thi se map thanh dataframe

plot3 = plot1+
  theme_classic()+ #mat o ly va mat duong line 
  annotate("text", x = -1.9, y = 75, label = "classic()" , col="orange" , size=5)
plot3

plot4 = plot1+
  theme_gray()+
  annotate("text", x = -1.9, y = 75, label = "gray()" , col="orange" , size=4)
plot4

plot5 = plot1+
  theme_linedraw()+
  annotate("text", x = -1.9, y = 75, label = "linedraw()" , col="orange" , size=4)

plot6 = plot1+
  theme_dark()+
  annotate("text", x = -1.9, y = 75, label = "dark()" , col="orange" , size=4)

plot7 = plot1+
  theme_get()+
  annotate("text", x = -1.9, y = 75, label = "get()" , col="orange" , size=4)

plot8 = plot1+
  theme_minimal()+
  annotate("text", x = -1.9, y = 75, label = "minimal()" , col="orange" , size=4)

# Arrange and display the plots into a 2x1 grid
install.packages("gridExtra")
library(gridExtra)
grid.arrange(plot1,plot2,plot3,plot4,ncol=2) #gop 4 plot vo chung de de~ xem hon
grid.arrange(plot5,plot6,plot7,plot8, ncol=2)


#=====BASIC=====#
## Histogram without border
# Create data 
my_variable=c(rnorm(1000,0,2) , rnorm(1000,9,2)) #rnorm(n,mean,sd)

# Draw the histogram with border=F
hist(my_variable , breaks=40 , col=rgb(0.2,0.8,0.5,0.5) , border=F , main="") #khong vien den cot
?break ???
  ?hist

## Histogram with colored tail
# Create data
my_variable=rnorm(2000, 0 , 10)

# Calculate histogram, but do not draw it
my_hist=hist(my_variable , breaks=40  , plot=F)

# Color vector
my_color= ifelse(my_hist$breaks < -10, rgb(0.2,0.8,0.5,0.5),
                 ifelse (my_hist$breaks >=10, "purple", rgb(0.2,0.2,0.2,0.2)))

# Final plot
plot(my_hist, col=my_color , border=F , main="" , xlab="value of the variable", xlim=c(-40,40))

# Two histogram with melt colors
# Create data
Ixos=rnorm(4000 , 100 , 30)     
Primadur=rnorm(4000 , 200 , 30) 

# Represent separately first
?par
par(mfrow=c(1,2))
hist(Ixos, breaks=30 , xlim=c(0,300) , col=rgb(1,0,0,0.5) , xlab="height" , ylab="nbr of plants" , main="" )
hist(Primadur, breaks=30 , xlim=c(0,300) , col=rgb(0,0,1,0.5) , xlab="height" , ylab="nbr of plants" , main="")

#But it is hard to visualize differences in the distribution. 
#It would be more interesting to have both distribution on the same graph, 
#with transparency permitting to see the whole distribution :
hist(Ixos, breaks=30, xlim=c(0,300), col=rgb(1,0,0,0.5), xlab="height", 
     ylab="nbr of plants", main="distribution of height of 2 durum wheat varieties")
hist(Primadur, breaks=30, xlim=c(0,300), col=rgb(0,0,1,0.5), add=T)
legend("topright", legend=c("Ixos","Primadur"), col=c(rgb(1,0,0,0.5), 
                                                      rgb(0,0,1,0.5)), pt.cex=2, pch=15)

# Mirror histogram
#Create Data
x1 = rnorm(100)
x2 = rnorm(100)+rep(2,100)
par(mfrow=c(2,1)) # 2,1 la 2 dong 1 cot

#Make the plot
par(mar=c(0,5,3,3))
hist(x1 , main="" , xlim=c(-2,5), ylab="Frequency for x1", xlab="", ylim=c(0,25) , xaxt="n", las=1 , col="slateblue1", breaks=10)
par(mar=c(5,5,0,3))
hist(x2 , main="" , xlim=c(-2,5), ylab="Frequency for x2", xlab="Value of my variable", ylim=c(25,0) , las=1 , col="tomato3"  , breaks=10)
?par #par(mar = c(bottom,left,top,right))
#dao nguoc ylim cua plot2 la ylim = c(25,0)

#=====INTERACTIVE VERSION=====#
library(plotly)
# Basic histogram of one vecotr only
graph1=plot_ly(x = rnorm(500), type = "histogram")
graph1


#2. Thuy Hong

#########################################220 BASIC GGPLOT2 HISTOGRAM
library(ggplot2)

# dataset:
data=data.frame(value=rnorm(10000))

#1. ve co ban k co mau
# Basic histogram
ggplot(data, aes(x=value)) + geom_histogram()

# Custom Binning. I can just give the size of the bin: charge bin size
ggplot(data, aes(x=value)) + geom_histogram(binwidth =0.05)



#2. ve co mau sac
#Uniform color: vien trang ben trong mau khac
ggplot(data, aes(x=value)) + 
  geom_histogram(binwidth = 0.2, color="white" , fill=rgb(0.2, 0.7,0.1, 0.4) ) 

# Proportional color: mau nhat theo chiu cao cot 
ggplot(data, aes(x=value)) + 
  geom_histogram(binwidth = 0.2, aes(fill = ..count..) )



#192 GGPLOT THEMES

library(ggplot2)
library(gridExtra)

# create data
set.seed(123)
var=rnorm(1000)

# Without theme
plot1 <- qplot(var , fill=I(rgb(0.1,0.2,0.4,0.6)) )

# With themes
plot2 = plot1+theme_bw()+annotate("text", x = -1.9, y = 75, label = "bw()" , col="orange" , size=4)
plot3 = plot1+theme_classic()+annotate("text", x = -1.9, y = 75, label = "classic()" , col="orange" , size=4)
plot4 = plot1+theme_gray()+annotate("text", x = -1.9, y = 75, label = "gray()" , col="orange" , size=4)
plot5 = plot1+theme_linedraw()+annotate("text", x = -1.9, y = 75, label = "linedraw()" , col="orange" , size=4)
plot6 = plot1+theme_dark()+annotate("text", x = -1.9, y = 75, label = "dark()" , col="orange" , size=4)
plot7 = plot1+theme_get()+annotate("text", x = -1.9, y = 75, label = "get()" , col="orange" , size=4)
plot8 = plot1+theme_minimal()+annotate("text", x = -1.9, y = 75, label = "minimal()" , col="orange" , size=4)


# Arrange and display the plots into a 2x1 grid : chia ra 4 bieu tren 1 plane
grid.arrange(plot1,plot2,plot3,plot4, ncol=2)
grid.arrange(plot5,plot6,plot7,plot8, ncol=2)





#337 INTERACTIVE DOTPLOT HISTOGRAM

# Library
library(tidyverse)
install.packages("plotly")
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

# Improve the plot, and make it interactive (right),mat nen o di
don=don %>% mutate(text=paste("ID: ", rownames(iris), "\n", "Sepal Length: ", Sepal.Length, "\n", "Species:: ", Species, sep="" )) 
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




################################### Base R
#25 HISTOGRAM WITHOUT BORDER
# Create data 
my_variable=c(rnorm(1000 , 0 , 2) , rnorm(1000 , 9 , 2))

# Draw the histogram with border=F
hist(my_variable , breaks=40 , col=rgb(0.2,0.8,0.5,0.5) , border=F , main="")

###190 MIRRORED HISTOGRAM
#Create Data
x1 = rnorm(100)
x2 = rnorm(100)+rep(2,100)
par(mfrow=c(2,1))

#Make the plot
par(mar=c(0,5,3,3))
hist(x1 , main="" , xlim=c(-2,5), ylab="Frequency for x1", xlab="", ylim=c(0,25) , xaxt="n", las=1 , col="slateblue1", breaks=10)
par(mar=c(5,5,0,3))
hist(x2 , main="" , xlim=c(-2,5), ylab="Frequency for x2", xlab="Value of my variable", ylim=c(25,0) , las=1 , col="tomato3"  , breaks=10)




####83 HISTOGRAM WITH COLORED TAIL
# Create data
my_variable=rnorm(2000, 0 , 10)

# Calculate histogram, but do not draw it
my_hist=hist(my_variable , breaks=40  , plot=F)

# Color vector
my_color= ifelse(my_hist$breaks < -10, rgb(0.2,0.8,0.5,0.5) , ifelse (my_hist$breaks >=10, "purple", rgb(0.2,0.2,0.2,0.2) ))

# Final plot
plot(my_hist, col=my_color , border=F , main="" , xlab="value of the variable", xlim=c(-40,40) )




###2 TWO HISTOGRAMS WITH MELT COLORS
##### create data
Ixos=rnorm(4000 , 100 , 30)     
Primadur=rnorm(4000 , 200 , 30) 

#Represent separately first
par(mfrow=c(1,2))
hist(Ixos, breaks=30 , xlim=c(0,300) , col=rgb(1,0,0,0.5) , xlab="height" , ylab="nbr of plants" , main="" )
hist(Primadur, breaks=30 , xlim=c(0,300) , col=rgb(0,0,1,0.5) , xlab="height" , ylab="nbr of plants" , main="")

#But it is hard to visualize differences in the distribution. 
#It would be more interesting to have both distribution on the same graph, 
#with transparency permitting to see the whole distribution :
hist(Ixos, breaks=30, xlim=c(0,300), col=rgb(1,0,0,0.5), xlab="height", 
     ylab="nbr of plants", main="distribution of height of 2 durum wheat varieties" )
hist(Primadur, breaks=30, xlim=c(0,300), col=rgb(0,0,1,0.5), add=T)
legend("topright", legend=c("Ixos","Primadur"), col=c(rgb(1,0,0,0.5), 
                                                      rgb(0,0,1,0.5)), pt.cex=2, pch=15 )





###25 HISTOGRAM WITHOUT BORDER
# Create data 
my_variable=c(rnorm(1000 , 0 , 2) , rnorm(1000 , 9 , 2))

# Draw the histogram with border=F
hist(my_variable , breaks=40 , col=rgb(0.2,0.8,0.5,0.5) , border=F , main="")







#####82 BOXPLOT ON TOP OF HISTOGRAM
# Creating data 
my_variable=c(rnorm(1000 , 0 , 2) , rnorm(1000 , 9 , 2))

# Layout to split the screen
layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))

# Draw the boxplot and the histogram 
par(mar=c(0, 3.1, 1.1, 2.1))
boxplot(my_variable , horizontal=TRUE , ylim=c(-10,20), xaxt="n" , col=rgb(0.8,0.8,0,0.5) , frame=F)
par(mar=c(4, 3.1, 1.1, 2.1))
hist(my_variable , breaks=40 , col=rgb(0.2,0.8,0.5,0.5) , border=F , main="" , xlab="value of the variable", xlim=c(-10,20))






##########################Interactive version

##112 BASIC INTERACTIVE HISTOGRAM | PLOTLY

# Library
library(plotly)

# Basic histogram of one vecotr only
graph1=plot_ly(x = rnorm(500), type = "histogram")
graph1
####113 OVERLAID INTERACTIVE HISTOGRAM | PLOTLY
# Library
library(plotly)

# Overlaid histogram of 2 vectors:
graph=plot_ly(x = rnorm(500), opacity = 0.6, type = "histogram") %>%
  add_trace(x = rnorm(500)+1) %>%
  layout(barmode="overlay")
graph






####111 INTERACTIVE 3D PLOT | PLOTLY
# Plotly library
library(plotly)

# Let's use the volcano dataset:
head(volcano)





# 3D plot :
p=plot_ly(z = volcano, type = "surface")
p
######337 INTERACTIVE DOTPLOT HISTOGRAM:cai tren dau
####76_SPLIT_SCREEN_FUNCTION
#Create data
a=seq(1,29)+4*runif(29,0.4)
b=seq(1,29)^2+runif(29,0.98)

# I divide the screen in 2 line and 1 column only
my_screen_step1 <- split.screen(c(2, 1))

# I add one graph on the screen number 1 which is on top :
screen(my_screen_step1[1])
plot( a,b , pch=20 , xlab="value of a" , cex=3 , col=rgb(0.4,0.9,0.8,0.5) )


# I divide the second screen in 2 columns :
my_screen_step2=split.screen(c(1, 2), screen = my_screen_step1[2])
screen(my_screen_step2[1])
hist(a, border=F , col=rgb(0.2,0.2,0.8,0.7) , main="" , xlab="distribution of a")
screen(my_screen_step2[2])
hist(b, border=F , col=rgb(0.8,0.2,0.8,0.7) , main="" ,  xlab="distribution of b")





###10 layout and histogram : k co code coi lai

####18 HISTOGRAM
par(bg="grey90")
par(mar=c(0,0,0,0))
a=rnorm(400 , mean=-5 , sd=12)
b=rnorm(400 , mean=20 , sd=3)
c=rnorm(200 , mean=-20 , sd=3)
hist(a , col=rgb(1,0.2,0.2,0.7) , breaks=50 , xlim=c(-40 , 40) , main="" , ylim=c(-20 , 70) )
hist(b ,  col=rgb(0,1,0.5,1) , breaks=15 , add=T)
hist(c ,  col=rgb(0.6,0.4,1,1) , breaks=15 , add=T)


####72 SET MARGIN SIZE WITH PAR MAR FUNCTION:ve k dc
#Set the margin size (huge margins)
par(mar=c(7,8,7,7))

#Create data
a=runif(29,0.4)
hist(a, border=F , col=rgb(0.2,0.2,0.8,0.7) , main="", ylab="" , xlab="")

# Add the text
mtext(c("Margin 1","Margin 2","Margin 3","Margin 4") , at=c(0.7,0.22,0.7,1.1) , 
      line=c(-25,-10,2,-10) , col="orange" , cex=1.5 )





####75 SPLIT SCREEN WITH LAYOUT: ve k dc
a=seq(129,1)+4*runif(129,0.4)
b=seq(1,129)^2+runif(129,0.98)

nf=layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
layout.show(nf)


nf=layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
hist(a , breaks=30 , border=F , col=rgb(0.1,0.8,0.3,0.5) , xlab="distribution of a" , main="")
boxplot(a , xlab="a" , col=rgb(0.8,0.8,0.3,0.5) , las=2)
boxplot(b , xlab="b" , col=rgb(0.4,0.2,0.3,0.5) , las=2)


# Set the layout
nf<-layout(matrix(c(1,1,2,3),2,2,byrow=TRUE), c(3,1), c(2,2),TRUE) 
layout.show(nf)

#Add the plots
hist(a , breaks=30 , border=F , col=rgb(0.1,0.8,0.3,0.5) , xlab="distribution of a" , main="")
boxplot(a , xlab="a" , col=rgb(0.8,0.8,0.3,0.5) , las=2)
boxplot(b , xlab="b" , col=rgb(0.4,0.2,0.3,0.5) , las=2)




####1 ONE TITLE FOR 2 GRAPHS
#Creating data 
Ixos=rnorm(4000,100,30)
Primadur=Ixos+rnorm(4000 , 0 , 30)

#Divide the screen in 1 line and 2 columns
par(mfrow=c(1,2),oma = c(0, 0, 2, 0))

#Make the margin around each graph a bit smaller
par(mar=c(4,2,2,2))

#Classical histogram and plot
hist(Ixos,  main="" , breaks=30 , col=rgb(0.3,0.5,1,0.4) , xlab="height" , ylab="nbr of plants")
plot(Ixos , Primadur,  main="" , pch=20 , cex=0.4 , col=rgb(0.3,0.5,1,0.4)  , xlab="primadur" , ylab="Ixos" )

#And I add only ONE title :
mtext("Primadur : Distribution and correlation with Ixos", outer = TRUE, cex = 1.5, font=4, col=rgb(0.1,0.3,0.5,0.5) )




######76_SPLIT_SCREEN_FUNCTION
#Create data
a=seq(1,29)+4*runif(29,0.4)
b=seq(1,29)^2+runif(29,0.98)

# I divide the screen in 2 line and 1 column only
my_screen_step1 <- split.screen(c(2, 1))

# I add one graph on the screen number 1 which is on top :
screen(my_screen_step1[1])
plot( a,b , pch=20 , xlab="value of a" , cex=3 , col=rgb(0.4,0.9,0.8,0.5) )


# I divide the second screen in 2 columns :
my_screen_step2=split.screen(c(1, 2), screen = my_screen_step1[2])
screen(my_screen_step2[1])
hist(a, border=F , col=rgb(0.2,0.2,0.8,0.7) , main="" , xlab="distribution of a")
screen(my_screen_step2[2])
hist(b, border=F , col=rgb(0.8,0.2,0.8,0.7) , main="" ,  xlab="distribution of b")




#3. Que Lam
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





#4. Truong Han




#5. Minh Ly



#6. Hong Phuc




#7. Cung Bac







