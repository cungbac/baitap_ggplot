

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




#3. Hong Lam





#4. Truong Han




#5. Minh Ly



#6. Hong Phuc




#7. Cung Bac







