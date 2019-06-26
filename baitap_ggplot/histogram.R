

library(ggplot2)

#1. Nhu Y
#-----------------HISTOGRAM----------------#
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


#2. Thuy Hong




#3. Hong Lam





#4. Truong Han




#5. Minh Ly



#6. Hong Phuc




#7. Cung Bac
ddata %>%
  filter( price<300 ) %>%
  ggplot( aes(x=price)) +
  stat_bin(breaks=seq(0,300,10), fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Night price distribution of Airbnb appartements") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=12)
  )








