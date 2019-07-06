# Library:
library(ggplot2)

# Dataset:
data = data.frame(value=rnorm(10000))

# Basic histogram:
ggplot(data, aes(x = value)) + geom_histogram()

# Custom Binning. I can just give the size of the bin
ggplot(data, aes(x = value)) + geom_histogram(binwidth = 0.05)

# Uniform color:
ggplot(data, aes(x=value)) + geom_histogram(binwidth = 0.2, color = 'white',
                                            fill =rgb(0.2,0.7,0.1,0.4))

# Proportional color
ggplot(data, aes(x = value)) + geom_histogram(binwidth = 0.2, aes(fill = ..count..))







