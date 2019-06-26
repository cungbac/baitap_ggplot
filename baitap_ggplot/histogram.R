

library(ggplot2)

#1. Nhu Y



#2. Thuy Hong




#3. Hong Lam





#4. Truong Han




#5. Minh Ly



#6. Hong Phuc




#7. Cung Bac
<<<<<<< HEAD
ddata %>%
  filter( price<300 ) %>%
  ggplot( aes(x=price)) +
  stat_bin(breaks=seq(0,300,10), fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Night price distribution of Airbnb appartements") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=12)
  )

=======
>>>>>>> 3aff99ab99bda9a8e541af0f6d194096488a911c








