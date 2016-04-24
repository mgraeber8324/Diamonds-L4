library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(datasets)
data("diamonds")

#diamond scatter
ggplot(diamonds, aes(x, price)) +
  geom_point()

#refined diamond scatter
ggplot(diamonds[diamonds$x > 0 & diamonds$price > 0, ]) + 
  geom_point(aes(x = x, y = price), alpha = .01, color = "green") + 
  labs(title = "diamond price vs length", x = "length", y = "price") +
  scale_x_continuous(breaks = 0:12, limits = c(min(diamonds$x[diamonds$x > 0]), quantile(diamonds$x, 0.999))) +
  scale_y_continuous(breaks = seq(0, max(diamonds$price), 5000))
ggsave("Diamond price vs len scat.png")

#price cor - i'd expect these to be very similar
with(diamonds,
     data.frame(cor_x_price = cor(x, price),
                cor_y_price = cor(y, price),
                cor_z_price = cor(z, price)
     )
)

#diamonds depth vs price scatter
ggplot(diamonds, aes(depth, price)) +
  geom_point() +
  labs(x = "depth (%)", y = "price", title = "diamond price vs. depth")

#refined
ggplot(diamonds, aes(depth, price)) +
  geom_point(alpha = 0.01) +
  scale_x_continuous(breaks = seq(0, 100, 2)) +
  labs(x = "depth (%)", y = "price", title = "diamond price vs. depth")
ggsave("diamond price vs depth.png")

#quantiles
with(diamonds,
     c(quantile(depth, 0.05),
       quantile(depth, 0.25),
       quantile(depth, 0.75),
       quantile(depth, 0.95)
     )
)

#correlation - none expected
cor(diamonds$depth, diamonds$price)
#correlation = -0.01 => I wouldn't use depth % to predict price

ggplot(diamonds, aes(carat, price)) +
  geom_point(colour = "green") +
  scale_x_continuous(limits = c(0, quantile(diamonds$carat, 0.99))) +
  scale_y_continuous(limits = c(0, quantile(diamonds$price, 0.99))) +
  labs(title = "diamond price vs. carats", x = "carats", y = "price") 
ggsave("diamond price vs carats.png")

#volume
diamonds$volume <- with(diamonds, x * y * z)

#price vs vol scatter
ggplot(diamonds, aes(volume, price)) + geom_point()

#price & 0 < volume < 800 cor
with(subset(diamonds, volume != 0 & volume < 800), cor(volume, price))

#refined price vs vol scatter
ggplot(data = subset(diamonds, volume != 0 & volume < 800), aes(volume, price)) + 
  geom_point(alpha = 0.05, color ="blue") +
  geom_smooth(method = "lm", color = "red")
ggsave("Refined price by volume.png")
#data appears to be exponential

#new diamondsByClarity dataframe
diamondsByClarity <- diamonds %>% 
  group_by(clarity) %>% 
  summarise(mean_price = mean(price),
            median_price = median(price),
            min_price = min(price),
            max_price = max(price),
            n = n())
diamondsByClarity

#2 bar graphs of diamond mean price by clarity and color
diamonds_by_clarity <- group_by(diamonds, clarity)
diamonds_mp_by_clarity <- summarise(diamonds_by_clarity, mean_price = mean(price))

diamonds_by_color <- group_by(diamonds, color)
diamonds_mp_by_color <- summarise(diamonds_by_color, mean_price = mean(price))

bgMPcla <- ggplot(diamonds_mp_by_clarity, aes(clarity, mean_price)) +
  geom_bar(stat = "identity") +
  labs(title = "diamonds mean price by clarity")

bgMPcol <- ggplot(diamonds_mp_by_color, aes(color, mean_price)) +
  geom_bar(stat = "identity") +
  labs(title = "diamonds mean price by color")

grid.arrange(bgMPcla,bgMPcol)

