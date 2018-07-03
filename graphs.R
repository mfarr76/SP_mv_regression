rm(list = ls())


# Library
library(dygraphs)
library(xts)  

# Create data
trend=sin(seq(1,41))+runif(41)
data=data.frame(time=seq(from=Sys.Date()-40, to=Sys.Date(), by=1 ), trend=trend, max=trend+abs(rnorm(41)), min=trend-abs(rnorm(41, sd=1)))
data=xts(x = data[,-1], order.by = data$time)

# Plot
dygraph(data) %>%
  dySeries(c("min", "trend", "max"))


# Create data (needs 4 data points per date stamp)
trend=sin(seq(1,41))+runif(41)
data=data.frame(time=seq(from=Sys.Date()-40, to=Sys.Date(), by=1 ), value1=trend, value2=trend+rnorm(41), value3=trend+rnorm(41), value4=trend+rnorm(41) )
data=xts(x = data[,-1], order.by = data$time)

# Plot it
dygraph(data) %>%
  dyCandlestick()

dygraph(data) %>%
  dyOptions( stemPlot=TRUE)

##lollipop===========================================================

# Library
library(tidyverse)

# Create data
value1=abs(rnorm(26))*2
data=data.frame(x=LETTERS[1:26], value1=value1, value2=value1+1+rnorm(26, sd=1) )

# Reorder data using average?
data = data %>% rowwise() %>% mutate( mymean = mean(c(value1,value2) )) %>% arrange(mymean) %>% mutate(x=factor(x, x))

# plot
ggplot(data) +
  geom_segment( aes(x=x, xend=x, y=value1, yend=value2), color="grey") +
  geom_point( aes(x=x, y=value1), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
  geom_point( aes(x=x, y=value2), color=rgb(0.7,0.2,0.1,0.5), size=3 ) +
  coord_flip() 

# With a bit more style
ggplot(data) +
  geom_segment( aes(x=x, xend=x, y=value1, yend=value2), color="grey") +
  geom_point( aes(x=x, y=value1), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
  geom_point( aes(x=x, y=value2), color=rgb(0.7,0.2,0.1,0.5), size=3 ) +
  coord_flip()+
  theme_light() +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
  ) +
  xlab("") +
  ylab("Value of Y")

