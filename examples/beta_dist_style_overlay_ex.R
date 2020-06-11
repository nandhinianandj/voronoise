 #Setup
options(scipen=999)  # turn off scientific notation like 1e+06
library(ggplot2)
library(voronoise)
library(stringr)
data("midwest", package = "ggplot2")  # load the data
# midwest <- read.csv("http://goo.gl/G1K41K") # alt source

# Init Ggplot

circle <- voronoise::entity_circle()
heart <- voronoise::entity_heart()
beta <- voronoise::entity_beta(shape1=1.1, shape2=1.2)
p <- ggplot() +  theme_void() +
     geom_text(aes(0,0,label='N/A')) +
     xlab(NULL)

p <- voronoise::style_overlay(p, border="black", fill="cyan", data=beta)

today <- as.character(Sys.Date())
time <- as.character(Sys.time())
fname <- str_c('beta_dist_', today, time, '.png')
# save the file
ggplot2::ggsave(
  filename = str_c("creations/", fname),
  plot = p,
  device = "png",
  width = 100/3,
  height = 100/3,
  dpi = 150
)
