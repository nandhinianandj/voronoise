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

beta <- voronoise::entity_beta(grain=500, shape1=1.1, shape2=1.2)
cauchy <- voronoise::entity_cauchy(grain=500, location=0, scale=1)
hypergeom <- voronoise::entity_hypergeometric(grain=500, m=120, n=380, k=10)
weibull <- voronoise::entity_weibull(grain=500, shape=1, scale=1)

createpic <- function (dtype) {
	p <- ggplot() +  theme_void() +
	     geom_text(aes(0,0,label='N/A')) +
	     xlab(NULL)

	today <- as.character(Sys.Date())
	time <- as.character(Sys.time())

	if (dtype == 'beta') {
		p <- voronoise::style_overlay(p, border="black", fill="cyan", data=beta)
		fname <- str_c('beta_dist_', today, time, '.png')
	} else if (dtype == 'cauchy') {
		p <- voronoise::style_overlay(p, border="black", fill="cyan", data=cauchy)
		fname <- str_c('cauchy_dist_', today, time, '.png')
	} else if (dtype == 'hypergeom') {
		p <- voronoise::style_overlay(p, border="black", fill="cyan", data=hypergeom)
		fname <- str_c('hypergeometry_dist_', today, time, '.png')
	} else {
		p <- voronoise::style_overlay(p, border="black", fill="cyan", data=weibull)
		fname <- str_c('weibull_dist_', today, time, '.png')
	}

	# save the file
	ggplot2::ggsave(
	  filename = str_c("creations/", fname),
	  plot = p,
	  device = "png",
	  width = 100/3,
	  height = 100/3,
	  dpi = 150
	)

}

createpic('weibull')
createpic('hypergeom')
createpic('beta')
createpic('cauchy')

