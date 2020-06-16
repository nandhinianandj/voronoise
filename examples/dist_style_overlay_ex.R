 #Setup
options(scipen=999)  # turn off scientific notation like 1e+06
library(ggplot2)
library(voronoise)
library(stringr)

# Init Ggplot
grainsize <- 10000
circle <- voronoise::entity_circle(grain=grainsize)
heart <- voronoise::entity_heart(grain=grainsize)

beta <- voronoise::entity_beta(grain=grainsize, shape1=1.1, shape2=1.2)
cauchy <- voronoise::entity_cauchy(grain=grainsize, location=0.4, scale=08)
hypergeom <- voronoise::entity_hypergeometric(grain=grainsize, m=120, n=380, k=10)
weibull <- voronoise::entity_weibull(grain=grainsize, shape=1, scale=1)

create_overlay_pic <- function (dtype) {
	p <- ggplot() +  theme_void() +
	     geom_text(aes(0,0,label='N/A')) +
	     xlab(NULL)

	today <- as.character(Sys.Date())
	time <- as.character(Sys.time())

	if (dtype == 'beta') {
		p <- voronoise::style_overlay(p, border="black", fill="cyan", data=beta)
		fname <- str_c('beta_dist_', today,  '.png')
	} else if (dtype == 'cauchy') {
		p <- voronoise::style_overlay(p, border="black", fill="cyan", data=cauchy)
		fname <- str_c('cauchy_dist_', today, '.png')
	} else if (dtype == 'hypergeom') {
		p <- voronoise::style_overlay(p, border="black", fill="cyan", data=hypergeom)
		fname <- str_c('hypergeometry_dist_', today, '.png')
	} else if (dtype=='weibull'){
		p <- voronoise::style_overlay(p, border="black", fill="cyan", data=weibull)
		fname <- str_c('weibull_dist_', today, '.png')
	} else if (dtype=='heart') {
		p <- voronoise::style_overlay(p, border="black", fill="cyan", data=heart)
		fname <- str_c('heart__', today, '.png')
	} else {
		p <- voronoise::style_overlay(p, border="black", fill="cyan", data=circle)
		fname <- str_c('circle__', today, '.png')
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
#create_overlay_pic('heart')
#gc()
create_overlay_pic('circle')
gc()
create_overlay_pic('weibull')
gc()
create_overlay_pic('hypergeom')
gc()
create_overlay_pic('beta')
gc()
create_overlay_pic('cauchy')

