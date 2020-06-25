library(voronoise)
library(stringr)
library(ggplot2)
library(dplyr)
library(hash)
library(foreach)
library(doParallel)

numCores <- 4
registerDoParallel(numCores)

possible_times <- 1:5
possible_splits <- 2:3
combo <- expand.grid(possible_times, possible_splits)



## Entities for style  overlay/ribbon
xpos <- 0
ypos <- 0

circle <- voronoise::entity_circle(seed=use_seed(1), xpos=xpos, ypos=ypos)
heart <- voronoise::entity_heart(seed=use_seed(1), xpos=xpos, ypos=ypos)
droplet <- voronoise::entity_droplet(seed=use_seed(1), xpos=xpos, ypos=ypos)

beta <- voronoise::entity_beta(grain=5000, shape1=1.1, shape2=1.2, xpos=xpos, ypos=ypos)
cauchy <- voronoise::entity_cauchy(grain=5000, location=0.4, scale=08, xpos=xpos, ypos=ypos)
hypergeom <- voronoise::entity_hypergeometric(grain=5000, m=120, n=380, k=10, xpos=xpos, ypos=ypos)
weibull <- voronoise::entity_weibull(grain=5000, shape=1, scale=1)

dists <- hash()
dists['circle'] = circle
dists['heart'] = heart
dists['droplet'] = droplet
dists['beta'] = beta
dists['cauchy'] = cauchy
dists['hypergeom'] = hypergeom
dists['weibull'] = weibull

create_flametree <- function (time, split) { #time, split, dist) {
	#time <- comb[1, 'Var1']
	#split <- comb[1, 'Var2']
	print(time)
	print(split)
	dat <- flametree_grow(seed = 5005, time = time, angle=seq(0, 120, by=1),
		      scale = c(0.9, 0.95), prune=0.15, split=split) # data structure

	img <- flametree_plot(tree = dat, background="pink", palette="viridis::inferno") # ggplot object
	#overlay <- sample(c('beta', 'cauchy', 'hypergeom', 'weibull'), 1)
	img <- voronoise::style_overlay(img, fill="pink", data=beta)


	today <- as.character(Sys.Date())
	time <- as.character(Sys.time())
	fname <- str_c('flametree_vtree_', today, time, '.jpeg')
	# save the file
	ggsave(
	  filename = str_c("./creations/", fname),
	  plot = img,
	  width = 100/3,
	  height = 100/3,
	  dpi = 150
	)
}
sapply(possible_times, function(x) sapply(possible_splits, function(y) create_flametree(x, y)))
#foreach(comb= combo) %dopar% {
# print(comb)
# create_flametree(comb)
# gc()
#}
#for (row  in 1:nrow(combo)){
#	for (dist in ls(dists)) {
#		time <- combo[row, 'Var1']
#		split <- combo[row, 'Var2']
#		create_flametree(time, split, dist=dists[dist])
#		gc()
#	}
#}
