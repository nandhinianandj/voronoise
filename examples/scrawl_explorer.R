library(ambient)
library(hash)
library(stringr)
library(voronoise)
library(dqsample)

options(bitmapType="cairo") # Device change to support semi-transparency
possible_paths <- seq(30000,60000, by=5000)
possible_steps <- 20:25

possible_pals <- c('acton', 'bamako', 'batlow', 'berlin', 'bilbao', 'broc', 'brocO', 'buda', 'cork', 'corkO', 'davos', 'devon', 'grayC', 'hawaii', 'imola', 'lajolla', 'lapaz', 'lisbon', 'nuuk', 'oleron', 'oslo', 'roma', 'romaO', 'tofino', 'tokyo', 'turku', 'vik', 'vikO')
combo <- expand.grid(possible_paths, possible_steps, 1:length(possible_pals))

generators <- c(ambient::gen_worley, ambient::gen_value, ambient::gen_white, ambient::gen_cubic, ambient::gen_perlin, ambient::gen_checkerboard, ambient::gen_waves, ambient::gen_simplex)
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



scrawl_draw <- function(path, step, palette=25, gen=ambient::gen_worley) {
	scrawl <- voronoise::scrawl_build(seed=1, n_paths=path, n_steps=step, sz_step=50, sz_slip=5, gen=gen)

	time <- as.character(Sys.time())
	fname <- str_c('./creations/', 'scrawl_', time, '.jpeg')

	#img <- voronoise::style_overlay(scrawl, fill="pink", data=dists$dist)
	plot <- scrawl_plot(scrawl, palette=possible_pals[palette])
	voronoise::scrawl_save(plot, fname, pixels=5000)

}

#sapply(possible_paths, function(x) sapply(possible_steps, function(y) scrawl_draw(x, y, 24)))

for (row  in 1:nrow(combo)){
	#for (dist in ls(dists)) {
		time <- combo[row, 'Var1']
		split <- combo[row, 'Var2']
		palette <- combo[row, 'Var3']
		#generator <- dqsample::sample(generators)
		generator <-  ambient::gen_worley
		scrawl_draw(time, split, palette, gen=generator)
		gc()
	#}
}
