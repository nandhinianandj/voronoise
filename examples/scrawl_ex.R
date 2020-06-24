library(voronoise)
library(stringr)
options(bitmapType="cairo")

heart <- voronoise::entity_heart(seed=use_seed(1), xpos=xpos, ypos=ypos)

droplet <- voronoise::entity_droplet(seed=use_seed(1), xpos=xpos, ypos=ypos)

beta <- voronoise::entity_beta(grain=5000, shape1=1.1, shape2=1.2, xpos=xpos, ypos=ypos)

scrawl <- voronoise::scrawl_build(seed=1, n_paths=10000, n_steps=10,sz_step=50, sz_slip=5)

time <- as.character(Sys.time())
fname <- str_c('./creations/', 'scrawl_', time, '.jpeg')

img <- voronoise::style_overlay(scrawl, fill="pink", data=dists$dist)
plot <- scrawl_plot(scrawl, palette='eo')
voronoise::scrawl_save(plot, fname, pixels=5000)
