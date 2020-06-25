library(voronoise)
library(stringr)
options(bitmapType="cairo")

xpos <- 0
ypos <- 0

heart <- voronoise::entity_heart(seed=use_seed(1), xpos=xpos, ypos=ypos)

droplet <- voronoise::entity_droplet(seed=use_seed(1), xpos=xpos, ypos=ypos)

beta <- voronoise::entity_beta(grain=5000, shape1=1.1, shape2=1.2, xpos=xpos, ypos=ypos)

scrawl <- voronoise::scrawl_build(seed=1, n_paths=10000, n_steps=30,sz_step=50, sz_slip=5)


time <- as.character(Sys.time())
fname <- str_c('./creations/', 'scrawl_droplet_', time, '.jpeg')

print(typeof(droplet))
plot <- scrawl_plot(scrawl, palette='tokyo')
img <- voronoise::style_overlay(plot, border="black", fill="tokyo", data=droplet)
voronoise::scrawl_save(plot, fname, pixels=5000)
