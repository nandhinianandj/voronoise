library(voronoise)
library(stringr)
library(dplyr)
library(viridis)
options(bitmapType="cairo") # Device change to support semi-transparency
filled_voronoise_trees <- function () {
	set.seed(1)

	dat <- tibble::tibble(
	  x = runif(n = 5000, min = .1, max = .9),
	  y = runif(n = 5000, min = .1, max = .9),
	  shade = sample(colours(), size = 5000, replace = TRUE)
	)

	base <- ggplot(
	  data = dat,
	  mapping = aes(x, y, fill = shade, group = 1)
	) +
	  scale_fill_identity() +
	  scale_x_continuous(expand = c(0, 0)) +
	  scale_y_continuous(expand = c(0, 0))
	  #theme_mono("pink") +
	  #coord_square()


	perturb_fall <- function(data) {
	  data %>%
	    group_by(group) %>%
	    mutate(
	      y = y - rbeta(1, 2, 1.1) * min(y)
	      #y = y - rgamma(1, 0.1, 1.1) * min(y)
	      #y = y - rnorm(1, 1.75 , 1.1) * min(y)
	    ) %>%
	    ungroup()
	}

	base +
	  geom_voronoise(fill = "pink") +
	  geom_voronoise(
	    mapping = aes(fill = shade),
	    perturb = perturb_fall
	  )

	today <- as.character(Sys.Date())
	time <- as.character(Sys.time())
	fname <- str_c('creations/','filled_voronoi_trees_', today, '_', time, '.png')
	ggsave(fname)

}

voronoise_blobs <- function () {

	set.seed(1)
	data <- voronoise_data(10000, viridis::magma(100))
	voronoise_base(
	  data = data, #unfold_meander(data=data),
	  background = "pink1"
	) +
	  geom_voronoise(fill = "cyan") +
	  geom_voronoise(
	    perturb = perturb_float(
	    #perturb = perturb_uniform(
	      angles = 120,
	      noise = seq(5, 20, by=1)
	    )
	  )

	today <- as.character(Sys.Date())
	time <- as.character(Sys.time())
	fname <- str_c('creations/', 'voronoise_blobs_', today, time, '.png')
}


jasmine_scenes <- function (){
	use_seed(1) %>%
	  scene_discs(
	    rings = 3, points = 10000, size = 5
	  ) %>%
	  mutate(ind = 1:n()) %>%
	  unfold_warp(
	    iterations = 1,
	    scale = .5,
	    output = "layer"
	  ) %>%
	  unfold_tempest(
	    iterations = 20,
	    scale = .01
	  ) %>%
	  style_ribbon(
	    palette = palette_named("vik"),
	    colour = "ind",
	    alpha = c(.1,.1),
	    background = "oldlace"
	  )
	}

stoneskip <- function () {
	today <- as.character(Sys.Date())
	time <- as.character(Sys.time())
	fname <- str_c('stoneskip_', today, '_', time, '.png')

	voronoise::stoneskip(seed=1, grains=10000, pixels=10000, shades=500,
			     filename=str_c("creations/", fname))
				#pal_which="viridis::inferno")
}



flametree_ex <- function () {
	dat <- voronoise::flametree_grow(seed = 10, time = 35,
					 angle=seq(30, 150, by=1), scale = c(0.9,0.95),
					 prune=0.1, split=5) # data structure
	img <- voronoise::flametree_plot(tree = dat)          # ggplot object
	p <- plot(img)


	today <- as.character(Sys.Date())
	time <- as.character(Sys.time())
	fname <- str_c('flametree_vtree_', today, time, '.png')
	# save the file
	voronoise::export_image(
	  filename = str_c("creations/", fname),
	  input = p,
	  width = 100/3,
	  height = 100/3,
	  dpi = 150
	)
}

voronoi_tree_ex <- function () {

	# set seed
	seed <- 1
	set.seed(seed)

	# the "flametree" itself
	ftree <- voronoise::flametree_grow(
	  seed = seed,
	  time = 35,
	  angle = c(-2:4) * 10,
	  scale = c(.6, .8, .9)
	)

	# "leaf" coordinates are at terminal locations (id_step = 2)
	# on the terminal branches (id_leaf == TRUE) in the tree
	vleaf <- ftree %>% filter(id_leaf == TRUE, id_step == 2)

	# a simple "perturb" function: drop each leaf to the ground
	leaf_fall <- function(data) {
	  data %>%
	    group_by(group) %>%
	    mutate(y = y - min(y)) %>%
	    ungroup()
	}


	# create the plot...
	p <- ggplot() +

	  # tree trunk is drawn using geom_bezier from the
	  # ggforce package (loaded by voronoise)
	  geom_bezier(
	    data = ftree,
	    mapping = aes(
	      x = coord_x,
	      y = coord_y,
	      group = id_path,
	      size = seg_wid
	    ),
	    lineend = "round",
	    show.legend = FALSE
	  ) +

	  # add points drawn at the leaves
	  geom_point(
	    data = vleaf,
	    mapping = aes(
	      x = coord_x,
	      y = coord_y
	    ),
	    size = 8
	  ) +

	  # add voronoi tiles with no perturbation
	  geom_voronoise(
	    data = vleaf,
	    mapping = aes(
	      x = coord_x,
	      y = coord_y
	    ),
	    max.radius = .2,
	    fill = "#ffffffcc",
	    colour = "black",
	    size = 2
	  ) +

	  # add voronoi tiles for falling leaves
	  geom_voronoise(
	    data = vleaf,
	    mapping = aes(
	      x = coord_x,
	      y = coord_y
	    ),
	    max.radius = .2,
	    fill = "#ffffffcc",
	    colour = "black",
	    size = 2,
	    perturb = leaf_fall
	  ) +

	  # styling
	  theme_void() +
	  coord_equal()


	today <- as.character(Sys.Date())
	time <- as.character(Sys.time())
	fname <- str_c('vtree_', today, time, '.jpeg')
	# save the file
	ggsave(
	  filename = str_c("creations/", fname),
	  plot = p,
	  width = 100/3,
	  height = 100/3,
	  dpi = 150
	)

}

scope_ex <- function() {
	dat <- scope_grid(
  		border = scope_hex(),
  		inner = scope_texture(
  		  noise = ambient::gen_simplex,
  		  frequency = 50
  		),
  		outer = scope_texture(
  		  noise = ambient::gen_simplex,
  		  frequency = 10
  		)
        )
	dat
	p<-voronoise::scope_plot(dat)
	today <- as.character(Sys.Date())
	time <- as.character(Sys.time())
	fname <- str_c('scope_', today, time, '.jpeg')
	# save the file
	ggsave(
	  filename = str_c("creations/", fname),
	  plot = p,
	  width = 100/3,
	  height = 100/3,
	  dpi = 150
	)

}

filled_voronoise_trees()
gc()
voronoise_blobs()
gc()
#stoneskip()
#gc()
flametree_ex()
gc()
voronoi_tree_ex()
gc()
jasmine_scenes()
