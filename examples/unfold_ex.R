library(voronoise)
library(dplyr)
library(stringr)
library(ggplot2)

dat <- voronoise::unfold_slice(iterations = 20)
dat <- dplyr::rename(dat, coord_x=x, coord_y=y, id_path=id)
dat <- tibble::add_column(dat, seg_wid=5000, seg_col=5000)
background <- "pink"
palette <- "viridis::inferno"


# specify the mapping
mapping <- ggplot2::aes(
	x = coord_x,      # x-coordinate
	y = coord_y,      # y-coordinate
	group = id_path,  # each segment/path is a single bezier curve
	size = seg_wid,   # the seg_wid variable is used to set line width
	color = seg_col   # the seg_col variable is used to set line colour
)

# build the ggplot
picture <- ggplot2::ggplot(data = dat, mapping = mapping) +
	ggforce::geom_bezier2(show.legend = FALSE, lineend = "round") +
	paletteer::scale_color_paletteer_c(palette = palette) #+
	#theme_mono( background)

p <- plot(picture)



today <- as.character(Sys.Date())
time <- as.character(Sys.time())
fname <- str_c('unfold_meander_', today, time, '.png')
# save the file
ggsave(
  filename = str_c("./creations/", fname),
  plot = p,
  width = 100/3,
  height = 100/3,
  dpi = 150
)
