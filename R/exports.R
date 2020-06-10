# the three user-facing functions of the package

#' Generate the data specifying a flametree
#'
#' @param seed Integer-valued seed for the random number generator
#' @param time Number of generations to run the iterative process
#' @param scale Vector of possible values for the "size rescaling" at each iteration
#' @param angle Vector of possible angle changes (in degrees) at each iteration
#' @param split Maximum number of shoots to generate from each tip at each iteration
#' @param prune Probability with which a generated shoot is pruned
#'
#' @return A tibble with the following columns: coord_x, coord_y, seg_deg,
#' seg_len, seg_col, seg_wid, id_time, id_path, id_step, id_leaf
#'
#' The two "coord" columns
#' specify the locations of a point. The "id" columns uniquely identify each
#' point: id_time specifies the generation, id_path identifies each segment, and
#' id_step contains the three values (0, 1 or 2) for the points that define each
#' segment. The segments consist of two end points (0 and 2) and one "control"
#' point (1) that is used to define a Bezier curve.
#'
#' The three "seg" columns provide summary information about each segment:
#' seg_len is the length of the segment, seg_col is a value used to colour
#' the segment, and seg_wid is a size parameter used to define the width of
#' the segment
#' @export
flametree_grow <- function(seed = 286,
                           time = 6,
                           scale = c(.8, .9),
                           angle = c( -10, 10),
                           split = 2,
			   startx = 0,
			   starty = 0,
                           prune = 0 ) {

  # parameters defining the tree
  param <- list(
    seed = seed,    # seed for the RNG
    time = time,    # time (iterations) to grow the tree
    scale = scale,  # possible values for rescaling at each time
    angle = angle,  # possible values for redirect at each time
    split = split,  # number of new shoots from each old shoot at each time
    prune = prune   # probability of immediately pruning a new shoot
  )

  # set the seed for the random number generator
  set.seed(param$seed)

  # growing the tree is a 3-step process
  tree <- grow_sapling(startx, starty) %>%  # sapling is the first segment
    grow_tree(param) %>%      # grow the tree with
    shape_tree()

  # add the leaf indicator
  tree$id_leaf <- tree$id_time == max(tree$id_time)

  return(tree)
}



#' Create a plot from a flametree data frame
#'
#' @param tree The data frame specifying the flametree
#' @param background The background colour of the image
#' @param palette A palette specification used by the paletteer package
#'
#' @return The output is ggplot2 object plots the coord_x and coord_y values
#' that define each segment as a bezier curve. To map each segment to its own
#' curve, the group aesthetic is id_path, and the geom is the geom_bezier2()
#' function in the ggforce package. The color aesthetic is mapped to seg_col,
#' and the size aesthetic is mapped to seg_wid.
#'
#' The background colour can be set using the "backgroud" argument, and the
#' palette used to colour the segments is generated using the
#' scale_color_paletteer_c() function from the paletteer package. To select
#' the palette, the "palette" argument must take the form of a palette
#' specification understood by paletteer.
#' @export
flametree_plot <- function(tree,
                           background = "antiquewhite4",
                           palette = "viridis::inferno") {

  # specify the mapping
  mapping <- ggplot2::aes(
    x = coord_x,      # x-coordinate
    y = coord_y,      # y-coordinate
    group = id_path,  # each segment/path is a single bezier curve
    size = seg_wid,   # the seg_wid variable is used to set line width
    color = seg_col   # the seg_col variable is used to set line colour
  )

  # build the ggplot
  picture <- ggplot2::ggplot(data = tree, mapping = mapping) +
    ggforce::geom_bezier2(show.legend = FALSE, lineend = "round") +
    paletteer::scale_color_paletteer_c(palette = palette) +
    theme_mono( background)

  return(picture)
}



#' Save a flametree image
#'
#' @param plot The ggplot object to save
#' @param filename The path to file to be saved
#' @param pixels The height and width of the image in pixels
#' @param ... Other arguments passed to ggsave
#'
#' @details This function is just a wrapper to ggsave. It's not strictly
#' necessary but I find it convenient to override the default image size.
#'
#' @export
flametree_save <- function(plot, filename, pixels = 5000, ...) {

  ggplot2::ggsave(
    filename = filename,
    plot = plot,
    width = pixels/300,
    height = pixels/300,
    dpi = 300,
    ...
  )
}


## Stone skip stuff

#' Generate and write a stoneskip image
#'
#' @param seed integer valued seed for the RNG state
#' @param filename path to image (if NULL, defaults to "stoneskip_x.png" where x is the seed)
#' @param grains size of the grid used for the automaton
#' @param pixels width and height of the final image in pixels
#' @param shades number of distinct colours in the image
#'
#' @return invisibly returns a matrix
#' @export
stoneskip <- function(seed = 1, filename = NULL, grains = 1000,
		      pixels = 5000, shades = 1000, pal_which="viridis::inferno") {

  set.seed(seed)

  # import the C++ code
  Rcpp::sourceCpp("src/code.cpp")

  # where to save the file
  if(is.null(filename)) {
    filename <- paste0("stoneskip_", seed, ".png")
  }
  str(pal_which)
  # palette is sampled randomly
  if (is.null(pal_which)) {
	  pal_names <- paletteer::palettes_c_names
	  pal_index <- dqsample::sample(nrow(pal_names), 1)
	  pal_which <- paste0(
	    pal_names$package[pal_index], "::", pal_names$palette[pal_index]
	  )
  }
  palette <- paletteer::paletteer_c(pal_which, n = shades)

  fractals <- list(
    ambient::billow,
    ambient::fbm,
    ambient::ridged
  )

  generators <- list(
    ambient::gen_simplex,
    ambient::gen_worley
  )

  sample_list <- function(...) {
    (sample(...))[[1]]
  }

  # create the base image using ambient -------------------------------------

  grid <- ambient::long_grid(
    x = seq(0, 1, length.out = grains),
    y = seq(0, 1, length.out = grains)
  )

  base <- ambient::fracture(
    noise     = sample_list(x = generators, size = 1),
    fractal   = sample_list(x = fractals, size = 1),
    octaves   = sample(x = 1:10, size = 1),
    frequency = sample(x = 1:10, size = 1),
    value     = "distance2",
    seed      = seed,
    x         = grid$x,
    y         = grid$y
  )

  # run cellular automaton over base image ----------------------------------

  cat("running stepping stone automaton...\n")
  iterations <- sample(x = 50:200, size = 1)
  input <- round(ambient::normalise(base, to = c(1, shades)))
  input <- matrix(as.integer(input), grains, grains)
  output <- step_stone(input, iterations)


  # write to an image file --------------------------------------------------

  cat("rendering image...\n")
  rast <- grDevices::as.raster(matrix(palette[output], grains, grains))
  grDevices::png(filename, pixels, pixels)
  op <- graphics::par(mar=c(0,0,0,0))
  #plot(rast)
  grDevices::dev.off()
  graphics::par(op)


  # invisibly return the output matrix --------------------------------------
  return(invisible(output))

}
