library(voronoise)
banner <- voronoise::make_banner()

today <- as.character(Sys.Date())
time <- as.character(Sys.time())
fname <- str_c('creations/','rainbow_banner_', today, '_', time, '.png')

plot <- ggplot(banner)
ggsave(fname, plot=p)
