#IMPROVE PLOTS AND IMAGES RESOLUTION
#Let's make a simple graph
x <- 1:100
y <- 1:100

#R by default export images at 72dpi
tiff("Plot1.tiff")
plot(x, y) # Make plot
dev.off()

#When we try to change resolution to 300dpi margins are too big
tiff("Plot2.tiff", res = 300)
plot(x, y) # Make plot
dev.off()

#we need to determine margins, and then we can get a image with better resolution
tiff("Plot3.tiff", width = 4, height = 4, units = 'in', res = 300)
plot(x, y) # Make plot
dev.off()

#We can do the same with png and bigger resolutions
png("Plot4.png", width = 4, height = 4, units = 'in', res = 600)
plot(x, y) # Make plot
dev.off()


