####
# some attractors in R - Pete Werner, 2013
#
# this is a much faster version thanks to Henrik Bengtsson
#
# you can see the original here https://github.com/petewerner/misc/blob/master/attractor-orig.R
# The original post has the suggestions/contributions in this script
# http://petewerner.blogspot.com.au/2013/11/just-for-fun-attractors-in-r.html
#
# More info on attractors:
#
# http://paulbourke.net/fractals/peterdejong/
# http://paulbourke.net/fractals/clifford/
####

library("compiler")

mapxy <- function(x, y, xmin, xmax, ymin=xmin, ymax=xmax) {
	sx <- (width - 1) / (xmax - xmin)
	sy <- (height - 1) / (ymax - ymin)
	row0 <- round( sx * (x - xmin) )
	col0 <- round( sy * (y - ymin) )
	col0 * height + row0 + 1
}

dejong <- function(x, y) {
	nidxs <- length(mat)
	counts <- integer(length=nidxs)
	for (i in 1:npoints) {
		xt <- sin(a * y) - cos(b * x)
		y <- sin(c * x) - cos(d * y)
		x <- xt
		idxs <- mapxy(x, y, -2, 2)
		counts <- counts + tabulate(idxs, nbins=nidxs)
	}
	mat <<- mat + counts
}

clifford <- function(x, y) {
	ac <- abs(c)+1
	ad <- abs(d)+1
	nidxs <- length(mat)
	counts <- integer(length=nidxs)
	for (i in 1:npoints) {
		xt <- sin(a * y) + c * cos(a * x)
		y <- sin(b * x) + d * cos(b * y)
		x <- xt
		idxs <- mapxy(x, y, -ac, ac, -ad, ad)
		counts <- counts + tabulate(idxs, nbins=nidxs)
	}
	mat <<- mat + counts
}

#color vector
cvec <- grey(seq(0, 1, length=10))
#can also try other colours, see help(rainbow)
#cvec <- heat.colors(10)

#we end up with npoints * n points
npoints <- 8
n <- 100000 
width <- 600
height <- 600

#make some random points
rsamp <- matrix(runif(n * 2, min=-2, max=2), nr=n)

#compile the functions
setCompilerOptions(suppressAll=TRUE)
mapxy <- cmpfun(mapxy)
dejong <- cmpfun(dejong)
clifford <- cmpfun(clifford)

#dejong
a <- 1.4
b <- -2.3
c <- 2.4
d <- -2.1

mat <- matrix(0, nr=height, nc=width)
dejong(rsamp[,1], rsamp[,2])

#this applies some smoothing of low valued points, from A.N. Spiess
#QUANT <- quantile(mat, 0.5)
#mat[mat <= QUANT] <- 0

dens <- log(mat + 1)/log(max(mat))
par(mar=c(0, 0, 0, 0))
image(t(dens), col=cvec, useRaster=T, xaxt='n', yaxt='n')

#clifford
a <- -1.4
b <- 1.6
c <- 1.0
d <- 0.7

mat <- matrix(0, nr=height, nc=width)
#QUANT <- quantile(mat, 0.5)
#mat[mat <= QUANT] <- 0
clifford(rsamp[,1], rsamp[,2])

dens <- log(mat + 1)/log(max(mat))
par(mar=c(0, 0, 0, 0))
image(t(dens), col=cvec, useRaster=T, xaxt='n', yaxt='n')

