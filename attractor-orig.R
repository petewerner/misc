####
# some attractors in R - Pete Werner, 2013
####
# post here: http://petewerner.blogspot.com/2013/11/just-for-fun-attractors-in-r.html
#
# faster vers: https://github.com/petewerner/misc/blob/master/attractor.R
#
# see also:
# http://paulbourke.net/fractals/peterdejong/
# http://paulbourke.net/fractals/clifford/
####

map <- function(x, imin, imax, omin, omax) {
	return( (x - imin) / (imax - imin) * (omax - omin) + omin )
}

dejong <- function(x, y) {
	for (i in 1:npoints) {
		xn <- sin(a * y) - cos(b * x)
		yn <- sin(c * x) - cos(d * y)
		row <- round(map(xn, -2, 2, 1, width))
		col <- round(map(yn, -2, 2, 1, height))
		mat[row,col] <<- mat[row,col] + 1
		x <- xn
		y <- yn
	}
}

clifford <- function(x, y) {
	for (i in 1:npoints) {
		xn <- sin(a * y) + c * cos(a * x)
		yn <- sin(b * x) + d * cos(b * y)
		row <- round(map(xn, -abs(c) - 1, abs(c) + 1, 1, width))
		col <- round(map(yn, -abs(d) - 1, abs(d) + 1, 1, height))
		mat[row,col] <<- mat[row,col] + 1
		x <- xn
		y <- yn
	}
}

cvec <- grey(seq(0, 1, length=10))

#we end up with npoints * n points
npoints <- 8
n <- 100000 
width <- 600
height <- 600

#make some random points
rsamp <- matrix(runif(n * 2, min=-2, max=2), nr=n)

#dejong
a <- 1.4
b <- -2.3
c <- 2.4
d <- -2.1

mat <- matrix(0, nr=height, nc=width)
#these can be slow with a lot of points
system.time(xx <- apply(rsamp, 1, function(x) dejong(x[1], x[2])))
dens <- log(mat + 1)/round(log(max(mat))) 
par(mar=c(0, 0, 0, 0))
image(t(dens), col=cvec, useRaster=T, xaxt='n', yaxt='n')

#clifford
a <- -1.4
b <- 1.6
c <- 1.0
d <- 0.7

mat <- matrix(0, nr=height, nc=width)
system.time(xx <- apply(rsamp, 1, function(x) clifford(x[1], x[2])))
dens <- log(mat + 1)/round(log(max(mat))) 
par(mar=c(0, 0, 0, 0))
image(t(dens), col=cvec, useRaster=T, xaxt='n', yaxt='n')

