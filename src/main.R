require('mclust')
require('ggplot2')
source('~/projects/fftiers/src/ff-functions.R')

### Parameters 

thisweek = 9
download = TRUE		# Do we want to download fresh data from fantasypros?
useold = FALSE		# Do we want to use the original version of the charts?

### Set and create input / output directories

mkdir <- function(dir) system(paste("mkdir -p", dir))
datdir = "~/projects/fftiers/dat/2014/"; mkdir(datdir)
outputdir = paste("~/projects/fftiers/out/week", thisweek, "/", sep=""); mkdir(outputdir)
outputdircsv = paste("~/projects/fftiers/out/week", thisweek, "/csv/", sep=""); mkdir(outputdircsv)
outputdirpng = paste("~/projects/fftiers/out/week", thisweek, "/png/", sep=""); mkdir(outputdirpng)
outputdirtxt = paste("~/projects/fftiers/out/week", thisweek, "/txt/", sep=""); mkdir(outputdirtxt)

### Curl data from fantasypros. Which positions do we want to fetch?

download.data(c('qb','rb','wr','te'))
download.data(c('flex','k','dst'))
download.data(c('ppr-rb','ppr-wr','ppr-te','ppr-flex'))
download.data(c('half-point-ppr-rb','half-point-ppr-wr','half-point-ppr-te','half-point-ppr-flex'))
download.data(c('ros-qb','ros-rb','ros-wr','ros-te'))
download.data(c('ros-flex','ros-k','ros-dst'))
download.data(c('ros-ppr-rb','ros-ppr-wr','ros-ppr-te','ros-ppr-flex'))

## If there are any injured players, list them here to remove them
injured <- c('')

## Weekly
draw.tiers("qb", 1, 26, 8, highcolor=360)
draw.tiers("rb", 1, 40, 10, highcolor=400)
draw.tiers("wr", 1, 60, 12, highcolor=500, XLOW=5)
draw.tiers("te", 1, 25, 6, XLOW=5)
draw.tiers("flex", 1, 80, 14, XLOW=5, highcolor=650)
draw.tiers("k", 1, 26, 5, XLOW=5)
draw.tiers("dst", 1, 26, 6, XLOW=5)

draw.tiers("ppr-rb", 1, 40, 11)
draw.tiers("ppr-wr", 1, 60, 9, highcolor=500)
draw.tiers("ppr-te", 1, 30, 8)
draw.tiers("ppr-flex", 1, 80, 14, XLOW=5, highcolor=650)

draw.tiers("half-point-ppr-rb", 1, 40, 9)
draw.tiers("half-point-ppr-wr", 1, 60, 10, highcolor=400)
draw.tiers("half-point-ppr-te", 1, 30, 7)
draw.tiers("half-point-ppr-flex", 1, 80, 15, XLOW=5, highcolor=650)



draw.tiers("ros-qb", 1, 32, 7, highcolor=360)
draw.tiers("ros-rb", 1, 50, 12, highcolor=500)
draw.tiers("ros-wr", 1, 64, 65/5, highcolor=550, XLOW=5)
draw.tiers("ros-te", 1, 30, 7, XLOW=5)
draw.tiers("ros-k", 1, 20, 5, XLOW=5)
draw.tiers("ros-dst", 1, 25, 5, XLOW=5)



draw.tiers("ros-ppr-rb", 1, 40, 10, highcolor=450)
draw.tiers("ros-ppr-wr", 1, 60, 10, highcolor=400, XLOW=5)
draw.tiers("ros-ppr-te", 1, 21, 6, XLOW=5)

# PRESEASON
COMMENT <- function() {
draw.tiers("all", 1, 65, 13, XLOW=5, highcolor=720)
draw.tiers("all", 66, 160, 9, adjust=13, XLOW=18, highcolor=540)
draw.tiers("all", 1, 100, 20, XLOW=5, highcolor=720)
draw.tiers("all", 1, 80, 11, XLOW=5, highcolor=720)
draw.tiers("all", 41, 160, 7, adjust=10, XLOW=18, highcolor=720)
draw.tiers("all", 93, 220, 4, adjust=16, XLOW=16, highcolor=500)

draw.tiers("all-ppr", 1, 70, 10, XLOW=5)
draw.tiers("all-ppr", 71, 140, 6, adjust=10, XLOW=16)
draw.tiers("all-ppr", 141, 200, 5, adjust=16, XLOW=30)

draw.tiers("all-half-ppr", 1, 70, 10, XLOW=5)
draw.tiers("all-half-ppr", 71, 140, 6, adjust=10, XLOW=16)
draw.tiers("all-half-ppr", 141, 200, 4, adjust=16, XLOW=30)
}