require('mclust')
require('ggplot2')
source('~/projects/fftiers/src/ff-functions.R')

### Parameters 

thisweek = 6
download = TRUE		# Do we want to download fresh data from fantasypros?
useold = FALSE		# Do we want to use the original version of the charts?

### Set and create input / output directories

mkdir <- function(dir) system(paste("mkdir -p", dir))
datdir = "~/projects/fftiers/dat/2014/"; mkdir(datdir)
outputdir = paste("~/projects/fftiers/out/week", thisweek, "/", sep=""); mkdir(outputdir)
outputdircsv = paste("~/projects/fftiers/out/week", thisweek, "/csv/", sep=""); mkdir(outputdircsv)
outputdirpng = paste("~/projects/fftiers/out/week", thisweek, "/png/", sep=""); mkdir(outputdirpng)
outputdirtxt = paste("~/projects/fftiers/out/week", thisweek, "/txt/", sep=""); mkdir(outputdirtxt)

### Curl data from fantasypros
# Which positions do we want to fetch?
download.data(c('qb','rb','wr','te'))
download.data(c('flex','k','dst'))
download.data(c('ppr-rb','ppr-wr','ppr-te','ppr-flex'))
download.data(c('half-point-ppr-rb','half-point-ppr-wr','half-point-ppr-te','half-point-ppr-flex'))

## If there are any injured players, list them here to remove them
injured <- c('A.J. Green')

## Week 1

draw.tiers("qb", 1, 26, 13, highcolor=500)
draw.tiers("qb", 1, 26, 6, highcolor=360)
draw.tiers("rb", 1, 40, 11, highcolor=450)
draw.tiers("wr", 1, 60, 15, highcolor=600, XLOW=5)
draw.tiers("te", 1, 25, 9, XLOW=5)
draw.tiers("flex", 1, 80, 16, XLOW=5, highcolor=550)
draw.tiers("k", 1, 25, 5, XLOW=5)
draw.tiers("dst", 1, 30, 7, XLOW=5)

draw.tiers("ppr-rb", 1, 40, 11)
draw.tiers("ppr-wr", 1, 60, 13, highcolor=400)
draw.tiers("ppr-te", 1, 26, 8)
draw.tiers("ppr-flex", 1, 80, 16, XLOW=5, highcolor=600)

draw.tiers("half-point-ppr-rb", 1, 40, 9)
draw.tiers("half-point-ppr-wr", 1, 60, 13, highcolor=400)
draw.tiers("half-point-ppr-te", 1, 26, 7)
draw.tiers("half-point-ppr-flex", 1, 80, 16, XLOW=5, highcolor=600)


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