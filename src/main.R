require('mclust')
require('ggplot2')
source('~/projects/fftiers/src/ff-functions.R')

### Parameters 

options(echo=TRUE)
args 	<- commandArgs(trailingOnly = TRUE)
if (length(args) != 1) {
	print('Expected args format: Rscript main.R TRUE/FALSE')
	stopifnot(FALSE)
}
download = toupper(as.character(args[1]))
if (download=='T') download = TRUE
if (download=='F') download = FALSE

thisweek 		= 2
download.ros 	= FALSE
useold 			= FALSE		# Do we want to use the original version of the charts?
year			= 2015
#download = FALSE		# Do we want to download fresh data from fantasypros?

### Set and create input / output directories

mkdir <- function(dir){
	system(paste("mkdir -p", dir))
}
datdir = "~/projects/fftiers/dat/2015/"; mkdir(datdir)
outputdir = paste("~/projects/fftiers/out/week", thisweek, "/", sep=""); mkdir(outputdir)
outputdircsv = paste("~/projects/fftiers/out/week", thisweek, "/csv/", sep=""); mkdir(outputdircsv)
outputdirpng = paste("~/projects/fftiers/out/week", thisweek, "/png/", sep=""); mkdir(outputdirpng)
outputdirtxt = paste("~/projects/fftiers/out/week", thisweek, "/txt/", sep=""); mkdir(outputdirtxt)
gd.outdir = "~/projects/fftiers/out/current/"; mkdir(gd.outdir)
gd.outputdircsv = paste(gd.outdir, "csv/", sep=""); mkdir(gd.outputdircsv)
gd.outputdirpng = paste(gd.outdir, "png/", sep=""); mkdir(gd.outputdirpng)
gd.outputdirtxt = paste(gd.outdir, "txt/", sep=""); mkdir(gd.outputdirtxt)

### Curl data from fantasypros. Which positions do we want to fetch?
if (download == TRUE) {
	download.data(c('qb','rb','wr','te'))
	download.data(c('flex','k','dst'))
	download.data(c('ppr-rb','ppr-wr','ppr-te','ppr-flex'))
	download.data(c('half-point-ppr-rb','half-point-ppr-wr','half-point-ppr-te','half-point-ppr-flex'))
	download.predraft.data()
}

if (download.ros == TRUE) {
	download.data(c('ros-qb','ros-rb','ros-wr','ros-te'))
	download.data(c('ros-flex','ros-k','ros-dst'))
	download.data(c('ros-ppr-rb','ros-ppr-wr','ros-ppr-te','ros-ppr-flex'))
}


# PRESEASON

## If there are any injured players, list them here to remove them
injured <- c('')
comment <- function() {
nt.std.1 = draw.tiers("all", 1, 69, 10, XLOW=5, highcolor=720)
nt.std.2 = draw.tiers("all", 70, 143, 7, adjust=1, XLOW=18, highcolor=720, num.higher.tiers=nt.std.1)
nt.std.3 = draw.tiers("all", 144, 210, 4, adjust=2, XLOW=20, highcolor=500, num.higher.tiers=(nt.std.1+nt.std.2))

nt.ppr.1 = draw.tiers("all-ppr", 1, 70, 10, XLOW=5)
nt.ppr.2 = draw.tiers("all-ppr", 71, 140, 6, adjust=1, XLOW=16, num.higher.tiers=nt.ppr.1)
nt.ppr.3 = draw.tiers("all-ppr", 141, 200, 5, adjust=2, XLOW=35, num.higher.tiers=(nt.ppr.1+nt.ppr.2) )

nt.halfppr.1 = draw.tiers("all-half-ppr", 1, 71, 10, XLOW=5)
nt.halfppr.2 = draw.tiers("all-half-ppr", 72, 143, 6, adjust=1, XLOW=20, num.higher.tiers=nt.halfppr.1)
nt.halfppr.3 = draw.tiers("all-half-ppr", 144, 200, 4, adjust=2, XLOW=30, num.higher.tiers=(nt.halfppr.1+nt.halfppr.2) )
}



## Weekly
draw.tiers("qb", 1, 26, 8, highcolor=360)
draw.tiers("rb", 1, 40, 9, highcolor=400)
draw.tiers("wr", 1, 60, 12, highcolor=500, XLOW=5)
draw.tiers("te", 1, 24, 8, XLOW=5)
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



ros.comment <- function() {
	draw.tiers("ros-qb", 1, 32, 7, highcolor=360)
	draw.tiers("ros-rb", 1, 50, 12, highcolor=500)
	draw.tiers("ros-wr", 1, 64, 65/5, highcolor=550, XLOW=5)
	draw.tiers("ros-te", 1, 30, 7, XLOW=5)
	draw.tiers("ros-k", 1, 20, 5, XLOW=5)
	draw.tiers("ros-dst", 1, 25, 5, XLOW=5)
	draw.tiers("ros-ppr-rb", 1, 40, 10, highcolor=450)
	draw.tiers("ros-ppr-wr", 1, 60, 10, highcolor=400, XLOW=5)
	draw.tiers("ros-ppr-te", 1, 21, 6, XLOW=5)
}
