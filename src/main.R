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

thisweek 		= as.numeric(floor((as.Date(Sys.Date(), format="%Y/%m/%d") - as.Date("2017-09-04", format="%Y-%m-%d"))/7))+1
#thisweek 		= 0 # for pre-draft
download.ros 	= FALSE
useold 			= FALSE		# Do we want to use the original version of the charts?
year			= 2017
#download = FALSE		# Do we want to download fresh data from fantasypros?

### Set and create input / output directories

mkdir <- function(dir){ 
	system(paste("mkdir -p", dir))
}
datdir = "~/projects/fftiers/dat/2017/"; mkdir(datdir)
outputdir = paste("~/projects/fftiers/out/week", thisweek, "/", sep=""); mkdir(outputdir)
outputdircsv = paste("~/projects/fftiers/out/week", thisweek, "/csv/", sep=""); mkdir(outputdircsv)
outputdirpng = paste("~/projects/fftiers/out/week", thisweek, "/png/", sep=""); mkdir(outputdirpng)
outputdirtxt = paste("~/projects/fftiers/out/week", thisweek, "/txt/", sep=""); mkdir(outputdirtxt)
gd.outdir = "~/projects/fftiers/out/current/"; mkdir(gd.outdir)
gd.outputdircsv = paste(gd.outdir, "csv/", sep=""); mkdir(gd.outputdircsv)
gd.outputdirpng = paste(gd.outdir, "png/", sep=""); mkdir(gd.outputdirpng)
gd.outputdirtxt = paste(gd.outdir, "txt/", sep=""); mkdir(gd.outputdirtxt)
system(paste('rm ', gd.outputdircsv, '*', sep=''))
system(paste('rm ', gd.outputdirpng, '*', sep=''))
system(paste('rm ', gd.outputdirtxt, '*', sep=''))

## If there are any injured players, list them here to remove them
injured <- c('')

### Predraft data
#if (download == TRUE) download.predraft.data()
predraft.comment <- function() {
	scoring.type.list = c('all', 'all-ppr', 'all-half-ppr')
	for (scoring.type in scoring.type.list) {
		high.level.tiers = draw.tiers(scoring.type, 1, 200, 3, XLOW=5, highcolor=720)
		nt.std.1 = draw.tiers(scoring.type, 1, high.level.tiers[1], 10, XLOW=10, highcolor=720)
		nt.std.2 = draw.tiers(scoring.type, high.level.tiers[1]+1, high.level.tiers[1]+high.level.tiers[2], 8, adjust=1, XLOW=18, highcolor=720, num.higher.tiers=length(nt.std.1))
		nt.std.3 = draw.tiers(scoring.type, high.level.tiers[1]+high.level.tiers[2]+1, high.level.tiers[1]+high.level.tiers[2]+high.level.tiers[3], 8, adjust=2, XLOW=20, highcolor=720, num.higher.tiers=(length(nt.std.1)+length(nt.std.2)))
	}
}


if (download == TRUE) {
	download.data(c('flx'), scoring='STD')
	download.data(c('flx','rb','wr','te'), scoring='STD')
	download.data(c('qb','k','dst'))
	download.data(c('flx','rb','wr','te'), scoring='PPR')
	download.data(c('flx','rb','wr','te'), scoring='HALF')
	#download.data(c('ppr-rb','ppr-wr','ppr-te','ppr-flex'))
	#download.data(c('half-point-ppr-rb','half-point-ppr-wr','half-point-ppr-te','half-point-ppr-flex'))
}

## Weekly
draw.tiers("qb", 1, 26, 8, highcolor=360, scoring='STD')
draw.tiers("rb", 1, 40, 9, highcolor=400, scoring='STD')
draw.tiers("wr", 1, 60, 12, highcolor=500, XLOW=10, scoring='STD')
draw.tiers("te", 1, 24, 8, XLOW=5, scoring='STD')
draw.tiers("flx", 1, 80, 14, XLOW=5, highcolor=650, scoring='STD')
draw.tiers("k", 1, 20, 5, XLOW=5)
draw.tiers("dst", 1, 20, 6, XLOW=2)

draw.tiers("ppr-rb", 1, 40, 10)
draw.tiers("ppr-wr", 1, 60, 12, highcolor=500, XLOW=10)
draw.tiers("ppr-te", 1, 25, 8)
draw.tiers("ppr-flex", 1, 80, 14, XLOW=5, highcolor=650)

draw.tiers("half-point-ppr-rb", 1, 40, 9)
draw.tiers("half-point-ppr-wr", 1, 60, 10, highcolor=400, XLOW=10)
draw.tiers("half-point-ppr-te", 1, 25, 7)
draw.tiers("half-point-ppr-flex", 1, 80, 15, XLOW=5, highcolor=650)

