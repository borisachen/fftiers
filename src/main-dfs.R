require('mclust')
require('ggplot2')
source('~/projects/fftiers/src/ff-functions.R')

thisweek 		= 4
useold 			= FALSE		# Do we want to use the original version of the charts?
year			= 2015
download = TRUE		# Do we want to download fresh data from fantasypros?

### Set and create input / output directories

mkdir <- function(dir){
	system(paste("mkdir -p", dir))
}
datdir = "~/projects/fbdfs/dat/2015/"; mkdir(datdir)
outputdir = paste("~/projects/fbdfs/out/week", thisweek, "/", sep=""); mkdir(outputdir)
outputdircsv = paste("~/projects/fbdfs/out/week", thisweek, "/csv/", sep=""); mkdir(outputdircsv)
outputdirpng = paste("~/projects/fbdfs/out/week", thisweek, "/png/", sep=""); mkdir(outputdirpng)
outputdirtxt = paste("~/projects/fbdfs/out/week", thisweek, "/txt/", sep=""); mkdir(outputdirtxt)
gd.outdir = "~/projects/fbdfs/out/current/"; mkdir(gd.outdir)
gd.outputdircsv = paste(gd.outdir, "csv/", sep=""); mkdir(gd.outputdircsv)
gd.outputdirpng = paste(gd.outdir, "png/", sep=""); mkdir(gd.outputdirpng)
gd.outputdirtxt = paste(gd.outdir, "txt/", sep=""); mkdir(gd.outputdirtxt)

download.data(c('QB','RB','WR','TE','FLEX','DST'), dfs=TRUE)
## If there are any injured players, list them here to remove them
injured <- c()

## Weekly
draw.tiers("qb", 1, 30, 8, dfs=TRUE)
draw.tiers("rb", 1, 43, 9, dfs=TRUE)
draw.tiers("wr", 1, 60, 12, dfs=TRUE)
draw.tiers("te", 1, 19, 8, dfs=TRUE)
draw.tiers("flex", 1, 80, 14, dfs=TRUE)
draw.tiers("dst", 1, 25, 6, dfs=TRUE)
