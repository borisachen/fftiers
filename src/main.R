# install.packages("ggplot2", dependencies = TRUE)
# install.packages("mclust", dependencies = TRUE)
require('mclust')
require('ggplot2')

get_fftiers_root <- function() {
	args_all <- commandArgs(trailingOnly = FALSE)
	script_arg <- sub("^--file=", "", args_all[grep("^--file=", args_all)])
	if (length(script_arg)) {
		return(normalizePath(file.path(dirname(script_arg), "..")))
	}
	env_root <- Sys.getenv("FFTIERS_ROOT", unset = "")
	if (nzchar(env_root)) {
		return(normalizePath(env_root))
	}
	mac_path <- file.path(Sys.getenv("HOME"), "projects", "fftiers")
	if (file.exists(file.path(mac_path, "src", "main.R"))) {
		return(normalizePath(mac_path))
	}
	stop("Could not locate fftiers repo root; set FFTIERS_ROOT or run via Rscript main.R")
}

fftiers.root <- get_fftiers_root()
year <- 2026
source(file.path(fftiers.root, "src", "ff-functions.R"))

### Parameters
options(echo=TRUE)
args 	<- commandArgs(trailingOnly = TRUE)
download = TRUE

if (length(args) != 1) {
	print('Expected args format: Rscript main.R TRUE/FALSE')
	stopifnot(FALSE)
}
download = toupper(as.character(args[1]))
if (download=='T') download = TRUE
if (download=='F') download = FALSE

year			     = year
weekonetuesday = "2026-09-08"  # Put the date of the Tuesday of Week 1 here.
thisweek 		   = as.numeric(floor((as.Date(Sys.Date(), format="%Y/%m/%d") - as.Date(weekonetuesday, format="%Y-%m-%d"))/7))+1
thisweek 		   = max(0, thisweek) # 0 for pre-draft
download.ros 	 = FALSE
useold 			   = FALSE	# Do we want to use the original version of the charts?

#download = FALSE		# Do we want to download fresh data from fantasypros?

### Set and create input / output directories

mkdir <- function(dir){
	system(paste("mkdir -p", dir))
}
datdir = file.path(fftiers.root, "dat", year); mkdir(datdir)
outputdir = file.path(fftiers.root, "out", paste0("week", thisweek)); mkdir(outputdir)
outputdircsv = file.path(outputdir, "csv"); mkdir(outputdircsv)
outputdirpng = file.path(outputdir, "png"); mkdir(outputdirpng)
outputdirtxt = file.path(outputdir, "txt"); mkdir(outputdirtxt)
gd.outdir = file.path(fftiers.root, "out", "current"); mkdir(gd.outdir)
gd.outputdircsv = file.path(gd.outdir, "csv"); mkdir(gd.outputdircsv)
gd.outputdirpng = file.path(gd.outdir, "png"); mkdir(gd.outputdirpng)
gd.outputdirtxt = file.path(gd.outdir, "txt"); mkdir(gd.outputdirtxt)
system(paste('rm -f', file.path(gd.outputdircsv, '*')))
system(paste('rm -f', file.path(gd.outputdirpng, '*')))
system(paste('rm -f', file.path(gd.outputdirtxt, '*')))

## If there are any injured players, list them here to remove them
injured <- c('')

### Predraft data
if (thisweek == 0) download.predraft.data()
if (thisweek == 0) {
	scoring.type.list = c('all', 'all-ppr', 'all-half-ppr')
	for (scoring.type in scoring.type.list) {
		high.level.tiers = draw.tiers(scoring.type, 1, 200, 3, XLOW=5, highcolor=720, save=FALSE)
		nt.std.1 = draw.tiers(scoring.type, 1, high.level.tiers[1], 10, XLOW=10, highcolor=720)
		nt.std.2 = draw.tiers(scoring.type, high.level.tiers[1]+1, high.level.tiers[1]+high.level.tiers[2], 8, adjust=1, XLOW=18, highcolor=720, num.higher.tiers=length(nt.std.1))
		nt.std.3 = draw.tiers(scoring.type, high.level.tiers[1]+high.level.tiers[2]+1, high.level.tiers[1]+high.level.tiers[2]+high.level.tiers[3], 8, adjust=2, XLOW=20, highcolor=720, num.higher.tiers=(length(nt.std.1)+length(nt.std.2)))
	}
}


if (download == TRUE) {
	download.data(c('qb','k','dst'))
	if (thisweek == 0) {
		download.data(c('rb','wr','te'), scoring='STD')
		download.data(c('rb','wr','te'), scoring='PPR')
		download.data(c('rb','wr','te'), scoring='HALF')
	}
	if (thisweek > 0) {
		download.data(c('flx','rb','wr','te'), scoring='STD')
		download.data(c('flx','rb','wr','te'), scoring='PPR')
		download.data(c('flx','rb','wr','te'), scoring='HALF')
	}
}

## Weekly
draw.tiers("qb", 1, 26, 8, highcolor=360)
draw.tiers("rb", 1, 40, 9, highcolor=400, scoring='STD')
draw.tiers("wr", 1, 60, 12, highcolor=500, XLOW=10, scoring='STD')
draw.tiers("te", 1, 24, 8, XLOW=5, scoring='STD')
draw.tiers("k", 1, 20, 5, XLOW=5)
draw.tiers("dst", 1, 20, 6, XLOW=2)

draw.tiers("rb", 1, 40, 10, scoring='PPR')
draw.tiers("wr", 1, 60, 12, highcolor=500, XLOW=10, scoring='PPR')
draw.tiers("te", 1, 25, 8, scoring='PPR')

draw.tiers("rb", 1, 40, 9, scoring='HALF')
draw.tiers("wr", 1, 60, 10, highcolor=400, XLOW=10, scoring='HALF')
draw.tiers("te", 1, 25, 7, scoring='HALF')

if (thisweek > 0) {
	draw.tiers("flx", 20, 95, 14, XLOW=5, highcolor=650, scoring='STD')
	draw.tiers("flx", 20, 95, 14, XLOW=5, highcolor=650, scoring='PPR')
	draw.tiers("flx", 20, 95, 15, XLOW=5, highcolor=650, scoring='HALF')
}
