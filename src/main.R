require('mclust')
require('ggplot2')

### Parameters 

thisweek = 0
download = TRUE		# Do we want to download fresh data from fantasypros?
useold = FALSE		# Do we want to use the original version of the charts?

### Set and create input / output directories

mkdir <- function(dir) system(paste("mkdir -p", dir))
datdir = "~/projects/fftiers/dat/2014/"; mkdir(datdir)
outputdir = paste("~/projects/fftiers/out/week", thisweek, "/", sep=""); mkdir(outputdir)

### Curl data from fantasypros

# Which positions do we want to fetch?
pos.list = c('qb','rb','wr','te','flex','k','dst')

# Can also download PPR, half PPR, and rest-of-season data
            # 'ppr-rb','ppr-wr','ppr-te','ppr-flex',
            # 'half-point-ppr-rb','half-point-ppr-wr','half-point-ppr-te','half-point-ppr-flex')
			# 'ros-qb','ros-rb','ros-wr','ros-te','ros-k', 'ros-dst')

if (download == TRUE) {
  # download data for each position
  for (mp in pos.list) {
 	curlstr = paste('curl http://www.fantasypros.com/nfl/rankings/',mp,
				'-cheatsheets.php?export=xls > ~/projects/fftiers/dat/2014/week-', 
				thisweek, '-',mp,'-raw.xls', sep="")
    system(curlstr); Sys.sleep(0.5)
    sedstr = paste("sed '1,4d' ~/projects/fftiers/dat/2014/week-", thisweek, '-',mp,'-raw.xls', 
  			  ' > ~/projects/fftiers/dat/2014/week_', thisweek, '_', mp, '.tsv',sep="")
    system(sedstr); Sys.sleep(0.5)
  }	
  
  # overall rankings download:
  overall.url = 'curl http://www.fantasypros.com/nfl/rankings/consensus-cheatsheets.php?export=xls > ~/projects/fftiers/dat/2014/week-0-all-raw.xls'
  system(overall.url); Sys.sleep(0.5)
  sedstr = paste("sed '1,4d' ~/projects/fftiers/dat/2014/week-", thisweek, '-all-raw.xls', 
  			  ' > ~/projects/fftiers/dat/2014/week_', thisweek, '_', 'all', '.tsv',sep="")
  system(sedstr);
  
}


### main plotting function

error.bar.plot <- function(pos="NA", low=1, high=24, k=8, format="NA", title="dummy", tpos="QB", dat, adjust=0, XLOW=0) {
	#if (tpos!='ALL') title = paste("Week ",thisweek," - ",tpos," Tiers", sep="")
	if (tpos!='ALL') title = paste("Pre-draft - ",tpos," Tiers", sep="")
	if (tpos=='ALL') title = paste("Pre-draft Tiers - Top 200", sep="")
	dat$Rank = 1:nrow(dat)
	this.pos = dat
	this.pos = this.pos[low:high,]
	this.pos$position.rank <- low+c(1:nrow(this.pos))-1	
  	this.pos$position.rank = -this.pos$position.rank

	# Find clusters
	df = this.pos[,c(which(colnames(this.pos)=="Ave.Rank"))]
	mclust <- Mclust(df, G=k)
	this.pos$mcluster <-  mclust$class
	
	# Print out names
	fileConn<-file(paste(outputdir,"text_",tpos,".txt",sep=""))
	tier.list = array("", k)
	for (i in 1:k) {
      foo <- this.pos[this.pos $cluster==i,]
      foo <- this.pos[this.pos $mcluster==i,]
      es = paste("Tier ",i,": ",sep="")
      for (j in 1:nrow(foo)) es = paste(es,foo$Player.Name[j], ", ", sep="")
      es = substring(es, 1, nchar(es)-2)
      tier.list[i] = es
    }
    writeLines(tier.list, fileConn); close(fileConn)
	this.pos$nchar 	= nchar(as.character(this.pos$Player.Name))
	this.pos$Tier 	= factor(this.pos$mcluster)
	if (adjust>0) this.pos$Tier 	= as.character(as.numeric(as.character(this.pos$mcluster))+adjust)

	bigfont			= c("QB","TE","K","DST", "PPR - TE", "ROS - TE", "0.5 PPR - TE", "ROS - QB")
	smfont			= c("RB", "PPR - RB", "ROS - RB", "0.5 PPR - RB")
	tinyfont		= c("WR","Flex", "PPR - WR", "ROS - WR","PPR - Flex", "0.5 PPR - WR","0.5 PPR - Flex", 'ALL')
	
	if (tpos %in% bigfont) {font = 3.5; barsize=1.5;  dotsize=2;   }
	if (tpos %in% smfont)  {font = 3;   barsize=1.25; dotsize=1.5; }
	if (tpos %in% tinyfont){font = 2.5; barsize=1;    dotsize=1;   }
	if (tpos %in% "ALL")   {font = 2.4; barsize=1;    dotsize=0.8;   }
	
	p = ggplot(this.pos, aes(x=position.rank, y=Ave.Rank))
	p = p + ggtitle(title)
    p = p + geom_errorbar(aes(ymin=Ave.Rank-Std.Dev/2, ymax= Ave.Rank + Std.Dev/2, 
    		width=0.2, colour=Tier), size=barsize*0.8, alpha=0.4)
	p = p + geom_point(colour="grey20", size=dotsize) 
    p = p + coord_flip()
    p = p + annotate("text", x = Inf, y = -Inf, label = "www.borischen.co", hjust=1.1, 
    		vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.8)
	if (tpos %in% bigfont)     			
    	p = p + geom_text(aes(label=Player.Name, colour=Tier, y = Ave.Rank - nchar/6 - Std.Dev/1.4), size=font)
	if (tpos %in% smfont)     			
    	p = p + geom_text(aes(label=Player.Name, colour=Tier, y = Ave.Rank - nchar/5 - Std.Dev/1.5), size=font) 
	if (tpos %in% tinyfont)     			
    	p = p + geom_text(aes(label=Player.Name, colour=Tier, y = Ave.Rank - nchar/3 - Std.Dev/1.8), size=font) 
    if (tpos == 'ALL')     			
    	p = p + geom_text(aes(label=Player.Name, colour=Tier, y = Ave.Rank - nchar/3 - Std.Dev/1.8), size=font) + geom_text(aes(label=Position, y = Ave.Rank + Std.Dev/1.8 + 1), size=font, colour='#888888') 
    p = p + scale_x_continuous("Weight Adjusted Expert Concensus Rank")
    p = p + ylab("Average Expert Rank")
    p = p + theme(legend.justification=c(1,1), legend.position=c(1,1))
    p = p + scale_colour_discrete(name="Tier")
	p = p + scale_colour_hue(l=55, h=c(0,300))
    maxy = max( abs(this.pos$Ave.Rank)+this.pos$Std.Dev/2) 
	if (tpos!='Flex') p = p + ylim(-4, maxy)
    if (tpos=="Flex") p = p + ylim(4, maxy)
	if (tpos=='ALL') p = p + ylim(low-XLOW, maxy+5)
	outfile = paste(outputdir, "week-", thisweek, "-", tpos, ".png", sep="")
	if (tpos=='ALL') outfile = paste(outputdir, "week-", thisweek, "-", tpos,'-adjust',adjust, ".png", sep="")
	
	if (useold == TRUE) {
		this.pos$position.rank = -this.pos$position.rank 
		this.pos$Ave.Rank = -this.pos$Ave.Rank 
	  p = ggplot(this.pos, aes(x=position.rank, y=Ave.Rank))
	  p = p + ggtitle(title)
      p = p + geom_errorbar(aes(ymin=Ave.Rank-Std.Dev/2, ymax= Ave.Rank + Std.Dev/2, width=0.2), colour="grey80")
  	  p = p + geom_point(colour="grey20", size=dotsize, alpha=0.5) 
      p = p + annotate("text", x = Inf, y = -Inf, label = "www.borischen.co", hjust=1.1, 
    		vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.8)
	  if (tpos %in% bigfont) p = p + geom_text(aes(label=Player.Name, colour=factor(mcluster), y = Ave.Rank), size=font, angle=15) 
	  if (tpos %in% smfont) p = p + geom_text(aes(label=Player.Name, colour=factor(mcluster), y = Ave.Rank), size=font, angle=15) 
	  if (tpos %in% tinyfont) p = p + geom_text(aes(label=Player.Name, colour=factor(mcluster), y = Ave.Rank), size=font, angle=15) 
      p = p + scale_x_continuous("Weighted Expert Concensus Rank")
      p = p + scale_y_continuous("Average Rank")
      p = p + theme(legend.position="none") 
      p = p + scale_colour_hue(l=60, h=c(0,280))
      outfile = paste(outputdir, "week-", thisweek, "-", tpos, "-old.png", sep="")
	}

    p
    ggsave(file=outfile, width=9.5, height=8, dpi=100)
	return(p)
}

## Wrapper function around error.bar.plot
draw.tiers <- function(pos, low, high, k, adjust=0, XLOW=0) {
	dat = read.delim(paste(datdir, "week_", thisweek, "_", pos, ".tsv",sep=""), sep="\t")
 	dat <- dat[!dat$Rank %in% injured,]
	tpos = toupper(pos); if(pos=="flex")tpos<-"Flex"
	error.bar.plot(low = low, high = high, k=k, tpos=tpos, dat=dat, adjust=adjust, XLOW=XLOW)
}

## If there are any injured players, list them here to remove them
injured <- c('David Wilson')


draw.tiers("all", 1, 70, 10, XLOW=5)
draw.tiers("all", 71, 140, 8, adjust=10, XLOW=16)
draw.tiers("all", 141, 200, 5, adjust=17, XLOW=30)

draw.tiers("qb", 1, 32, 9)
draw.tiers("rb", 1, 40, 10)
draw.tiers("wr", 1, 60, 10)
draw.tiers("te", 1, 24, 7)
draw.tiers("flex", 15, 75, 13)
draw.tiers("k", 1, 29, 5)
draw.tiers("dst", 1, 32, 6)


# PPR

dat = read.delim(paste(datdir, "week_",thisweek,"_ppr-rb.tsv",sep=""), sep="\t")
error.bar.plot(low = 1, high = 40, k=7, tpos="PPR - RB", dat=dat)
dat = read.delim(paste(datdir, "week_",thisweek,"_ppr-wr.tsv",sep=""), sep="\t")
error.bar.plot(low = 1, high = 60, k=8, tpos="PPR - WR", dat=dat)
dat = read.delim(paste(datdir, "week_",thisweek,"_ppr-te.tsv",sep=""), sep="\t")
error.bar.plot(low = 1, high = 30, k=5, tpos="PPR - TE", dat=dat)
dat = read.delim(paste(datdir, "week_",thisweek,"_ppr-flex.tsv",sep=""), sep="\t")
error.bar.plot(low = 15, high = 75, k=11, tpos="PPR - Flex", dat=dat)

dat = read.delim(paste(datdir, "week_",thisweek,"_half-point-ppr-rb.tsv",sep=""), sep="\t")
error.bar.plot(low = 1, high = 40, k=6, tpos="0.5 PPR - RB", dat=dat)
dat = read.delim(paste(datdir, "week_",thisweek,"_half-point-ppr-wr.tsv",sep=""), sep="\t")
error.bar.plot(low = 1, high = 60, k=8, tpos="0.5 PPR - WR", dat=dat)
dat = read.delim(paste(datdir, "week_",thisweek,"_half-point-ppr-te.tsv",sep=""), sep="\t")
error.bar.plot(low = 1, high = 30, k=5, tpos="0.5 PPR - TE", dat=dat)
dat = read.delim(paste(datdir, "week_",thisweek,"_half-point-ppr-flex.tsv",sep=""), sep="\t")
error.bar.plot(low = 15, high = 75, k=9, tpos="0.5 PPR - Flex", dat=dat)

