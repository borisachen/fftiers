
download.data <- function(pos.list=c('qb','rb','wr','te','flex','k','dst') ) {
if (download == TRUE) {
  # download data for each position
  for (mp in pos.list) {
 	curlstr = paste('curl http://www.fantasypros.com/nfl/rankings/',mp,'.php?export=xls > ~/projects/fftiers/dat/2014/week-', thisweek, '-',mp,'-raw.xls', sep="")
    system(curlstr); #Sys.sleep(3)
    sedstr = paste("sed '1,4d' ~/projects/fftiers/dat/2014/week-", thisweek, '-',mp,'-raw.xls', 
  			  ' > ~/projects/fftiers/dat/2014/week_', thisweek, '_', mp, '.tsv',sep="")
    system(sedstr); #Sys.sleep(3)
  }	  
}
}




  # overall rankings download:
download.predraft.data <- function() {
  overall.url = 'curl http://www.fantasypros.com/nfl/rankings/consensus-cheatsheets.php?export=xls > ~/projects/fftiers/dat/2014/week-0-all-raw.xls'
  ppr.url = 'curl http://www.fantasypros.com/nfl/rankings/ppr-cheatsheets.php?export=xls > ~/projects/fftiers/dat/2014/week-0-all-ppr-raw.xls'
  half.ppr.url = 'curl http://www.fantasypros.com/nfl/rankings/half-point-ppr-cheatsheets.php?export=xls > ~/projects/fftiers/dat/2014/week-0-all-half-ppr-raw.xls'
  system(overall.url); Sys.sleep(0.5); system(ppr.url); Sys.sleep(0.5); system(half.ppr.url); Sys.sleep(0.5)
  sedstr = paste("sed '1,4d' ~/projects/fftiers/dat/2014/week-", thisweek, '-all-raw.xls', ' > ~/projects/fftiers/dat/2014/week_', thisweek, '_', 'all', '.tsv',sep="")
  sedstr2 = paste("sed '1,4d' ~/projects/fftiers/dat/2014/week-", thisweek, '-all-ppr-raw.xls', 
  			  ' > ~/projects/fftiers/dat/2014/week_', thisweek, '_', 'all-ppr', '.tsv',sep="")
  sedstr3 = paste("sed '1,4d' ~/projects/fftiers/dat/2014/week-", thisweek, '-all-half-ppr-raw.xls', 
  			  ' > ~/projects/fftiers/dat/2014/week_', thisweek, '_', 'all-half-ppr', '.tsv',sep="")
  system(sedstr);  
  system(sedstr2); system(sedstr3);
}  


### main plotting function

error.bar.plot <- function(pos="NA", low=1, high=24, k=8, format="NA", title="dummy", tpos="QB", dat, adjust=0, XLOW=0, highcolor=360) {
	if (tpos!='ALL') title = paste("Week ",thisweek," - ",tpos," Tiers", sep="")
	#if (tpos!='ALL') title = paste("Pre-draft - ",tpos," Tiers", sep="")
	if (tpos=='ALL') title = paste("Pre-draft Tiers - Top 200", sep="")
	dat$Rank = 1:nrow(dat)
	this.pos = dat
	this.pos = this.pos[low:high,]
	this.pos$position.rank <- low+c(1:nrow(this.pos))-1	
  	this.pos$position.rank = -this.pos$position.rank

	# Find clusters
	colnames(this.pos)[which(colnames(this.pos)=="Ave.Rank")] <- 'Avg.Rank'
	df = this.pos[,c(which(colnames(this.pos)=="Avg.Rank"))]
	mclust <- Mclust(df, G=k)
	this.pos$mcluster <-  mclust$class
	
	# if there were less clusters than we asked for, shift the indicies
	clusters.found <- levels(factor(this.pos$mcluster))
	if (length(clusters.found) < k) 
	{
		missing.cluster <- which(!(c(1:k) %in% clusters.found))
		if (length(missing.cluster)==1) 
		{
			print('Only 1 missing clusters, we can fix that!')
			for (i in (missing.cluster+1):k) 
			{
				this.pos$mcluster[which(this.pos$mcluster==i)] <- i-1
			}
		}
		if (length(missing.cluster)>1) 
		{
			print('More than 2 missing clusters, try another k!')
		}
	}
	
	# Print out names
	txt.path = paste(outputdirtxt,"text_",tpos,".txt",sep="")
	if (file.exists(txt.path)) system(paste('rm', txt.path))
	fileConn <- file(txt.path)
	if ((tpos == 'ALL') | (tpos == 'ALL-PPR')| (tpos == 'ALL-HALF-PPR')) fileConn<-file(paste(outputdirtxt,"text_",tpos,'-adjust', adjust,".txt",sep=""))
	tier.list = array("", k)
	for (i in 1:k) {
      foo <- this.pos[this.pos $cluster==i,]
      foo <- this.pos[this.pos $mcluster==i,]
      es = paste("Tier ",i,": ",sep="")
      if (adjust>0) es = paste("Tier ",i+adjust,": ",sep="")
      for (j in 1:nrow(foo)) es = paste(es,foo$Player.Name[j], ", ", sep="")
      es = substring(es, 1, nchar(es)-2)
      tier.list[i] = es
    }
    writeLines(tier.list, fileConn); close(fileConn)
	this.pos$nchar 	= nchar(as.character(this.pos$Player.Name))
	this.pos$Tier 	= factor(this.pos$mcluster)
	if (adjust>0) this.pos$Tier 	= as.character(as.numeric(as.character(this.pos$mcluster))+adjust)


	bigfont			= c("QB","TE","K","DST", "PPR-TE", "ROS-TE","ROS-PPR-TE", "0.5 PPR-TE", "ROS-QB",'HALF-POINT-PPR-TE')
	smfont			= c("RB", "PPR-RB", "ROS-RB","ROS-PPR-RB", "ROS-K", "ROS-DST", "0.5 PPR-RB", 'HALF-POINT-PPR-RB')
	tinyfont		= c("WR","Flex", "PPR-WR", "ROS-WR","ROS-PPR-WR","PPR-Flex","PPR-FLEX", 
						"0.5 PPR-WR","0.5 PPR-Flex", 'ALL', 'ALL-PPR', 'ALL-HALF-PPR',
						'HALF-POINT-PPR-WR','HALF-POINT-PPR-FLEX')
	
	if (tpos %in% bigfont) {font = 3.5; barsize=1.5;  dotsize=2;   }
	if (tpos %in% smfont)  {font = 3;   barsize=1.25; dotsize=1.5; }
	if (tpos %in% tinyfont){font = 2.5; barsize=1;    dotsize=1;   }
	if (tpos %in% "ALL")   {font = 2.4; barsize=1;    dotsize=0.8;   }
	
	p = ggplot(this.pos, aes(x=position.rank, y=Avg.Rank))
	p = p + ggtitle(title)
    p = p + geom_errorbar(aes(ymin=Avg.Rank-Std.Dev/2, ymax= Avg.Rank + Std.Dev/2, 
    		width=0.2, colour=Tier), size=barsize*0.8, alpha=0.4)
	p = p + geom_point(colour="grey20", size=dotsize) 
    p = p + coord_flip()
    p = p + annotate("text", x = Inf, y = -Inf, label = "www.borischen.co", hjust=1.1, 
    		vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.8)
	if (tpos %in% bigfont)     			
    	p = p + geom_text(aes(label=Player.Name, colour=Tier, y = Avg.Rank - nchar/6 - Std.Dev/1.4), size=font)
	if (tpos %in% smfont)     			
    	p = p + geom_text(aes(label=Player.Name, colour=Tier, y = Avg.Rank - nchar/5 - Std.Dev/1.5), size=font) 
	if (tpos %in% tinyfont)     			
    	p = p + geom_text(aes(label=Player.Name, colour=Tier, y = Avg.Rank - nchar/3 - Std.Dev/1.8), size=font) 
    if ((tpos == 'ALL') | (tpos == 'ALL-PPR'))
    	p = p + geom_text(aes(label=Player.Name, colour=Tier, y = Avg.Rank - nchar/3 - Std.Dev/1.8), size=font) + geom_text(aes(label=Position, y = Avg.Rank + Std.Dev/1.8 + 1), size=font, colour='#888888') 
    p = p + scale_x_continuous("Expert Consensus Rank")
    p = p + ylab("Average Expert Rank")
    p = p + theme(legend.justification=c(1,1), legend.position=c(1,1))
    p = p + scale_colour_discrete(name="Tier")
	p = p + scale_colour_hue(l=55, h=c(0, highcolor))
    maxy = max( abs(this.pos$Avg.Rank)+this.pos$Std.Dev/2) 
    
	if (tpos!='Flex') p = p + ylim(-5, maxy)
    if ((tpos=="Flex") | (tpos=="PPR-FLEX") | (tpos=="HALF-POINT-PPR-FLEX")) p = p + ylim(0-XLOW, maxy)
    #p = p + ylim(0-XLOW, maxy)
	if ((tpos == 'ALL') | (tpos == 'ALL-PPR') | (tpos == 'ALL-HALF-PPR')) p = p + ylim(low-XLOW, maxy+5)
	outfile = paste(outputdirpng, "week-", thisweek, "-", tpos, ".png", sep="")
	if ((tpos == 'ALL') | (tpos == 'ALL-PPR') | (tpos == 'ALL-HALF-PPR')) outfile = paste(outputdirpng, "week-", thisweek, "-", tpos,'-adjust',adjust, ".png", sep="")
	
	if (useold == TRUE) {
		this.pos$position.rank = -this.pos$position.rank 
		this.pos$Avg.Rank = -this.pos$Avg.Rank 
	  p = ggplot(this.pos, aes(x=position.rank, y=Avg.Rank))
	  p = p + ggtitle(title)
      p = p + geom_errorbar(aes(ymin=Avg.Rank-Std.Dev/2, ymax= Avg.Rank + Std.Dev/2, width=0.2), colour="grey80")
  	  p = p + geom_point(colour="grey20", size=dotsize, alpha=0.5) 
      p = p + annotate("text", x = Inf, y = -Inf, label = "www.borischen.co", hjust=1.1, 
    		vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.8)
	  if (tpos %in% bigfont) p = p + geom_text(aes(label=Player.Name, colour=factor(mcluster), y = Avg.Rank), size=font, angle=15) 
	  if (tpos %in% smfont) p = p + geom_text(aes(label=Player.Name, colour=factor(mcluster), y = Avg.Rank), size=font, angle=15) 
	  if (tpos %in% tinyfont) p = p + geom_text(aes(label=Player.Name, colour=factor(mcluster), y = Avg.Rank), size=font, angle=15) 
      p = p + scale_x_continuous("xpert Consensus Rank")
      p = p + scale_y_continuous("Average Rank")
      p = p + theme(legend.position="none") 
      p = p + scale_colour_hue(l=60, h=c(0, highcolor))
      outfile = paste(outputdir, "week-", thisweek, "-", tpos, "-old.png", sep="")
	}

	# write the table to csv
	outfilecsv = paste(outputdircsv, "week-", thisweek, "-", tpos, ".csv", sep="")
	if ((tpos == 'ALL') | (tpos == 'ALL-PPR') | (tpos == 'ALL-HALF-PPR')) outfilecsv = paste(outputdircsv, "week-", thisweek, "-", tpos,'-adjust',adjust, ".csv", sep="")
	this.pos$position.rank <- this.pos$X <- this.pos$mcluster <- this.pos$nchar <- NULL
	write.csv(this.pos, outfilecsv)
	
    p
    ggsave(file=outfile, width=9.5, height=8, dpi=100)
	return(p)
}

## Wrapper function around error.bar.plot
draw.tiers <- function(pos, low, high, k, adjust=0, XLOW=0, highcolor=360) {
	dat = read.delim(paste(datdir, "week_", thisweek, "_", pos, ".tsv",sep=""), sep="\t")
	if (thisweek != -10) colnames(dat) <- colnames(dat[2:ncol(dat)])
 	dat <- dat[!dat$Player.Name %in% injured,]
	tpos = toupper(pos); 
	if (pos == "flex") tpos <- "Flex"
	error.bar.plot(low = low, high = high, k=k, tpos=tpos, dat=dat, adjust=adjust, XLOW=XLOW, highcolor=highcolor)
}

