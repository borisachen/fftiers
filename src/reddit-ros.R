require('mclust')
require('ggplot2')

thisweek=8
mkdir <- function(dir) system(paste("mkdir -p", dir))
datdir = "~/projects/fftiers/redditros/2014/"; mkdir(datdir)
outputdir = paste("~/projects/fftiers/redditros/week", thisweek, "/", sep=""); mkdir(outputdir)
outputdircsv = paste("~/projects/fftiers/redditros/week", thisweek, "/csv/", sep=""); mkdir(outputdircsv)
outputdirpng = paste("~/projects/fftiers/redditros/week", thisweek, "/png/", sep=""); mkdir(outputdirpng)
outputdirtxt = paste("~/projects/fftiers/redditros/week", thisweek, "/txt/", sep=""); mkdir(outputdirtxt)

### main plotting function

error.bar.plot <- function(pos="NA", low=1, high=24, k=8, format="NA", title="dummy", tpos="QB", dat, adjust=0, XLOW=0, highcolor=360) {
	dat$Rank = 1:nrow(dat)
	this.pos = dat
	this.pos = this.pos[low:high,]
	this.pos$position.rank <- low+c(1:nrow(this.pos))-1	
  	this.pos$position.rank = -this.pos$position.rank	
  	this.pos$Score = -this.pos$Score
  	this.pos$Std.Dev=1

	# Find clusters
	df = this.pos[,c(which(colnames(this.pos)=="Score"))]
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
      for (j in 1:nrow(foo)) es = paste(es,foo$Ideas[j], ", ", sep="")
      es = substring(es, 1, nchar(es)-2)
      tier.list[i] = es
    }
    writeLines(tier.list, fileConn); close(fileConn)
	this.pos$nchar 	= nchar(as.character(this.pos$Ideas))
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
	
	this.pos$Tier = factor(this.pos$Tier)
	this.pos$Ideas = factor(this.pos$Ideas)
	
	title = paste('Reddit ROS - ',tpos)
	p = ggplot(this.pos, aes(x=position.rank, y=Score))
	p = p + ggtitle(title)
    p = p + geom_errorbar(aes(ymin=Score-Std.Dev/2, ymax= Score + Std.Dev/2, 
    		width=0.2, colour=Tier), size=barsize*0.8, alpha=0.4)
	p = p + geom_point(colour="grey20", size=dotsize) 
    p = p + coord_flip()
	if (tpos %in% bigfont)     			
    	p = p + geom_text(aes(label=Ideas, colour=Tier, y = Score - nchar/2 - Std.Dev/1.4), size=font)
	if (tpos %in% smfont)     			
    	p = p + geom_text(aes(label=Ideas, colour=Tier, y = Score - nchar/5/3 - Std.Dev/1.5-5), size=font) 
	if (tpos %in% tinyfont)     			
    	p = p + geom_text(aes(label=Ideas, colour=Tier, y = Score - nchar/3/3 - Std.Dev/1.8-3), size=font) 
    if ((tpos == 'ALL') | (tpos == 'ALL-PPR'))
    	p = p + geom_text(aes(label=Ideas, colour=Tier, y = Score - nchar/3/3 - Std.Dev/1.8), size=font) + geom_text(aes(label=Position, y = Score + Std.Dev/1.8 + 1), size=font, colour='#888888') 
    p = p + scale_x_continuous("Reddit Rank")
    p = p + ylab("Reddit Score")
    p = p + theme(legend.justification=c(1,1), legend.position=c(1,1))
    p = p + scale_colour_discrete(name="Tier")
	p = p + scale_colour_hue(l=55, h=c(0, highcolor))
    #maxy = max( abs(this.pos$Score)+this.pos$Std.Dev/2) 
    
	#if (tpos!='Flex') p = p + ylim(-5, maxy)
    #if ((tpos=="Flex") | (tpos=="PPR-FLEX") | (tpos=="HALF-POINT-PPR-FLEX")) p = p + ylim(0-XLOW, maxy)
    #p = p + ylim(0-XLOW, maxy)
	if ((tpos == 'ALL') | (tpos == 'ALL-PPR') | (tpos == 'ALL-HALF-PPR')) p = p + ylim(low-XLOW, maxy+5)
	outfile = paste(outputdirpng, "week-", thisweek, "-", tpos, ".png", sep="")
	if ((tpos == 'ALL') | (tpos == 'ALL-PPR') | (tpos == 'ALL-HALF-PPR')) outfile = paste(outputdirpng, "week-", thisweek, "-", tpos,'-adjust',adjust, ".png", sep="")
	
		# write the table to csv
	outfilecsv = paste(outputdircsv, "week-", thisweek, "-", tpos, ".csv", sep="")
	#if ((tpos == 'ALL') | (tpos == 'ALL-PPR') | (tpos == 'ALL-HALF-PPR')) outfilecsv = paste(outputdircsv, "week-", thisweek, "-", tpos,'-adjust',adjust, ".csv", sep="")
	#write.csv(this.pos, outfilecsv)
	
    p
    ggsave(file=outfile, width=9.5, height=8, dpi=100)
	return(p)
}

## Wrapper function around error.bar.plot
draw.tiers <- function(pos, k, adjust=0, XLOW=0, highcolor=360) {
	dat = read.delim(paste("~/Downloads/reddit ros - ",pos,'.csv', sep=""), sep=",")
	tpos = toupper(pos); 
	low =  1
	high = nrow(dat)
	error.bar.plot(low = low, high = high, k=k, tpos=tpos, dat=dat, adjust=adjust, XLOW=XLOW, highcolor=highcolor)
}

draw.tiers('qb', 11)
draw.tiers('rb', 15, highcolor=600)
draw.tiers('wr', 17, highcolor=720)
draw.tiers('te', 9)


