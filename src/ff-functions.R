download.py.call <- function(url, dest) {
	me = system('whoami', intern = TRUE)
	parent = 'Users'
	if (me=='ubuntu') parent = 'home'
	if (me=='borischen') parent = 'Users'
	dl_call = paste('python /',parent,'/',me,'/projects/fftiers/src/fp_dl.py -u ',url,' -d ', dest, sep='')
	system(dl_call)
}

download.data <- function(pos.list=c('qb','rb','wr','te','flex','k','dst'), dfs=FALSE ) {
	if (download == TRUE) {
	  # download data for each position
	  for (mp in pos.list) {
	 	#curlstr = paste('curl http://www.fantasypros.com/nfl/rankings/',mp,'-cheatsheets.php?export=xls > ~/projects/fftiers/dat/2016/week-', thisweek, '-',mp,'-raw.xls', sep="")
	 	rmold1 = paste('rm ~/projects/fftiers/dat/2016/week-', thisweek, '-',mp,'-raw.xls', sep='')
	  	system(rmold1)
	  	
	  	url = paste('https://www.fantasypros.com/nfl/rankings/',mp,'-cheatsheets.php?export=xls', sep='')
	    dest = paste('~/projects/fftiers/dat/2016/week-', thisweek, '-',mp,'-raw.xls', sep="")
		download.py.call(url, dest)

		rmold2 = paste('rm ~/projects/fftiers/dat/2016/week_', thisweek, '_', mp, '.tsv', sep='')
	  	system(rmold2)
	    sedstr = paste("sed '1,4d' ~/projects/fftiers/dat/2016/week-", thisweek, '-',mp,'-raw.xls', 
	  			  ' > ~/projects/fftiers/dat/2016/week_', thisweek, '_', mp, '.tsv',sep="")
	    system(sedstr);  
	  }	  
	}
}

  # overall rankings download:
download.predraft.data <- function() {

	url = 'https://www.fantasypros.com/nfl/rankings/consensus-cheatsheets.php?export=xls'
	dest = '~/projects/fftiers/dat/2016/week-0-all-raw.xls'
	download.py.call(url, dest)
	
	url = 'https://www.fantasypros.com/nfl/rankings/ppr-cheatsheets.php?export=xls'
	dest = '~/projects/fftiers/dat/2016/week-0-all-ppr-raw.xls'
	download.py.call(url, dest)

	url = 'https://www.fantasypros.com/nfl/rankings/half-point-ppr-cheatsheets.php?export=xls'
	dest = '~/projects/fftiers/dat/2016/week-0-all-half-ppr-raw.xls'
	download.py.call(url, dest)

	sedstr = paste("sed '1,4d' ~/projects/fftiers/dat/2016/week-", thisweek, '-all-raw.xls', 
				' > ~/projects/fftiers/dat/2016/week_', thisweek, '_', 'all', '.tsv',sep="")
	sedstr2 = paste("sed '1,4d' ~/projects/fftiers/dat/2016/week-", thisweek, '-all-ppr-raw.xls', 
				' > ~/projects/fftiers/dat/2016/week_', thisweek, '_', 'all-ppr', '.tsv',sep="")
	sedstr3 = paste("sed '1,4d' ~/projects/fftiers/dat/2016/week-", thisweek, '-all-half-ppr-raw.xls', 
				' > ~/projects/fftiers/dat/2016/week_', thisweek, '_', 'all-half-ppr', '.tsv',sep="")
	system(sedstr);  
	system(sedstr2); 
	system(sedstr3);
}  

is.tpos.all <- function(tpos) {
	value = (tpos == 'ALL') | (tpos == 'ALL-PPR') | (tpos == 'ALL-HALF-PPR')
	return(value)
}

## Wrapper function around error.bar.plot
draw.tiers <- function(pos='all', low=1, high=100, k=3, adjust=0, XLOW=0, highcolor=360, num.higher.tiers=0, dfs=FALSE) {
	#dat = read.delim(paste(datdir, "week_", thisweek, "_", pos, ".tsv",sep=""), sep="\t", header=FALSE)
	IS.FLEX = (pos=='flex') | (pos=='ppr-flex') | (pos=='half-point-ppr-flex')
	if (!IS.FLEX) {
		tsvpath = paste(datdir, "week_", thisweek, "_", pos, ".tsv",sep="")
		dat = read.delim(tsvpath, sep="\t")
		#colnames(dat)[1:7]=colnames(dat)[2:8]
		dat=dat[,c(1:9)]
		#dat=dat[,c(1:7)]
		dat$Rank=1:nrow(dat)
	}
	if ( IS.FLEX ) {
		tsvpath = paste(datdir, "week_", thisweek, "_", pos, ".tsv",sep="")
		if (dfs==TRUE) tsvpath = paste(paste('~/projects/fbdfs/dat/week',thisweek,'/fantasypros/',sep=''), toupper(pos), '.tsv',sep="")
		dat = read.delim(tsvpath, sep="\t", header=FALSE)
		colnames(dat)= c("Rank","Player.Name" ,'pos',"Team","Matchup","Best.Rank","Worst.Rank","Avg.Rank","Std.Dev","X")
		dat=dat[2:nrow(dat),]
	}
	if (thisweek>0) { 
		dat$Rank = as.numeric(as.character(dat$Rank))
		dat$Best.Rank = as.numeric(as.character(dat$Best.Rank))
		dat$Worst.Rank = as.numeric(as.character(dat$Worst.Rank))
		dat$Avg.Rank = as.numeric(as.character(dat$Avg.Rank))
		dat$Std.Dev = as.numeric(as.character(dat$Std.Dev))
	}
 	dat <- dat[!dat$Player.Name %in% injured,]
	tpos = toupper(pos); 
	if (pos == "flex") tpos <- "Flex"
	if (k <= 10) highcolor <- 360
	if (k > 11) highcolor <- 450
	if (k > 13) highcolor <- 550
	if (k > 15) highcolor <- 650
	num.tiers=error.bar.plot(low = low, high = high, k=k, tpos=tpos, dat=dat, 
		adjust=adjust, XLOW=XLOW, highcolor=highcolor,num.higher.tiers=num.higher.tiers)
	return(num.tiers)
}


### main plotting function

error.bar.plot <- function(pos="NA", low=1, high=24, k=8, format="NA", title="dummy", tpos="QB", dat, 
	adjust=0, XLOW=0, highcolor=360, num.higher.tiers=0) {
	
	Sys.setenv(TZ='PST8PDT')
	curr.time = as.character(format(Sys.time(), "%a %b %d %Y %X"))
	if (tpos!='ALL') title = paste("Week ",thisweek," - ",tpos," Tiers", ' - ', curr.time, ' PST', sep="")
	if (tpos=='ALL') title = paste("Pre-draft Tiers - Top 200", ' - ', curr.time, sep="")
	if ((thisweek==0) && (tpos!='ALL')) title = paste("2016 Draft - ",tpos," Tiers", ' - ', curr.time, ' PST', sep="")
	if ((thisweek==0) && (tpos=='ALL')) title = paste("2016 Draft - Top 200 Tiers", ' - ', curr.time, ' PST', sep="")
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
	clusters.found = as.numeric(clusters.found)
	for (i in 1:k) {
		if ( sum(this.pos$mcluster ==i)==0 ) { # if you don't find any of this cluster
			# decrease everything above it by one
			this.pos$mcluster[this.pos$mcluster>i] <- this.pos$mcluster[this.pos$mcluster>i]-1
		}
	}
	
	# Print out names
	txt.path 	= paste(outputdirtxt,"text_",tpos,".txt",sep="")
	gd.txt.path = paste(gd.outputdirtxt,"text_",tpos,".txt",sep="")
	if (is.tpos.all(tpos)) {
		txt.path 	= paste(outputdirtxt,"text_",tpos,'-adjust',adjust,".txt",sep="")
		gd.txt.path = paste(gd.outputdirtxt,"text_",tpos,'-adjust',adjust,".txt",sep="")
	}
	

	if (file.exists(txt.path)) system(paste('rm', txt.path))
	fileConn <- file(txt.path)
	gd.fileConn <- file(gd.txt.path)
	if (is.tpos.all(tpos)) fileConn<-file(paste(outputdirtxt,"text_",tpos,'-adjust', num.higher.tiers,".txt",sep=""))
	tier.list = array("", k)
	bad.rows = c()
	for (i in 1:k) {
      foo <- this.pos[this.pos $cluster==i,]
      foo <- this.pos[this.pos $mcluster==i,]
      es = paste("Tier ",i,": ",sep="")
      if (num.higher.tiers>0) es = paste("Tier ",i+num.higher.tiers,": ",sep="")
      for (j in 1:nrow(foo)) es = paste(es,foo$Player.Name[j], ", ", sep="")
      es = substring(es, 1, nchar(es)-2)
      tier.list[i] = es
      if (nrow(foo)==0) bad.rows = c(bad.rows, i)
    }
    if (length(bad.rows)>0) tier.list = tier.list[-bad.rows]
    num.tiers = length(tier.list)
    writeLines(tier.list, fileConn); close(fileConn)
    writeLines(tier.list, gd.fileConn); close(gd.fileConn)

	this.pos$nchar 	= nchar(as.character(this.pos$Player.Name))
	this.pos$Tier 	= factor(this.pos$mcluster)
	if (num.higher.tiers>0) this.pos$Tier 	= as.character(as.numeric(as.character(this.pos$mcluster))+num.higher.tiers)

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
    p = p + geom_errorbar(aes(ymin = Avg.Rank - Std.Dev/2, ymax = Avg.Rank + Std.Dev/2, width=0.2, colour=Tier), size=barsize*0.8, alpha=0.4)
	p = p + geom_point(colour="grey20", size=dotsize) 
    p = p + coord_flip()
    p = p + annotate("text", x = Inf, y = -Inf, label = "www.borischen.co", hjust=1.1, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.8)
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
    
	if (tpos  != 'Flex') p = p + ylim(-5, maxy)
    if ((tpos == "Flex") | (tpos=="PPR-FLEX")  | (tpos == "HALF-POINT-PPR-FLEX")) p = p + ylim(0-XLOW, maxy)
	if ((tpos == 'ALL')  | (tpos == 'ALL-PPR') | (tpos == 'ALL-HALF-PPR')) p = p + ylim(low-XLOW, maxy+5)

	outfile 	= paste(outputdirpng, "week-", thisweek, "-", tpos, ".png", sep="")
	gd.outfile 	= paste(gd.outputdirpng, "weekly-", tpos, ".png", sep="")
	if (is.tpos.all(tpos)) {
		outfile 	= paste(outputdirpng, "week-", thisweek, "-", tpos,'-adjust',adjust, ".png", sep="")
		gd.outfile 	= paste(gd.outputdirpng, "weekly-", tpos,'-adjust',adjust, ".png", sep="")
	}

	# write the table to csv
	outfilecsv = paste(outputdircsv, "week-", thisweek, "-", tpos, ".csv", sep="")
	gd.outfilecsv = paste(gd.outputdircsv, "weekly-", tpos, ".csv", sep="")
	if (is.tpos.all(tpos)) {
		outfilecsv 		= paste(outputdircsv, "week-", thisweek, "-", tpos,'-adjust',adjust, ".csv", sep="")
		gd.outfilecsv 	= paste(gd.outputdircsv, "weekly-", tpos, ".csv", sep="")
	}
	this.pos$position.rank <- this.pos$X <- this.pos$mcluster <- this.pos$nchar <- NULL

	if (is.tpos.all(tpos)) { # Reorder for online spreadsheet
		this.pos = this.pos[,c(1:2,10,3:9)]
	}
	write.csv(this.pos, outfilecsv)

	if (adjust <= 0) write.csv(  this.pos, gd.outfilecsv, row.names=FALSE)
	if (adjust >  0) write.table(this.pos, gd.outfilecsv, row.names=FALSE, append=TRUE, col.names=FALSE, sep=',')
	
    DPI=150
    ggsave(file=outfile, width=9.5, height=8, dpi=DPI)
    ggsave(file=gd.outfile, width=9.5, height=8, dpi=DPI)

	if (is.tpos.all(tpos)) {
		val = c()
		for (i in min(as.numeric(levels(factor(this.pos$Tier)))):max(as.numeric(levels(factor(this.pos$Tier)))))  {
			val = c(val, sum(this.pos$Tier==i))
		}
		return(val)
	}
	return(num.tiers)
}

