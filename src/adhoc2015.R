dat1 = read.csv('~/projects/fftiers/out/current/csv/weekly-ALL-copy.csv')
dat2 = read.table('~/Downloads/dynasty_fp.tsv', sep='\t', row.names=NULL)

colnames(dat2)[1:9]= colnames(dat2)[2:10]
dat2=dat2[,c(1:9)]

datb = data.frame(player.name=dat2$Player.Name,
	dynasty.rank=dat2$Rank,
	dynasty.avg.rank=dat2$Avg.Rank)

foo = merge(dat1, datb, by.x='Player.Name', by.y='player.name')
foo = foo[order(foo$dynasty.avg.rank, decreasing=FALSE),]
foo = foo[order(foo$Tier, decreasing=FALSE),]
foo[1:15,]

write.csv(foo, '~/ffsheet.csv')