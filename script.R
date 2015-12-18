#Piotroski in R
d <- read.csv(commandArgs(trailingOnly = FALSE)[6],sep =',',stringsAsFactors=FALSE)
useful_info <- d[c(3,4),-c(1)]
d <- d[-(0:4),]
names <- t(d[,1])
d <- d[,-c(1)]
d <- as.data.frame(sapply(d,as.numeric))
d[is.na(d)] <- 0
rownames(d) <- names
useful_info[1,] <- substr(useful_info[1,],nchar(useful_info[1,])-1,nchar(useful_info[1,]))
prefix_list <- factor(unlist(c(useful_info[1,])))
levels(prefix_list) <- c('n','p','c','f')
adnames <- paste(prefix_list,unlist(c(useful_info[2,])),sep="")
colnames(d) <- adnames
d['Accrual'] <- d['cCurrent assets']-d['pCurrent assets']+d['cCorporate tax']-d['pCorporate tax']+d['cShort term bank borrowings']-d['pShort term bank borrowings']-d['cDepreciation (net of transfer from revaluation reserves)']-d['cAmortisation']+d['pCurrent liabilities']-d['cCurrent liabilities']+d['pNet cash flow due to net changes in cash and cash equivalents']-d['cNet cash flow due to net changes in cash and cash equivalents']
for(i in c('cAdjusted Closing Price ','fAdjusted Closing Price ')){d[,i]<-replace(d[,i],d[,i]==0,NA)}
d <- d[complete.cases(d),]
sorted_d <- d[order(d[,'Accrual']),]
bottom_decile <-sorted_d[1:(nrow(sorted_d)/10),]
top_decile <-sorted_d[1:(9*nrow(sorted_d)/10),]
top_returns <- sum((top_decile[,'fAdjusted Closing Price ']-top_decile[,'cAdjusted Closing Price '])/top_decile[,'cAdjusted Closing Price '])/nrow(top_decile)
bottom_returns <- sum((-bottom_decile[,'fAdjusted Closing Price ']+bottom_decile[,'cAdjusted Closing Price '])/bottom_decile[,'cAdjusted Closing Price '])/nrow(bottom_decile)
returns <- top_returns+bottom_returns
print(returns*100)