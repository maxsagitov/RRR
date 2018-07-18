options(stringsAsFactors = FALSE)
d = read.table("d://Downloads/hw6.counts.txt",check.names = FALSE)
d = as.matrix(d)
d[1:2,1:7]
colnames(d)
dim(d)
d = d[apply(d,1,mean)>=10,]
d = sweep(d,2,apply(d,2,sum),'/')
#way 1
d = t(scale(t(d)))
#way 2
d=sweep(d,1,apply(d,1,mean))
d=sweep(d,1,apply(d,1,sd),'/')
#way 3
d = t(apply(d,1,function(x)(x-mean(x))/sd(x)))

pca = prcomp(t(d))
mds = cmdscale(1-cor(d))

meta = strsplit(colnames(d),'_',fixed = TRUE)
meta = data.frame(
  tissue=sapply(meta, '[',1),
  age=as.numeric(sapply(meta, '[',2)))
meta$cex = log(meta$age)
meta$cex = meta$cex-min(meta$cex)
meta$cex = 0.5 + meta$cex/max(meta$cex)*2
meta$col = NA
f = meta$tissue == 'brain'
meta$col[f] = rgb(2.5,2.5-meta$cex[f],2.5-meta$cex[f],maxColorValue = 2.5)
f = meta$tissue == 'liver'
meta$col[f] = rgb(2.5-meta$cex[f],2.5,2.5-meta$cex[f],maxColorValue = 2.5)
meta$pch = ifelse(meta$tissue=='brain',19,10)

plot(mds,cex=meta$cex,col=meta$col,pch=meta$pch)
plot(pca$x,cex=meta$cex,col=meta$col,pch=meta$pch)
