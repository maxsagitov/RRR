d = read.table("hw6.counts.txt", check.names = F)
d = as.matrix(d)
d = d[apply(d,1,mean)>=10,]
dim(d)
d = sweep(d,2,apply(d,2,sum), '/')
d = t(apply(d,1,function(x)(x-mean(x))/sd(x)))
#colMeans(d)

m = dist(t(d))
h=hclust(m)
plot(h)

cl.2 = cutree(h,k=2)
cl.4 = cutree(h,k=4)
cl.6 = cutree(h,k=6)

meta = strsplit(colnames(d),'_',fixed = TRUE)
meta = data.frame(
  tissue=sapply(meta, '[',1),
  age=as.numeric(sapply(meta, '[',2)))

fisher.test(table(cl.2, meta$tissue))

col = c(rep('red',20),rep('blue',20))

cr = cor(d, use = 'pair',method = 'p')
mds = cmdscale(1-cr)
plot(mds, pch=cl.2, cex = 5, col = ifelse(meta$tissue == 'brain', 'red', 'green'), main='MDS',xlab='',ylab='')


