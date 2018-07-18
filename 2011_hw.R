

db = read.table("hw6.counts.txt", check.names = F)

# 1
db = db[apply(db, 1, mean) >= 10,]
dim(db)
db = as.matrix(db)
db = sweep(db,2,apply(db,2,sum), '/')
db = t(scale(t(db)))
pca = prcomp(t(db))
mds = cmdscale(1-cor(db))


meta = strsplit(colnames(db), '_')
meta = data.frame(
  tissue = sapply(meta, '[', 1),
  age = as.numeric(sapply(meta, '[', 2))
)

meta$cex = log(meta$age)
meta$cex = meta$cex-min(meta$cex)
meta$cex = 0.5 + meta$cex/max(meta$cex)*2
table(meta$tissue)

f = meta$tissue == 'brain'
meta$col[f] = rgb(2.5, 2.5 - meta$cex[f], 2.5 - meta$cex[f], maxColorValue = 2.5)

meta$col = NA
plot(mds, cex = meta$cex, col = meta$col)
plot(pca$x, pch = 19, cex = meta$cex, col = meta$col)

#
#db2 = t(db)
#db2 = sweep(db2,2,apply(db2,2,sum), '/') #
#db2 = t(db2)
#plot(db1, db2)
#points(db1[1,], db2[1,])
#db1 = as.matrix(db1)
#db2 = as.matrix(db2)

#pca=prcomp(cbind(db1,db2))

#2
table(apply(db1,1,sum) == 0)
db3 = db1[apply(db1,1,mean) > 0,]
dim(db3)
pv = apply(db3,1,function(x)t.test(x[1:56], x[57:113])$p.value)
pv.bh = p.adjust(pv, method = 'BH')
sum(pv.bh<0.05) #12676
pv.thr = max(pv[pv.bh<0.05]) #0.03820295
pv.thr
#comb = combn(1:113,56) #combinations for permutation of cols

