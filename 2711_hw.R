d = read.table("hw6.counts.txt", check.names = F)
d = as.matrix(d)
d = d[apply(d,1,mean)>=10,]
dim(d)
d = sweep(d,2,apply(d,2,mean), '/')

meta = strsplit(colnames(d),'_',fixed = TRUE)
meta = data.frame(
  tissue=sapply(meta, '[',1),
  age=as.numeric(sapply(meta, '[',2)))

t = meta[,1]
a = meta[,2]^0.25
a2 = meta[,2]^0.5
m = mes ~ t + a + a2 + t*a + t*a2 + t:a + t:a2

d_sh = d[1:1000,]

pv.lm = pv.lm.p = matrix(ncol = 5, nrow = nrow(d_sh))
for (i in 1:nrow(d_sh)) {
  mes = d_sh[i,]
  pv.lm[i,] = anova(lm(m))[1:5,5]
  mes = mes[sample(1:ncol(d_sh))]
  pv.lm.p[i,] = anova(lm(m))[1:5,5]
}

pv.lm.bh = apply(pv.lm, 2, p.adjust, m = 'BH')
lm.sgn = cbind(tis = pv.lm.bh[,1] < 0.05,
               age = pv.lm.bh[,2] < 0.05 | pv.lm.bh[,3] < 0.05,
               tis.age = pv.lm.bh[,4] < 0.05 | pv.lm.bh[,5] < 0.05)

apply(lm.sgn, 2, sum)
