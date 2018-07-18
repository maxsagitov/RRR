d = read.table("hw6.counts.txt", check.names = F)
d = as.matrix(d)
dim(d)
mes = cbind(d["XLOC_111636",], obs_sum - d["XLOC_111636",])
#obs_sum = apply(d, 2, sum)
#obs_sum - d["XLOC_111636",]
mes
meta = strsplit(colnames(d), '_')
meta = data.frame(
  tissue = sapply(meta, '[', 1),
  age = as.numeric(sapply(meta, '[', 2))
)
#meta = as.matrix(meta)
#f = meta$tissue == 'brain'

t = meta[,1]
a = meta[,2]^0.25
a2 = meta[,2]^0.5
m = mes ~ t + a + a2 + t*a + t*a2 + t:a + t:a2
mq=glm(m,family='quasibinomial')
anova(mq,test='Chisq')
summary(mq)
#gm = glm(yy ~ xx,family='gaussian')