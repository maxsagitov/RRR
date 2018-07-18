options(stringsAsFactors = F)
d = read.table('hw6.counts.txt', check.names = F)
d = as.matrix(d)
dim(d)

d = d[apply(d,1,mean)>=10,]
dim(d)
d = sweep(d,2,apply(d,2,sum),'/')

#razdelili kawdoe imya po podcherkivaniyu
meta = strsplit(colnames(d),'_',fixed = TRUE)

#eto list, poetomy nado v matrix
meta = do.call(rbind, meta) #matrix, no tekstovaya
m = as.data.frame(meta) #matrix v data frame
colnames(m) = c('tissue', 'age')
m$age = as.numeric(m$age) #chtobi vozrast stal ciframi

age = m$age^0.25
age_2 = m$age^0.5
model = y ~ m$tissue + age +age_2 + m$tissue:age + m$tissue:age_2


pv.lm = matrix(ncol = 5, nrow = nrow(d))

for (i in 1:nrow(d)) {
   y = d[i,]
    reg=lm(model)
    pv.lm[i,] = anova(reg)[1:5,5]
    }
colnames(pv.lm) = c('tissue', 'age', 'age_2', 'tissue:age', 'tissue:age_2')
pv.lm.bh = apply(pv.lm, 2, p.adjust, m = 'BH') #bonferroni correction

sum(pv.lm.bh[,1] <0.05) # # significant tissue
sum(pv.lm.bh[,2] <0.05) # # significant age
sum(pv.lm.bh[,3] <0.05) # # significant age_2
sum(pv.lm.bh[,4] <0.05)# # significant age:tissue
sum(pv.lm.bh[,5] <0.05)# # significant age_2:tissue

pv.thr = c()
for (i in 1:ncol(pv.lm.bh)){
  pv.th = max(pv.lm[pv.lm.bh[,i]<0.05,i])
  pv.thr = c(pv.thr, pv.th)
} 
pv.thr

pv.lm.perm = matrix(ncol = 5, nrow = nrow(d))
#permutation
for (i in 1:5) {
   order = sample(1:ncol(d))
     d_2 =d[,order]
#permutation lm p-values
  for (i in 1:nrow(d)) {
     y = d_2[i,]
    reg=lm(model)
    pv.lm.perm[i,] = anova(reg)[1:5,5]
    }
pv.lm.bh.perm = apply(pv.lm.perm, 2, p.adjust, m = 'BH')

sum(pv.lm.bh.perm[,1] <pv.thr[1])} # # significant tissue 0


    

     
