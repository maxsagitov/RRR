d = read.table('d:/Downloads/hw6.counts.txt',check.names = F)
t = sapply(strsplit(colnames(d),'_'),'[',1)
a = as.numeric(sapply(strsplit(colnames(d),'_'),'[',2))
d = as.matrix(d)
d = sweep(d,2,apply(d,2,sum),'/')
meta = data.frame(t=t,a=a)

ms = apply(d,1,function(x)
           lm(x ~ t + I(a^0.25) + I(a^0.5)+ 
                I(a^0.25):t + I(a^0.5):t))
pv = sapply(ms,function(x)anova(x)[,5])
pv = t(pv)
plot(a^0.25,d[1,],pch=16,col=ifelse(t=='brain','red','green'))

c = ms[[1]]$coefficients
x = seq(from=min(a^0.25),to=max(a^0.25),length.out = 100)
b = function(x){c[1]+c[3]*x+c[4]*x^2}
l = function(x){c[1]+c[2]+(c[3]+c[5])*x+(c[4]+c[6])*x^2}
lines(x,b(x),col='red')
lines(x,l(x),col='green')

r = predict(ms[[1]],newdata = list(t=rep('brain',100),a=x^4))
l = predict(ms[[1]],newdata = list(t=rep('liver',100),a=x^4))
#lines(a[t=='brain']^0.25,r[t=='brain'],col='blue')
lines(x,r,col='red')
lines(x,l,col='green')

#GLM
col=ifelse(t=='brain','red','green')
totals = colSums(d)
tab = cbind(d[1,],totals-d[1,])
a = a^0.25
plot(a,tab[,1]/totals,col=col)
gm = glm(tab ~ t + a + I(a^2) + a:t + I(a^2):t,family = 'quasibinomial')

x = seq(from=min(a),to=max(a),length.out = 100)
c = gm$coefficients
b = function(x){c[1]+c[3]*x+c[4]*x^2}
l = function(x){c[1]+c[2]+(c[3]+c[5])*x+(c[4]+c[6])*x^2}
lines(x,1/(1+exp(-b(x))),col='red')
lines(x,1/(1+exp(-l(x))),col='green')

bp=predict(gm,newdata = list(a=x,t=rep('brain',100)),type = 'resp')
lines(x,bp,col='red')
