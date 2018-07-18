y = cbind(rnorm(10,0),rnorm(10,2),rnorm(10,5))
x = matrix(1:30,ncol=3)
y.mean = apply(y,2,mean)

resid = sum(sweep(y,2,y.mean)^2)
total = sum((y-mean(y))^2)
var(as.numeric(y))*29 # то же самое
explained = total - resid
f.stat = (explained/2)/(resid/27)
pf(f.stat,2,27,lower.tail=F) # p-value!

yy = as.numeric(y)
xx = as.numeric(x)
# calc regression
b = cov(xx,yy)/var(xx)
a = mean(y) - b*mean(x)
plot(xx,yy,pch=19)
abline(a=a,b=b,col='red')
arrows(xx,yy,xx,a+b*xx,length=0.15)
# calc p-value
resid = sum((yy-(a+b*xx))^2)
explained = total - resid
f.stat = (explained/1)/(resid/28)
pf(f.stat,1,28,lower.tail=F)

f = factor(rep(1:3,each=10))
m = lm(yy ~ f)
anova(m)

m = lm(yy ~ xx)
anova(m)