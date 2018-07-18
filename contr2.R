dt = read.table("1.txt", check.names = T)
#dt = as.matrix(dt)
#dt = t(dt)

res = dt[,1]
sex = dt[,2]
wep = dt[,3]
alh = dt[,4]
mt = dt[,2:4]#data.frame(sex, wep, alh)
length(sex)
#GLM
col=ifelse( sex == 'f','red','blue')

#res_sum = sum(dt[,1])
#x = cbind(dt[i,], res_sum - dt[i,])

#plot(alh,dt[,1],pch=16,col=ifelse( sex =='f','red','blue'))

m = res ~ sex*alh*wep# + sex:alh + sex:wep + alh:wep

pv.glm = anova(glm(m, family = 'poisson'), test = 'Chisq')#[-1, 5]

gm = glm(m)

x = seq(from=min(alh),to=max(alh),length.out = 100)
c = gm$coefficients

bp = predict(gm,mt,type = 'resp')

plot(dt$res, bp, col=ifelse(sex =='f','red','blue'))

#b = function(x){c[1]+c[3]*x+c[4]*x^2}
#l = function(x){c[1]+c[2]+(c[3]+c[5])*x+(c[4]+c[6])*x^2}
#lines(x,1/(1+exp(-b(x))),col='red')
#lines(x,1/(1+exp(-l(x))),col='green')