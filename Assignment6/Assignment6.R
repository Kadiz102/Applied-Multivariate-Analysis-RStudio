> dat = read.csv("C:/Users/Kyle/Desktop/tests.csv")
> var.names = c('test1','test2','test3','test4','test5','test6')
> names(dat) = var.names
> Y = as.matrix(dat); n = nrow(Y); p = nrow(Y);
> S=cov(Y);R=cor(Y);
> model = specifyModel(quiet=T)
1: f1 <-> f1,NA, 1
2: f2 <-> f2, NA, 1
3: f1 <-> f2, phi12, NA
4: f1 -> test1, 111, NA
5: f1 -> test2, 121, NA
6: f1 -> test3, 131, NA
7: f2 -> test4, 142, NA
8: f2 -> test5, 152, NA
9: f2 -> test6, 162, NA
10: test1 <-> test1, psi1, NA
11: test2 <-> test2, psi2, NA
12: test3 <-> test3, psi3, NA
13: test4 <-> test4, psi4, NA
14: test5 <-> test5, psi5, NA
15: test6 <-> test6, psi6, NA
> opt = options(fit.indices=c("GFI","AGFI","RMSEA","NFI", "NNFI","CFI","SRMR","AIC","BIC"))
> sem1 = sem(model,S,n,obs.variables=var.names,start.tol=1E-10)
> summary(sem1)
> model.r  = specifyModel(quiet=T)
1: f1 <-> f1,NA, 1
2: f2 <-> f2, NA, 1
3: f1 <-> f2, NA, 0
4: f1 -> test1, 111, NA
5: f1 -> test2, 121, NA
6: f1 -> test3, 131, NA
7: f2 -> test4, 142, NA
8: f2 -> test5, 152, NA
9: f2 -> test6, 162, NA
10: test1 <-> test1, psi1, NA
11: test2 <-> test2, psi2, NA
12: test3 <-> test3, psi3, NA
13: test4 <-> test4, psi4, NA
14: test5 <-> test5, psi5, NA
15: test6 <-> test6, psi6, NA
> sem2 = sem(model.r,S,n,obs.variables=var.names,start.tol=1E-10)
> summary(sem2)
> anova(sem1,sem2)
> d = dist(Y,method="euclidean") 
> hc = hclust(d,method="average")
> cor.coph = cor(d,cophenetic(hc))
> hh  =  hc$height
> k = 1.25; hh.crit = mean(hh)+k*sd(hh)
> groups = cutree(hc,h=hh.crit)
> table(groups)
> plot(hc,hang=-1)   
> abline(h=hh.crit,lty=2,lwd=2)


