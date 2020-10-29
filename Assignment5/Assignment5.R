vR = c( 1.000, 0.577, 0.509, 0.387, 0.462,
        0.577, 1.000, 0.599, 0.389, 0.322,
        0.509, 0.599, 1.000, 0.436, 0.426,
        0.387, 0.389, 0.436, 1.000, 0.523,
        0.462, 0.322, 0.426, 0.523, 1.000 )
      
> R = matrix(vR,5,5,byrow=T)
> names = c('chem','dupont','carbide','exxon','texaco') 
> rownames(R) = colnames(R) = names
> n = 100; p = nrow(R);
> m = 2;
> install.packages("psych")
> library(psych)
> install.packages("GPArotation")
> library(GPArotation)
> pc = princomp(covmat=R)
> spc = summary(pc,loadings=F,cutoff=0)
> lam=pc$sd^2
> lam.v=pc$loadings[,1:m]
> pc.load=lam.v%*%diag(sqrt(lam[1:m]))
> plot(pc,type="lines");abline(h=1);
> summary(pc)
> pc.load
> fa.pc= fa(R,m,fm="pa",rotate="none",max.iter=1,SMC=F)
> fa.pc
> fa.pc= fa(R,m,fm="pa",rotate="none",max.iter=1,SMC=false)
> print(fa.pc$load,cutoff=0)
> print(fa.pc,cutoff=0)
> print(fa.ml$loadings,cutoff=.4)
>c(fa.ml$factors,fa.ml$objective,fa.ml$dof,fa.ml$STATISTIC,fa.ml$PVAL)
> fa.ml1 = fa(R,m,fm="ml",rotate="varimax",normalize=T,SMC=F)
> print(fa.ml1$loadings,cutoff=0)
> fa.ml2 = fa(R,m,fm="ml",rotate="oblimin",normalize=T,SMC=F)
> print(fa.ml2$loadings,cutoff=0)
> c(fa.ml1$complex,mean_complex=mean(fa.ml1$complex))
> c(fa.ml2$complex,mean_complex=mean(fa.ml2$complex))
> c(fa.pf$complex,mean_complex=mean(fa.pf$complex))
> Lam = fa.ml$load[]; psi = fa.ml$uniquenesses
> R-(Lam%*%t(Lam)+diag(psi))
