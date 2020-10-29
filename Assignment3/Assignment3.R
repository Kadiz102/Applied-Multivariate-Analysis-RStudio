> library(car)
> library(MASS)
> library(biotools)
dat = read.csv("C:/Users/Kyle/Desktop/admission.csv")
> Y = as.matrix(dat[,1:2]); n = nrow(Y); p = ncol(Y);
> Gn = dat[,3];
> G = as.factor(Gn);
> levels(G)[3] = "border"
> levels(G)[2] = "no"
> levels(G)[1] = "yes"
> G = relevel(G,ref="border")
> t.G = table(G)
> n1 = t.G[1]; n2 = t.G[2]; n3 = t.G[3]
> ybar = apply(Y,2,mean)
> ybark=by(Y,G,function(x) apply(x,2,mean))
> Sk = by(Y,G,cov)
> mlm = lm(Y~G)
> smlm = summary.aov(mlm)
> MA = Manova(mlm)
> SA = summary(MA)
> E = MA$SSPE
> B = mlm$coefficients
> C = c(0,-1,1);
> bm.test = boxM(Y,G)
> Sp = E/mlm$df.residual
> Uh = mlm$residuals
> Spih = chol(solve(Sp))
> RM = Uh%*%t(Spih)
> par(mfrow=c(1,2))
> hist(RM)
> qqnorm(RM,pch=16,main=NULL);abline(a=0,b=1);
> shapiro.test(RM)
> Tp = ((nk[1]-1)*Sk[[1]]+(nk[2]-1)*Sk[[2]]+(nk[3]-1)*Sk[[3]])
> Sp = Tp/(sum(nk)-3)
> CVS = T
> prior0 = c(1,1,1)/3;
> da2 = qda(G~Y,prior=prior0,CV=CVS)
> if (CVS==F) post.prob = predict(da,as.data.frame(Y))$posterior
> if (CVS==T) post.prob = da$posterior
> if (CVS==F) G.class = predict(da,as.data.frame(Y))$class
> if (CVS==T) G.class = da$class
> t.class = table(G,G.class)
> correct.rate = sum(diag(t.class))/n
> error.rate = 1-sum(diag(t.class))/n
> miss = data.frame(Y,G,G.class)[G!=G.class,]
> scatterplot(Y[,1]~Y[,2]|G,smooth=
> FALSE,ellipse=TRUE,by.groups=TRUE)
> points(miss[,2],miss[,1],pch=22,cex=3,lwd=1.5,col="purple")
> dat = data.frame(Y,G); G.class = NULL
> for(i in 1:n) { # leave-one-out #
> dat.i = dat[i,]; dat.xi = dat[-i,];
> fit = qda(G~.,data=dat.xi,CV=F)
> G.lab = predict(fit,newdata=dat.i,type="class")[[1]]
> G.class = c(G.class,as.character(G.lab)) }
> ct = table(G,G.class)