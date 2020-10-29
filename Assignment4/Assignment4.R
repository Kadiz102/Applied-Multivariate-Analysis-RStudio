> dat = read.csv('C:/Users/Kyle/Desktop/pottery.csv')
> Y.o = dat[,1:9];
> n = nrow(Y.o); p = ncol(Y.o)
> G = as.factor(dat[,10])
> R = as.factor(1*(G==1)+2*(G==2|G==3)+3*(G==4|G==5))
> Y = scale(Y.o,center=TRUE,scale=TRUE)
> set.seed(23)
> fit = 
> (G~.,data=dat)
> randomForest
> import = importance(fit)
> plot(fit,lwd=2)
> #fit = svm(G~.,data=dat,type=”C-classification”)
> dat = data.frame(Y,G); G.class = rep(NA,n)
> K = n 
> f = ceiling(n/K) 
> set.seed(0)
> ss = sample(rep(1:K,f),n)
> for(i in 1:K) { 
> dat.i = dat[ss==i,]; dat.xi = dat[ss!=i,];
> set.seed(23)
> fit = randomForest(G~.,data=dat.xi) 
> #fit = svm(G~.,data=dat.xi) 
> G.lab = predict(fit,newdata=dat.i,type="class")
> G.class[ss==i] = as.character(G.lab) }
> ct.cv = table(G,G.class) 
> ct = ct.cv
> correct.rate = sum(diag(ct))/n
> error.rate = 1-sum(diag(ct))/n
> c(correct.rate=correct.rate,error.rate=error.rate)


> dat = read.csv('C:/Users/Kyle/Desktop/pottery.csv')
> n = nrow(Y.o); p = ncol(Y.o)
> G = as.factor(dat[,10])
> R = as.factor(1*(G==1)+2*(G==2|G==3)+3*(G==4|G==5))
> Y = scale(Y.o,center=TRUE,scale=TRUE)
> d = dist(Y,method="euclidean")
> dat.G = data.frame(Y,G)
> dat.R = data.frame(Y,R)
> library(MASS)
> set.seed(23)      
> kc = kmeans(Y,3,algorithm="Hartigan-Wong") 
> groups = kc$cluster
> g.means = kc$centers
> cc0 = 6; wss = (n-1)*sum(apply(Y,2,var)); F = NA;
> for (i in 2:cc0) {
+ set.seed(23)
+ kci = kmeans(Y,i)
+ ssw = sum(kci$withinss) 
+ ssb = sum(kci$betweenss)
+ f = (ssb/(i-1))/(ssw/(n-i))
+ wss = c(wss,ssw); F = c(F,f);  }
> cc = which.max(F) 
> plot(1:cc0,wss,type="b",pch=16) 
> score.pca = princomp(Y,cor=FALSE)$scores
> score.lda = predict(lda(groups~Y))$x
> score = score.lda
> score.s = scale(score,center=TRUE,scale=TRUE)
> plot(score.s[,1:2],col=c("blue","red","green")[groups],
+ pch=c(15,16,17)[groups],ylim=c(-2.1,2.1))
> text(score.s[,1],score.s[,2]-.3,labs,cex=0.9)


> dat = read.csv('dat = read.csv('G:/4300/pottery.csv')/pottery.csv')

> 
> dat = read.csv(C:/Users/Kyle/Desktop/pottery.csv')
Error: unexpected '/' in "dat = read.csv(C:/"
> dat = read.csv('C:/Users/Kyle/Desktop/pottery.csv')
> Y.o = dat[,1:9];
> n = nrow(Y.o); p = ncol(Y.o)
> G = as.factor(dat[,10])
> R = as.factor(1*(G==1)+2*(G==2|G==3)+3*(G==4|G==5))
> Y = scale(Y.o,center=TRUE,scale=TRUE)
> d = dist(Y,method="euclidean")
> dat.G = data.frame(Y,G) 
> dat.R = data.frame(Y,R)
> set.seed(23) 
> set.seed(23)           
> kc = kmeans(Y,3,algorithm="Hartigan-Wong")
> groups = kc$cluster
> g.means = kc$centers 
> cc0 = 6; wss = (n-1)*sum(apply(Y,2,var)); F = NA;
> for (i in 2:cc0) {
+ set.seed(23)
+ kci = kmeans(Y,i)  
+ ssw = sum(kci$withinss)  
+ ssb = sum(kci$betweenss)  
+ f = (ssb/(i-1))/(ssw/(n-i))  
+ wss = c(wss,ssw); F = c(F,f);  }
> cc = which.max(F)
> G = as.factor(groups)
> library(randomForest)
> library(MASS)
> set.seed(23)
> fit = randomForest(G~.,data=dat)
> import = importance(fit)
> plot(fit,lwd=2)
> dat = data.frame(Y,G); G.class = rep(NA,n) 
> K = n
> f = ceiling(n/K) 
> set.seed(0)
> ss = sample(rep(1:K,f),n) 
> for(I in 1:K){
+ dat.i = dat[ss==i,]; dat.xi = dat[ss!=i,];
+ set.seed(23)
+ fit = randomForest(G~.,data=dat.xi) 
+ G.lab = predict(fit,newdata=dat.i,type="class")
+ G.class[ss==i] = as.character(G.lab) }
> ct.cv = table(G,G.class)
> ct = ct.cv
> correct.rate = sum(diag(ct))/n
> error.rate = 1-sum(diag(ct))/n
> c(correct.rate=correct.rate,error.rate=error.rate)



