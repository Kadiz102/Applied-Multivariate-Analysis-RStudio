> datv = scan()
1: 72 26 9 10 11 70
7: 63 76 7 85 22 93
13: 96 31 7 83 63 73
19: 96 98 6 82 75 97
25: 84 94 6 36 77 97
31: 66 10 5 28 24 75
37: 31 40 9 64 23 75
43: 45 14 2 19 15 50
49: 42 18 6 33 13 70
55: 79 74 4 23 14 90
61: 39 12 2 37 13 70
67: 54 35 3 23 74 53
73: 60 75 5 45 58 83
79: 63 45 5 22 67 53
85: 
Read 84 items
> dat = matrix(datv,14,6,byrow=T)
> Y = as.matrix(dat[,1:3]);
> colnames(Y) = c("career","supervisor","finance")
> X = as.matrix(dat[,4:6]);
> colnames(X) = c("variety","feedback","autonomy")
> n = nrow(dat); p = ncol(Y); m = ncol(X);
> R = cor(dat)
> mlm = lm(Y~X[,1]+X[,2]+X[,3])
> smlm = summary(mlm)
> MA = Manova(mlm,test='Wilks')
> SA = summary(MA)

Y = as.matrix(dat[,1:3]);
> Y = as.matrix(dat,[1:3]);
> X = as.matrix(dat[,4:6]);
> n = row(Y); p = ncol(Y); m = ncol(X)
> R = cor(dat); R2 = R^2
> smlm.Y = summary(lm(Y~X))
> smlm.X = summary(lm(X~Y))
> cc = cca(X,Y,xscale=T,yscale=T)
> Ryv = cc$ycrosscorr
> Rxu = cc$xcrosscorr
