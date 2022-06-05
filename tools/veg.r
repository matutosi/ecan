  # D:/matu/work/stat/R/veg.r
  # 不要なものは削除している

## ジニ係数を計算する関数(ローレンツ曲線は描かない)
	# http://aoki2.si.gunma-u.ac.jp/R/Gini-index.html の部分関数
Gini.index.2 <- function(y){
    stopifnot(y >= 0)
    n <- length(y <- sort(y))
    y <- c(0, (y <- cumsum(y))/y[n])
    x <- seq(0, 1, length=n+1)
    2*sum(x-y)/n
}
## 地点ごとのGini係数を計算する関数
gini.per.stand <- function(df){
	table <- tapply(df$cover, list(df$stand, df$species), sum)	# テーブルを作成
	table[is.na(table)] <- 0
	g <- numeric()
	for (i in 1:nrow(table)){	# 地点ごとに繰り返し
		table.sub <- table[i,]
		g <- c(g, Gini.index.2(table.sub[table.sub>0]))	# http://aoki2.si.gunma-u.ac.jp/R/Gini-index.html
	}
	names(g) <- rownames(table)	# 地点名を追加
	round(g,4)	# 丸める
}
## 地点ごとのLorenz Asymmetry Coefficientを計算する関数
lorenz.asym.per.stand <- function(df){
	table <- tapply(df$cover, list(df$stand, df$species), sum)	# テーブルを作成
	table[is.na(table)] <- 0
	asym <- numeric()
	for (i in 1:nrow(table)){	# 地点ごとに繰り返し
		table.sub <- table[i,]
		asym <- c(asym, mean(lorenz(table.sub[table.sub>0])$asymmetry.coefficient))
	}
	names(asym) <- rownames(table)	# 地点名を追加
	round(asym, 4)	# 丸める
}

## 種の出現の有無を散布図で表示
plot.present <- function(df, title=T, sort=T, ... ){	# df: data1(環境経度), data2(環境経度), label(種名), title:ラベルの表題の表示, sort:ラベルの出現回数順ソート(T)か名前順ソート(F)
	#	label <- factor(names(sort(table(df[[3]]), decreasing=T)))	# 多い順にソートしたラベル
	if(sort==T){
		label <- factor(names(sort(table(df[[3]]), decreasing=T)))	# 多い順にソートしたラベル
	} else {
		label <- sort(unique(df[[3]]))	# 名前順でソート
	}
	for(i in 1:length(label)){	# ラベルごとに散布図を描画
		sub.df <- subset(df, df[[3]]==label[i])
		df.plot <- unique(df[1:2])
		plot(df.plot, ...)
		points(sub.df[[1]], sub.df[[2]], pch=16, ...)
		if(title==T) title(as.character(label[i]))	# 表題を描画
	}
}

## クラスター分析の結果から地点をクラスターに分割する
cut.cluster <- function(agnes, div=2, c.name=paste("cluster",as.character(1:div),sep="")){
	# 入力
		# agnes：library(cluster) の関数 agness() で解析した結果
		# div：分割数
		# c.name：クラスターの名称
	# 使用例
		# library(vegan)
		# library(cluster)
		# data(dune)
		# cluster <- agnes(dune)
		# div.cluster(cluster)
	if(div!=length(c.name)) return("分割数とクラスターの名称数が異なります")	# エラーチェック
	c.point <- agnes$height	# 結合点
	stand <- agnes$order	# 地点名
		# 1つに分割の場合：全部同ラスター
	if(div==1) return(data.frame(stand=stand, cluster=rep(c.name[1], length(c.point)+1)))
		# 要素数以上に分割：全部違うクラスター
	if(div>=length(c.point)) return(data.frame(stand=stand, cluster=paste("cluster",as.character(1:(length(c.point)+1)),sep="")))
	thres <- sort(c.point, decreasing=T)[div]	# 閾値を設定
	j <- 1
	cluster <- c.name[j]
	for(i in 1:length(c.point)){
		if(c.point[i]>thres) j <- j+1	# 次のクラスター
		cluster <- c(cluster, c.name[j])
	}
	data.frame(stand=stand, cluster=cluster)
}

## twinspan

  # twinspan パッケージでmodified twinspanも実行できるらしい(詳細は不明)
  # https://github.com/jarioksa/twinspan
  #     modified twinspan: https://github.com/zdealveindy/twinspanR
  #   library (twinspanR)
  library (twinspan)
  library (vegan)
  data(dune)
  tw <- twinspan::twinspan(dune)



## 類似度指数
similarity <- function(x, y, method="cc"){
	x <- as.character(x[[1]])
	y <- as.character(y[[1]])
	a <- length(intersect(x,y))
	b <- length(setdiff(x,y))
	c <- length(setdiff(y,x))
	if(method=="cc"){
		a / (a + b + c)	# Jaccard(1901)の群集係数(CC: coefficient of community)
	} else {	# 
		2 * a / (2 * a + b + c)	# Sorensen(1948の類似商(QS: quotient of similarity)
	}
}
## 類似度指数(a,b,c,dの分割表)：a：共通の出現地点, b：sp1のみの地点，c：sp2のみの地点，d：不出現の地点(sp1・sp2とも)
	# jaccardの類似度指数
sim.jaccard <- function(a,b,c,d){
	# pl.jc <- round(freq$pl.com     / (freq$pl.com + freq$pl.sp1 + freq$pl.sp2)                        , 2)
}
	# simpsonの類似度指数
sim.simpson <- function(a,b,c,d){
	# pl.sm <- round(freq$pl.com     / apply(cbind(c(freq$pl.com + freq$pl.sp1), c(freq$pl.com + freq$pl.sp2)), 1, min), 2)
}
	# diceの類似度指数
sim.dice    <- function(a,b,c,d){
	# pl.dc <- round(2 * freq$pl.com / (freq$pl.com + freq$pl.sp1 + freq$pl.com + freq$pl.sp2)          , 2)
}
	# cohenのKappa係数(一致率)
sim.cohen <- function(a,b,c,d){
	# http://www.mizumot.com/stats/kappa.htm
	# https://en.wikipedia.org/wiki/Cohen%27s_kappa
	po <- (a+d) / (a+b+c+d)
	pe <- ((a+b)*(a+c) + (b+d)*(c+d)) / (a+b+c+d)^2
	(po - pe) / (1 - pe)
}



## 最小面積とSα
Sa <- function(c, z) c*(1/(c*z))^(z/(z-1))	# Sαを求める関数
Aa <- function(c, z) (1/(c*z))^(1/(z-1))	# Aαを求める関数
sp.area <- function(sp){	# sp：veganのspecaccumの結果データ(面積と種数)
	# https://www.naro.affrc.go.jp/project/results/laboratory/nilgs/2009/nilgs09-30.html
	# 種数-面積関係：S=cA^z
	# 対数変換した式：log10(S) = z*log(A) + log(c)
	coef <- coef(glm(log10(sp$richness) ~ log10(sp$sites)))
	z <- coef[2]	# 係数(z)
	c <- 10^coef[1]	# 切片(c, Intercept)は対数変換したので，もとに戻す
	sa <- Sa(c=c, z=z)
	aa <- Aa(c=c, z=z)
	return(list(c=c, z=z, sa=sa, aa=aa))
	# c1 * sp1$sites^z1
	# sp1$richness
	# plot(c1 * sp1$sites^z1)  # モデルで計算したデータ
	# points(sp1$richness, col="red")  # 実際のデータの平均値
	# 以下の3行は，対数変換後の確認用
	# z1 * log10(sp1$sites) + log10(c1)
	# log10(sp1$richness)
	# plot(z1 * log10(sp1$sites) + log10(c1) , log10(sp1$richness))
}


## make sub-web from complecated lfood web(d:/matu/OneDrive/reference/id/03528.pdf)
fw.sub <- function(df){	# $resorce, $consumer，複雑な食物網でのnestednessの計算用
	cons <- unique(df$consumer)	# 1 consumerの種一覧
	n.consumer <- length(cons)	# 1 consumerの種数
	con.sample <- sample(cons, size=sample(x=1:n.consumer, size=1), replace=F)	# 2 consumerから抽出
	con.sample <- subset(df, df$consumer %in% con.sample)	# 3 consumerの該当のsubwebを抽出
	both <- intersect(con.sample$resorce, con.sample$consumer)	# 4 consumerかつresorceの種(both)
	res <- subset(con.sample, !(con.sample$resorce %in% both))	# 5 bothがresorceの種を除去
	res
}


## PodaniさんのS, D, R指数の基本的な計算
sdr <- function(mt, i, j){
	# d:/matu/OneDrive/reference/id/03377.pdf
	a <- sum(mt[,i] + mt[,j] ==  2)	# 共通種
	b <- sum(mt[,i] - mt[,j] == -1)	# 片方だけの種
	c <- sum(mt[,j] - mt[,i] == -1)	# 片方だけの種
	n <- sum(mt[,j] + mt[,i] >   0)	# 両方を結合した種数
	Sjac <- a / n
	# Brel <- (b+c)/n 
	Drel <- abs(b-c)/n
	# Arel <- (a+2*min(b,c))/n
	Rrel <- 2*min(b,c)/n
	# Nrel <- if(a>0) (a + abs(b-c))/n else 0
	c(Sjac, Drel, Rrel)
}
## PodaniさんのD, S, R指数のペアごとの計算
sdr.pair <- function(mt){
	# d:/matu/OneDrive/reference/id/03377.pdf
	m <- ncol(mt)	# 地点数
	mt[mt>0] <- 1	# 有無に変換
	st1 <- rep(1:(m-1), (m-1):1)	# 繰り返しの設定
	st2 <- unlist(mapply(":", 2:m, m))	# 繰り返しの設定
	# cbind(st1, st2)
	res <- matrix(0, ncol=3)	# 答えの入れ物
	colnames(res) <- c("Sjac", "Drel", "Rrel")
	for(Si in 1:length(st1)) res <- rbind(res, sdr(mt, st1[Si], st2[Si]))
	res <- as.data.frame(res[-1,])
	res
}
	# 以下はテストデータ
	# test <- matrix(ncol=19, byrow=T, 
	# c(
	# 1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	# 1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	# 0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,
	# 0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,
	# 0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,
	# 0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,
	# 0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,
	# 0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,
	# 0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,
	# 0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,0,0,
	# 0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,0,
	# 0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,
	# 0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,
	# 0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,
	# 0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,
	# 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,
	# 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,
	# 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1
	# ))
	# triangle.plot(sdr.pair(test), scale=F, show.position=F)
## PodaniさんのD, S, Rの平均値
sdr.mean <- function(sdr){
	sdr.mean <- apply(sdr, 2, mean)
	sdr.mean <- matrix(c(sdr.mean, 1 - sdr.mean), ncol=6)
	colnames(sdr.mean) <- c("Sjac", "Drel", "Rrel", "Brel",  "Arel",  "Nrel")
	as.data.frame(sdr.mean)
}

## Baselga(2012)の1つの個別計算
basel.base <- function(mt, i, j, stand=F){
	# d:/matu/OneDrive/reference/id/03381.pdf
	a <- sum(mt[,i] + mt[,j] ==  2)	# 共通種
	b <- sum(mt[,i] - mt[,j] == -1)	# 片方だけの種
	c <- sum(mt[,j] - mt[,i] == -1)	# 片方だけの種
	n <- a + b + c	# 両方を結合した種数
	Bjac <- (b+c) / n
	Bjtu <- 2 * min(b,c) / (a + 2 * min(b,c))
	Bjne <- ((max(b,c) - min(b,c)) / (a + b + c)) * (a / (a + 2 * min(b,c)))
	if(stand==T) c(Bjac, Bjtu, Bjne, i, j) else c(Bjac, Bjtu, Bjne)	# 地点情報の有無
}
## Baselga(2012)
basel <- function(mt, stand=F){
	# d:/matu/OneDrive/reference/id/03381.pdf
	m <- ncol(mt)	# 地点数
	mt[mt>0] <- 1	# 有無に変換
	st1 <- rep(1:(m-1), (m-1):1)	# 繰り返しの設定
	st2 <- unlist(mapply(":", 2:m, m))	# 繰り返しの設定
	# cbind(st1, st2)
	nc <- if(stand==T) 5 else 3	# 地点情報の有無
	res <- matrix(0, ncol=nc)	# 答えの入れ物
	colnames(res) <- c("Bjac", "Bjtu", "Bjne", "st1", "st2")[1:nc]	# 地点情報の有無
	for(Si in 1:length(st1)) res <- rbind(res, basel.base(mt, st1[Si], st2[Si], stand=stand))
	res <- as.data.frame(res[-1,])
	res
}
basel2 <- function(mt, stand=T, divide=10000){
	# d:/matu/OneDrive/reference/id/03381.pdf
	m <- ncol(mt)	# 地点数
	mt[mt>0] <- 1	# 有無に変換
	st1 <- rep(1:(m-1), (m-1):1)	# 繰り返しの設定
	st2 <- unlist(mapply(":", 2:m, m))	# 繰り返しの設定
	nc <- if(stand==T) 5 else 3	# 地点情報の有無
	# divideよりも組み合わせが多かったら，分割して実行
	dev <- ceiling(length(st1) / divide)	# 区切り数
	for(Di in 1:dev){
		bgn <- (Di-1) * divide + 1	# 該当回の最初
		end <- min(Di * divide, length(st1))	# 該当回の最後(全体の最終：length(st1))
		res <- matrix(0, ncol=nc)	# 答えの入れ物
		for(Si in bgn:end) res <- 
			rbind(res, basel.base(mt, st1[Si], st2[Si], stand=stand))
		res <- as.data.frame(res[-1,])
		fn <- paste("d:/tmp", Di, ".txt", sep="")
		write.table(res, file=fn, quote=F, row.names=F, col.names=F, sep="\t")
	}
	res <- matrix(0, ncol=nc)	# 答えの入れ物
	for(Di in 1:dev){
		fn <- paste("d:/tmp", Di, ".txt", sep="")
		res <- rbind(res, read.table(file=fn, sep="\t", header=F))
	}
	res <- as.data.frame(res[-1,])
	colnames(res) <- c("Bjac", "Bjtu", "Bjne", "st1", "st2")[1:nc]	# 地点情報の有無
	res
}
## Baselga(2012) C言語バージョン
baselgaC <- function(mt){
	# Cに渡す引数
	mt <- as.matrix(mt)	# 行列に変換
	mt[mt>0] <- 1	# 有無に変換
	nr <- nrow(mt)	# 種数
	nc <- ncol(mt)	# 地点数
	nn <- nc * (nc -1) / 2	# 組み合わせ数
	# Cによる計算
	bas <- .C("baselga",
		mt = as.integer(mt),	# 引数(行列)
		nr = as.integer(nr),	# 引数(種数)
		nc = as.integer(nc),	# 引数(地点数)
		bjac = double(nn),	# 返り値(Jaccardの類似度)
		bjtu = double(nn),	# 返り値(TurnOver)
		bjne = double(nn)	# 返り値(Nestedness)
	)
	# 計算後の処理
	stn <- if(is.null(colnames(mt))) 1:length(mt) else colnames(mt)	# 地点名
	st1 <- rep(1:(nc-1), (nc-1):1)	# 繰り返しの設定
	st2 <- unlist(mapply(":", 2:nc, nc))	# 繰り返しの設定
	bas <- cbind(as.data.frame(cbind(bas$bjac, bas$bjtu, bas$bjne)), 
		as.data.frame(cbind(stn[st1], stn[st2])))
	colnames(bas) <- c("Bjac", "Bjtu", "Bjne", "st1", "st2")
	return(bas)
}
	# dyn.load("D:/matu/work/stat/R/c/matutosi.dll")
	# dyn.unload("D:/matu/work/stat/R/c/matutosi.dll")
	# 以下はCのソースコード
	# #include <stdio.h>
	# // Baselga 2012の指数
	# void baselga(int *mt, int *nr, int *nc, 	// 引数
	# 	double *bjac, double *bjtu, double *bjne)	// 返り値
	# {
	# 	double a, b, c, d, e;	// あとでbjtuと合わせるので，doubleで設定
	# 	int i=0, j=0, k=0, m=0;
	# 	for(i=0; i < *nc; i++){
	# 		for(j=i+1; j < *nc; j++){
	# 				a=0; b=0; c=0; d=0; e=0;	// d：max(b,c)，e：min(b,c)
	# 				for(k=0; k < *nr; k++){
	# 				if(mt[(i * *nr + k)] + mt[(j * *nr + k)] ==  2) a++;
	# 				if(mt[(i * *nr + k)] - mt[(j * *nr + k)] == -1) b++;
	# 				if(mt[(j * *nr + k)] - mt[(i * *nr + k)] == -1) c++;
	# 			}
	# 			if(b>c) {d=b;} else {d=c;}	//max(b,c)
	# 			if(b<c) {e=b;} else {e=c;}	// min(b,c)
	# 			bjac[m] = (b + c) / (a + b + c);	// 類似度(Jaccard)
	# 			bjtu[m] = (2 * e) / (a + 2 * e);	// 入れ替わり(TurnOver)
	# 			bjne[m] = ((d - e) / (a + b + c)) * (a / (a + 2 * e));	// 入れ子(Nestedness)
	# 			m++;
	# 	// d:/matu/OneDrive/reference/id/03381.pdf
	# 		}
	# 	}
	# }
	# // D:/matu/work/stat/R/c/c.bat

## 三角プロット(https://staff.aist.go.jp/a.noda/programs/ternary/ternary.plot から)
	# Ternary.plot
	# Drawing ternary plot of compositional data
	# (modified from triangle.plot in ade4)
	#
	# Noda Atsushi
	# Last updated: 2008/09/17 16:27:46.
	# 0.1.2: axes. 軸（三角形の辺）を描くかどうかを選択できるようにした．
	# 0.1.1: プロットされる点の型 (pch) と色(col) と大きさ (cex) を選択できるようにした．
	# 0.1: just beggining.
ternary.plot <- function (dat, label=rownames(dat), axes=TRUE, clabel=0, cpoint=1, cline=0, lwd=1, addline = TRUE, label.vertices=TRUE, cline.col="black", pch=par("pch"), col=par("col"), cex=par("cex"), cex.text=1.5, seg.col=seg.col) {
    par.orig <- par(mar = par("mar"))
    on.exit(par(par.orig))
    dat <- dat[c(3,2,1)]	# triangle.plotと合わせるために並べ替え(by matsut)
    nam <- colnames(dat)
    dat <- dat/apply(dat,1,sum)
    xy <- matrix(NA,nrow=nrow(dat),ncol=2)
    xy[,1] <- (dat[,1]+2*dat[,3])/2
    xy[,2] <- sqrt(3)*dat[,1]/2
    X <- c(1/2,sqrt(3)/2)
    Y <- c(0,0)
    Z <- c(1,0)
	#   plot(0, 0, type="n", xlim=c(0, 1), ylim=c(-0.1, 1), xlab = "", ylab = "", xaxt = "n", yaxt = "n", asp = 1, frame.plot=FALSE)
			plot(0, 0, type="n", xlim=c(0, 1), ylim=c(-0.1, 1), xlab = "", ylab = "", xaxt = "n", yaxt = "n", asp = 1, frame.plot=FALSE)
    if (addline) {
      int <- 10
      x.add <- matrix(NA,nrow=(int-1),ncol=2)
      y.add <- matrix(NA,nrow=(int-1),ncol=2)
      z.add <- matrix(NA,nrow=(int-1),ncol=2)
        for (i in 1:nrow(x.add)) {
            x.add[i,] <- Z + (i/10) * (X-Z)
            y.add[i,] <- X - (i/10) * (X-Y)
            z.add[i,] <- Y + (i/10) * (Z-Y)
          }
      for (i in 1:nrow(x.add)) {
        segments(x.add[i,1], x.add[i,2], z.add[int-i,1], z.add[int-i,2], col = "lightgrey")
        segments(y.add[i,1], y.add[i,2], z.add[int-i,1], z.add[int-i,2], col = "lightgrey")
        segments(x.add[i,1], x.add[i,2], y.add[int-i,1], y.add[int-i,2], col = "lightgrey")
      }
    }
    if (axes) {
      segments(X[1],X[2],Y[1],Y[2],lwd=2)
      segments(Y[1],Y[2],Z[1],Z[2],lwd=2)
      segments(Z[1],Z[2],X[1],X[2],lwd=2)
    }
    if (label.vertices) {
	#         text(X[1], X[2], labels = nam[1], cex = 1.5, pos = 3)
	#         text(Y[1], Y[2], labels = nam[2], cex = 1.5, pos = 1)
	#         text(Z[1], Z[2], labels = nam[3], cex = 1.5, pos = 1)
			text(X[1], X[2], labels = nam[1], cex=cex.text, pos = 3)	# 文字サイズの変更
			text(Y[1], Y[2], labels = nam[2], cex=cex.text, pos = 1)	# 
			text(Z[1], Z[2], labels = nam[3], cex=cex.text, pos = 1)	# 
      }
    if (cpoint > 0) 
	#   points(xy, pch=pch, cex=cex*cpoint, col=col)
			par(new=T)	# sunflowerplot用
			sunflowerplot(xy, cex=cex*cpoint, col=col, seg.col=seg.col,	# 重複点を示す
				size=0.05, seg.lwd=0.5, xlim = c(-0.1, 1.1), ylim = c(-0.1, 1), xlab = "", ylab = "", xaxt = "n", yaxt = "n", asp = 1, frame.plot=FALSE)
	#  bty="n"
    if (cline > 0) 
        lines(xy,lwd=1,col=cline.col)
    if (clabel > 0) 
        text(xy[, 1], xy[, 2], label, clabel)
    return(invisible(xy))
}

## PodaniさんのDSRを三角プロット(ternary.plotは上で定義)で表示
tp2 <- function(sdr, sdr.m, col=1, pch=16, cex=4, cex.text=1.5, seg.col=2){
	# https://staff.aist.go.jp/a.noda/programs/ternary/ternary.html#R
	ternary.plot(sdr.m[1:3], col=col, pch=pch, cex=cex, cex.text=cex.text, seg.col=seg.col)	# 平均値
	par(new=T)
	ternary.plot(sdr, cex.text=cex.text, seg.col=seg.col)	# 個々の点
	# 計算結果の表示
	tex.x <- c( 0.00,       1.00,      0.50,     0.50,     0.50,    0.85,     0.85,     0.15,      0.15)
	tex.y <- c(-0.11,      -0.11,      0.99,    -0.05,    -0.11,    0.45,     0.52,     0.45,      0.52)
	sdr.m <- round(sdr.m, 2)
	tex   <- c(sdr.m[1], sdr.m[2], sdr.m[3],   "Nrel", sdr.m[6],  "Arel", sdr.m[4],  "Brel ", sdr.m[5])
	# tex   <- c("左下",  "右下",   "上",      "下",       "下",  "右辺",  "左辺",  "右辺",  "左辺")
	text(x=tex.x, y=tex.y, tex, cex=cex.text, font=2)
}
