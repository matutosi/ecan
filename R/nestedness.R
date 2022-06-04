## internal function in nodf
n_paired <- function(Ni, Nj){
  n_p <- sum((Ni+Nj)==2)  / sum(Nj) *100
  if(sum(Ni) <= sum(Nj)) n_p <- 0
  return(n_p)
}

## NODF (nestedness)
nodf <- function(mt, nest="matrix"){
  mt[mt>0] <- 1  # convert to presence / absense
  mt <- mt[, order(colSums(mt), decreasing=T)]  # desc order of frequency
  mt <- mt[order(rowSums(mt), decreasing=T), ]
  nc <- ncol(mt)
  nr <- nrow(mt)
  n_p_col <- 0
  n_p_row <- 0
  for(Ni in 1:(nc-1)){ # repeat in cols
    for(Nj in (Ni+1):nc){
      # print(paste("Ni:", Ni, "  Nj:", Nj, sep="")) # for debug
      # print(n_paired(mt[,Ni], mt[,Nj]))
      n_p_col <- n_p_col + n_paired(mt[,Ni], mt[,Nj])
    }
  }
  for(Nk in 1:(nr-1)){  # repeat in rows
    for(Nl in (Nk+1):nr){
      n_p_row <- n_p_row + n_paired(mt[Nk,], mt[Nl,])
    }
  }
  if(nest=="matrix"){ # nestedness in matrix
    return((n_p_col + n_p_row) / (nc*(nc-1)/2 + nr*(nr-1)/2))
  } else if(nest=="col"){ # nestedness in columns
    return(n_p_col / nc*(nc-1)/2)
  } else if(nest=="row"){ # nestedness in row
    return(n_p_row / nr*(nr-1)/2)
  }
}
## NODFの検定用ランダム配置
rand_mt <- function(mt, method="cr"){
  if(method=="cr"){ # 行・列の確率の平均
    m_row <- matrix(rep(rowSums(mt)/ncol(mt), times=ncol(mt)), byrow=F, ncol=ncol(mt))
    m_col <- matrix(rep(colSums(mt)/nrow(mt), times=nrow(mt)), byrow=T, ncol=ncol(mt))
    m_cr <- (m_row+m_col)/2
    mt <- matrix(mapply(rbinom, n=1, size=1, prob=m_cr), byrow=F, ncol=ncol(mt))
  } else if(method=="all"){  # 全体を無作為に配置
    mt <- matrix(sample(x=as_vector(mt), size=length(mt)), ncol=ncol(mt))
    # matrix(rbinom(n=length(mt), size=1, prob=mean(mt)), ncol=ncol(mt))
  }else if(method=="col"){ # 
    for(Ci in 1:ncol(mt)) mt[,Ci] <- sample(x=mt[,Ci], size=nrow(mt))
  }else if(method=="row"){  # 行で無作為に配置
    for(Ri in 1:nrow(mt)) mt[Ri,] <- sample(x=mt[Ri,], size=ncol(mt))
  }
  return(mt)
}
## NODFを使った検定
test_nodf <- function(mt, method="cr", times=1000){
  rnd <- c()
    # 1回目
  for(i in 1:times) rnd <- c(rnd, nodf(rand_mt(mt, method=method)))
  rnd <- rnd[!is.na(rnd)]
  times2 <- length(rnd)
  times <- (times-times2) * times/times2 * 1.5
    # 2回目で不足分を補う
  for(i in 1:times) rnd <- c(rnd, nodf(rand_mt(mt, method=method)))
  rnd <- rnd[!is.na(rnd)]
  times <- length(rnd)
    # 結果のまとめ
  p <- sum(rnd > nodf(mt)) / times
  list(values=rnd, matrix=mt, nodf=nodf(mt), p=p, times=times)
}
