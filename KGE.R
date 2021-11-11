options(CRAN="https://cran.ism.ac.jp/")
options(repos="https://cran.ism.ac.jp/")
options(warn=-1)
tryCatch(
  { library(wordcloud2) }
  , error = function(e) { install.packages("wordcloud2"); library(wordcloud2)}
)

DF = read.table("https://github.com/sjfjmt/MC/raw/main/20211110.csv",sep=",",header=TRUE)

DD = DF[["DD"]]
TTQ = as.character(DF[["TTQ"]])
TRF = DF[["TRF"]]

sc <- function(CEX=1.2) {
  for (ii in rev(unique(DD))) {
    idx <- DD == ii
    par(mai=c(1,1,1,1),omi=c(0,0,0,0))
    plot(c(0,1),c(0,1),type="n",xlab="",ylab="",axes=FALSE)
    par(xpd=TRUE)
    len <- length(which(idx))
    text(0,1,"順位",adj=1,cex=CEX)
    text(0.2,1,"検索語",adj=0,cex=CEX)
    text(1,1,"件数+",adj=1,cex=CEX)
    trfmax <- 0xffffffff
    for (jj in seq_len(len)) {
      if (trfmax<TRF[idx][jj]) break
      trfmax <- TRF[idx][jj]
      text(0,1-0.1*jj,jj,adj=1,cex=CEX)
      text(0.2,1-0.1*jj,TTQ[idx][jj],adj=0,cex=CEX)
      text(1,1-0.1*jj,TRF[idx][jj],adj=1,cex=CEX)
    }
    mtext(paste(substring(ii,1,4),substring(ii,5,6),substring(ii,7,8),sep="/"),3,cex=2)
    sl <- select.list(c("前の日"))
    if (sl=="") break
  }
}

TTQ2 <- TTQ[order(TRF,decreasing=TRUE)]
TRF2 <- TRF[order(TRF,decreasing=TRUE)]
TTQ2 <- substr(TTQ2,1,9)
TRF2 <- TRF2[!duplicated(TTQ2)]
TTQ2 <- TTQ2[!duplicated(TTQ2)]

wc <- function(s="circle",c="random-dark",bc="white",f=0,size=1,ellipticity=1,gridSize=6,rm=NULL,...) {
  if (f==1) { ff <- "HG創英角ﾎﾟｯﾌﾟ体" }
  else if (f==2) { ff <- "HG正楷書体-PRO" }
  else if (f==3) { ff <- "HG丸ｺﾞｼｯｸM-PRO" }
  else if (f==4) { ff <- "ＭＳ 明朝" }
  else { ff <- "MS ゴシック" }
  if (is.integer(rm)) {
     wordcloud2(data.frame(Term=TTQ2[-rm],Freq=TRF2[-rm]),size=size,ellipticity=ellipticity,gridSize=gridSize,shape=s,color=c,backgroundColor=bc,fontFamily=ff,...)
  }
  else {
    wordcloud2(data.frame(Term=TTQ2,Freq=TRF2),size=size,ellipticity=ellipticity,gridSize=gridSize,shape=s,color=c,backgroundColor=bc,fontFamily=ff,...)
  }
}

cat("#--- オープンキャンパス用のライブラリのインポートが完了しました ---#\n")

