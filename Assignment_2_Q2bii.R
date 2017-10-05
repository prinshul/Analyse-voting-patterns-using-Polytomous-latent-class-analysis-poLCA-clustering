load("2016_First6Votes_YesNoAbsent.Rdata")

cat_dataframe<-as.data.frame(votes)
rownames(cat_dataframe)<-NULL
f<- cbind(ED1,ED2,Credit,Confidence1,Confidence2,Trade)~1   #formula for poLCA
bic_array=vector()
aic_array=vector()
for(k in 2:7){             # check for cluster k=2 to 7
  min_bic=100000
  min_aic=100000
  for(j in 1:500){        # try to avoid local maxima by running multiple times
    res<-poLCA(f, cat_dataframe, nclass = k, maxiter = 10000)
    if(res$bic < min_bic)
    {
      min_bic = res$bic
    }
    if(res$aic < min_aic)
    {
      min_aic = res$aic
    }
  }
  bic_array<-c(bic_array,c(min_bic))
  aic_array<-c(aic_array,c(min_aic))
}