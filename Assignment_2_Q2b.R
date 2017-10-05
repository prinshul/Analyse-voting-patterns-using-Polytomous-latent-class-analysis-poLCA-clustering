library(MASS)
library(poLCA)
load("2016_First6Votes_YesNoAbsent.Rdata")
bin_data <- ((votes==1)*1) +1    # 1 absent 2 present
bin_dataframe<-as.data.frame(bin_data)
rownames(bin_dataframe)<-NULL
f<- cbind(ED1,ED2,Credit,Confidence1,Confidence2,Trade)~1   #formula for poLCA
bic_array=vector()
aic_array=vector()
for(k in 2:7){             # check for cluster k=2 to 7
  min_bic=100000
  min_aic=100000
  for(j in 1:500){        # try to avoid local maxima by running multiple times
    res1<-poLCA(f, bin_dataframe, nclass = k, maxiter = 10000)
    if(res1$bic < min_bic)
    {
      min_bic = res1$bic
    }
    if(res1$aic < min_aic)
    {
      min_aic = res1$aic
    }
  }
  bic_array<-c(bic_array,c(min_bic))
  aic_array<-c(aic_array,c(min_aic))
}
plot(bic_array,x=c(2:7),t='l',xlab = "K", ylab="BIC")

