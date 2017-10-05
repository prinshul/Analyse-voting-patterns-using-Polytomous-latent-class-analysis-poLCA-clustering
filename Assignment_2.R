library(MASS)

hotelling_test <- function(homeData){
homesData.PA <- subset(homeData, homeData$Area =="PA")  #find the data with Area PA
homesData.MP <- subset(homeData, homeData$Area =="MP")    #find the data with Area MP
covPA <- cov(homesData.PA[,-1])    # covariance for PA
covMP <- cov(homesData.MP[,-1])    # covariance for MP
estimate_cov <- (((nrow(homesData.PA)-1)*covPA) + ((nrow(homesData.MP)-1)*covMP))/((nrow(homesData.PA)+nrow(homesData.MP))-2)
meanPA <- colMeans(homesData.PA[,-1])
meanMP <- colMeans(homesData.MP[,-1])
mahalanobis_dist<- (t(meanPA-meanMP)%*%solve(estimate_cov)%*%(meanPA-meanMP))
numeratr <- nrow(homesData.PA) + nrow(homesData.MP) - ncol(homesData.PA[,-1]) - 1
denom <- (nrow(homesData.PA) + nrow(homesData.MP) - 2) * ncol(homesData.PA[,-1])
hotelling_tsq <- (nrow(homesData.PA) * nrow(homesData.MP))/ (nrow(homesData.PA) + nrow(homesData.MP))
hotelling_tsq <- hotelling_tsq * mahalanobis_dist
f_stat <- (numeratr / denom) * hotelling_tsq
p_val<-pf(f_stat,df1=ncol(homesData.PA[,-1]), df2=numeratr,lower.tail=FALSE)
print(p_val)
if(p_val < 0.05) {  # 0.05 significance level
print("The two communities are significantly different with respect to the characteristics of the properties available for sale")
} else
  {
  print("The two communities are NOT significantly different with respect to the characteristics of the properties available for sale")
  }
}
data <- read.csv("prices.csv")    # read prices data
hotelling_test(data)  # test the function
