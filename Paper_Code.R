load("PartyMembership.Rdata")
res<-poLCA(f, cat_dataframe, nclass = 4, maxiter = 10000, graphs = TRUE)
tab<-table(members.party$TD,res$predclass)
cluster1=vector()  # create empty cluster to hold each member
cluster2=vector()
cluster3=vector()
cluster4=vector()
for(i in 1:166){
  for(j in 1:4){
    if(tab[i,][j]==1)
    {
      if(j==1)
      {
        cluster1=c(cluster1,c(members.party[i,]$TD)) 
      }
      else if(j==2)
      {
        cluster2=c(cluster2,c(members.party[i,]$TD)) 
      }
      else if(j==3)
      {
        cluster3=c(cluster3,c(members.party[i,]$TD)) 
      }
      else
      {
        cluster4=c(cluster4,c(members.party[i,]$TD)) 
      }
    }
  }
}
print(table(members.party$Party,res$predclass))
print(res$probs.se)

yvect<-vector()  # boxplot of posterior probabilities
xvect<-vector()
for(clust in 1:4)
{
  yvect=c(yvect,c(as.numeric(unlist(res$posterior[,clust]))))
  xvect=c(xvect,c(rep(clust,length(unlist(res$posterior[,clust])))))
}
newdf<-data.frame(y=yvect,x=xvect)  #reshape the dataframe
with(newdf, boxplot(y~x,xlab="cluster", ylab="Posterior membership class Probability",ylim = c(0, 1)))