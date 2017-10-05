library(MASS)
load("2016_First6Votes_YesNoAbsent.Rdata")
bin_data <- (votes==1)*1   #convert data in binary values
rownames(bin_data)<-NULL
d<- dist(bin_data,method = "binary")  # binary dissimilarity
c<- hclust(d,method = "complete") #hierarchical clustering 
plot(c)
hcl = cutree(c, k = 5)
print(table(hcl))