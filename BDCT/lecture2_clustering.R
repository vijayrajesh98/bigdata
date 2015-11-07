install.packages("xlsx")
install.packages("rpart")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("dplyr")

library(xlsx) #Loading the package needed to read Excel file
library(rpart) #Loading the package needed to construct and prune trees
library(dplyr)#R package to quickly join datasets
library(tidyr)
library(ggplot2)
# Uses logic from http://rpubs.com/hrbrmstr/customer-segmentation-r
# Also refer https://rstudio-pubs-static.s3.amazonaws.com/33876_1d7794d9a86647ca90c4f182df93f0e8.html

DIRNAME <- "/Users/rvijayaraghavan/Desktop/rajesh/Personal/Teaching/DataDrivenSrikantClass/"
XLFILENAME <- "ClusteringWorkbook.xlsx"
FILENAME <- paste0(DIRNAME,XLFILENAME)
NUMCLUSTERS <- 4


# Read the offer information dataset.
lecture2data.offers <- read.xlsx(FILENAME, sheetName="OfferInformation")
colnames(lecture2data.offers) <- c("offerid", "campaign", "varietal", 
                                   "minqty", "discount", "origin", "pastpeak")



# Read the transaction information dataset.

lecture2data.transactions <- read.xlsx(FILENAME, 
                                       sheetName="Transactions") ##reading the transactions worksheet. 

lecture2data.transactions <- lecture2data.transactions[ -c(3:5) ] ##remove unwanted columns. In this case it is columns 3 to 5.

colnames(lecture2data.transactions) <- c("customername", 
                                         "offerid") ##set column names to customername, and offerid. 


clustering.vector.inp <- NULL
clustering.vector <- NULL
clustering.vector.dataset <- NULL


# set the variable isoffer to 1. This is the first step in setting 1 for the offer corresponding to the customer.
lecture2data.transactions$isoffer <- 1

# Now let us link the two sheets, and store this in the dataset called clustering.vector.dataset
clustering.vector.dataset <- dplyr::left_join(lecture2data.transactions, lecture2data.offers)
head(clustering.vector.dataset) ##look at the first five elements of the dataset.

clustering.vector <- clustering.vector.dataset[,(1:3)] ##take the first three columns that are sufficient for our purpose.
head(clustering.vector)

clustering.vector <-  tidyr::spread(clustering.vector,offerid,isoffer) ##spread the value of 1 across other transactions. The ones that do not have a transaction for the customer, are stored at Not Available i.e., NA at this point. But remember in our case, we have it set to zero. 
clustering.vector [is.na(clustering.vector )] <- 0

nrow(clustering.vector) #number of rows for input vector
ncol(clustering.vector) #number of columns

kmeans.result <- kmeans(clustering.vector.inp,NUMCLUSTERS)

set.seed(1234) ##set so that the centroids generated through kmeans dont flip after every run in one session. 

clustering.vector.inp <- clustering.vector[,-1] #input for kmeans, all columns apart from the first-column in clustering.vector. This corresponds to cells L2 to DG333 in the Excel workbook that was done in class. 

head(clustering.vector.inp)

kmeans.result <- kmeans(clustering.vector.inp,NUMCLUSTERS)

kmeans.result

tab1 <- table(kmeans.result$cluster) #to show statistics of how the clusters are assigned across customers. 
tab1 # show the clusters
barplot(tab1) #to show a bar plot with the result from above. 

head(clustering.vector.inp)

clustering.vector <- merge(clustering.vector,kmeans.result$cluster , by=0, all=TRUE) 
names(clustering.vector)[names(clustering.vector) == 'y'] <- 'cluster'
clustering.vector <- left_join(clustering.vector,clustering.vector.dataset) #joining the two dataset using customername column. 


cluster.show <- filter(clustering.vector, cluster == 1)

cluster.show ##give the entire set of columns based on filtered by cluster 1. This shows all transactions by the corresponding cluster. Note that there are 324 transactions between 100 customers. 

cluster.show$customername ##gives just the customer name from the above.

## Similarly repeat for customer 2
cluster.show <- filter(clustering.vector, cluster == 2)
cluster.show$customername

## Similar for customer 3
cluster.show <- filter(clustering.vector, cluster == 3)
cluster.show$customername

## Similar for customer 4
cluster.show <- filter(clustering.vector, cluster == 4)
cluster.show$customername

kmeans.result$withinss

install.packages('cluster')
devtools::install_github("sinhrks/ggfortify")

library(cluster)
clusplot(clustering.vector.inp, kmeans.result$cluster, main='2D representation of the Cluster solution', color=TRUE, shade=TRUE, labels=2, lines=0)



library(ggfortify)
autoplot(kmeans.result, data=clustering.vector.inp, frame=TRUE, frame.type='norm')



MAX <- 10 #Maximum number of clusters to try
sil <- NULL

for (i in 2:MAX) #Loop runs for all value of i between 2 and MAX. 
{
  temp.kmeans <- kmeans(clustering.vector.inp, 
                        centers=i) #Performs K-means clustering for the corresponding value of i. 
  
  temp.clusters <- temp.kmeans$cluster #get the clusters from the output. 
  
  temp.silval <- silhouette(temp.clusters,
                            dist(clustering.vector.inp)) # calculate the silhouette value and store it in the temp.silval.
  
  sil[i] <- mean(temp.silval[,3]) #store the mean silhouette value that is used for plotting the value in the next step. 
}



plot(1:MAX, sil)
lines(1:MAX, sil) 

