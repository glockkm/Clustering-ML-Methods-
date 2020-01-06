sink("Spinal.final.console.txt") 
pdf("Spinal_Final_PDF_Plots.pdf")


#https://archive.ics.uci.edu/ml/datasets/Vertebral+Column
#three classes: Normal (100 patients), Disk Hernia (60 patients) or Spondylolisthesis (150 patients)
#7 columns/attributes total with 7 being the class
#6 biomechanical attributes derived from the shape and orientation of the pelvis and lumbar spine (in this order): pelvic incidence, pelvic tilt, lumbar lordosis angle, sacral slope, pelvic radius and grade of spondylolisthesis


data = read.table(file = "column_3C.dat")
View(data)
#change attribute names from default to actual names
names(data)[names(data) == "V1"] = "pelvic incidence"
names(data)[names(data) == "V2"] = "pelvic tilt"
names(data)[names(data) == "V3"] = "lumbar lordosis angle"
names(data)[names(data) == "V4"] = "sacral slope"
names(data)[names(data) == "V5"] ="pelvic radius"
names(data)[names(data) == "V6"] = "grade of spondylolisthesis"

data_no_class = data[ -c(7) ] #takes out the class attribute
targ = as.numeric(data$V7) #changes text in our class column to intgers


library(tidyverse)
library(cluster)
library(factoextra)
library(gridExtra)

#overall cluster method and k# suggestions

#assess clustering tendency
get_clust_tendency(data_no_class, n = 300,
                   gradient = list(low = "steelblue",  high = "white"))
#Hopkins statistic: if the value is close to zero (below 0.5) 
   #dataset is significantly clusterable
#####0.12 so clusterable

#optimal number of clusters
library("NbClust")
set.seed(123)
#Hubert index, look at the knee of the plot
result = NbClust(data_no_class, distance = "euclidean",
                 min.nc = 2, max.nc = 10, 
                 method = "complete", index ="all") 
#####best number of clusters is 4
factoextra::fviz_nbclust(result) + theme_minimal()

#how to choose best clustering algorithms
#uses connectivity, dunn, and silhouette for validation measures
library("clValid")
intern = clValid(data_no_class, nClust = 2:6, 
                 clMethods = c("hierarchical","kmeans","pam"),
                 validation = "internal")
summary(intern)
#####hierarchical 2 clusters as best clustering algorithm for our data


#compute Euclidean distance

#when data is dense or continuous, this is a proximity measure. 
#the Euclidean distance between two points is the length of the path 
   #connecting them
dist.eucl = dist(data_no_class, method = "euclidean")
#subset the first 6 columns and rows and Round the values
round(as.matrix(dist.eucl)[1:3, 1:6], 1)
library(factoextra)
fviz_dist(dist.eucl)
#red high similarity (0 distance) and blue low (larger distance)



#######################################################################################
####### hierarchical #######

fviz_nbclust(method ="silhouette", FUNcluster=hcut, x = data_no_class)
#chose 2 clusters as best and 3 as second best for hierarchical

library(dplyr)

set.seed(123)
results2.hc = data_no_class %>%
  scale() %>%
  eclust("hclust", k = 2, graph = FALSE)

results3.hc = data_no_class %>%
  scale() %>%
  eclust("hclust", k = 3, graph = FALSE)

results4.hc = data[, -7] %>%
  scale() %>%
  eclust("hclust", k = 4, graph = FALSE)


#visualize

#view as a dendogram
dendogram.plt2 = fviz_dend(results2.hc, palette = "jco",
                           rect = TRUE, show_labels = FALSE) 
dendogram.plt2
dendogram.plt3 = fviz_dend(results3.hc, palette = "jco",
                           rect = TRUE, show_labels = FALSE) 
dendogram.plt3
dendogram.plt4 = fviz_dend(results4.hc, palette = "jco",
                           rect = TRUE, show_labels = FALSE) 
dendogram.plt4


#look at silhouette

#measures how well an observation is clustered and estimates the 
   #average distance between clusters
sil.info2 = silhouette(results2.hc$cluster, dist(data_no_class))
sil.info2  #which have negatives and to what cluster are they closest?
#silhouette plot is one of the many measures for inspecting 
   #and validating clustering results
#close to 1 indicates that the object is well clustered/object is similar 
   #to the other objects in its group.
#close to -1 indicates that the object is poorly clustered/assignment 
   #to some other cluster would probably improve the overall results
sil.info3 = silhouette(results3.hc$cluster, dist(data_no_class))
sil.info3
sil.info4 = silhouette(results4.hc$cluster, dist(data_no_class))
sil.info4
#view silhouette  
fviz_silhouette(results2.hc)
fviz_silhouette(results3.hc)
fviz_silhouette(results4.hc)
#objects with negative silhouette
neg_sil_index2 = which(sil.info2[, 'sil_width'] < 0)
neg_sil_index2
neg_sil_index3 = which(sil.info3[, 'sil_width'] < 0)
neg_sil_index3
neg_sil_index4 = which(sil.info4[, 'sil_width'] < 0)
neg_sil_index4
#https://www.datanovia.com/en/blog/types-of-clustering-methods-overview-and-quick-start-r-code/


#other ways to visualize

#http://www.sthda.com/english/wiki/beautiful-dendrogram-visualizations-in-r-5-must-known-methods-unsupervised-machine-learning
#another way to convert hclust into a dendrogram 
hcd2 = as.dendrogram(results2.hc)
plot(hcd2)
plot(hcd2, type = "triangle", ylab = "Height")
hcd3 = as.dendrogram(results3.hc)
hcd4 = as.dendrogram(results4.hc)
#plot(hcd2)
fviz_dend(results2.hc, show_labels = FALSE,
          palette = "jco", as.ggplot = TRUE)
fviz_dend(results2.hc, type = "circular", show_labels = FALSE,
          palette = "jco", as.ggplot = TRUE)
fviz_dend(results2.hc, type = "phylogenic", show_labels = FALSE,
          palette = "jco", as.ggplot = TRUE)
#triangle plot
plot(hcd2, type = "triangle", ylab = "Height")
library(ape)
#unrooted
plot(as.phylo(results2.hc), type = "unrooted", cex = 0.6,
     no.margin = TRUE)
#extract the data (for rectangular lines) #type can be "rectangle" or "triangle"
library(ggdendro)
dend_data = dendro_data(results2.hc, type = "rectangle")
#what contains dend_data
names(dend_data)
head(dend_data$segments)
head(dend_data$labels)
dend_data$segments
dend_data$labels


#statistics 

#https://www.datanovia.com/en/lessons/cluster-validation-statistics-must-know-methods/
#compute cluster stats
#https://www.rdocumentation.org/packages/fpc/versions/2.2-3/topics/cluster.stats

#compute stats: avg between, avg within, average silhouette width, and dunn


#average.between (want large) = 
   #average distance between clusters-want it to be as large as possible
#average.within (want small) = 
   #average distance within clusters-want it to be as small as possible
#Dunn index (D) (maximize is good, larger)= 
   #if clusters are compact around medoid and clusters are well separated
   #the diameter of cluster should be small and 
   #distance between clusters should be large
#Dunn index should be maximized. D =
   #min separation(inter cluster separation)/
   #max diameter(max intra cluster distance which is compactness)
library(fpc)
hc_stats2 = cluster.stats(dist(data_no_class),  results2.hc$cluster)
hc_stats2
hc_stats3 = cluster.stats(dist(data_no_class),  results3.hc$cluster)
hc_stats3
hc_stats4 = cluster.stats(dist(data_no_class),  results4.hc$cluster)
hc_stats4



#compute cluster stats: rand using data classes

#corrected Rand index=
   #(-1 no aggreement and +1 perfect agreement)
   #measure for assessing the similarity between two partitions, adjusted for chance. 
   #an agreement between the class types and the cluster solution 
   #so need true data classes in the function
classes = as.numeric(data$V7)
classes
clust_stats2hc = cluster.stats(d = dist(data_no_class), 
                               classes, results2.hc$cluster)
clust_stats2hc
clust_stats2hc$corrected.rand
#clust_stats2$vi

clust_stats3hc = cluster.stats(d = dist(data_no_class), 
                               classes, results3.hc$cluster)
clust_stats3hc
clust_stats3hc$corrected.rand
#clust_stats3$vi
clust_stats4hc = cluster.stats(d = dist(data_no_class), 
                               classes, results4.hc$cluster)
clust_stats4hc
clust_stats4hc$corrected.rand
#clust_stats4hc$vi

#clustering results

#classes versus clustered
table(data$V7, results2.hc$cluster)
table(data$V7, results3.hc$cluster)
table(data$V7, results4.hc$cluster)
#hierarchical clusters no and dh togther and sl separate.
#This may be because a disk herniating does not cause much 
   #change in the bio measurements here
#Where sl doescause biomechanical measurements to change.
   #May be due to bones being involved in this diagnosis 
   #and misplacement of biological features therefore a 
   #domino effect on other features measured


#confusion matrix 

#accuracy = overall how often is the model correct
#sensitivity aka recall aka true positive rate = 
   #TP(correct P pred)/total of P-closer to 1 is good and 0 is not
#specificity aka true negative rate = 
   #TN(correct N pred)/ total of N-best is 1 and bad is 0

library(caret)
library(klaR)

View(factor(results2.hc$cluster))
class(factor(results2.hc$cluster))
class(factor(targ))
length(factor(results2.hc$cluster))
length(factor(targ))
confusionMatrix(factor(results2.hc$cluster), factor(targ))
#####accuracy .28
confusionMatrix(factor(results3.hc$cluster), factor(targ))
#####accuracy .506

#cm won't run for k4 and we did not have time to investigate further.



####################################################################################
####### pamk ####### 

#https://www.datanovia.com/en/courses/partitional-clustering-in-r-the-essentials/

#look at optimal # of clusters

fviz_nbclust(method ="silhouette", FUNcluster=pam, x = data_no_class)
#####chose 2
#method="silhouette" (for average silhouette width), "wss" (for total within sum of square) and "gap_stat" (for gap statistics)
#FUNcluster possible values are kmeans, pam, hcut, clara, ...
#https://www.rdocumentation.org/packages/factoextra/versions/1.0.5/topics/fviz_nbclust


library(cluster)
library(fpc)

#compute PAM

#pam() choosing k#
pam.result2 = pam(data_no_class, 2)
pam.result2
pam.result3 = pam(data_no_class, 3)
pam.result3
pam.result4 = pam(data_no_class, 4)
pam.result4
#output is a matrix, rows are the medoids and columns are variables
#output has a clustering vector
   #vector of integers indicating the cluster to which each point belongs
pam.result2$medoids
head(pam.result2$clustering)
pam.result3$medoids
head(pam.result3$clustering)
pam.result4$medoids
head(pam.result4$clustering)
#add data point classifications
dd = cbind(data_no_class, cluster = pam.result2$cluster)
head(dd, n = 25)

#visualize

fviz_cluster(pam.result2)
fviz_cluster(pam.result3)
fviz_cluster(pam.result4)


#pamk() chooses k# for you

#pamk vs pam https://stats.stackexchange.com/questions/15047/clustering-using-the-pam-algorithm-in-r
#?pamk() #wrapper for pam()-uses pam on smaller datasets vs clara
   #chooses k# for you
 
pamk.result = pamk(data_no_class) #storing k medoids result in .result #pam = partitioning around medoids
pamk.result
pamk.result$nc #number of clusters chosen by algorithm not me
#chose 2

#visualize results

plot(pamk.result$pamobject) 
#above plot- graph on left is a clustering plot 
   #and right graph shows silhoutte



#look at silhouette
#silhouette plot is one of the many measures 
   #for inspecting and validating clustering results
   #measures how well an observation is clustered 
   #and estimates the average distance between clusters
#silhouette (s1) closest to 1 is a good cluster
   #s1 closer to 0 means lies between clusters
   #negative s1 means it's clustered wrong
sil.info2pam = silhouette(pam.result2$cluster , dist(data_no_class))
sil.info2pam  
#which have negatives and to what cluster are they closest?

sil.info3pam = silhouette(pam.result3$cluster , dist(data_no_class))
sil.info3pam
sil.info4pam = silhouette(pam.result4$cluster , dist(data_no_class))
sil.info4pam
#view silhouette  
fviz_silhouette(sil.info2pam)
fviz_silhouette(sil.info3pam)
fviz_silhouette(sil.info4pam)
#objects with negative silhouette
neg_sil_index2 = which(sil.info2pam[, 'sil_width'] < 0)
neg_sil_index2
neg_sil_index3 = which(sil.info3pam[, 'sil_width'] < 0)
neg_sil_index3
neg_sil_index4 = which(sil.info4pam[, 'sil_width'] < 0)
neg_sil_index4



#compute cluster stats

#compute stats: avg between, avg within, average silhouette width, and dunn


#average.between = 
   #average distance between clusters-want it to be as large as possible
#average.within = 
   #average distance within clusters-want it to be as small as possible
#Dunn index (D)= 
   #if clusters are compact around medoid and clusters are well separated
   #the diameter of cluster should be small and distance 
   #between clusters should be large
#Dunn index should be maximized. 
#D =min separation(inter cluster separation)
   #/max diameter(max intra cluster distance which is compactness)
pam_stats2 = cluster.stats(dist(data_no_class), pam.result2$cluster )
pam_stats2
pam_stats3 = cluster.stats(dist(data_no_class), pam.result3$cluster)
pam_stats3
pam_stats4 = cluster.stats(dist(data_no_class),  pam.result4$cluster)
pam_stats4



#compute cluster stats: rand using data classes

#corrected Rand index= 
   #measure for assessing the similarity between two partitions, adjusted for chance. 
   #range is -1 (no agreement) to 1 (perfect agreement)
   #agreement is between the class types and the cluster solution 
classes = as.numeric(data$V7)
clust_stats_pam2 = cluster.stats(d = dist(data_no_class), 
                                 classes, pam.result2$cluster)
clust_stats_pam2
clust_stats_pam3 = cluster.stats(d = dist(data_no_class), 
                                 classes, pam.result3$cluster)
clust_stats_pam3
clust_stats_pam4 = cluster.stats(d = dist(data_no_class), 
                                 classes, pam.result4$cluster)
clust_stats_pam4

#rand
clust_stats_pam2$corrected.rand
#clust_stats_pam2$vi
clust_stats_pam3$corrected.rand
#clust_stats_pam3$vi
clust_stats_pam4$corrected.rand
#clust_stats_pam4$vi


#clustering results 

table(pam.result2$cluster, data$V7) #compare clustering to actual species
table(pam.result3$cluster, data$V7)
table(pam.result4$cluster, data$V7)

#confusion matrix

View(factor(pam.result2$cluster))
class(factor(pam.result2$cluster))
confusionMatrix(factor(pam.result2$cluster), factor(targ))
#####accuracy .19
confusionMatrix(factor(pam.result3$cluster), factor(targ))
#####accuracy .487
confusionMatrix(factor(pam.result4$cluster), factor(targ))
#cm won't run with k4



##########################################################################################
####### spectral #######
#spectral clustering/3 ways:kernlab, kknn, samspectral

library(kernlab)
data_matrix = as.matrix(data_no_class)
View(data_matrix)
?specc


#specc using kernlab

sp2 = specc(data_matrix, centers=2, kernel = laplacedot)
sp2
centers(sp2)
size(sp2)
length(sp2)
sp2@.Data

#exploring with a different kernal k2

sp2rbf = specc(data_matrix, centers=2, kernel = "rbfdot") 
#using a different kernal, no change
sp2rbf
#not much difference

sp3 = specc(data_matrix, centers=3, kernel = laplacedot)
sp3

sp4 = specc(data_matrix, centers=4, kernel = laplacedot)
sp4

plot(data_matrix, pch = (23-2*sp2))
plot(data_matrix, col=sp2)
plot(data_matrix, pch = (sp3))
plot(data_matrix, pch = (sp4))



#look at silhouette

#measures how well an observation is clustered 
   #and estimates the average distance between clusters
sil.info2spec = silhouette(sp2, dist(data_no_class))
sil.info2spec  #which have negatives and to what cluster are they closest?
#close to 1 indicates that the object is well clustered/object is similar to the other objects in its group.
#close to -1 indicates that the object is poorly clustered/assignment to some other cluster would probably improve the overall results
sil.info3spec = silhouette(sp3, dist(data_no_class))
sil.info3spec
sil.info4spec = silhouette(sp4, dist(data_no_class))
sil.info4spec
#view silhouette  
fviz_silhouette(sil.info2spec)
fviz_silhouette(sil.info3spec)
fviz_silhouette(sil.info4spec)
#objects with negative silhouette
neg_sil_index2spec = which(sil.info2spec[, 'sil_width'] < 0)
neg_sil_index2spec
neg_sil_index3spec = which(sil.info3spec[, 'sil_width'] < 0)
neg_sil_index3spec
neg_sil_index4spec = which(sil.info4spec[, 'sil_width'] < 0)
neg_sil_index4spec

#https://www.rdocumentation.org/packages/kernlab/versions/0.9-27/topics/specc
#https://filterbubblecn.blogspot.com/2016/03/hands-on-spectral-clustering-in-r.html


#another way samspectral- similar looking results- visualization only for demo

#http://bioconductor.org/packages/release/bioc/html/SamSPECTRAL.html

library(SamSPECTRAL)
set.seed(123)
data_matrix = as.matrix(data_no_class)
sam2 = SamSPECTRAL(data_matrix, number.of.clusters = 2,dimensions = c(1,2,3,4,5,6), 
                   normal.sigma = 200, separation.factor = 0.6)
plot(data_matrix, pch = "*", col = sam2)


#another way based on k nearest neighbor graph
#laplacian is constructed from from nearest neighbors 

library(kknn)
?specClust
output2 = specClust(centers = 2, data[,1:6], nn = 5)
class_col_ver = as.character(as.numeric(data$V7)) #class column as a verifier
#visualize
pairs(data[1:6], pch=class_col_ver, 
      col = c("green", "red")[output2$cluster])


#compute cluster stats

#compute stats: avg between, avg within, average silhouette width, and dunn

#average.between = 
   #average distance between clusters-want it to be as large as possible
#average.within = 
   #average distance within clusters-want it to be as small as possible
#Dunn index (D)= 
   #if clusters are compact around medoid and clusters are well separated
   #the diameter of cluster should be small and distance between clusters should be large
   #Dunn index should be maximized
   #D =min separation(inter cluster separation)/max diameter(max intra cluster distance which is compactness)
spec_stats2 = cluster.stats(dist(data_no_class), sp2)
spec_stats2
spec_stats3 = cluster.stats(dist(data_no_class), sp3)
spec_stats3
spec_stats4 = cluster.stats(dist(data_no_class),  sp4)
spec_stats4



#compute stats: rand

#corrected Rand index=
   #measure for assessing the similarity between two partitions, adjusted for chance. 
   #range is -1 (no agreement) to 1 (perfect agreement)
   #agreement is between the class types and the cluster solution 
classes = as.numeric(data$V7)
clust_stats_spec2 = cluster.stats(d = dist(data_no_class), 
                                  classes, sp2)
clust_stats_spec2
clust_stats_spec2$corrected.rand
#clust_stats_spec2$vi

clust_stats_spec3 = cluster.stats(d = dist(data_no_class), 
                                  classes, sp3)
clust_stats_spec3
clust_stats_spec3$corrected.rand
#clust_stats_spec3$vi

clust_stats_spec4 = cluster.stats(d = dist(data_no_class), 
                                  classes, sp4)
clust_stats_spec4
clust_stats_spec4$corrected.rand
#clust_stats_spec4$vi



#clustering results

table(data[,7], sp2) 
table(data[,7], sp3)
table(data[,7], sp4)


#confusion matrix

View(sp3@.Data)
View(factor(data$V7))
levels(sp3@.Data)
class(factor(sp3@.Data))
class(factor(data$V7))
targ = as.numeric(data$V7)
View(targ)

confusionMatrix(factor(sp2@.Data), factor(targ))
#####accuracy .3097
confusionMatrix(factor(sp3@.Data), factor(targ))
#####accuracy .0226
confusionMatrix(factor(sp4@.Data), factor(targ))
#cm won't run with k4




#############################################################################################
#sl class cluster
#cluster only the sl class as it tends to 
   #cluster to itself and further into two clusters
sl = data[c(61:210),c(1:7)] #subset only the rows that are of class sl 61-210
View(sl)
sl_no_class = sl[ -c(7) ]
View(sl_no_class)
sl_no_class_matr = as.matrix(sl_no_class)

#spec
sp_sl = specc(sl_no_class_matr, centers=2, kernel = laplacedot)
sp_sl
centers(sp_sl)
size(sp_sl)
length(sp_sl)
sp_sl@.Data

#visualize
plot(sp_sl, col=sp2)

#compute stats
spec_sl_stats = cluster.stats(dist(sl_no_class), sp_sl)
spec_sl_stats

sil.info.sl = silhouette(sp_sl, dist(sl_no_class))
sil.info.sl  
#objects with negative silhouette
neg_sil_index_sl = which(sil.info.sl[, 'sil_width'] < 0)
neg_sil_index_sl

#clustering results
table(sl[,7], sp_sl)

#cannot do confusion matrix for k =4 at present due to what we suspect: all sl being all class 3 in orig data
#p-values do not seem to be calculating correctly. we cannot accept or reject the null hypothesis. Further investigation is need.
#we cannot statistically state that our dataset clusters but visually we suspect that our dataset does cluster.
#we hypothesize that sl clusters to itself vs no and hd because it does cause the attributes
#and measurements to stand out and be unique compared to the hd/no measurements.
#Clustering was clearly able to pick up sl as being different from hd and no.
#Clustering also shows us that sl can be broken down into two clusters
#We hypothesize these two clusters are different grades of sl, one cluster being the lower
#and the other cluster being the higher grade
#It is possible that clustering, given these attributes, could diagnose sl in patients and the level of sl
#in comparison to normal and hd patients
#we hypothesize that hd and no often cluster together because the particular attributes
#in this dataset do not capture the hd abnormalities as the attributes might not
#change in patients with hd as it is a herniated disk and the attributes measure bones etc.
#which would perhaps not shift or move due to a disk.
#we hypothesize that sl is picked up as its own clusters due to the attributes/measurements changing in those 
#patients as sl deals with broken and misplaced bones



graphics.off()
sink()

