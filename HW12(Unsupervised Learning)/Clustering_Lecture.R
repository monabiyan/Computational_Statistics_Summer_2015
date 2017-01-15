
#now clustering analysis:

#There are literally hundreds of ways to do cluster analysis,
#but perhaps the most long-established is the k-means algorithm.
#This is a suprisingly simple algorithm, with only one tricky part: 
  #the number of clusters to divide the data into. 
#This is similar to the question of the number of factors to keep, 
#and let us similarly put the question off briefly.

#K-means Method 


#The k-means algorithm is just two steps, repeated:
  
#1)  Assign a group category at random to every data point.

#2-a) For each category, calculate the mean of all 
 #    the points in that group (ie, the mean x1 value, 
  #  the mean x2 value, etc; ie, the "centroid" of all those points).

#2-b) After calculating all the group centroids,
   #reassign each point to the group of the nearest centroid. 
   #Go back to 2-a) and repeat until group assignments stop changing
   #(ie, the process has converged).

#That's it. But it works pretty nicely.
#Let's try it on our simulated data. 
#Usually you can't know ahead of time how many groups to use,
#but for now let's cheat and use two.





###################################################################

# K-means  in R
set.seed(100)

kout <- kmeans(msq2,centers=2,nstart=25)      #means 25 times change the random initialization of the clustring
                                              # and choose the best one that has minimum average distance
centroids <- kout$centers
kout$tot.withinss                            #the distance between all data and their assigned centroids  (not important)

as.vector(kout$cluster)                       #The assigned cluster

topvars_centroid1 <- centroids[1,order(centroids[1,])]
topvars_centroid2 <- centroids[2,order(centroids[2,])]
tail(topvars_centroid1)     
tail(topvars_centroid2)
############################################################


#Hierarchial Clustering

  #Unlike k means, hierarchical clustering assembles clusters piece by piece:
  #first a few similar observations are joined into a multitude of small clusters, 
  #and then similar similar small clusters are attached together to former larger, 
  #on and upward until everything is just one cluster. 
  #And then the user can just decide where in this process 
  #to draw the line and take the clusters are that level of aggregation.


#As you might have noticed, the key idea here is "similarity" 
#- we join similar observations, and then similar clusters, 
#until we've joined everything. Similarity between observations is simple enough:
#if we just take the distance between two observations (ie, the Euclidean distance),
#that's a pretty good measure of similarity.
#But how do we measure the similarity between two small clusters,
#or between a cluster and a single observation we are considering adding to it?

#As usual, it turns out there are multiple answers to this question. 
#It's easiest to define them in terms of dissimilarity, 
#which is just the inverse of similarity (ie, we will join clusters with the lowest dissimilarity).

#Complete: the dissimilarity between two clusters A and B is 
#equal to the largest distance between a member of group A and a member of group B.

#Single: like Complete, but sets the dissimilarity between A and B to the smallest distance
#between a member of A and a member of B.

#Average: the average dissimilarity between every member of A and every member of B.

#Centroid: the distance between the centroid of A and the centroid of B.






  

hout <- hclust(dist(msq2),method="complete") #or method="average" or...
plot(hout2,labels=FALSE)
#the height where any two clusters join is equal to
#the dissimilarity between those two clusters. 




#To get the cluster assignments, we just apply cutree to the hclust output,
#choose either the height to cut it at, or the number of clusters:
as.vector(cutree(hout,2))  # That means we have 2 clusters
as.vector(cutree(hout,h=21)) # cut the plot at height=21
abline(a=16.5,b=0,col="red")
abline(a=21,b=0,col="blue")

#Two plausible places to make the cut are shown with the blue and red lines, 
#which divide the tree into 2 and 5 clusters respectively.
#Whether we prefer 2 or 5, or something inbetween,
#or something more numerous, is ultimately 
#up to the judgment of the researcher based on her substantive knowledge of the results.

