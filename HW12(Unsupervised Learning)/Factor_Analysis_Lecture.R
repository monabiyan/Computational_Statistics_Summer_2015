install.packages("psych")
install.packages("GPArotation")

library(psych)  #have some data as well as some machine learning functions like fa()
library(GPArotation)

########## getting data from psych package and clean the data and put it in a dataframe ####
data(msq)
msq1 <- msq[,2:72]     
msq1[msq1=="9"] = NA      
msq1 <- data.frame(msq1) 
msq2 <- na.omit(msq1)    
names(msq2)
dim(msq2)
msq2[1:5,1:5]
###########################################################################################


########     Finding Factors #####################################################################
#fa(). It actually does much more than simply finding the eigenvectors of the covariance matrix. 
#One of the most important tweaks is that it rotates the principal components a little bit to nudge things 
#so that each component loads only on a few variables, and not on any of the rest. Without the rotation,
#we might get for the first principal component that it is 40% X1, 30% X2, 20% X3, and 10% X4; 
#after the rotation, we might get 60% X1 and 40% X2, and none of the rest. But as we will see, 
#often this extra rotation doesn't make much difference, and ultimately we end up with the same eigenvectors as ever.



fact <- fa(msq2,nfactors=2)  #Here we are only calculating the first two factors, but that's enough for our purposes for now.
                             #When we say nfactor=2 we mean the first two with the highest eigen values.
                             # In fact, for msq2 with 71 columns, we have 71 eigne vectors and 71 corresponding eigne values. 

fact1 <- fact$loadings[,1]   #extracts from the output the loadings for the first factor,                              #or the projections of the variables onto the factors, {??11,??12,...}
fact2<-fact$loadings[,2] 



#The best way to read the factor, then, is to look not at the loadings as ordered by the variables (ie, {??11,??12,...}),
#but rather to order them from largest to smallest, so we can see what variables load most positively, 
#and what variables load most negatively:
#ie, what are the two poles along which the first latent dimension varies.

fact1[order(fact1)]     #order(f) gives a vector of indexes of the vector f according to their values from low to high

#This is a lot to look at, but we can immediately tell what variables go into the first factor
#by looking at the lowest- and highest-scoring variables (the sign, ie, which direction is positive vs negative,                                                                                                                                                       #is arbitrary and meaningless).
#What we see is that while one end is happy, enthusiastic, cheerful, full.of.pep, lively, 
#and energetic, the other side is not unhappy so much as sluggish, tired, dull, sleepy, etc. Ie,
#the most important factor underlying all these 71 moods is not happy vs unhappy,
#but more like energetic (and happy) vs tired, sleepy, and depressed. An important discovery!


fact2[order(fact2)]
#Now we see a second dimension that is interestingly different from the first: not energetic vs sleepy, 
#but relaxed and calm versus tense and frustrated. Here too, 
#the factor is both not quite what we expected, but at the same time familiar and plausible.

#Within the fa() output are also the projections of individuals onto each of these dimensions
#(ie, how energetic-vs-tired someone seems to be, and how calm-vs-tense they seem to be, 
#and so on for all the other factors we might estimate), 
#and a large number of other diagnostics and outputs (too many to show here!). 
#There's a whole world to explore in factor analysis,
#but the basic truth is that the fundamental factors are the same 
#however we estimate them.

#############################


####### Other methods for finding factors 

#For instance, here are two other methods for estimating principal components, 
#both built into the base R package, but using different approaches.
#The first uses singular value decomposition (another unsupervised approach similar to factor analysis),
#and the second uses the covariance/eigenvector approach.


# prcomp method using SVD
pcaA <- prcomp(msq2)
pcaA1 <- pcaA$rotation[,1]

# Princomp method using eigen of cov
pcaB <- princomp(msq2)
pcaB1 <- pcaB$loadings[,1]



#Finally, let's do it manually ourselves using cov() to estimate the covariance matrix and eigen() to get the eigenvectors:

# Direct eigen of cov
covm <- cov(msq2)
eigenm <- eigen(covm)
eigen1 <- eigenm$vectors[,1]
################################################


#############Comparison of these methods ###################

#How similar are these methods?
f1mat <- cbind(fact1,pcaA1,pcaB1,eigen1)
cor(f1mat)
#Essentially identical, with fa() being slightly different due to the rotation - but not very different.

#######################################





###Estimation bonus#################################

#One final method for estimating the components directly from the data is perhaps a bit surprising.
#Because eigenvectors are, in a sense, both the underlying truth of the data and the equilibrium of any process
#that the data describes,
#one additional way to find the first eigenvector / principal component is to take any random vector va of length p
#and multiply it by the data matrix; 
#then take the resulting vector vb (now of length n) and multiply it back through the matrix to get another vector v'a,
#and so on back and forth.
#If you keep doing this a few times, ultimately the process converges so that the final va is almost identical to the first eigenvector.
#(This is basically how singular value decomposition works.)
#It's a crude but very effective way to get the first component (and can be adapted to get the others), 
#and works both quickly and well on even very large datasets.

tm.msq2 <- t(as.matrix(msq2))
m.msq2 <- as.matrix(msq2)
va <- rnorm(ncol(msq2))
for(i in 1:10)
{
  vb <- m.msq2 %*% scale(va)
  va <- tm.msq2 %*% scale(vb)
}

#(Note how we have to rescale va and vb each time, 
#otherwise they increase because the eigenvalue stretches the eigenvector each time it's multiplied through the matrix. 
#We only care about the direction of v (ie, the relative sizes of the loadings),
#not the absolute size, so we rescale it to keep it from getting unwieldly big.)

#So does the final va match the first component?

cor(va,pcaA1)


#Yes it does.


##########################################################


########### How many factors are needed?              ##################################

# 1) one of the oldest and most established methods is looking at the eigenvalues,
#which in a sense determine the relative importance of each of the factors: 
#the bigger the eigenvalue, 
#the more of the variation in the original data that eigenvector/factor explains.

#One common way of examining these values is to plot them,
#ordered by size, in something called a "scree plot" (because it looks the the scree on the side of a mountain).
#All of our estimation methods above include the values as well as the factors,
#and here is a simple scree plot of them from our manual eigenvector method.

plot(eigenm$values,type="b")


#There are two common criteria to use for choosing the number of factors based on an examination of these values. 
#First, one looks for the "elbow" in the curve - where it goes from the steep decline, 
#to the flat area, where the presumption is the flat are is all the factors that are just noise. 
#So in this figure, counting from the left, it might be 5 or so factors that are significant, and the rest are just noise. 
#Another method is to only keep those factors with values above 1 
#(because that distinguishes eigenvectors that grow vs those that shrink),
#which in this case yields 6 factors to retain - so the two methods are in fairly close agreement.



#2) Cumulative variance
#One final way to see this is the plot the proportion of the total variance in the data explained by the factors as you add them together.
#Since the eigenvalues are equivalent to the amount of variance each component explains in the original data,
#we can just do the same plot as before,
#but use the cumsum function to plot the cumulative amount
#(cumsum just turns a vector v of i values in a vector c where each value ci is the sum of all the vi up to the ith), 
#and normalize it by dividing by the total.

plot(cumsum(eigenm$values)/sum(eigenm$values),ylim=c(0,1))

#We can see that this is a fairly smooth curve, 
#with a bit of an elbow at around the fourth or fifth value - about the same as before. More interestingly,
#over 50% of the total variance in the data is explained by just those first 4 factors, 
#so only 4 out 71 factors are already explaining over half of the variation.
#This plus the previous tests suggests that 4-6 factors do explain quite a lot of what's going on in the variation of mood among people over time.
#We may not be 70-dimensional emotional thinkers, but 4-6 seems like a plausibly complex psychological model.

##########################################################################

