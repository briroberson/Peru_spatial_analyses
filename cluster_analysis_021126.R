#exploratory cluster analysis following methods from Swanson (2025) 

#load packages 
library(vegan)
library(ecodist)
library(gplots)
library(dendextend)
library(colorspace)
library(scales)
library(reshape)
library(funrar)
library(mctoolsr)
library(rvg)
library(officer)
library(phyloseq)
library(PERMANOVA)

#load data
filt_lia2 <- readRDS("E:/r_outputs/microbial/filt_lia2")

distlia<- distance(filt_lia2, method='wunifrac')
# hclust on the distance
hc <- hclust(distlia, method='ward.D2')

#PERMANOVA TESTING #####################

#k: selected level of agglomeration to test at 

#Trial 1: k = 3 
set.seed(123)
dist_matrix <- as.matrix(distlia)
cl <- cutree(hc, k = 3)
DistC <- DistContinuous(dist_matrix, coef="Bray_Curtis") 
perm <- PerMANOVA.Simple(DistC$D, nperm = 999, as.factor(cl))
perm$Inicial$Fexp #pseudo f-ratio
perm$pval #p-value
#Fexp: 2.841703, p-val: 0.013 (significant)

#Trial 2: k = 2
cl2 <- cutree(hc, k = 2)
DistC2 <- DistContinuous(dist_matrix, coef="Bray_Curtis") 
perm2 <- PerMANOVA.Simple(DistC2$D, nperm = 999, as.factor(cl2))
perm2$Inicial$Fexp #pseudo f-ratio
perm2$pval #p-value
#Fexp: 3.037355, p-val: 0.021 (significant)

#Trial 3: k = 1 
cl3 <- cutree(hc, k = 1)
DistC3 <- DistContinuous(dist_matrix, coef="Bray_Curtis") 
perm3 <- PerMANOVA.Simple(DistC3$D, nperm = 999, as.factor(cl3))
perm3$Inicial$Fexp #pseudo f-ratio
perm3$pval #p-value
#this didnt work ... 

#OPTIMIZING CLUSTER RESULTS ####################

#Trial 1: k = 3 
k <- 3
single <- "n"  #choose "y" to test only original singletons, else "n"

#Function to compute within-group sum of squares
#from distance matrix (Dist)
#for input classification (cls)
ssw <- function(dist_matrix, cls)
  {
  rs <- rowsum(as.matrix(dist_matrix^2), cls)
  dia <- diag(rowsum(t(rs), cls)/2)
  n <- table(cls)
  return(sum(dia/n))
}


#Create a list of all subgroups that compose each group from hclust "merge" object

N <- nrow(hc$merge)
memsall <- vector("list", N)
for (i in seq(1:(N-1))){
  memsall[i] <- list(hc$merge[i,])  #retrieve the groups to merge
  #if either group is a positive number (multi-member), retrieve and append its memsall
  #if not it is an original single observation that can be appended as is
  ifelse(memsall[[i]][1] > 0, m1 <- c(memsall[[i]][1], memsall[[memsall[[i]][1]]]), m1 <- memsall[[i]][1])
  ifelse(memsall[[i]][2] > 0, m2 <- c(memsall[[i]][2], memsall[[memsall[[i]][2]]]), m2 <- memsall[[i]][2])
  memsall[[i]] <- c(m1, m2)
}


#Create list of all component members of each subgroup (original neg plot numbers only)
members <- list()
for (i in seq(1:(N-1))){
  members[i] <- list(hc$merge[i,])  #retrieve the groups to merge
  #if either group is a positive number (multi-member), replace it with its members
  ifelse(members[[i]][1] > 0, m1 <- members[[members[[i]][1]]], m1 <- members[[i]][1])
  ifelse(members[[i]][2] > 0, m2 <- members[[members[[i]][2]]], m2 <- members[[i]][2])
  members[[i]] <- c(m1, m2)
}


#Find cluster class numbers of each observation for chosen k
#compute from merge output from hclust not cutree, to get actual cluster numbers
ct <- -seq(1:N)  #initial values
for (i in seq(1:(N-k))){
  ct[ct==hc$merge[i,1]] <- i  #re-assign classes based on merge number
  ct[ct==hc$merge[i,2]] <- i
}

mem <- mem0 <- ct  #initialize starting and optimized class memberships

#THIS  IS WHERE WE ARE HAVING ISSUES 2/13/26

#compute SSW for initial classes and initialize "best" SSW
ssw_best <- ssw(dist_matrix, mem)
grps <- unique(mem)

#compile list of all subgroups
subgrps <- c(unlist(memsall[grps[grps>0]]), grps[grps<0])
if (single=="y") {subgrps <- subgrps[subgrps<0]}

#create table of initial groups to which subgroups belong, for output
grps_pos <- grps[grps>0]
grps_neg <- grps[grps<0]
subgrps_pos <- memsall[grps_pos]
grp_init <- c(rep(grps_pos, lengths(subgrps_pos)), grps_neg)
subgrp <- c(unlist(subgrps_pos), grps_neg)
df_grps_init <- data.frame(subgrp, grp_init)

#initialize output data frame with SSW of cluster analysis
changes1 <- data.frame(subgrp=NA, grp_new=NA, SSW=ssw(Dist, mem0))
#Outer loop tests a single subgroup’s class
#interrupted by break when no improvement is noted
#maximum of 2N updates allowed, should converge well before this
for (i in 1:(2*N)){
  for (subgrp in subgrps){  #iterate through all subgrps
    mem_test <- mem  #re-initialized “test” group assignments
    for (grp_test in grps){  #iterate through available groups at selected k
      if (subgrp > 0){  #if subgroup is not a singleton
        #row numbers match neg of original singleton groups
        subgrp_mem <-  -unlist(members[subgrp])
        #trial re-assign of multi-member subgroup
        mem_test[subgrp_mem] <- grp_test
        #else re-assign singleton subgroup
      } else {mem_test[-subgrp] <- grp_test}
      ssw_test <- ssw(Dist, mem_test)
      #save results of test for that observation if ssw is improved
      if (ssw_test < ssw_best){
        best <- c(subgrp=subgrp, grp_new=grp_test, SSW=ssw_test)
        ssw_best <- ssw_test
      }
    }
  }
  
  #if best result improves upon previous iteration, change group of the best one
  if (best[3] < changes1[nrow(changes1), 3]) {
    if (best[1] < 0) {mem[-best[1]] <- best[2]} else {
      mem[-unlist(members[best[1]])] <- best[2]}
    changes1 <- rbind(changes1, best)  #keep record of changes
  } else {break}  #exit main loop if no improvement found
}


####### ANOSIM ANALYSIS following Noyer et al. 2023 #########
library(vegan)


distlia<- distance(filt_lia2, method='wunifrac') #compute distance matrix of weighted Unifrac distances 
hclust <- hclust(distlia, method='ward.D2') #Ward's hierarchical clustering
clusters3 <- cutree(hclust, k = 3) #define number threshold for sample groups (3 here following Noyer et al)

anosim_res3 <- anosim(distlia, clusters3)
print(anosim_res3)
#ANOSIM statistic R: 0.7143 -> positive value, so groups are more distinct than expected by chance 
#Significance: 0.007 -> differences between groups statistically significant 

#Visualize results 
plot(hclust, main="Hierarchical Clustering")

# Add colored clusters to the dendrogram
rect.hclust(hclust, k = 3, border = 2:4)

#Okay, now test at higher levels of k 
clusters5 <- cutree(hclust, k = 5)
anosim_res5 <- anosim(distlia, clusters5)
print(anosim_res5)
#ANOSIM statistic R: 0.9792 
#Significance: 0.002
rect.hclust(hclust, k = 5, border = 2:4)

# k = 2
clusters2 <- cutree(hclust, k = 2)
anosim_res2 <- anosim(distlia, clusters2)
print(anosim_res2)
#ANOSIM statistic R: 0.6308 
#Significance: 0.022
rect.hclust(hclust, k = 2, border = 2:4)

# k = 4
clusters4 <- cutree(hclust, k = 4)
anosim_res4 <- anosim(distlia, clusters4)
print(anosim_res4)
#ANOSIM statistic R: 0.9478 
#Significance: 0.001
rect.hclust(hclust, k = 2, border = 2:4)

# k = 6
clusters6 <- cutree(hclust, k = 6)
anosim_res6 <- anosim(distlia, clusters6)
print(anosim_res6)
#ANOSIM statistic R: 1 
#Significance: 0.003
rect.hclust(hclust, k = 2, border = 2:4)


####### bootstraps following Pannoni et al #########
library(pvclust)
asv_table <- t(otu_table(filt_lia2))
ASVCLR_result_filt_lia2 <- pvclust(asv_table, method.dist = "euclidean", 
                             method.hclust = "ward.D2", nboot = 10, parallel = TRUE)
pvrect2(ASVCLR_result_filt_lia2, alpha = 0.95, pv = "au", max.only = FALSE, 
        xpd = TRUE, lower_rect = -0.035)








