data(varespec)

## Bray-Curtis distances between samples
dis <- vegdist(varespec)

## First 16 sites grazed, remaining 8 sites ungrazed
groups <- factor(c(rep(1,16), rep(2,8)), labels = c("grazed","ungrazed"))

## Calculate multivariate dispersions
mod <- betadisper(dis, groups)
mod

## Perform test
anova(mod)
TukeyHSD(mod)

## Permutation test for F
permutest(mod, pairwise = TRUE, permutations = 99)

## Tukey's Honest Significant Differences
(mod.HSD <- TukeyHSD(mod))
plot(mod.HSD)

## Plot the groups and distances to centroids on the
## first two PCoA axes
plot(mod)

## with data ellipses instead of hulls
plot(mod, ellipse = TRUE, hull = FALSE) # 1 sd data ellipse
plot(mod, ellipse = TRUE, hull = FALSE, conf = 0.90) # 90% data ellipse

## can also specify which axes to plot, ordering respected
plot(mod, axes = c(3,1), seg.col = "forestgreen", seg.lty = "dashed")

## Draw a boxplot of the distances to centroid for each group
boxplot(mod)

## `scores` and `eigenvals` also work
scrs <- scores(mod)
str(scrs)
head(scores(mod, 1:4, display = "sites"))
# group centroids/medians 
scores(mod, 1:4, display = "centroids")
# eigenvalues from the underlying principal coordinates analysis
eigenvals(mod) 

## try out bias correction; compare with mod3
(mod3B <- betadisper(dis, groups, type = "median", bias.adjust=TRUE))
anova(mod3B)
permutest(mod3B, permutations = 99)

## should always work for a single group
group <- factor(rep("grazed", NROW(varespec)))
(tmp <- betadisper(dis, group, type = "median"))
(tmp <- betadisper(dis, group, type = "centroid"))

## simulate missing values in 'd' and 'group'
## using spatial medians
groups[c(2,20)] <- NA
dis[c(2, 20)] <- NA
mod2 <- betadisper(dis, groups) ## messages
mod2
permutest(mod2, permutations = 99)
anova(mod2)
plot(mod2)
boxplot(mod2)
plot(TukeyHSD(mod2))

## Using group centroids
mod3 <- betadisper(dis, groups, type = "centroid")
mod3
permutest(mod3, permutations = 99)
anova(mod3)
plot(mod3)
boxplot(mod3)
plot(TukeyHSD(mod3))







####################################################################################
library(ape)

#Spatial correlation
ozone <- read.table("https://stats.idre.ucla.edu/stat/r/faq/ozone.csv", 
                    sep=",", header=T)
head(ozone, n=10)

str(ozone)
ozone.dists <- as.matrix(dist(cbind(ozone$Lon, ozone$Lat)))

ozone.dists.inv <- 1/ozone.dists
diag(ozone.dists.inv) <- 0

ozone.dists.inv[1:5, 1:5]

Moran.I(ozone$Av8top, ozone.dists.inv)


##################################################################################

# two similar populations:
dat1a<-matrix(sample(c(0,1,1,1),200,replace=T),10,20)
dat1b<-matrix(sample(c(0,1,1,1),200,replace=T),10,20)

# generating a third sample from the same population, but with reduced
# number of species occurrences. this set will have higher
# beta-diversity (or "multivariate spread"):
dat2<-matrix(sample(c(0,0,0,1),200,replace=T),10,20)



# distance matrices:
fac<-gl(2,10)
dist11<-vegdist(rbind(dat1a,dat1b))
dist12<-vegdist(rbind(dat1a,dat2)): 
  
  # when computing sets with same beta-dispersion we get a
  # correct adonis result with no sign. group differences
  # in species composition:
  anova(betadisper(dist11,fac))
adonis(rbind(dat1a,dat1b)~fac)



