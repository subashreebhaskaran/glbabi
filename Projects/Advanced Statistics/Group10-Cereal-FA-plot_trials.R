setwd("/Users/anand/Documents/BABI/AdvStats/Group Assignment-AS-May")

#read the dataset & EDA
cereal_data <- read.csv("Dataset_Cereal.csv")
summary(cereal_data)
str(cereal_data)
head(cereal_data)
tail(cereal_data)
cereal_matrix$names

#pre-requisites:
install.packages("nFactors", repos = "http://cran.us.r-project.org")
library(nFactors)

#use cereal_data as matrix
cereal_matrix <- as.matrix(cereal_data[,2:26])

#check for correlated attributes
corrplot(cor(cereal_matrix))

#To know # of factors - get eigen value & plot scree
ev_cereal <- eigen(cor(cereal_matrix))
ev_cereal$values
ev_values <- nScree(x=ev_cereal$values)
ev_values
plotnScree(ev_values)
#From above nScree, # of optimal factors will be 5

#Perform FA for 5 factors and no rotation
factor_cereal <- factanal(~cereal_matrix, 5, rotation = "none")
factor_cereal
original_loadings = factor_cereal$loadings
load1 <- factor_cereal$loadings[,1:5]
load1
original_loadings

library(ggplot2)
library(psych)
?ggplot
#----------------------
#For each test, plot the loading as length and fill color of a bar
# note that the length will be the absolute value of the loading but the 
# fill color will be the signed value, more on this below
ggplot(as.data.frame(load1), aes(factor_cereal$scores, names(cereal_data[-1]), fill=load1[,1]))
  +
  facet_wrap(~ names(load1), nrow=1) + #place the factors in separate facets
  geom_bar(stat="identity") + #make the bars
  coord_flip() + #flip the axes so the test names can be horizontal  
  #define the fill color gradient: blue=positive, red=negative
  scale_fill_gradient2(name = "Loading", 
                       high = "blue", mid = "white", low = "red", 
                       midpoint=0, guide=F) +
  ylab("Loading Strength") + #improve y-axis label
  theme_bw(base_size=10) #use a black-and-white theme with set font size
#---------------------
plot(load1, type= "n")
text(load1, labels=names(cereal_data), load1=factor_cereal$loadings[,1:5])
#without rotation, there is no clear distinguish between the factors

#try varimax / promax rotation to improve Factor 5 loadings - Promax seems to give better results.
factor_cereal_pro <- factanal(~cereal_matrix, 5, rotation = "promax")
factor_cereal_pro

load2 <- factor_cereal_pro$loadings[,1:5]
load2

plot(load2, type= "n")
text(load2, labels=factor_cereal_pro$loadings[,1:5], names(cereal_data))
#consider only loadings above 0.4 to consolidate the variable
print(factor_cereal_pro, digits = 2, cutoff = 0.4)
