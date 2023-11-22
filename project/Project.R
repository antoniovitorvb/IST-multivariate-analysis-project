library(psych)
library(ggplot2)
library(GGally)
library(colorspace)

data <- read.csv('project.csv')
head(data)

# Create a named vector with the mapping
star_type_names <- c("Brown Dwarf", "Red Dwarf", "White Dwarf", "Main Sequence",
                     "Supergiant", "Hypergiant")
names(star_type_names) <- c(0, 1, 2, 3, 4, 5)

# Apply the mapping to the Star.type column
data$Star.type <- star_type_names[as.character(data$Star.type)]

# Convert some variables to a factor or categorical variable
data$Star.type <- as.factor(data$Star.type)
data$Star.color <- as.factor(data$Star.color)
data$Spectral.Class <- as.factor(data$Spectral.Class)

summary(data)
describe(data)

#Describe the data by each Star type
describeBy(data[,1:4], group=data$Star.type)

#Calculate the variance
round(var(data[,1:4]),3)

#Calculate the covariance
round(cov(data[,1:4]),3)

################################################################################
##                                Correlation                                 ##
################################################################################

#Calculate the correlation
round(cor(data[,1:4]),3)

#Plot correlation matrix
ggcorr(data[,1:4], method = c("everything", "pearson"))

################################################################################
##                                    Tables                                  ##
################################################################################

table(data$Star.type)
table(data$Star.color)
table(data$Spectral.Class)

################################################################################
##                                  Bar Charts                                ##
################################################################################

#Star type Bar chart
aux<-table(data$Star.type)
colors <- rainbow(n = 7)
barplot(height=as.vector(aux), names=names(aux), col=colors)

#Star color Bar chart
aux<-table(data$Star.color)
colors <- rainbow(n = 19)
barplot(height=as.vector(aux), names=names(aux), col=colors)

#Spectral Class Bar chart
aux<-table(data$Spectral.Class)
colors <- rainbow(n = 7)
barplot(height=as.vector(aux), names=names(aux), col=colors)

################################################################################
##                                  Boxplots                                  ##
################################################################################

#Temperature boxplot by type
ggplot(data, aes(y=Temperature..K., fill=Star.type)) + 
  geom_boxplot(alpha=0.3)

#Luminosity boxplot by type
ggplot(data, aes(y=Luminosity.L.Lo., fill=Star.type)) + 
  geom_boxplot(alpha=0.3)

#Radius boxplot by type
ggplot(data, aes(y=Radius.R.Ro., fill=Star.type)) + 
  geom_boxplot(alpha=0.3)

#Absolute magnitude boxplot by type
ggplot(data, aes(y=Absolute.magnitude.Mv., fill=Star.type)) + 
  geom_boxplot(alpha=0.3)

################################################################################
##                                Histograms                                  ##
################################################################################

#Calculate k and delta for the Sturges Rule
k<-floor(1+log(nrow(data))/log(2))
delta<-(range(data$Temperature..K.)[2]-
          range(data$Temperature..K.)[1])/k
#Temperature Histogram
ggplot(data, aes(x=Temperature..K.)) +
  geom_histogram(binwidth=delta, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Temparature histogram") +
  theme(plot.title = element_text(size=15))

#Recalculating delta
delta<-(range(data$Luminosity.L.Lo.)[2]-
          range(data$Luminosity.L.Lo.)[1])/k
#Luminosity Histogram
ggplot(data, aes(x=Luminosity.L.Lo.)) +
  geom_histogram(binwidth=delta, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Luminosity histogram") +
  theme(plot.title = element_text(size=15))

#Recalculating delta
delta<-(range(data$Radius.R.Ro.)[2]-
          range(data$Radius.R.Ro.)[1])/k
#Radius Histogram
ggplot(data, aes(x=Radius.R.Ro.)) +
  geom_histogram(binwidth=delta, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Radius histogram") +
  theme(plot.title = element_text(size=15))

#Recalculating delta
delta<-(range(data$Absolute.magnitude.Mv.)[2]-
          range(data$Absolute.magnitude.Mv.)[1])/k
#Absolute magnitude Histogram
ggplot(data, aes(x=Absolute.magnitude.Mv.)) +
  geom_histogram(binwidth=delta, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Absolute magnitude histogram") +
  theme(plot.title = element_text(size=15))


################################################################################
##                               Density plot                                 ##
################################################################################

#Temperature Density 
plot(density(data$Temperature..K.), main="Temperature (K)")

#Luminosity Density 
plot(density(data$Luminosity.L.Lo.), main="Luminosity (L/Lo)")

#Radius Density 
plot(density(data$Radius.R.Ro.), main="Radius (R/Ro)")

#Absolute magnitude Density 
plot(density(data$Absolute.magnitude.Mv.), main="Absolute magnitude (Mv)")


################################################################################
##            Relationship scatterplot matrices by Star Type                  ##
################################################################################

#Relationship between numerical variables
pairs(data[,1:4],col=(data$Star.type))

#Relationship between categorical variables
pairs(data[,5:7],col=(data$Star.type))

#Relationship between all variables
pairs(data[,1:7],col=(data$Star.type))

################################################################################
##                Complete Relationship scatterplot matrices                  ##
################################################################################

pairs.panels(data[,1:7], smooth = FALSE, scale = FALSE, density=TRUE,
             ellipses=FALSE,digits = 2,hist.col="green")



