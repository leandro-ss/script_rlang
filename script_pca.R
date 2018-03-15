#######################################################################
################# NAO MONITORADO ######################################
#######################################################################

# Carregando dados
data(USArrests)

#Avaliando dados
names(USArrests)
summary(USArrests)

print(USArrests)

#Analisando dados
apply(USArrests, 2, var)

#Pré-processando dados
scaledData = scale(USArrests)
summary(USArrests)
summary(scaledData)

hist(USArrests$Murder)
hist(scaledData)

#Executando PCA
pca = prcomp(USArrests, scale=TRUE, center=TRUE)

#Analisando PCA
names(pca)
pca$center
pca$scale
pca$scale^2
#rota??es para eu ter todos os componentes repre
pca$rotation

#Plotando os componentes principais
biplot(pca, scale=0)

#Definindo o número de componentes
pca_var = pca$sd


#############################################################################
#############################################################################
#############################################################################

library(readr)
library(clusterSim)
face <- read_delim("C:/dev/workspace_r/br.com.pca/dataset/dataset_facebook.csv",
                   ";", escape_double = FALSE, col_types = cols(Category = col_skip(),
                                                                Paid = col_skip(), `Post Hour` = col_skip(),
                                                                `Post Month` = col_skip(), `Post Weekday` = col_skip(),
                                                                Type = col_skip()), trim_ws = TRUE)


library(zoo)

#Executando PCA
Sample <- face[10:12]
Scaled <- data.frame(apply(Sample, 2, scale))
Scaled <- na.aggregate(Scaled)
sample <- data.frame(t(na.omit(t(Scaled))))

print(sample)

pca = prcomp(sample, scale=TRUE, center=TRUE)

summary(pca)
pca$x
pca$rotation
pca$scale
pca$center

biplot(pca)
screeplot(pca)

#Definindo o número de componentes
pca_var = pca$sdev^2
print (pca_var)

pve = pca_var/sum(pca_var)
print (cumsum(pve))
plot(cumsum(pve), xlab="Componente Principal", ylab="Propor??o de Variancia Cumulativa", ylim=c(0,1), type='b')




pc=predict(pca)[,1:2]
plot(pc[,1], pc[,2], col="blue", main="2 Componentes Principais")

# Using the elbow method to find the optimal number of clusters
set.seed(1); wcss = vector()
for (i in 1:10) wcss[i] = sum(kmeans(pc, i)$withinss)
plot(1:10,
     wcss,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab = 'Number of clusters',
     ylab = 'WCSS')


kmeans = kmeans(x = pc, centers = 4)
y_kmeans = kmeans$cluster

library(cluster)
clusplot(pc,
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of customers'),
         xlab = 'Annual Income',
         ylab = 'Spending Score')


# Using the dendrogram to find the optimal number of clusters
dendrogram_x = hclust(d = dist(pc, method = 'euclidean'), method = 'ward.D')
plot(dendrogram_x)


######################################################################################
######################################################################################
######################################################################################

library(readr)
wine <- read_csv("dataset/wine.csv", col_types = cols(Customer_Segment = col_skip()))

#Executando PCA
pca = prcomp(wine, scale=TRUE, center=TRUE)

#Definindo o número de componentes
pca_var = pca$sdev^2


pca_var
pve = pca_var/sum(pca_var)
pve
plot(cumsum(pve), xlab="Componente Principal", ylab="Propor??o de Variancia Cumulativa", ylim=c(0,1), type='b')


pc=predict(pca)[,1:2]
plot(pc[,1], pc[,2], col="blue", main="2 Componentes Principais")

# Using the elbow method to find the optimal number of clusters
set.seed(1); wcss = vector()
for (i in 1:10) wcss[i] = sum(kmeans(pc, i)$withinss)
plot(1:10,
     wcss,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab = 'Number of clusters',
     ylab = 'WCSS')


kmeans = kmeans(x = pc, centers = 3)
y_kmeans = kmeans$cluster

library(cluster)
clusplot(pc,
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of customers'),
         xlab = 'Annual Income',
         ylab = 'Spending Score')


# Using the dendrogram to find the optimal number of clusters
dendrogram_x = hclust(d = dist(pc, method = 'euclidean'), method = 'ward.D')
plot(dendrogram_x)


