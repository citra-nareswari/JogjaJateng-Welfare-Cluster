library(readxl) 
data <- read_excel("C:/!! Kuliah/PKL/sumber data/Dataset.xlsx", col_names=TRUE, sheet = "FIX")
summary(data)

#Uji Korelasi Pearson
data1 <- scale(data[-1])
cor(data1)
colnames(data1)
library(corrplot)
corrplot(cor(data1), method = "color", type = "upper",
         tl.col = "black", tl.cex = 0.8, addCoef.col = "black", 
         col = colorRampPalette(c("blue", "white", "red"))(200),
         number.cex = 0.7, diag = FALSE)

#PCA
coba_pca<- prcomp(data1, center = TRUE, scale. = TRUE)
summary(coba_pca)
pca_data <- coba_pca$x[, 1:3] #Ambil dimensi = 3 karena varians > 80

#Mencari banyak klaster
library(mclust)
library(cluster)
library(factoextra)

bic_values <- numeric()
silhouette_values <- numeric()

for (k in 2:7) {
  model <- Mclust(pca_data, G = k)
  bic_values[k] <- model$bic

  sil <- silhouette(model$classification, dist(pca_data))
  silhouette_values[k] <- mean(sil[, 3])  # nilai silhouette rata-rata
}

hasil_iterasi <- data.frame(
  Clusters = 2:7,
  BIC = bic_values[2:7],
  Silhouette = silhouette_values[2:7]
)

round(hasil_iterasi,3)  #klaster terpilih: 2

#GMM dengan k=2

mbc2 <- Mclust(pca_data, 2)
data$Cluster <- mbc2$classification

klaster <- data.frame(Wilayah = data[,1], Cluster = data$Cluster)
a <- klaster[klaster$Cluster==1,] #Anggota klaster 1
a
nrow(a)
b <- klaster[klaster$Cluster==2,] #Anggota klaster 2
nrow(b)
b$Provinsi.Kabupaten.Kota

#Identifikasi karakteristik tiap klaster
library(dplyr)
rata_rata_klaster <- data %>%
  select(-1) %>%
  group_by(Cluster) %>%
  summarise_all(mean)

rata_rata_klaster

#Plot klaster 3 dimensi
library(plotly)
pca_cluster <- data.frame(pca_data[,1:3], Cluster = factor(data$Cluster))
fig <- plot_ly(pca_cluster, 
               x = ~PC1, y = ~PC2, z = ~PC3, 
               color = ~Cluster, colors = c("#66c2a5", "#fc8d62"),
               type = 'scatter3d', mode = 'markers',
               marker = list(size = 5)) %>%
  layout(title = "Plot 3D Hasil Klastering GMM (PCA)",
         scene = list(xaxis = list(title = 'PC1'),
                      yaxis = list(title = 'PC2'),
                      zaxis = list(title = 'PC3')))

fig

library(scatterplot3d)
colors <- c("green", "orange")

scatterplot3d(pca_cluster$PC1, pca_cluster$PC2, pca_cluster$PC3,
              color = colors[as.numeric(pca_cluster$Cluster)],
              pch = 19, main = "Plot 3D PCA Klastering GMM k = 2",
              sub = "Pengelompokan Wilayah DIY & Jawa Tengah",
              xlab = "PC1", ylab = "PC2", zlab = "PC3")


#Plot DIY-Jateng
library(sf)
library(ggplot2)
library(dplyr)
colnames(klaster)

gadm <- st_read("C:/!! Kuliah/PKL/gadm41_IDN_2.json")

peta <- gadm %>% filter(NAME_1 %in% c("JawaTengah", "Yogyakarta"))
peta <- peta %>% filter(NAME_2 != "WadukKedungombo")

peta_klaster <- peta %>%
  left_join(klaster, by = c("NAME_2" = "Provinsi.Kabupaten.Kota")) #GROBOGAN

plot_peta <- ggplot(peta_klaster) +
  geom_sf(aes(fill = as.factor(Cluster)), color = "white", size = 0.2) +
  geom_sf_text(aes(label = NAME_2), size = 1.5, color = "black", check_overlap = TRUE) +
  scale_fill_brewer(palette = "Set2", name = "Klaster") +
  labs(title = "Peta Hasil Klastering GMM k=2",
       subtitle = "Wilayah DIY & Jawa Tengah") +
  theme_minimal() +
  theme(legend.position = "right")
plot_peta

ggsave("Peta_Klaster_GMM.png", 
      plot = plot_peta, 
      width = 10, height = 8, units = "in", 
      dpi = 300)
