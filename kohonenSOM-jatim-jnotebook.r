library(kohonen)
set.seed(222)

data_raw <- read.csv("data-daerah.csv")
head(data_raw, 10)

data_daerah <- data_raw[,1]
head(data_daerah, 10)

X <- as.matrix(data_raw[,-1])
head(X, 10)

# x = 4, y = 1
g <- somgrid(xdim = 4, ydim = 1, topo = "rectangular")

# alpha = 0,05 & 0,01
map <- som(X, grid = g, alpha = c(0.05, 0.01), radius = 1, user.weights = runif(36*13))

kategori_kelompok <- map$unit.classif
df_kelompok <- data.frame(data_daerah, kategori_kelompok)
names(df_kelompok) <- c("Daerah", "Kelompok") 
# output:
df_kelompok <- df_kelompok[order(df_kelompok$Kelompok),]

df_kelompok[which(df_kelompok$Kelompok == 1),]

df_kelompok[which(df_kelompok$Kelompok == 2),]

df_kelompok[which(df_kelompok$Kelompok == 3),]

df_kelompok[which(df_kelompok$Kelompok == 4),]

# Attribut per kelompok
df_attr <- data.frame(map$codes)
df_attr

#drop atribute yang berupa subsidi, pinjaman, dan bantuan
df_attr$X8 <- NULL
df_attr$X9 <- NULL
df_attr$X11 <- NULL
df_attr$X12 <- NULL
df_attr$X13 <- NULL
#rata-rata dari atribute pendapatan per kelompok
df_attr$Average <- (rowSums(df_attr[, c(2,3,4,5,6,7,8)]))/8
#output data frame berupa daerah beserta jumlah penduduk dan rata-rata pendapatan:
df_average <- df_attr[,c("X1","Average")]
df_average

plot(map)

plot(map, type='count')

plot(map, type='mapping')

plot(map, type='dist.neighbours')
