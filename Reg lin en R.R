#https://rpubs.com/joser/RegresionSimple

df <- read.table('http://verso.mat.uam.es/~joser.berrendero/datos/EdadPesoGrasas.txt', header = TRUE)
names(df)

pairs(df)

cor(df)

regresion <- lm(grasas ~ edad, data = df)
summary(regresion)

plot(df$edad, df$grasas, xlab='Edad', ylab='Grasas')
abline(regresion)

