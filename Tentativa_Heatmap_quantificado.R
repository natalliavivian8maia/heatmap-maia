setwd("C:/Users/natal/Downloads")

# Tentativa de um mapa de calor em uma variável quantitativa

#Quero criar um heatmap de ocupação

#Pacotes possíveis: magick, imager, EBImage, terra

library(magick)

arquivo <- file.choose() #serve para escolher a imagem manualmente 
img <- image_read(arquivo)

img <- image_read("mapa4.jpg")
print(img)

library(imager) #pacote que faz o processamento da imagem

im <- load.image("mapa de calor gir 3 cam 10.jpg")


R <- R(im)
G <- G(im)
B <- B(im)
dim(im)

plot(R)
plot(G)
plot(B)


summary(R)
summary(G)
summary(B)


mask <- (R < 0.99) | (G < 0.99) | (B < 0.99)
plot(mask)
sum(mask)


table(mask)

layout(matrix(c(1,2),1,2))

plot(im)

plot(mask)



mask <- as.cimg(mask)

class(mask)

dim(mask)
is.logical(mask)
