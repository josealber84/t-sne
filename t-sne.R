# t-sne plot de las viviendas variando la perplexity
library(Rtsne)
library(data.table)
library(tidyverse)
library(ggplot2)
library(animation)

train = read_csv("train.csv")
datos <- data.table(train)
datos <- datos[,sapply(datos,is.numeric),with=FALSE]
datos <- datos[complete.cases(datos),] 
price <- datos[,SalePrice]
datos <- datos[, SalePrice := NULL]
datos <- scale(datos, center = F)

generar_animacion <- function(datos, price, perplexities){
  
  for(p in perplexities){
    
    cat(paste0("Plotting perplexity = ", p), fill = T)
    rtsne_out <- Rtsne(datos, perplexity = p)
    datos_plot <- as_data_frame(rtsne_out$Y)
    datos_plot$price = price
    print(ggplot(data = datos_plot) + 
            geom_point(mapping = aes(x = V1, y = V2, color = price)) +
            ggtitle(paste0("perplexity = ", p)) + 
            scale_color_gradientn(colours = rainbow(7)) +
            theme_bw())
    
  }
  
}

# saveGIF(expr = generar_animacion(datos[1:50], price[1:50], seq(from = 1, to = 10, by = 1)))
ani.options(ani.width = 1000, ani.heigh = 1000, interval = 0.5, )
saveGIF(expr = generar_animacion(datos, price, seq(from = 10, to = 100, by = 10)))
