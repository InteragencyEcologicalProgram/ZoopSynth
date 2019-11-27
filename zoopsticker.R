#Create a hexlogo for the zoopsynth project

#devtools::install_github("GuangchuangYu/hexSticker")

library(hexSticker)
library(ggplot2)
library(ggimage)
imgurl <- system.file("zoopsynth_hexlogo.png")
sticker("zoopsynth_hexlogo.png", s_width = 1, s_x = 0.9, s_y = 0.8, 
        package = "ZoopSynth", p_size = 20, p_color = "black",
        h_fill = "white", h_color = "black",
        filename = "ZoopSynth_hex1.png")

p <- ggplot(data.frame(x=1,y=1,image="zoophex2.png"), aes(x,y)) +
  geom_image(aes(image=image), size=0.91) + theme_void()
p

sticker(p, 
        s_x = 1,
        s_y = 1, 
        s_width = 1.73,
        s_heigh = 2,
        package = "ZoopSynth", p_size = 20, p_color = "black",
        h_fill = "white", h_color = "black", white_around_sticker = T,
        filename = "ZoopSynth_hex.png")
