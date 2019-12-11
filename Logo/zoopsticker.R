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

p <- ggplot(data.frame(x=1,y=1,image="zoophex3.png"), aes(x,y)) +
  geom_image(aes(image=image), size=0.91) + theme_void()
p

sticker(p, 
        s_x = 1,
        s_y = 1, 
        s_width = 1.73,
        s_height = 2,
        package = "ZoopSynth", p_size = 20, p_color = "black",
        h_fill = "white", h_color = "black", white_around_sticker = T,
        filename = "ZoopSynth_hex2.png")


p2 <- ggplot(data.frame(x=1,y=1,image="zoopercopepod.png"), aes(x,y)) +
  geom_image(aes(image=image), size=1) + theme_void()
p2

sticker(p2, 
        s_x = .8,
        s_y = .8, 
        s_width = 1.5,
        s_height = 1.5,
        package = "zooper", p_size = 30, p_color = "black",
        h_fill = "white", h_color = "black", white_around_sticker = T,
        filename = "zooperhex.png")


p3 <- ggplot(data.frame(x=1,y=1,image="zooperdelta.png"), aes(x,y)) +
  geom_image(aes(image=image), size=1) + theme_void()
p3

sticker(p3, 
        s_x = 1,
        s_y = 1, 
        s_width = 2.3,
        s_height = 2.3,
        package = "zooper", p_size = 30, p_color = "black", 
        p_family = "wqy-microhei",
        h_fill = "white", h_color = "black", white_around_sticker = T,
        filename = "zooperhex2.png")



p4 <- ggplot(data.frame(x=1,y=1,image="Logo/zooperdelta2.png"), aes(x,y)) +
  geom_image(aes(image=image), size=1) + theme_void()
p4


sticker(p4, 
        s_x = 1,
        s_y = 1, 
        s_width = 2.3,
        s_height = 2.3,
        package = "zooper", p_size = 30, p_color = "black", 
        p_family = "wqy-microhei",
        h_fill = "white", h_color = "black", white_around_sticker = T,
        filename = "Logo/zooperhex3.png")

