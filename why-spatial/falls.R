library(magick)
library(raster)
library(ggplot2)
library(patchwork)

falls <- magick::image_read("figures/falls.png")

image <- image_fill(falls, 'none')
raster <- as.raster(image)

df <- data.frame(xmin = c(7, 1.75, 5.5), xmax = c(8, 2.75, 6.5), ymin = c(5.5, 2.75, 2), ymax = c(6.5, 3.75, 3))

df_text <- df %>%
  dplyr::mutate(x = 0.5 * (xmin + xmax), y = 0.5 * (ymin + ymax), label = c("A", "B", "C")) %>%
  dplyr::select(x, y, label)

df_col <- c("#636c17", "#7f8abf", "#76635c")

plot_abc <- ggplot(df) + 
  annotation_raster(raster, xmin = 0, xmax = 10, ymin = 0, ymax = 10) + 
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "black", alpha = 1) +
  geom_text(data = df_text, aes(x = x, y = y, label = label), col = "white") +
  lims(x = c(0, 10), y = c(0, 10)) +
  theme_void() +
  coord_fixed()

ggsave("figures/falls1.png", h = 3.5, w = 4)

colA <- ggplot() +
  geom_rect(aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1), fill = "#636c17") +
  theme_void() +
  coord_fixed()

colB <- ggplot() +
  geom_rect(aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1), fill = "#76635c") +
  theme_void() +
  coord_fixed()

colC <- ggplot() +
  geom_rect(aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1), fill = "#7f8abf") +
  theme_void() +
  coord_fixed()

plot_bc <- df[2:3, ] %>%
  ggplot() + 
  annotation_raster(raster, xmin = 0, xmax = 10, ymin = 0, ymax = 10) + 
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "black", alpha = 1) +
  geom_text(data = df_text[2:3, ], aes(x = x, y = y, label = label), col = "white") +
  lims(x = c(0, 10), y = c(0, 10)) +
  theme_void() +
  coord_fixed()

plot_bc + colA + plot_layout(widths = c(2.5, 1)) +
  plot_annotation(title = "Under box A")

ggsave("figures/falls2.png", h = 3.5, w = 5)

plot_c <- df[3, ] %>%
  ggplot() + 
  annotation_raster(raster, xmin = 0, xmax = 10, ymin = 0, ymax = 10) + 
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "black", alpha = 1) +
  geom_text(data = df_text[3, ], aes(x = x, y = y, label = label), col = "white") +
  lims(x = c(0, 10), y = c(0, 10)) +
  theme_void() +
  coord_fixed()

plot_c + colB + plot_layout(widths = c(2.5, 1)) +
  plot_annotation(title = "Under box B")

ggsave("figures/falls3.png", h = 3.5, w = 5)

plot <- ggplot() + 
  annotation_raster(raster, xmin = 0, xmax = 10, ymin = 0, ymax = 10) + 
  lims(x = c(0, 10), y = c(0, 10)) +
  theme_void() +
  coord_fixed()

plot + colC + plot_layout(widths = c(2.5, 1)) +
  plot_annotation(title = "Under box C")

ggsave("figures/falls4.png", h = 3.5, w = 5)
