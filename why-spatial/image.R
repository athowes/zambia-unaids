library(magick)
library(raster)
library(ggplot2)
library(patchwork)

input_path <- "figures/cheetah-bw.jpg"
output_path <- "figures/cheetah-bw-small.jpg"

img_large <- magick::image_read(input_path)
img <- magick::image_scale(img_large, "80")

image_ggplot(img)

image_write(img, output_path)

noisy_img <- magick::image_noise(img)
image_ggplot(noisy_img)

image_write(noisy_img, paste0("noisy-", output_path))

noisy_smoothed_img <- magick::image_blur(noisy_img)
image_ggplot(noisy_smoothed_img)

image_write(noisy_smoothed_img, paste0("noisy-smoothed-", output_path))

f <- function(path, name) {
  M <- as.matrix(raster(path))
  M_norm <- M / 255
  M_melt <- reshape2::melt(M_norm)
  M_melt$type <- name
  return(M_melt)
}

df1 <- f(output_path, name = "Original")
df2 <- f(paste0("noisy-", output_path), name = "Noisy")
df3 <- f(paste0("noisy-smoothed-", output_path), name = "Smoothed")

df <- dplyr::bind_rows(df1, df2, df3) %>%
  dplyr::mutate(type = factor(type, levels = c("Original", "Noisy", "Smoothed")))

ggplot(df, aes(x = value, fill = type)) +
  geom_histogram(col = "grey30") +
  facet_grid(type ~ .) +
  scale_fill_manual(values = cbpalette) +
  lims(x = c(0, 1)) +
  labs(x = "", y = "Number of pixels") +
  guides(fill = "none") +
  theme_minimal()


