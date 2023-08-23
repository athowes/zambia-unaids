library(ggplot2)
library(ggpubr)
library(patchwork)

set.seed(2)

cbpalette <- c("#56B4E9","#009E73", "#E69F00", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")

sf <- sf::st_read("zmb_areas_admin3.geojson")

sf_constituency_full <- dplyr::filter(sf, area_level == 3)
sf_constituency <- sf::st_simplify(sf_constituency_full, dTolerance = 1000)

ggplot(sf_constituency) +
  geom_sf() +
  labs(subtitle = paste0("The ", nrow(sf_constituency), " constituencies of Zambia")) +
  theme_void()

ggsave("figures/constituency.png", h = 3.5, w = 6)

simulate_icar <- function(W, sd = 1) {
  n <- ncol(W)
  num <- rowSums(W)
  Q <- -W
  diag(Q) <- num
  Q_aux <- eigen(Q)$vectors[, order(eigen(Q)$values)]
  D_aux <- sort(eigen(Q)$values)
  rnd <- rnorm(n - 1, 0, sqrt(sd * (1/D_aux[-1])))
  rnd <- Q_aux %*% c(0, rnd)
  return(as.vector(rnd))
}

nb <- spdep::poly2nb(sf_constituency_full)

nb_sf <- spdep::nb2lines(nb, coords = sp::coordinates(as(sf_constituency, "Spatial"))) %>%
  as("sf") %>%
  sf::st_set_crs(sf::st_crs(sf_constituency))

ggplot(sf_constituency) +
  geom_sf(data = nb_sf) +
  theme_void()

ggsave("figures/graph.png", h = 3.5, w = 6)

W <- spdep::nb2mat(neighbours = nb, style = "B", zero.policy = TRUE)

beta <- -2
u <- simulate_icar(W, sd = 0.5)

eta <- beta + u
rho <- plogis(eta)
sf_constituency$rho <- rho

base <- ggplot(sf_constituency, aes(fill = rho)) +
  geom_sf(size = 0.1, color = "grey30") +
  scale_fill_viridis_c(
    option = "C", direction = -1, limits = c(0, 0.6),
    labels = scales::label_percent(1)
  ) +
  labs(fill = "", subtitle = "Underlying truth") +
  theme_void()

base

ggsave("figures/base.png", h = 3.5, w = 6)

m1 <- 5
y1 <- rbinom(n = nrow(sf_constituency), size = m1, prob = rho)
sf_constituency$rho1 <- y1 / m1

survey1 <- ggplot(sf_constituency, aes(fill = rho1)) +
  geom_sf(size = 0.1, color = "grey30") +
  scale_fill_viridis_c(
    option = "C", direction = -1, limits = c(0, 0.6),
    labels = scales::label_percent(1), na.value = "#481668"
  ) +
  labs(fill = "", subtitle = "Direct estimates") +
  guides(fill = "none") +
  theme_void()

survey1 + base +
  plot_annotation(title = paste0("Survey of size ", m1))

ggsave("figures/survey1-base.png", h = 3.5, w = 6)

ggplot(sf_constituency, aes(x = rho, y = rho1)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  stat_cor(aes(label = after_stat(r.label)), method = "pearson", label.x = 0.9, label.y = 0.1) +
  labs(x = "Underlying truth", y = "Estimate", title = paste0("Survey of size ", m1)) +
  lims(x = c(0, 1), y = c(0, 1)) +
  theme_minimal()

ggsave("figures/scatter-survey1.png", h = 3.5, w = 6)

m2 <- 25
y2 <- rbinom(n = nrow(sf_constituency), size = m2, prob = rho)
sf_constituency$rho2 <- y2 / m2

survey2 <- ggplot(sf_constituency, aes(fill = rho2)) +
  geom_sf(size = 0.1, color = "grey30") +
  scale_fill_viridis_c(
    option = "C", direction = -1, limits = c(0, 0.6),
    labels = scales::label_percent(1)
  ) +
  labs(fill = "", subtitle = "Direct estimates") +
  guides(fill = "none") +
  theme_void()

survey2 + base +
  plot_annotation(title = paste0("Survey of size ", m2))

ggsave("figures/survey2-base.png", h = 3.5, w = 6)

ggplot(sf_constituency, aes(x = rho, y = rho2)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  stat_cor(aes(label = after_stat(r.label)), method = "pearson", label.x = 0.9, label.y = 0.1) +
  labs(x = "Underlying truth", y = "Estimate", title = paste0("Survey of size ", m2)) +
  lims(x = c(0, 1), y = c(0, 1)) +
  theme_minimal()

ggsave("figures/scatter-survey2.png", h = 3.5, w = 6)

m3 <- 125
y3 <- rbinom(n = nrow(sf_constituency), size = m3, prob = rho)
sf_constituency$rho3 <- y3 / m3

survey3 <- ggplot(sf_constituency, aes(fill = rho3)) +
  geom_sf(size = 0.1, color = "grey30") +
  scale_fill_viridis_c(
    option = "C", direction = -1, limits = c(0, 0.6),
    labels = scales::label_percent(1)
  ) +
  labs(fill = "", subtitle = "Direct estimates") +
  guides(fill = "none") +
  theme_void()

survey3 + base +
  plot_annotation(title = paste0("Survey of size ", m3))  

ggsave("figures/survey3-base.png", h = 3.5, w = 6)

ggplot(sf_constituency, aes(x = rho, y = rho3)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  stat_cor(aes(label = after_stat(r.label)), method = "pearson", label.x = 0.9, label.y = 0.1) +
  labs(x = "Underlying truth", y = "Estimate", title = paste0("Survey of size ", m3)) +
  lims(x = c(0, 1), y = c(0, 1)) +
  guides(fill = "none") +
  theme_minimal()

ggsave("figures/scatter-survey3.png", h = 3.5, w = 6)

fit_model <- function(dat) {
  tau_prior <- list(prec = list(prior = "logtnormal", param = c(0, 1/2.5^2), initial = 0, fixed = FALSE))
  beta_prior <- list(mean.intercept = -2, prec.intercept = 1)
  spdep::nb2INLA("sf.adj", nb)
  g <- INLA::inla.read.graph(filename = "sf.adj")
  formula <- y ~ 1 + f(id, model = "besag", graph = g, scale.model = TRUE, constr = TRUE, hyper = tau_prior)
  
  fit <- INLA::inla(
    formula, family = "binomial", control.family = list(control.link = list(model = "logit")),
    control.fixed = beta_prior, data = dat, Ntrials = m, control.predictor = list(compute = TRUE, link = 1),
    control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE, config = TRUE)
  )
  
  return(fit)
}

dat1 <- list(id = 1:nrow(sf_constituency), y = y1, m = m1)
fit1 <- fit_model(dat1)

dat2 <- list(id = 1:nrow(sf_constituency), y = y2, m = m2)
fit2 <- fit_model(dat2)

dat3 <- list(id = 1:nrow(sf_constituency), y = y3, m = m3)
fit3 <- fit_model(dat3)

sf_constituency$smooth1 <- fit1$summary.fitted.values$mean
sf_constituency$smooth2 <- fit2$summary.fitted.values$mean
sf_constituency$smooth3 <- fit3$summary.fitted.values$mean

sf_constituency %>%
  sf::st_drop_geometry() %>%
  dplyr::select(rho, rho1, smooth1) %>%
  tidyr::pivot_longer(cols = -rho, names_to = "name", values_to = "value") %>%
  mutate(name = forcats::fct_recode(name, "Direct estimates" = "rho1", "Modelled estimates" = "smooth1")) %>%
  ggplot(aes(x = rho, y = value)) +
  geom_point(alpha = 0.5) +
  facet_grid(~ name) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  stat_cor(aes(label = after_stat(r.label)), method = "pearson", label.x = 0.75, label.y = 0.1) +
  labs(x = "Underlying truth", y = "Estimate", title = paste0("Survey of size ", m1)) +
  lims(x = c(0, 1), y = c(0, 1)) +
  theme_minimal()

ggsave("figures/scatter-survey1-modelled.png", h = 3.5, w = 6)

scatter2 <- sf_constituency %>%
  sf::st_drop_geometry() %>%
  dplyr::select(rho, rho2, smooth2) %>%
  tidyr::pivot_longer(cols = -rho, names_to = "name", values_to = "value") %>%
  mutate(name = forcats::fct_recode(name, "Direct estimates" = "rho2", "Modelled estimates" = "smooth2")) %>%
  ggplot(aes(x = rho, y = value)) +
  geom_point(alpha = 0.5) +
  facet_grid(~ name) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  stat_cor(aes(label = after_stat(r.label)), method = "pearson", label.x = 0.75, label.y = 0.1) +
  labs(x = "Underlying truth", y = "Estimate", title = paste0("Survey of size ", m2)) +
  lims(x = c(0, 1), y = c(0, 1)) +
  theme_minimal()

scatter2

ggsave("figures/scatter-survey2-modelled.png", h = 3.5, w = 6)

sf_constituency %>%
  sf::st_drop_geometry() %>%
  dplyr::select(rho, rho3, smooth3) %>%
  tidyr::pivot_longer(cols = -rho, names_to = "name", values_to = "value") %>%
  mutate(name = forcats::fct_recode(name, "Direct estimates" = "rho3", "Modelled estimates" = "smooth3")) %>%
  ggplot(aes(x = rho, y = value)) +
  geom_point(alpha = 0.5) +
  facet_grid(~ name) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  stat_cor(aes(label = after_stat(r.label)), method = "pearson", label.x = 0.75, label.y = 0.1) +
  labs(x = "Underlying truth", y = "Estimate", title = paste0("Survey of size ", m3)) +
  lims(x = c(0, 1), y = c(0, 1)) +
  theme_minimal()

ggsave("figures/scatter-survey3-modelled.png", h = 3.5, w = 6)

modelled1 <- ggplot(sf_constituency, aes(fill = smooth1)) +
  geom_sf(size = 0.1, color = "grey30") +
  scale_fill_viridis_c(
    option = "C", direction = -1, limits = c(0, 0.6),
    labels = scales::label_percent(1)
  ) +
  labs(fill = "", subtitle = "Modelled estimates") +
  theme_void()

survey1 + modelled1 +
  plot_annotation(title = paste0("Survey of size ", m1))

ggsave("figures/survey1-modelled.png", h = 3.5, w = 6)

modelled2 <- ggplot(sf_constituency, aes(fill = smooth2)) +
  geom_sf(size = 0.1, color = "grey30") +
  scale_fill_viridis_c(
    option = "C", direction = -1, limits = c(0, 0.6),
    labels = scales::label_percent(1)
  ) +
  labs(fill = "", subtitle = "Modelled estimates") +
  theme_void()

survey2 + modelled2 +
  plot_annotation(title = paste0("Survey of size ", m2))

ggsave("figures/survey2-modelled.png", h = 3.5, w = 6)

modelled3 <- ggplot(sf_constituency, aes(fill = smooth3)) +
  geom_sf(size = 0.1, color = "grey30") +
  scale_fill_viridis_c(
    option = "C", direction = -1, limits = c(0, 0.6),
    labels = scales::label_percent(1)
  ) +
  labs(fill = "", subtitle = "Modelled estimates") +
  theme_void()

survey3 + modelled3 +
  plot_annotation(title = paste0("Survey of size ", m3))

ggsave("figures/survey3-modelled.png", h = 3.5, w = 6)

scatter2 / (survey2 + modelled2)

ggsave("figures/abstract.png", h = 5, w = 6)
