
library(plyr); library(dplyr); library(ggplot2); library(readr)
# library(stringi); library(stringr)
# library(tidyr)
library(viridisLite); library(viridis)

library(lme4)

# library(afex)
# library(sjPlot)


# ## Packages for zero-inflated modeling ####
# library(mgcv)
# library(glmmTMB)
# # install.packages("GLMMadaptive")
# library(GLMMadaptive)


## Model using GLMMadaptive ####
# set.seed(1234)
# n <- 100 # number of subjects
# K <- 8 # number of measurements per subject
# t_max <- 5 # maximum follow-up time

## we constuct a data frame with the design:
## everyone has a baseline measurment, and then measurements at random follow-up times
# DF <- data.frame(id = rep(seq_len(n), each = K),
#                  time = c(replicate(n, c(0, sort(runif(K - 1, 0, t_max))))),
#                  sex = rep(gl(2, n/2, labels = c("male", "female")), each = K))

## design matrices for the fixed and random effects non-zero part
# X <- model.matrix(~ sex * time, data = DF)
# Z <- model.matrix(~ 1, data = DF)
# # design matrices for the fixed and random effects zero part
# X_zi <- model.matrix(~ sex, data = DF)
# Z_zi <- model.matrix(~ 1, data = DF)

# betas <- c(1.5, 0.05, 0.05, -0.03) # fixed effects coefficients non-zero part
# shape <- 2 # shape/size parameter of the negative binomial distribution
# gammas <- c(-1.5, 0.5) # fixed effects coefficients zero part
# D11 <- 0.5 # variance of random intercepts non-zero part
# D22 <- 0.4 # variance of random intercepts zero part

## we simulate random effects
# b <- cbind(rnorm(n, sd = sqrt(D11)), rnorm(n, sd = sqrt(D22)))
# # linear predictor non-zero part
# eta_y <- as.vector(X %*% betas + rowSums(Z * b[DF$id, 1, drop = FALSE]))

## linear predictor zero part
# eta_zi <- as.vector(X_zi %*% gammas + rowSums(Z_zi * b[DF$id, 2, drop = FALSE]))
# # we simulate negative binomial longitudinal data
# DF$y <- rnbinom(n * K, size = shape, mu = exp(eta_y))
# # we set the extra zeros
# DF$y[as.logical(rbinom(n * K, size = 1, prob = plogis(eta_zi)))] <- 0
#
# summary(DF)
#
# hist(DF$y)
# nrow(filter(DF, y==0))
#
# ## ZIP models
# fm1 <- mixed_model(count ~ spp * mined, random = ~ 1 | site,
#                    data = Salamanders,
#                    family = zi.poisson(), zi_fixed = ~ spp)
#
# zip_gam <- gam(list(count ~ spp * mined + s(site, bs = "re"),
#                     ~ spp * mined),
#                data = Salamanders, family = ziplss, method = "REML")
#
# zip_glmm <- glmmTMB(count ~ spp * mined + (1 | site),
#                     zi = ~ spp, data = Salamanders, family = poisson)
# fm1
# zip_glmm
# zip_gam


# nbgam2 <- gam(count ~ spp * mined + s(site, bs = "re"), data = Salamanders,
#               family = nb, method = "ML")
# nbm2 <- glmmTMB(count ~ spp * mined + (1 | site), data = Salamanders,
#                 family = nbinom2)



## Load SERDP data ####
plotlevel_data <- read_csv("data/all_plotlevel_data.csv")
str(plotlevel_data)

model_data <- plotlevel_data %>%
      select(-starts_with("se_"), -starts_with("sd_"),
             -n_obs) %>%
      filter(!is.na(imcy_inv))
str(model_data)

summary(model_data)
model_data <- filter(model_data, !is.na(years_since_fire))

## Grouping factors: installation, plot_id, visit_year
## plot_id + visit_year = unique id
## Factors of interest: fire, vegetation

## Add variables to data frame ####
model_data <- model_data %>%
      mutate(clusters_1_2 = rowSums(cbind(total_clusters1m,total_clusters2m)),
             day_of_year = lubridate::yday(visit_date),
             litter_volume = avg_dry_litter_gm2*avg_litter_depth_all,
             veg_volume = avg_dry_standing_gm2*avg_herb_ht_all) %>%
      rename(litter_gm2 = avg_dry_litter_gm2,
             litter_cover = avg_pct_litter,
             tick_count = total_ticks,
             canopy_cover = avg_canopy_cover,
             veg_cover = plot_avg_veg_cover
             ) %>%
      select(-pid2)

names(model_data)
summary(model_data)
# model_data <- as.data.frame(model_data, row.names = NULL)
# str(model_data)

GGally::ggpairs(select(model_data, imcy_inv, years_since_fire, tick_count, contains("litter"), contains("pct"), canopy_cover, total_clusters1m, clusters_1_2, total_dbh, richness, pct_pinus_dbh,  day_of_year, lat, -last_fire_year, -contains("1"), -avg_woody_ht_all), aes(color = imcy_inv))

# GGally::ggpairs(select(model_data, imcy_inv, years_since_fire, total_ticks, avg_dry_litter_gm2, avg_pct_bare, plot_avg_veg_cover, avg_dry_standing_gm2, pct_pinus_dbh, avg_canopy_cover, ycoord_lat, total_clusters1m), aes(color = imcy_inv))

# GGally::ggpairs(select(model_data, imcy_inv, years_since_fire, tick_count, litter_gm2, avg_pct_bare, plot_avg_veg_cover, richness, pct_pinus_dbh, canopy_cover, lat, total_clusters1m), aes(color = imcy_inv))


## Zero-inflated mixed model exploration ####
glmm1 <- glmer(
      tick_count ~
            # years_since_fire +
            day_of_year +
            litter_gm2 +
            # avg_pct_litter +
            canopy_cover +
            # richness +
            # lat +
            # clusters_1_2 +
            (1|installation) + (1|plot_id),
      family = "poisson", data = model_data)
# plot(glmm1)
summary(glmm1)

zip_glmm1 <- glmmTMB(
      tick_count ~
            # years_since_fire +
            day_of_year +
            litter_gm2 +
            # litter_cover +
            canopy_cover +
            # richness +
            # lat +
            # clusters_1_2 +
            (1|installation) + (1|plot_id),
      zi = ~ 1, data = model_data, family = poisson)
summary(zip_glmm1)


mm_litter <- lmer(litter_gm2 ~ canopy_cover + years_since_fire + (1|plot_id), data = model_data)
# plot(mm_litter)
# summary(mm_litter)


## Model exploration ####

tick_mod <- glmer(tick_count ~
                        # years_since_fire +
                        day_of_year +
                        litter_gm2 +
                        # litter_volume + #tick_mod1
                        # avg_pct_litter +
                        # canopy_cover +
                        richness +
                        # lat +
                        clusters_1_2 +
                        (1|installation) + (1|plot_id),
                  family = "poisson", data = model_data)
summary(tick_mod)
# summary(tick_mod1)


litter_mod <- lmer(litter_gm2 ~ years_since_fire + canopy_cover + (1|installation) + (1|plot_id), data = model_data)
# summary(litter_mod)
# https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html
# library(numDeriv)
# tt <- getME(litter_mod, "theta")
# ll <- getME(litter_mod, "lower")
# min(tt[ll==0])
# derivs1 <- litter_mod@optinfo$derivs
# lt_grad1 <- with(derivs1, solve(Hessian, gradient))
# max(abs(lt_grad1))
# max(pmin(abs(lt_grad1), abs(derivs1$gradient)))
# dd <- update(litter_mod, devFunOnly=TRUE)
# pars <- unlist(getME(litter_mod, c("theta","fixef")))
# grad2 <- grad(dd,pars)
# ss <- getME(litter_mod, c("theta","fixef"))
# litter_mod <- update(litter_mod, start=ss, control=lmerControl(optCtrl=list(maxfun=2e4)))
summary(litter_mod)
car::Anova(litter_mod)

canopy_mod <- lmer(canopy_cover ~ years_since_fire + (1|installation) + (1|plot_id), data = model_data),
# summary(canopy_mod)

host_mod <- glmer(clusters_1_2 ~ canopy_cover + years_since_fire + (1|installation) + (1|plot_id), family = "poisson", data = model_data)

