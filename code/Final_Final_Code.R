

# load in packages and the dataset

library(tidyverse)
library(janitor)
library(brms)
library(tidybayes)
library(ggridges)

model_data <- read.csv("data/filtered_fixed_foraging_data.csv")

model_data$time <- as.numeric(model_data$time)
model_data$treatment <- as.character(model_data$treatment)

# set up the censored data

# set censor threshold
t1 <- 300

# create data set (d) where we have a column of values that are =< the threshold (y1) and a 
# column that indicates if the value was censored or not (cen1)
d <-
  model_data %>% 
  mutate(time = as.double(time)) %>% 
  mutate(y1   = if_else(time > t1, t1, time),
         cen1 = if_else(time > t1, "right", "none"))

d

# create another column in the dataset that indicates NA for any censored values (y_na)
d <-
  d %>% 
  mutate(y_na = ifelse(cen1 == "none", time, NA))

d

# setting up the list of values (means and standard deviations from the censored data) to
# be used as the sampling start points for the model (in the original example these values
# were also used as the priors for a very basic (not great) model)
mean_y <- mean(d$y_na, na.rm = T)
sd_y   <- sd(d$y_na, na.rm = T)

stanvars <- 
  stanvar(mean_y, name = "mean_y") + 
  stanvar(sd_y,   name = "sd_y")

inits <- list(Intercept = mean_y)

inits_list <- list(inits, inits, inits, inits)


# model

test_cens <- brm(y1 | cens(cen1) ~ 1 + Species * treatment * Trial + (1| Turtle),
                 data = d,
                 family = hurdle_gamma(link="log"),
                 prior = c(prior(normal(4,2), class = "Intercept"),
                           prior(normal(0, 1), class = "b"),
                           prior(exponential(0.1), class = "sd")),
                 chains = 4, iter = 4000)

saveRDS(test_cens, file="models/test_cens.RDS")


test_cens$data %>% 
  distinct() %>% 
  add_epred_draws(test_cens) %>% 
  ggplot(aes(.epred))+
  geom_histogram()

test_cens$data %>% 
  distinct() %>% 
  add_epred_draws(test_cens) %>% 
  ggplot(aes(treatment, .epred, fill=Trial))+
  geom_boxplot(outlier.shape = NA)+
  facet_wrap(~Species)+
  #scale_y_log10()+
  geom_point(data=test_cens$data, aes(treatment, y1, group=Trial),
             position=position_dodge(width=0.75))


pp_check(test_cens, type="boxplot")

library(ggridges)


#average time overall
test_cens$data %>% 
  distinct() %>% 
  add_epred_draws(test_cens) %>% 
  ungroup() %>% 
  select(-.row, -.chain) %>% 
  group_by(.draw, Species, treatment) %>% 
  summarize(average_time = mean(.epred)) %>% 
  ggplot(aes(x = average_time, y = treatment, fill = Species)) + 
  geom_density_ridges()
  


