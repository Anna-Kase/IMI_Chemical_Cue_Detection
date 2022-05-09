

library(tidyverse)
library(janitor)
library(brms)
library(tidybayes)

all_tbl <- read.csv("data/master_all_turtle_data.csv")

all_tbl$treatment <- as.factor(all_tbl$treatment)


swim_time <- all_tbl  %>% 
  group_by(Turtle, Trial, treatment, Species, experiment_group, Side, Behavior) %>%
  filter(Behavior == "Swimming") %>% 
  summarise(
    swim_time = sum(total_seconds)
  ) 

swim_time[is.na(swim_time)]<-0


swim_2 <- brm(swim_time ~ 0 + Side*treatment*Species*Trial +
                (1|Turtle),
              family = Gamma(link = "log"),
              data = swim_time,
              prior = c(prior(normal(2,2), ub = 5.703782, class = "b"),
                        prior(exponential(0.1), class = "shape")),
              #sample_prior = "only",
              cores = 4, chains = 1, iter = 1000)

conditional_effects(swim_2)

swim_2$data %>% 
  distinct() %>% 
  add_epred_draws(swim_2) %>% 
  ggplot(aes(Trial, .epred, fill=treatment))+
  geom_boxplot()+
  facet_grid(Side~Species)+
  labs(title="Overall Swimming Predictions")


swim_2$data %>% 
  distinct() %>% 
  add_epred_draws(swim_2) %>% 
  filter(Side == "Stimulus") %>% 
  ggplot(aes(Trial, .epred, fill=treatment))+
  geom_boxplot()+
  facet_wrap(~Species)+
  labs(title="Stimulus Side Swimming Predictions")



swim_3 <- brm(swim_time ~ 0 + Side*treatment*Species*Trial +
                (1|Turtle),
              family = Gamma(link = "log"),
              data = swim_time,
              prior = c(prior(normal(4.5,2), ub = 5.703782, class = "b"),
                        prior(exponential(0.1), class = "shape")),
              #sample_prior = "only",
              cores = 4, chains = 1, iter = 1000)


swim_3$data %>% 
  distinct() %>% 
  add_epred_draws(swim_3) %>% 
  ggplot(aes(Trial, .epred, fill=treatment))+
  geom_boxplot()+
  facet_grid(Side~Species)+
  labs(title="Overall Swimming Predictions")




swim_4 <- brm(swim_time ~ 0 + Side*treatment*Species*Trial +
                (1|Turtle),
              family = Gamma(link = "log"),
              data = swim_time,
              prior = c(prior(normal(4,2), lb = 0, ub = 5.703782, class = "b"),
                        prior(exponential(0.1), class = "shape")),
              sample_prior = "only",
              cores = 4, chains = 4, iter = 1000)


swim_4$data %>% 
  distinct() %>% 
  add_epred_draws(swim_4) %>% 
  ggplot(aes(Trial, .epred, fill=treatment))+
  geom_boxplot()+
  facet_grid(Side~Species)+
  labs(title="Overall Swimming Predictions - Prior Only - normal(4,2)")

post4 <- swim_4$data %>% 
  distinct() %>% 
  add_epred_draws(swim_4)



swim_5 <- brm(swim_time ~ 1 + Side*treatment*Species*Trial + 
                (Side*treatment*Species*Trial|Turtle),
              family = Gamma(link = "log"),
              data = swim_time,
              prior = c(prior(normal(4,2), class = "Intercept"),
                        prior(normal(0,1), class = "b"),
                        prior(exponential(0.1), class = "shape")),
              prior(exponential(1), class = "sd"),
              #sample_prior = "only",
              cores = 4, chains = 1, iter = 1000)

swim_5$data %>% 
  distinct() %>% 
  add_epred_draws(swim_5) %>% 
  ggplot(aes(Trial, .epred, fill=treatment))+
  geom_boxplot()+
  facet_grid(Side~Species)+
  labs(title="Overall Swimming Predictions - Prior Only - normal(4,2) - Varying Slope")