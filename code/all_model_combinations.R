
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
    time = sum(total_seconds)
  ) 


# Models for every variable combination

side_only <- brm(time ~ 1 + Side + (Side|Turtle),
                 family = Gamma(link = "log"),
                 data = swim_time,
                 prior = c(prior(normal(4,2), class = "Intercept"),
                           prior(normal(0,1), class = "b"),
                           prior(exponential(0.1), class = "shape"),
                           prior(exponential(1), class = "sd")),
                 #sample_prior = "only",
                 cores = 4, chains = 1, iter = 1000)

side_only$data %>% 
  distinct() %>% 
  add_epred_draws(side_only) %>% 
  ggplot(aes(Side, .epred))+
  geom_boxplot()+
  labs(title="Side Only")



species_only <- brm(swim_time ~ 1 + Species + (Species|Turtle),
                 family = Gamma(link = "log"),
                 data = swim_time,
                 prior = c(prior(normal(4,2), class = "Intercept"),
                           prior(normal(0,1), class = "b"),
                           prior(exponential(0.1), class = "shape"),
                           prior(exponential(1), class = "sd")),
                 #sample_prior = "only",
                 cores = 4, chains = 1, iter = 1000)

species_only$data %>% 
  distinct() %>% 
  add_epred_draws(species_only) %>% 
  ggplot(aes(Species, .epred))+
  geom_boxplot()+
  labs(title="Species Only")




treatment_only <- brm(swim_time ~ 1 + treatment + (treatment|Turtle),
                 family = Gamma(link = "log"),
                 data = swim_time,
                 prior = c(prior(normal(4,2), class = "Intercept"),
                           prior(normal(0,1), class = "b"),
                           prior(exponential(0.1), class = "shape"),
                           prior(exponential(1), class = "sd")),
                 #sample_prior = "only",
                 cores = 4, chains = 1, iter = 1000)

treatment_only$data %>% 
  distinct() %>% 
  add_epred_draws(treatment_only) %>% 
  ggplot(aes(treatment, .epred))+
  geom_boxplot()+
  labs(title="Treatment Only")




trial_only <- brm(swim_time ~ 1 + Trial + (Trial|Turtle),
                 family = Gamma(link = "log"),
                 data = swim_time,
                 prior = c(prior(normal(4,2), class = "Intercept"),
                           prior(normal(0,1), class = "b"),
                           prior(exponential(0.1), class = "shape"),
                           prior(exponential(1), class = "sd")),
                 #sample_prior = "only",
                 cores = 4, chains = 1, iter = 1000)

trial_only$data %>% 
  distinct() %>% 
  add_epred_draws(trial_only) %>% 
  ggplot(aes(Trial, .epred))+
  geom_boxplot()+
  labs(title="Trial Only")




side_species <- brm(swim_time ~ 1 + Side*Species + (Side*Species|Turtle),
                 family = Gamma(link = "log"),
                 data = swim_time,
                 prior = c(prior(normal(4,2), class = "Intercept"),
                           prior(normal(0,1), class = "b"),
                           prior(exponential(0.1), class = "shape"),
                           prior(exponential(1), class = "sd")),
                 #sample_prior = "only",
                 cores = 4, chains = 1, iter = 1000)

side_species$data %>% 
  distinct() %>% 
  add_epred_draws(side_species) %>% 
  ggplot(aes(Side, .epred, fill = Species))+
  geom_boxplot()+
  labs(title="Side and Species")





side_treatment <- brm(swim_time ~ 1 + Side*treatment + (Side*treatment|Turtle),
                    family = Gamma(link = "log"),
                    data = swim_time,
                    prior = c(prior(normal(4,2), class = "Intercept"),
                              prior(normal(0,1), class = "b"),
                              prior(exponential(0.1), class = "shape"),
                              prior(exponential(1), class = "sd")),
                    #sample_prior = "only",
                    cores = 4, chains = 1, iter = 1000)

side_treatment$data %>% 
  distinct() %>% 
  add_epred_draws(side_treatment) %>% 
  ggplot(aes(Side, .epred, fill = treatment))+
  geom_boxplot()+
  labs(title="Side and Treatment")






side_trial <- brm(swim_time ~ 1 + Side*Trial + (Side*Trial|Turtle),
                    family = Gamma(link = "log"),
                    data = swim_time,
                    prior = c(prior(normal(4,2), class = "Intercept"),
                              prior(normal(0,1), class = "b"),
                              prior(exponential(0.1), class = "shape"),
                              prior(exponential(1), class = "sd")),
                    #sample_prior = "only",
                    cores = 4, chains = 1, iter = 1000)

side_trial$data %>% 
  distinct() %>% 
  add_epred_draws(side_trial) %>% 
  ggplot(aes(Side, .epred, fill = Trial))+
  geom_boxplot()+
  labs(title="Side and Trial")





species_treatment <- brm(swim_time ~ 1 + Species*treatment + (Species*treatment|Turtle),
                    family = Gamma(link = "log"),
                    data = swim_time,
                    prior = c(prior(normal(4,2), class = "Intercept"),
                              prior(normal(0,1), class = "b"),
                              prior(exponential(0.1), class = "shape"),
                              prior(exponential(1), class = "sd")),
                    #sample_prior = "only",
                    cores = 4, chains = 1, iter = 1000)

species_treatment$data %>% 
  distinct() %>% 
  add_epred_draws(species_treatment) %>% 
  ggplot(aes(Species, .epred, fill = treatment))+
  geom_boxplot()+
  labs(title="Species and Treatment")





species_trial <- brm(swim_time ~ 1 + Species*Trial + (Species*Trial|Turtle),
                         family = Gamma(link = "log"),
                         data = swim_time,
                         prior = c(prior(normal(4,2), class = "Intercept"),
                                   prior(normal(0,1), class = "b"),
                                   prior(exponential(0.1), class = "shape"),
                                   prior(exponential(1), class = "sd")),
                         #sample_prior = "only",
                         cores = 4, chains = 1, iter = 1000)

species_trial$data %>% 
  distinct() %>% 
  add_epred_draws(species_trial) %>% 
  ggplot(aes(Species, .epred, fill = Trial))+
  geom_boxplot()+
  labs(title="Species and Trial")





treatment_trial <- brm(swim_time ~ 1 + treatment*Trial + (treatment*Trial|Turtle),
                         family = Gamma(link = "log"),
                         data = swim_time,
                         prior = c(prior(normal(4,2), class = "Intercept"),
                                   prior(normal(0,1), class = "b"),
                                   prior(exponential(0.1), class = "shape"),
                                   prior(exponential(1), class = "sd")),
                         #sample_prior = "only",
                         cores = 4, chains = 1, iter = 1000)

treatment_trial$data %>% 
  distinct() %>% 
  add_epred_draws(treatment_trial) %>% 
  ggplot(aes(treatment, .epred, fill = Trial))+
  geom_boxplot()+
  labs(title="Treatment and Trial")




side_species_treatment <- brm(swim_time ~ 1 + Side*Species*treatment + 
                                (Side*Species*treatment|Turtle),
                    family = Gamma(link = "log"),
                    data = swim_time,
                    prior = c(prior(normal(4,2), class = "Intercept"),
                              prior(normal(0,1), class = "b"),
                              prior(exponential(0.1), class = "shape"),
                              prior(exponential(1), class = "sd")),
                    #sample_prior = "only",
                    cores = 4, chains = 1, iter = 1000)

side_species_treatment$data %>% 
  distinct() %>% 
  add_epred_draws(side_species_treatment) %>% 
  ggplot(aes(Side, .epred, fill = treatment))+
  geom_boxplot()+
  facet_wrap(~Species)+
  labs(title="Side, Species, and Treatment")
  
  



  
side_species_trial <- brm(swim_time ~ 1 + Side*Species*Trial + 
                                (Side*Species*Trial|Turtle),
                              family = Gamma(link = "log"),
                              data = swim_time,
                              prior = c(prior(normal(4,2), class = "Intercept"),
                                        prior(normal(0,1), class = "b"),
                                        prior(exponential(0.1), class = "shape"),
                                        prior(exponential(1), class = "sd")),
                              #sample_prior = "only",
                              cores = 4, chains = 1, iter = 1000)

side_species_trial$data %>% 
  distinct() %>% 
  add_epred_draws(side_species_trial) %>% 
  ggplot(aes(Side, .epred, fill = trial))+
  geom_boxplot()+
  facet_wrap(~Species)+
  labs(title="Side, Species, and Trial")





side_treatment_trial <- brm(swim_time ~ 1 + Side*treatment*Trial + 
                                (Side*treatment*Trial|Turtle),
                              family = Gamma(link = "log"),
                              data = swim_time,
                              prior = c(prior(normal(4,2), class = "Intercept"),
                                        prior(normal(0,1), class = "b"),
                                        prior(exponential(0.1), class = "shape"),
                                        prior(exponential(1), class = "sd")),
                              #sample_prior = "only",
                              cores = 4, chains = 1, iter = 1000)

side_treatment_trial$data %>% 
  distinct() %>% 
  add_epred_draws(side_treatment_trial) %>% 
  ggplot(aes(Side, .epred, fill = treatment))+
  geom_boxplot()+
  facet_wrap(~Trial)+
  labs(title="Side, Treatment, and Trial")







species_treatment_trial <- brm(swim_time ~ 1 + Species*treatment*Trial + 
                              (Species*treatment*Trial|Turtle),
                            family = Gamma(link = "log"),
                            data = swim_time,
                            prior = c(prior(normal(4,2), class = "Intercept"),
                                      prior(normal(0,1), class = "b"),
                                      prior(exponential(0.1), class = "shape"),
                                      prior(exponential(1), class = "sd")),
                            #sample_prior = "only",
                            cores = 4, chains = 1, iter = 1000)

species_treatment_trial$data %>% 
  distinct() %>% 
  add_epred_draws(species_treatment_trial) %>% 
  ggplot(aes(Species, .epred, fill = treatment))+
  geom_boxplot()+
  facet_wrap(~Trial)+
  labs(title="Species, Treatment, and Trial")



side_species_treatment_trial <- brm(swim_time ~ 1 + Side*Species*treatment*Trial + 
                              (Side*Species*treatment*Trial|Turtle),
                            family = Gamma(link = "log"),
                            data = swim_time,
                            prior = c(prior(normal(4,2), class = "Intercept"),
                                      prior(normal(0,1), class = "b"),
                                      prior(exponential(0.1), class = "shape"),
                                      prior(exponential(1), class = "sd")),
                            #sample_prior = "only",
                            cores = 4, chains = 1, iter = 1000)

side_species_treatment_trial$data %>% 
  distinct() %>% 
  add_epred_draws(side_species_treatment_trial) %>% 
  ggplot(aes(Trial, .epred, fill = treatment))+
  geom_boxplot()+
  facet_grid(Side~Species)+
  labs(title="Side, Species, Treatment, and Trial")