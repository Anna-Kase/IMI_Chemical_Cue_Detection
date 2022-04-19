

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


# Simplified Models

side_only <- brm(swim_time ~ 1 + Side + (Side|Turtle),
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





side_treatment_species <- brm(swim_time ~ 1 + Side*treatment*Species + 
                                (Side*treatment*Species|Turtle),
                      family = Gamma(link = "log"),
                      data = swim_time,
                      prior = c(prior(normal(4,2), class = "Intercept"),
                                prior(normal(0,1), class = "b"),
                                prior(exponential(0.1), class = "shape"),
                                prior(exponential(1), class = "sd")),
                      #sample_prior = "only",
                      cores = 4, chains = 1, iter = 1000)


side_treatment_species$data %>% 
  distinct() %>% 
  add_epred_draws(side_treatment_species) %>% 
  ggplot(aes(Side, .epred, fill = treatment))+
  geom_boxplot()+
  facet_wrap(~Species)+
  labs(title="Side, Treatment, and Species")






side_treatment_species_trial <- brm(swim_time ~ 1 + Side*treatment*Species*Trial + 
                                (Side*treatment*Species*Trial|Turtle),
                              family = Gamma(link = "log"),
                              data = swim_time,
                              prior = c(prior(normal(4,2), class = "Intercept"),
                                        prior(normal(0,1), class = "b"),
                                        prior(exponential(0.1), class = "shape"),
                                        prior(exponential(1), class = "sd")),
                              #sample_prior = "only",
                              cores = 4, chains = 1, iter = 1000)

# This model is where it all breaks down... It comes up with:
# SAMPLING FOR MODEL 'b21379f5ab2f9b5fa89528670237476f' NOW (CHAIN 1).
# Chain 1: Rejecting initial value:
#   Chain 1:   Log probability evaluates to log(0), i.e. negative infinity.
# Chain 1:   Stan can't start sampling from this initial value.



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


# Same error as the model above... Is it a problem with the Trial variable? 



trial_only <- brm(swim_time ~ 1 + Trial + 
                              (Trial|Turtle),
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

# Ok, so it wasn't the trial variable itself... Let's try adding in the other variables
# one by one now and see where it breaks down

trial_species <- brm(swim_time ~ 1 + Trial*Species + 
                    (Trial*Species|Turtle),
                  family = Gamma(link = "log"),
                  data = swim_time,
                  prior = c(prior(normal(4,2), class = "Intercept"),
                            prior(normal(0,1), class = "b"),
                            prior(exponential(0.1), class = "shape"),
                            prior(exponential(1), class = "sd")),
                  #sample_prior = "only",
                  cores = 4, chains = 1, iter = 1000)

trial_species$data %>% 
  distinct() %>% 
  add_epred_draws(trial_species) %>% 
  ggplot(aes(Trial, .epred, fill = Species))+
  geom_boxplot()+
  labs(title="Trial and Species")

# Ok, so that is actually kind of a cool plot and worth the model


trial_species_treatment <- brm(swim_time ~ 1 + Trial*Species*treatment + 
                       (Trial*Species*treatment|Turtle),
                     family = Gamma(link = "log"),
                     data = swim_time,
                     prior = c(prior(normal(4,2), class = "Intercept"),
                               prior(normal(0,1), class = "b"),
                               prior(exponential(0.1), class = "shape"),
                               prior(exponential(1), class = "sd")),
                     #sample_prior = "only",
                     cores = 4, chains = 1, iter = 1000)

# There was a huge error message here... Error was copy and pasted into a different script

# So, let's take species back out and try this with trial and treatment only

trial_treatment <- brm(swim_time ~ 1 + Trial*treatment + 
                                 (Trial*treatment|Turtle),
                               family = Gamma(link = "log"),
                               data = swim_time,
                               prior = c(prior(normal(4,2), class = "Intercept"),
                                         prior(normal(0,1), class = "b"),
                                         prior(exponential(0.1), class = "shape"),
                                         prior(exponential(1), class = "sd")),
                               #sample_prior = "only",
                               cores = 4, chains = 1, iter = 1000)

trial_treatment$data %>% 
  distinct() %>% 
  add_epred_draws(trial_treatment) %>% 
  ggplot(aes(Trial, .epred, fill = treatment))+
  geom_boxplot()+
  labs(title="Trial and Treatment")

# That one also works decently... does the order of the variables actually matter?

trial_treatment_species <- brm(swim_time ~ 1 + Trial*treatment*Species + 
                         (Trial*treatment*Species|Turtle),
                       family = Gamma(link = "log"),
                       data = swim_time,
                       prior = c(prior(normal(4,2), class = "Intercept"),
                                 prior(normal(0,1), class = "b"),
                                 prior(exponential(0.1), class = "shape"),
                                 prior(exponential(1), class = "sd")),
                       #sample_prior = "only",
                       cores = 4, chains = 1, iter = 1000)

# This one came up with that big error message again... So let's try isolating treatment
# and species now

treatment_species <- brm(swim_time ~ 1 + treatment*Species + 
                           (treatment*Species|Turtle),
                         family = Gamma(link = "log"),
                         data = swim_time,
                         prior = c(prior(normal(4,2), class = "Intercept"),
                                   prior(normal(0,1), class = "b"),
                                   prior(exponential(0.1), class = "shape"),
                                   prior(exponential(1), class = "sd")),
                         #sample_prior = "only",
                         cores = 4, chains = 1, iter = 1000)

treatment_species$data %>% 
  distinct() %>% 
  add_epred_draws(treatment_species) %>% 
  ggplot(aes(treatment, .epred, fill = Species))+
  geom_boxplot()+
  labs(title="Treatment and Species")

# This one worked, but it had some non-convergent chains, however the output still looks decent


# So the trial, treatment, and species models have not worked... nor has side, treatment,
# and trial
