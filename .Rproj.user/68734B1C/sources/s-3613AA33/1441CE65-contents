



# This works on its own with no varying intercept nor varying slope
side_species_treatment_trial <- brm(time ~ 1 + Side*Species*treatment*Trial,
                                    family = Gamma(link="log"),
                                    data = swim_time,
                                    prior = c(prior(normal(4,2), class = "Intercept"),
                                              prior(normal(0,1), class = "b"),
                                              prior(exponential(0.1), class = "shape")),
                                    #sample_prior = "only",
                                    cores = 4, chains = 4, iter = 1000)

side_species_treatment_trial$data %>% 
  distinct() %>% 
  add_epred_draws(side_species_treatment_trial) %>% 
  ggplot(aes(Trial, .epred, fill = treatment))+
  geom_boxplot()+
  facet_grid(Side~Species)+
  labs(title="Side, Species, Treatment, and Trial")


# This one works with just a varying intercept, needs some work on the priors, but it works
sstt_varI <- brm(time ~ 1 + Side*Species*treatment*Trial +
                                      (1 | Turtle),
                                    family = Gamma(link="log"),
                                    data = swim_time,
                                    prior = c(prior(normal(4,2), class = "Intercept"),
                                              prior(normal(0,1), class = "b"),
                                              prior(exponential(0.1), class = "shape"),
                                              prior(cauchy(0, 1), class = "sd")),
                                    #sample_prior = "only",
                                    cores = 4, chains = 4, iter = 1000)

sstt_varI$data %>% 
  distinct() %>% 
  add_epred_draws(sstt_varI) %>% 
  ggplot(aes(Trial, .epred, fill = treatment))+
  geom_boxplot()+
  facet_grid(Side~Species)+
  labs(title="Side, Species, Treatment, and Trial")



#This one sort of worked with a varying intercept and a varying slope, but they had to be
#specified as orthogonal/statistically independent of one another and I'm not sure if that
#is correct
sstt_var <- brm(time ~ 1 + Side*Species*treatment*Trial +
                   (1 + Side*Species*treatment*Trial || Turtle),
                 family = Gamma(link="log"),
                 data = swim_time,
                 prior = c(prior(normal(4,2), class = "Intercept"),
                           prior(normal(0,1), class = "b"),
                           prior(exponential(0.1), class = "shape"),
                           prior(cauchy(0, 1), class = "sd")),
                 #sample_prior = "only",
                 cores = 4, chains = 1, iter = 1000)

sstt_var$data %>% 
  distinct() %>% 
  add_epred_draws(sstt_var) %>% 
  ggplot(aes(Trial, .epred, fill = treatment))+
  geom_boxplot()+
  facet_grid(Side~Species)+
  labs(title="Side, Species, Treatment, and Trial")


sstt_var$data %>% 
  distinct() %>% 
  add_epred_draws(sstt_var) %>% 
  ggplot(aes(.epred, fill = treatment, alpha=0.5))+
  geom_density()+
  xlim(0,300)


sstt_var$data %>% 
  distinct() %>% 
  add_epred_draws(sstt_var) %>% 
  ggplot(aes(Trial, .epred, fill = treatment))+
  geom_boxplot()+
  facet_wrap(~Turtle)+
  ylim(0,500)
