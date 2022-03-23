
library(tidyverse)

ccde <- read_csv("~/Cue_Detection/data/master_data.csv")

#subgrouping big data set into total time turtles spent on an arena side regardless
#of what behavior they were doing

sub_side <- ccde  %>% 
  group_by(Turtle, Trial, Treatment, Experimental_Group, Side) %>% 
  summarise(
    Tot_Side_Time = sum(total_seconds)
  ) %>% 
  unite(col="Side_Time", Side, sep="_") %>% 
  spread(Side_Time, Tot_Side_Time)

sub_side[is.na(sub_side)]<-0

#changing control to say 0 so I can work with it in the prior simulation easier
sub_side$Treatment <- ifelse(sub_side$Treatment == "Control", "0", ifelse(sub_side$Treatment == "0.1", "2", ifelse(sub_side$Treatment == "1", "1",ifelse(sub_side$Treatment == "10", "10", "TURTLES"))))
# also changing the treatment column to be double format instead of
#character, again so I can work with it in the prior simulation
sub_side$Treatment <- as.double(sub_side$Treatment)

# gamma anova 

# y ~ dgamma(scale, shape)
# scale = mu/shape
# log(mu)  = a + b01*x01 + b1*x1 + b10*x10
# a ~ ?
# b ~ ?
# shape ~ ?

N = 100  # number of simulations

# simulate priors
priors <- tibble(a = rnorm(N, 5, 4),
                 b2 = rnorm(N, 0, 1),
                 b1 = rnorm(N, 0, 1),
                 b10 = rnorm(N, 0, 1),
                 shape = rexp(N, 0.1),
                 sim = 1:N)

# simulate
prior_and_x <- priors %>%  
  mutate(mu_0 = exp(a + b2*0 + b1*0 + b10*0),                                 # simulate means
         mu_2 = exp(a + b2*1 + b1*0 + b10*0),
         mu_1 = exp(a + b2*0 + b1*1 + b10*0),
         mu_10 = exp(a + b2*0 + b1*0 + b10*1)) %>%
  pivot_longer(cols = c(mu_0, mu_2, mu_1, mu_10), values_to = "mu") %>% 
  separate(name, c("measure", "x")) %>% 
  mutate(x = as.numeric(x)) %>% 
  right_join(sub_side %>% select(Treatment) %>% rename(x = Treatment)) %>%             # repeat means for each row of gear in original data
  mutate(y = rgamma(nrow(.), scale = mu/shape, shape = shape))        # simulate data (e.g., y_rep)


prior_and_x %>% 
  ggplot(aes(x = x, y =mu)) + 
  geom_point()+
  scale_y_log10()


# make treatment a factor
ccde_brm <- sub_side %>% mutate(Treatment = as.factor(Treatment)) 

#add a super small amount to each data value in the stimulus column
#because the gamma distribution needs values > 0
ccde_brm$Stimulus <- ccde_brm$Stimulus + 0.000001



anova_gamma_brm_ccde <- brm(Stimulus ~ Treatment,
                       family = Gamma(link = "log"),
                       data = ccde_brm,
                       prior = c(prior(normal(5, 4), class = "Intercept"),
                                 prior(normal(0, 1), class = "b"),
                                 prior(exponential(0.1), class = "shape")),
                       cores = 4, chains = 1, iter = 1000)


#playing with the posterior
anova_gamma_post_ccde <- posterior_samples(anova_gamma_brm_ccde)


derived <- anova_gamma_post_ccde %>% mutate(sec_0 = exp(b_Intercept),
                                       sec_0_1 = exp(b_Intercept+b_Treatment0),
                                       sec_1 = exp(b_Intercept+b_Treatment1),
                                       sec_10 = exp(b_Intercept+b_Treatment10)) %>% 
  select(sec_0, sec_0_1, sec_1, sec_10)



derived %>% mutate(diff_0_01 = sec_0 - sec_0_1,
                   diff_0_1 = sec_0 - sec_1,
                   diff_0_10 = sec_0 - sec_10,
                   diff_01_1 = sec_0_1 - sec_1,
                   diff_01_10 = sec_0_1 - sec_10,
                   diff_1_10 = sec_1 - sec_10)



#what is the probability that the control group spent more time on the 
#stimulus side than the 0.1 group

derived %>% mutate(diff_0_01 = sec_0 - sec_0_1,
                   diff_0_1 = sec_0 - sec_1,
                   diff_0_10 = sec_0 - sec_10,
                   diff_01_1 = sec_0_1 - sec_1,
                   diff_01_10 = sec_0_1 - sec_10,
                   diff_1_10 = sec_1 - sec_10) %>% 
  summarize(prob_0_01 = sum(diff_0_01>0)/nrow(.))

# 30.8% prob

#what is the probability that the control group spent more time on the 
#stimulus side than the 1 group

derived %>% mutate(diff_0_01 = sec_0 - sec_0_1,
                   diff_0_1 = sec_0 - sec_1,
                   diff_0_10 = sec_0 - sec_10,
                   diff_01_1 = sec_0_1 - sec_1,
                   diff_01_10 = sec_0_1 - sec_10,
                   diff_1_10 = sec_1 - sec_10) %>% 
  summarize(prob_0_1 = sum(diff_0_1>0)/nrow(.))

#33.2% prob


#what is the probability that the control group spent more time on the 
#stimulus side than the 10 group

derived %>% mutate(diff_0_01 = sec_0 - sec_0_1,
                   diff_0_1 = sec_0 - sec_1,
                   diff_0_10 = sec_0 - sec_10,
                   diff_01_1 = sec_0_1 - sec_1,
                   diff_01_10 = sec_0_1 - sec_10,
                   diff_1_10 = sec_1 - sec_10) %>% 
  summarize(prob_0_10 = sum(diff_0_10>0)/nrow(.))

#41.0%


#plots

conditional_effects(anova_gamma_brm_ccde)

derived %>% 
  pivot_longer(cols=everything(), names_to = "Treatment",
               values_to = "seconds") %>% 
  group_by(Treatment) %>%
  ggplot(aes(x=Treatment, y=seconds, fill=Treatment))+
  geom_violin()+
  geom_jitter(data=sub_side, aes(x=Treatment, y=Stimulus))



sub_side %>% 
  ggplot(aes(x=Treatment, y=Stimulus))+
  geom_bar(stat="identity")


plot_raw_data <- sub_side %>% 
  mutate(Treatmentx = Treatment, 
         secondsx = Stimulus) %>% 
  mutate(Treatmentx = case_when(Treatmentx == "0" ~ "sec_0",
                                Treatmentx == "2" ~ "sec_0_1",
                                Treatmentx == "1" ~ "sec_1",
                                TRUE ~ "sec_10")) %>%
  ungroup() %>% 
  select(Treatmentx, secondsx)


plot_derived <- derived %>% 
  pivot_longer(cols=everything(), names_to = "Treatment",
               values_to = "seconds") %>% 
  group_by(Treatment)


ggplot()+
  geom_violin(data=plot_derived, aes(x=Treatment, y=seconds, fill=Treatment))+
  geom_jitter(data=plot_raw_data, aes(x=Treatmentx, y=secondsx))+
  scale_fill_manual(name = "Treatment", labels = c("0 ug/L", "0.1 ug/L", "1 ug/L", "10 ug/L"), values = c("turquoise3", "chocolate1", "green","blue"))+
  scale_x_discrete(labels=c("sec_0" = "0 ug/L", "sec_0_1" = "0.1 ug/L", "sec_1" = "1 ug/L", "sec_10" = "10 ug/L"))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x="Imidacloprid Treatment Group", y="Seconds Spent on \n Stimulus Side of Arena")
 



