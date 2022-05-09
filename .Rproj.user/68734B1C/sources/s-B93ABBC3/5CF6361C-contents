
# Welcome to the Imidacloprid Chemical Cue Detection Experiment Analysis!

# First things first, load in your packages
library(tidyverse)
library(janitor)
library(brms)
library(tidybayes)

# Now some notes about how to handle the data. When I collected the behavior data from the
# trial videos I designated one sheet of an Excel workbook to each turtle for each trial.
# The resulting Excel workbook then had 128 individual sheets of data plus 1 "master sheet"
# where I kept track of video file names, start and stop times, etc. This may seem like a 
# nightmare except I found a bit of code to enter into the VBA of Excel to get each of my
# sheets to save as individual .csv files.


# Instructions for saving data .xlsx file into multiple .csv files:

# 1. Press Alt + F11 keys simultaneously to open the Microsoft Visual Basic Application window.
# 
# 2. In the Microsoft Visual Basic Application window, click Insert > Module. Then copy and paste the following code into the Module window.
# 
# VBA code: Export all sheets to separated csv files:
#   
#   Sub ExportSheetsToCSV()
# Dim xWs As Worksheet
# Dim xcsvFile As String
# For Each xWs In Application.ActiveWorkbook.Worksheets
# xWs.Copy
# xcsvFile = CurDir & "\" & xWs.Name & ".csv"
#         Application.ActiveWorkbook.SaveAs Filename: = xcsvFile, _
#         FileFormat: = xlCSV, CreateBackup: = False
#         Application.ActiveWorkbook.Saved = True
#         Application.ActiveWorkbook.Close
#     Next
# End Sub


# Now that all of the Excel sheets are saved into individual .csv files, they are slightly
# (only slightly) easier to work with in R!

# ----------------------------------------------------------------------------------------

# Let's start with the Painted Turtle data

# Read in multiple .csv files
painted_files <- list.files(path = "~/GitHub/IMI_Chemical_Cue_Detection/data/painted_turtles", pattern = "*.csv", full.names = T)
painted_tbl <- sapply(painted_files, read_csv, simplify=FALSE) %>% 
  bind_rows(.id = "id")

View(painted_tbl)


# Removing the X9, X10, and "Just trying to make a folder to be able to downlaod all of my 
# crazy datasets into" columns becasue I don't need them
painted_tbl$X9 <- NULL
painted_tbl$X10 <- NULL
painted_tbl$`Just trying to make a folder to be able to download all of my crazy datasets into.` <- NULL

View(painted_tbl)


# Remove rows of NAs
painted_tbl <- na.omit(painted_tbl)


# Now let's add a trial column based on the end of the id file name
painted_tbl$Trial <- ifelse(endsWith(painted_tbl$id, "T2.csv"), "Trial_2", 
                    (ifelse(endsWith(painted_tbl$id, "T1.csv"), "Trial_1", 
                            (ifelse(endsWith(painted_tbl$id, "PT.csv"), "Prelim_Trial", 
                                    (ifelse(endsWith(painted_tbl$id, "T3.csv"), "Trial_3", "TURTLES")))))))


# Now let's add a turtle ID number column based on the file name
numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 

painted_tbl$Turtle <- numextract(painted_tbl$id)


# Now let's create a total seconds of behavior column with unit conversion to seconds (Thanks Jeff!)
painted_tbl$total_seconds <- as.numeric((painted_tbl$End_Time - painted_tbl$Start_Time)/60)


# Now let's add a column designating which IMI treatment group each turtle was in as well
# as a column designating which experimental group each turtle was in since we had to run
# groups of turtles on different days because running 32 turtles in one day was not feasible
painted_tbl <- painted_tbl %>% 
  mutate(treatment = case_when(Turtle %in% c(86,91,83,93,80,76,58,75) ~ "0",
                               Turtle %in% c(90,88,84,94,92,62,73,54) ~ "0.1",
                               Turtle %in% c(96,87,89,82,55,68,65,2000) ~ "1",
                               TRUE ~ "10")) %>% 
  mutate(experiment_group = case_when(Turtle == 74 ~ "1",
                                      Turtle %in% c(59,58,62,55,73,64,68,65,54,75) ~ "2",
                                      Turtle %in% c(96,90,88,87,86,89,91,83,84,78,85) ~ "3",
                                      TRUE ~ "4")) 


# I made a mistake in one of the spellings of "Control" - time to fix that here
painted_tbl$Side <- ifelse(painted_tbl$Side == "Stimulus", "Stimulus", "Control")


# Export formatted Master Data Frame as a .csv
write.csv(painted_tbl, "~/GitHub/IMI_Chemical_Cue_Detection/data/master_painted_data.csv", row.names=FALSE)

# ------------------------------------------------------------------------------------------

# Now this entire process can be repeated with the False Map Turtle Data!

# Read in multiple .csv files
fmt_files <- list.files(path = "~/GitHub/IMI_Chemical_Cue_Detection/data/false_map_turtles", pattern = "*.csv", full.names = T)
fmt_tbl <- sapply(fmt_files, read_csv, simplify=FALSE) %>% 
  bind_rows(.id = "id")

View(fmt_tbl)


# Removing the X9 and X10 columns becasue I don't need them
fmt_tbl$X9 <- NULL
fmt_tbl$X10 <- NULL

View(fmt_tbl)


# Remove rows of NAs
fmt_tbl <- na.omit(fmt_tbl)


# Now let's add a trial column based on the end of the id file name
fmt_tbl$Trial <- ifelse(endsWith(fmt_tbl$id, "T2.csv"), "Trial_2", 
                            (ifelse(endsWith(fmt_tbl$id, "T1.csv"), "Trial_1", 
                                    (ifelse(endsWith(fmt_tbl$id, "PT.csv"), "Prelim_Trial", 
                                            (ifelse(endsWith(fmt_tbl$id, "T3.csv"), "Trial_3", "TURTLES")))))))


# Now let's add a turtle ID number column based on the file name
numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 

fmt_tbl$Turtle <- numextract(fmt_tbl$id)

View(fmt_tbl)


# Now let's create a total seconds of behavior column with unit conversion to seconds (Thanks Jeff!)
fmt_tbl$total_seconds <- as.numeric((fmt_tbl$End_Time - fmt_tbl$Start_Time)/60)


# Now let's add a column designating which IMI treatment group each turtle was in as well
# as a column designating which experimental group each turtle was in since we had to run
# groups of turtles on different days because running 32 turtles in one day was not feasible
fmt_tbl <- fmt_tbl %>% 
  mutate(treatment = case_when(Turtle %in% c(109,124,113,123,120,115,3000,119) ~ "0",
                               Turtle %in% c(130,121,122,132,128,108,129,107) ~ "0.1",
                               Turtle %in% c(114,126,131,116,117,136,111,125) ~ "1",
                               TRUE ~ "10")) %>% 
  mutate(experiment_group = case_when(Turtle %in% c(130,114,121,126,122,127,131,109,135,124,113) ~ "1",
                                      Turtle %in% c(155,132,128,123,108,116,120,146,134,117,136) ~ "2",
                                      TRUE ~ "3")) 

View(fmt_tbl)

# I made a mistake in one of the spellings of "Stimulus" - time to fix that here
fmt_tbl$Side <- ifelse(fmt_tbl$Side == "Center", "Center", 
                       ifelse(fmt_tbl$Side == "Control", "Control", "Stimulus"))


# Export formatted Master Data Frame as a .csv
write.csv(fmt_tbl, "~/GitHub/IMI_Chemical_Cue_Detection/data/master_fmt_data.csv", row.names=FALSE)

# ------------------------------------------------------------------------------------------

# Now that all of the data is formatted into two workable dataframes, let's add a species
# column to each of them and then bind them together so we can make some plots and models

painted_tbl <- painted_tbl %>% 
  mutate(Species = "painted")

fmt_tbl <- fmt_tbl %>%
  mutate(Species = "fmt")

all_tbl <- rbind(painted_tbl, fmt_tbl)

View(all_tbl)

# Might as well write this dataframe into a master .csv just to be safe
write.csv(all_tbl, "~/GitHub/IMI_Chemical_Cue_Detection/data/master_all_turtle_data.csv", row.names=FALSE)


# ------------------------------------------------------------------------------------------

# Welcome to the preliminary plots section so we can see what we are working with here


# Plot of time spent swimming on the stimulus side of the arena throughout the trials
ss <- all_tbl %>% 
  filter(Side == "Stimulus") %>% 
  filter(Behavior == "Swimming")

ss %>% 
  group_by(Turtle, Trial, treatment, experiment_group, Species) %>% 
  summarise(
    Tot_SS_Time = sum(total_seconds)
  ) %>% 
  ggplot(aes(Trial, Tot_SS_Time, group=Turtle, color=Species))+
  geom_line()+
  geom_point()+
  facet_wrap(~Turtle)

# This looks so bad... Maybe a bar plot
ss %>% 
  group_by(Turtle, Trial, treatment, experiment_group, Species) %>% 
  summarise(
    Tot_SS_Time = sum(total_seconds)
  ) %>% 
  ggplot(aes(Trial, Tot_SS_Time, fill=Species))+
  geom_bar(stat="identity", position=position_dodge())


# Instead of Trial on the x let's try treatment on the x
ss %>% 
  group_by(Turtle, Trial, treatment, experiment_group, Species) %>% 
  summarise(
    Tot_SS_Time = sum(total_seconds)
  ) %>% 
  ggplot(aes(treatment, Tot_SS_Time, fill=Species))+
  geom_bar(stat="identity", position=position_dodge())+
  facet_wrap(~Trial)


# I am quickly losing faith that this experiment worked

# -----------------------------------------------------------------------------------------

# Welcome to the Model Fitting section - Let's see what happens here

# -----------------------------------------------------------------------------------------

# Model 1 - Time on an Arena Side ~ Treatment*Species*Trial


# Subgrouping big data set into total time turtles spent on an arena side regardless
# of what behavior they were doing

sub_side <- all_tbl  %>% 
  group_by(Turtle, Trial, treatment, Species, experiment_group, Side) %>% 
  summarise(
    Tot_Side_Time = sum(total_seconds)
  ) %>% 
  unite(col="Side_Time", Side, sep="_") %>% 
  spread(Side_Time, Tot_Side_Time)

sub_side[is.na(sub_side)]<-0


# Add a super small amount to each data value in the stimulus column
# because the gamma distribution needs values > 0 ---- I will go back and run a 
# zero-inflated gamma for the final stats
sub_side$Stimulus <- sub_side$Stimulus + 0.000001

# Model time spent on stimulus side as a function of treatment group, species, and trial 
# with gamma distribution, no intercept, and an upper bound at 300 to account for
# trial length - by doing this we don't have to make this proportion data with a beta distribution
# and we can keep everything as seconds bound within the length of the trial
side <- brm(Stimulus ~ 0 + treatment*Species*Trial,
             family = Gamma(link = "log"),
             data = sub_side,
             prior = c(prior(normal(2,2), ub = 5.703782, class = "b"),
                       prior(exponential(0.1), class = "shape")),
             #sample_prior = "only",
             cores = 4, chains = 1, iter = 1000)

side

conditional_effects(side)

plot(conditional_effects(side), points=T)




# Model 2 - Behavior ~ Treatment*Species*Trial


# Subgrouping big data set into total time turtles spent doing a behavior regardless
# of what side of the arena they were on

sub_beh <- all_tbl  %>% 
  group_by(Turtle, Trial, treatment, Species, experiment_group, Behavior) %>% 
  summarise(
    Tot_Beh_Time = sum(total_seconds)
  ) %>% 
  unite(col="Behavior_Time", Behavior, sep="_") %>% 
  spread(Behavior_Time, Tot_Beh_Time)

sub_beh[is.na(sub_beh)]<-0


sub_beh$Swimming <- sub_beh$Swimming + 0.000001


swim <- brm(Swimming ~ 0 + treatment*Species*Trial,
            family = Gamma(link = "log"),
            data = sub_beh,
            prior = c(prior(normal(2,2), ub = 5.703782, class = "b"),
                      prior(exponential(0.1), class = "shape")),
            #sample_prior = "only",
            cores = 4, chains = 1, iter = 1000)

swim

conditional_effects(swim)

plot(conditional_effects(swim), points=T)



# Model 3 - Behavior+Side ~ Treatment*Species*Trial

sub_side_beh <- all_tbl  %>% 
  group_by(Turtle, Trial, treatment, Species, experiment_group, Behavior, Side) %>% 
  summarise(
    Tot_Side_Time = sum(total_seconds)
  ) %>% 
  unite(col="Side_Time", Side, sep="_") %>%
  unite(col="total", Behavior, Side_Time, sep="_") %>% 
  spread(total, Tot_Side_Time)

sub_side_beh[is.na(sub_side_beh)]<-0



sub_side_beh$Swimming_Stimulus <- sub_side_beh$Swimming_Stimulus + 0.000001


swim_stim <- brm(Swimming_Stimulus ~ 0 + treatment*Species*Trial,
            family = Gamma(link = "log"),
            data = sub_side_beh,
            prior = c(prior(normal(2,2), ub = 5.703782, class = "b"),
                      prior(exponential(0.1), class = "shape")),
            #sample_prior = "only",
            cores = 4, chains = 1, iter = 1000)

conditional_effects(swim_stim)




# Play with the posterior

post_ss <- swim_stim$data %>% 
  distinct(treatment, Species, Trial) %>% 
  add_epred_draws(swim_stim, re_formula = NA)

post_ss$.row=NULL

expand_post_ss <- post_ss %>% 
  select(-.chain, -.iteration) %>%
  distinct() %>% 
  pivot_wider(names_from = "Species", values_from = ".epred") %>% 
  pivot_wider(id_cols=c(treatment, .draw),
              names_from=Trial, 
              values_from=c("fmt", "painted")) %>% 
  pivot_wider(id_cols=c(.draw),
              names_from=treatment,
              values_from=c("fmt_Prelim_Trial", "fmt_Trial_1", "fmt_Trial_2", 
                            "fmt_Trial_3", "painted_Prelim_Trial", "painted_Trial_1",
                            "painted_Trial_2", "painted_Trial_3"))


#Calculating the difference between fmt and painted by treatment in only PT
species_trt_pt <- expand_post_ss %>% 
  mutate(pt_0 = fmt_Prelim_Trial_0 - painted_Prelim_Trial_0) %>% 
  mutate(pt_01 = fmt_Prelim_Trial_0.1 - painted_Prelim_Trial_0.1) %>% 
  mutate(pt_1 = fmt_Prelim_Trial_1 - painted_Prelim_Trial_1) %>% 
  mutate(pt_10 = fmt_Prelim_Trial_10 - painted_Prelim_Trial_10)

species_trt_pt %>% 
  summarize(higher=sum(pt_0>0)/nrow(.))
# 23.8% probability that the False Map Turtles spent more time swimming on the stimulus side
# of the arena than Painted Turtles in the Control Group during the preliminary trial

species_trt_pt %>% 
  summarize(higher=sum(pt_01>0)/nrow(.))
# 39% probability that the False Map Turtles spent more time swimming on the stimulus side
# of the arena than Painted Turtles in the 0.1 Group during preliminary trial

species_trt_pt %>% 
  summarize(higher=sum(pt_1>0)/nrow(.))
# 12% probability that the False Map Turtles spent more time swimming on the stimulus side
# of the arena than Painted Turtles in the 1 Group during preliminary trial

species_trt_pt %>% 
  summarize(higher=sum(pt_10>0)/nrow(.))
# 29% probability that the False Map Turtles spent more time swimming on the stimulus side
# of the arena than Painted Turtles in the 10 Group during preliminary trial



#Calculating the difference between fmt and painted by treatment in only T1
species_trt_t1 <- expand_post_ss %>% 
  mutate(t1_0 = fmt_Trial_1_0 - painted_Trial_1_0) %>% 
  mutate(t1_01 = fmt_Trial_1_0.1 - painted_Trial_1_0.1) %>% 
  mutate(t1_1 = fmt_Trial_1_1 - painted_Trial_1_1) %>% 
  mutate(t1_10 = fmt_Trial_1_10 - painted_Trial_1_10)

species_trt_t1 %>% 
  summarize(higher=sum(t1_0>0)/nrow(.))
# 44% probability that the False Map Turtles spent more time swimming on the stimulus side
# of the arena than Painted Turtles in the Control Group during Trial 1

species_trt_t1 %>% 
  summarize(higer=sum(t1_01>0)/nrow(.))
# 23% probability that the False Map Turtles spent more time swimming on the stimulus side
# of the arena than Painted Turtles in the 0.1 Group during Trial 1

species_trt_t1 %>% 
  summarize(higer=sum(t1_1>0)/nrow(.))
# 13.6% probability that the False Map Turtles spent more time swimming on the stimulus side
# of the arena than Painted Turtles in the 1 Group during Trial 1

species_trt_t1 %>% 
  summarize(higer=sum(t1_10>0)/nrow(.))
# 50% probability that the False Map Turtles spent more time swimming on the stimulus side
# of the arena than Painted Turtles in the 10 Group during Trial 1


#Calculating the difference between fmt and painted by treatment in only T2
species_trt_t2 <- expand_post_ss %>% 
  mutate(t2_0 = fmt_Trial_2_0 - painted_Trial_2_0) %>% 
  mutate(t2_01 = fmt_Trial_2_0.1 - painted_Trial_2_0.1) %>% 
  mutate(t2_1 = fmt_Trial_2_1 - painted_Trial_2_1) %>% 
  mutate(t2_10 = fmt_Trial_2_10 - painted_Trial_2_10)

species_trt_t2 %>% 
  summarize(higher=sum(t2_0>0)/nrow(.))
# 43% probability that the False Map Turtles spent more time swimming on the stimulus side
# of the arena than Painted Turtles in the Control Group during Trial 2

species_trt_t2 %>% 
  summarize(higher=sum(t2_01>0)/nrow(.))
# 32.4% probability that the False Map Turtles spent more time swimming on the stimulus side
# of the arena than Painted Turtles in the 0.1 Group during Trial 2

species_trt_t2 %>% 
  summarize(higher=sum(t2_1>0)/nrow(.))
# 32.2% probability that the False Map Turtles spent more time swimming on the stimulus side
# of the arena than Painted Turtles in the 1 Group during Trial 2

species_trt_t2 %>% 
  summarize(higher=sum(t2_10>0)/nrow(.))
# 17.6% probability that the False Map Turtles spent more time swimming on the stimulus side
# of the arena than Painted Turtles in the 10 Group during Trial 2



#Calculating the difference between fmt and painted by treatment in only T3
species_trt_t3 <- expand_post_ss %>% 
  mutate(t3_0 = fmt_Trial_3_0 - painted_Trial_3_0) %>% 
  mutate(t3_01 = fmt_Trial_3_0.1 - painted_Trial_3_0.1) %>% 
  mutate(t3_1 = fmt_Trial_3_1 - painted_Trial_3_1) %>% 
  mutate(t3_10 = fmt_Trial_3_10 - painted_Trial_3_10)

species_trt_t3 %>% 
  summarize(higher=sum(t3_0>0)/nrow(.))
# 45% probability that the False Map Turtles spent more time swimming on the stimulus side
# of the arena than Painted Turtles in the Control Group during Trial 3

species_trt_t3 %>% 
  summarize(higher=sum(t3_01>0)/nrow(.))
# 37% probability that the False Map Turtles spent more time swimming on the stimulus side
# of the arena than Painted Turtles in the 0.1 Group during Trial 3

species_trt_t3 %>% 
  summarize(higher=sum(t3_1>0)/nrow(.))
# 11.8% probability that the False Map Turtles spent more time swimming on the stimulus side
# of the arena than Painted Turtles in the 1 Group during Trial 3

species_trt_t3 %>% 
  summarize(higher=sum(t3_10>0)/nrow(.))
# 27.6% probability that the False Map Turtles spent more time swimming on the stimulus side
# of the arena than Painted Turtles in the 10 Group during Trial 3





# Just species comparison regardless of treatments and trials
post_species <- post_ss %>% 
  select(-.chain, -.iteration) %>%
  distinct() %>% 
  pivot_wider(names_from = "Species", values_from = ".epred")

#Calculating the overall difference between fmt and painteds 
post_species %>% 
  mutate(fmt_paint = fmt - painted) %>% 
  ungroup() %>% 
  summarize(higher=sum(fmt_paint>0)/nrow(.))

# 30.1% probability that the False Map Turtles spent more time swimming on the stimulus side
# of the arena than Painted Turtles overall

median(post_species$fmt)
median(post_species$painted)

mean(post_species$fmt)
mean(post_species$painted)

post_species %>% 
  ungroup() %>% 
  group_by(treatment) %>% 
  summarize(
    fmt_mean = mean(fmt),
    paint_mean = mean(painted)
  )


# (new-old)/old  * 100

# Within species comparisons

trt_post_ss <- post_ss %>% 
  select(-.chain, -.iteration) %>%
  distinct() %>% 
  pivot_wider(names_from = "Species", values_from = ".epred") %>% 
  pivot_wider(id_cols=c(Trial, .draw),
              names_from=treatment, 
              values_from=c("fmt", "painted")) 


#Calculating the difference between fmt treatments regardless of trial
fmt_trts <- trt_post_ss %>% 
  ungroup() %>% 
  mutate(control_low = fmt_0 - fmt_0.1) %>% 
  mutate(control_med = fmt_0 - fmt_1) %>% 
  mutate(control_high = fmt_0 - fmt_10) %>% 
  mutate(low_med = fmt_0.1 - fmt_1) %>% 
  mutate(low_high = fmt_0.1 - fmt_10) %>% 
  mutate(med_high = fmt_1 - fmt_10)


fmt_trts %>% 
  summarize(higher=sum(control_low>0)/nrow(.))
# 66.6% probability that the False Map Turtles in the control group spent more time 
# swimming on the stimulus side of the arena than False Map Turtles in the 0.1 group overall

fmt_trts %>% 
  summarize(higher=sum(control_med>0)/nrow(.))
# 67% probability that the False Map Turtles in the control group spent more time 
# swimming on the stimulus side of the arena than False Map Turtles in the 1 group overall

fmt_trts %>% 
  summarize(higher=sum(control_high>0)/nrow(.))
# 59.4% probability that the False Map Turtles in the control group spent more time 
# swimming on the stimulus side of the arena than False Map Turtles in the 10 group overall

fmt_trts %>% 
  summarize(higher=sum(low_med>0)/nrow(.))
# 51.4% probability that the False Map Turtles in the 0.1 group spent more time 
# swimming on the stimulus side of the arena than False Map Turtles in the 1 group overall

fmt_trts %>% 
  summarize(higher=sum(low_high>0)/nrow(.))
# 40.9% probability that the False Map Turtles in the 0.1 group spent more time 
# swimming on the stimulus side of the arena than False Map Turtles in the 10 group overall

fmt_trts %>% 
  summarize(higher=sum(med_high>0)/nrow(.))
# 40.2% probability that the False Map Turtles in the 1 group spent more time 
# swimming on the stimulus side of the arena than False Map Turtles in the 10 group overall




#Calculating the difference between fmt treatments within trials
fmt_trts_trial <- trt_post_ss %>% 
  mutate(control_low = fmt_0 - fmt_0.1) %>% 
  mutate(control_med = fmt_0 - fmt_1) %>% 
  mutate(control_high = fmt_0 - fmt_10) %>% 
  mutate(low_med = fmt_0.1 - fmt_1) %>% 
  mutate(low_high = fmt_0.1 - fmt_10) %>% 
  mutate(med_high = fmt_1 - fmt_10)

fmt_trts_trial %>% 
  summarize(higher=sum(control_low>0)/nrow(.))

fmt_trts_trial %>% 
  summarize(higher=sum(control_med>0)/nrow(.))

fmt_trts_trial %>% 
  summarize(higher=sum(control_high>0)/nrow(.))

fmt_trts_trial %>% 
  summarize(higher=sum(low_med>0)/nrow(.))

fmt_trts_trial %>% 
  summarize(higher=sum(low_high>0)/nrow(.))

fmt_trts_trial %>% 
  summarize(higher=sum(med_high>0)/nrow(.))




# Calculating the difference between painted treatments regardless of trial
paint_trts <- trt_post_ss %>% 
  ungroup() %>% 
  mutate(control_low = painted_0 - painted_0.1) %>%
  mutate(control_med = painted_0 - painted_1) %>%
  mutate(control_high = painted_0 - painted_10) %>%
  mutate(low_med = painted_0.1 - painted_1) %>%
  mutate(low_high = painted_0.1 - painted_10) %>%
  mutate(med_high = painted_1 - painted_10)


paint_trts %>% 
  summarize(higher=sum(control_low>0)/nrow(.))
# 54.6% probability that the Painted Turtles in the control group spent more time 
# swimming on the stimulus side of the arena than Painted Turtles in the 0.1 group overall

paint_trts %>% 
  summarize(higher=sum(control_med>0)/nrow(.))
# 41.1% probability that the Painted Turtles in the control group spent more time 
# swimming on the stimulus side of the arena than Painted Turtles in the 1 group overall

paint_trts %>% 
  summarize(higher=sum(control_high>0)/nrow(.))
# 47.1% probability that the Painted Turtles in the control group spent more time 
# swimming on the stimulus side of the arena than Painted Turtles in the 10 group overall

paint_trts %>% 
  summarize(higher=sum(low_med>0)/nrow(.))
# 33.8% probability that the Painted Turtles in the 0.1 group spent more time 
# swimming on the stimulus side of the arena than Painted Turtles in the 1 group overall

paint_trts %>% 
  summarize(higher=sum(low_high>0)/nrow(.))
# 41.2% probability that the Painted Turtles in the 0.1 group spent more time 
# swimming on the stimulus side of the arena than Painted Turtles in the 10 group overall

paint_trts %>% 
  summarize(higher=sum(med_high>0)/nrow(.))
# 54.6% probability that the Painted Turtles in the 1 group spent more time 
# swimming on the stimulus side of the arena than Painted Turtles in the 10 group overall




#Calculating the difference between painted treatments within trials
paint_trts_trial <- trt_post_ss %>% 
  mutate(control_low = painted_0 - painted_0.1) %>%
  mutate(control_med = painted_0 - painted_1) %>%
  mutate(control_high = painted_0 - painted_10) %>%
  mutate(low_med = painted_0.1 - painted_1) %>%
  mutate(low_high = painted_0.1 - painted_10) %>%
  mutate(med_high = painted_1 - painted_10)

paint_trts_trial %>% 
  summarize(higher=sum(control_low>0)/nrow(.))

paint_trts_trial %>% 
  summarize(higher=sum(control_med>0)/nrow(.))

paint_trts_trial %>% 
  summarize(higher=sum(control_high>0)/nrow(.))

paint_trts_trial %>% 
  summarize(higher=sum(low_med>0)/nrow(.))

paint_trts_trial %>% 
  summarize(higher=sum(low_high>0)/nrow(.))

paint_trts_trial %>% 
  summarize(higher=sum(med_high>0)/nrow(.))


post_ss %>% 
  median_qi() %>% 
  write.table(file = "~/GitHub/IMI_Chemical_Cue_Detection/tables/full_median_qi.csv", sep = ",", quote = FALSE, row.names = F)





# Attempt to make a plot
# paint_trts_trial %>% 
#   select(Trial, .draw, control_low, control_med, control_high, low_med,
#          low_high, med_high) %>% 
#   gather(comparison, "difference", -c(Trial, .draw)) %>% 
#   ggplot(aes(comparison, difference, fill=Trial))+
#   geom_boxplot()



post_ss %>% 
  ggplot(aes(treatment, .epred, fill=Species))+
  geom_boxplot()



swim_stim <- brm(swim_time ~ 0 + side*treatment*Species*Trial,
                 family = Gamma(link = "log"),
                 data = sub_side_beh,
                 prior = c(prior(normal(2,2), ub = 5.703782, class = "b"),
                           prior(exponential(0.1), class = "shape")),
                 #sample_prior = "only",
                 cores = 4, chains = 1, iter = 1000)


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
  ggplot(aes(treatment, .epred, fill=Trial))+
  geom_boxplot()+
  facet_wrap(~Species)

