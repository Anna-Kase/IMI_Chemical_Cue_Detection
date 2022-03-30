
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

