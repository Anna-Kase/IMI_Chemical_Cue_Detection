
# Welcome to the Imidacloprid Chemical Cue Detection Experiment Analysis!

# First things first, load in your packages
library(tidyverse)
library(janitor)
library(brms)
library(tidybayes)
library(ggridges)

# ---------------------------------------------------------------------------------

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
painted_tbl$...9 <- NULL
painted_tbl$...10 <- NULL
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
fmt_tbl$...9 <- NULL
fmt_tbl$...10 <- NULL

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
  mutate(treatment = case_when(Turtle %in% c(109,124,113,123,120,115,1000,119) ~ "0",
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


# -------------------------------------------------------------------------------

# more data manipulation

all_tbl <- read.csv("data/master_all_turtle_data.csv")

# make treatment a factor rather than an integer
all_tbl$treatment <- as.factor(all_tbl$treatment)

# sort the dataset so we have time data for just the swimming behavior on just the stimulus side
# of the arena
swim_time <- all_tbl  %>% 
  filter(Behavior == "Swimming") %>% 
  filter(Side == "Stimulus") %>% 
  group_by(Turtle, Trial, treatment, Species, experiment_group, Side, Behavior) %>%
  summarise(
    time = sum(total_seconds)
  ) 

# more data manipulation - I had to make sure there were zeroes in the data set for
# trials in which the turtle didn't forage at all. I wasn't sure how to do this more
# efficiently than making a vector for each missing row of data and then adding it
# to a simpler dataset before adding back in the necessary columns of species and 
# treatment (Side, Behavior, and Experiment Group aren't super necessary to the model)

a <- c("107", "Trial_1", "0")
b <- c("108", "Prelim_Trial", "0")
c <- c("108", "Trial_1", "0")
d <- c("108", "Trial_2", "0")
e <- c("111", "Prelim_Trial", "0")
f <- c("114", "Trial_1", "0")
g <- c("114", "Trial_3", "0")
h <- c("119", "Trial_1", "0")
i <- c("119", "Trial_2", "0")
j <- c("119", "Trial_3", "0")
k <- c("122", "Trial_1", "0")
l <- c("122", "Trial_3", "0")
m <- c("124", "Prelim_Trial", "0")
n <- c("125", "Trial_1", "0")
o <- c("125", "Trial_3", "0")
p <- c("126", "Trial_3", "0")
q <- c("128", "Trial_3", "0")
r <- c("129", "Prelim_Trial", "0")
s <- c("129", "Trial_1", "0")
t <- c("129", "Trial_2", "0")
u <- c("131", "Prelim_Trial", "0")
v <- c("132", "Prelim_Trial", "0")
w <- c("132", "Trial_2", "0")
x <- c("133", "Prelim_Trial", "0")
y <- c("133", "Trial_2", "0")
z <- c("134", "Trial_2", "0")
aa <- c("134", "Trial_3", "0")
bb <- c("136", "Trial_3", "0")
cc <- c("137", "Trial_3", "0")
dd <- c("58", "Prelim_Trial", "0")
ee <- c("64", "Trial_1", "0")
ff <- c("73", "Prelim_Trial", "0")
gg <- c("73", "Trial_1", "0")
hh <- c("74", "Trial_1", "0")
ii <- c("76", "Trial_3", "0")
jj <- c("78", "Trial_1", "0")
kk <- c("78", "Trial_3", "0")
ll <- c("79", "Trial_1", "0")
mm <- c("79", "Trial_3", "0")
nn <- c("82", "Prelim_Trial", "0")
oo <- c("83", "Trial_2", "0")
pp <- c("84", "Trial_1", "0")
qq <- c("84", "Trial_3", "0")
rr <- c("85", "Trial_1", "0")
ss <- c("85", "Trial_3", "0")
tt <- c("86", "Trial_2", "0")
uu <- c("90", "Prelim_Trial", "0")
vv <- c("90", "Trial_1", "0")
ww <- c("90", "Trial_3", "0")


data3 <- swim_time %>% 
  ungroup() %>% 
  select(Turtle, Trial, time)

data4 <- rbind(data3, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x,
               y, z, aa, bb, cc, dd, ee, ff, gg, hh, ii, jj, kk, ll, mm, nn, oo, pp, qq, rr, 
               ss, tt, uu, vv, ww)

View(data4)

data5 <- data4 %>% 
  mutate(treatment = case_when(Turtle %in% c(86,91,83,93,80,76,58,75,109,124,113,123,120,115,1000,119) ~ "0",
                               Turtle %in% c(90,88,84,94,92,62,73,54,130,121,122,132,128,108,129,107) ~ "0.1",
                               Turtle %in% c(96,87,89,82,55,68,65,2000,114,126,131,116,117,136,111,125) ~ "1",
                               TRUE ~ "10")) %>% 
  mutate(Species = case_when(Turtle %in% c(107,108,109,111,112,113,114,115,116,117,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,145,155,1000) ~ "fmt",
                             TRUE ~ "painted")) %>% 
  mutate(Side = "Stimulus") %>% 
  mutate(Behavior = 'Swimming')

View(data5)


write.csv(data5, "~/GitHub/IMI_Chemical_Cue_Detection/data/filtered_fixed_foraging_data.csv", row.names=FALSE)

# ---------------------------------------------------------------------------------

# final model

model_data <- read.csv("data/filtered_fixed_foraging_data.csv")

model_data$time <- as.numeric(model_data$time)
model_data$treatment <- as.character(model_data$treatment)


final <- brm(time ~ 1 + Species * treatment * Trial + (1| Turtle),
                 data = model_data,
                 family = hurdle_gamma(link="log"),
                 prior = c(prior(normal(4,2), class = "Intercept"),
                           prior(normal(0, 1), class = "b"),
                           prior(exponential(0.1), class = "sd")),
                 chains = 4, iter = 4000)

saveRDS(final, file="models/final.RDS")


sensitivity_analysis <- brm(time ~ 1 + Species * treatment * Trial + (1| Turtle),
                            data = model_data,
                            family = hurdle_gamma(link="log"),
                            prior = c(prior(normal(4,4), class = "Intercept"),
                                      prior(normal(0, 2), class = "b"),
                                      prior(exponential(0.01), class = "sd")),
                            chains = 4, iter = 4000)

saveRDS(sensitivity_analysis, file="models/sensitivity_analysis.RDS")

# ------------------------------------------------------------------------------

# plots

conditional_effects(final)

final$data %>% 
  distinct() %>% 
  add_epred_draws(final) %>% 
  ggplot(aes(.epred))+
  geom_histogram()



final$data %>% 
  distinct() %>% 
  add_epred_draws(final) %>% 
  mutate(species_labels = case_when(Species == "fmt" ~ "False Map Turtle",
                                    TRUE ~ "Painted Turtle")) %>% 
  ggplot(aes(treatment, .epred, fill=Trial))+
  geom_boxplot(outlier.shape = NA)+
  facet_wrap(~species_labels)+
  geom_point(data=final$data, aes(treatment, time, group=Trial),
             position=position_dodge(width=0.75), alpha=0.25) +
  ylim(0,300) +
  labs(x = "Imidacloprid Concentration (ug/L)", y = "Time Spent Foraging (seconds)")+
  scale_fill_manual(name="Trial", labels = c("Preliminary Trial", "Trial 1", 
                                               "Trial 2", "Trial 3"),
                      values = c("#44AA99", "#AA4499", "#6699CC", "#DDCC77"))+
  theme_bw()
ggsave("~/GitHub/IMI_Chemical_Cue_Detection/plots/boxplot_3variables.png")


pp_check(final, type="boxplot")
ggsave("~/GitHub/IMI_Chemical_Cue_Detection/plots/pp_check_boxplot.png")

pp_check(final)
ggsave("~/GitHub/IMI_Chemical_Cue_Detection/plots/pp_check_density.png")


final$data %>% 
  distinct() %>% 
  add_epred_draws(final) %>% 
  mutate(species_labels = case_when(Species == "fmt" ~ "False Map Turtle",
                                    TRUE ~ "Painted Turtle")) %>% 
  ggplot(aes(Trial, .epred, fill=treatment))+
  geom_boxplot(outlier.shape = NA)+
  facet_wrap(~species_labels)+
  geom_point(data=final$data, aes(Trial, time, group=treatment),
             position=position_dodge(width=0.75), alpha=0.25) +
  ylim(0,300) +
  labs(x = "Trial", y = "Time Spent Foraging (seconds)")+
  scale_fill_manual(name="Imidacloprid Treatment", labels = c("0 ug/L", "0.1 ug/L", 
                                             "1 ug/L", "10 ug/L"),
                    values = c("#D55E00", "#56B4E9", "#009E73", "#999999"))+
  scale_x_discrete(labels = c("Prelim_Trial" = "Preliminary \n Trial", 
                              "Trial_1" = "Trial 1",
                              "Trial_2" = "Trial 2",
                              "Trial_3" = "Trial 3")) +
  theme_bw()
ggsave("~/GitHub/IMI_Chemical_Cue_Detection/plots/boxplot_3variables_colTrt.png")


#average time overall
final$data %>% 
  distinct() %>% 
  add_epred_draws(final) %>% 
  ungroup() %>% 
  select(-.row, -.chain) %>% 
  group_by(.draw, Species, treatment) %>% 
  summarize(average_time = mean(.epred)) %>% 
  ggplot(aes(x = average_time, y = treatment, fill = Species)) + 
  geom_density_ridges(alpha = 0.75)+
  labs(x = "Average Time Spent Foraging (seconds)", y = "Imidacloprid Concentration (ug/L)")+
  scale_fill_manual(name="Species", labels = c("False Map Turtle", "Painted Turtle"),
                    values = c("#117733", "#888888"))+
  theme_bw()
ggsave("~/GitHub/IMI_Chemical_Cue_Detection/plots/species_avg_density.png")



final$data %>% 
  distinct() %>% 
  add_epred_draws(final) %>% 
  ungroup() %>% 
  select(-.row, -.chain) %>% 
  group_by(.draw, Species, treatment) %>% 
  summarize(average_time = mean(.epred)) %>% 
  ggplot(aes(x = treatment, y = average_time, fill = Species)) + 
  geom_violin(outlier.shape = NA)+
  geom_boxplot(width=0.1, color="black", alpha=0.2, outlier.shape=NA, position=position_dodge(width=0.9))+
  labs(y = "Average Time Spent Foraging (seconds)", x = "Imidacloprid Concentration (ug/L)")+
  scale_fill_manual(name="Species", labels = c("False Map Turtle", "Painted Turtle"),
                    values = c("#117733", "#888888"))+
  theme_bw()
ggsave("~/GitHub/IMI_Chemical_Cue_Detection/plots/species_avg_violin.png")



final_post_plot <- final$data %>% 
  distinct() %>% 
  add_epred_draws(final) %>% 
  mutate(model_name = "Model")

sensitivity_post_plot <- sensitivity_analysis$data %>% 
  distinct() %>% 
  add_epred_draws(final) %>% 
  mutate(model_name = "Sensitivity Analysis")

plot_post <- rbind(final_post_plot, sensitivity_post_plot)

plot_post %>% 
  mutate(species_labels = case_when(Species == "fmt" ~ "False Map Turtle",
                                    TRUE ~ "Painted Turtle")) %>% 
  ggplot(aes(Trial, .epred, fill=treatment))+
  geom_boxplot(outlier.shape = NA)+
  facet_grid(model_name ~ species_labels)+
  geom_point(data=final$data, aes(Trial, time, group=treatment),
             position=position_dodge(width=0.75), alpha=0.25) +
  ylim(0,300) +
  labs(x = "Trial", y = "Time Spent Foraging (seconds)")+
  scale_fill_manual(name="Imidacloprid Treatment", labels = c("0 ug/L", "0.1 ug/L", 
                                                              "1 ug/L", "10 ug/L"),
                    values = c("#D55E00", "#56B4E9", "#009E73", "#999999"))+
  scale_x_discrete(labels = c("Prelim_Trial" = "Preliminary \n Trial", 
                              "Trial_1" = "Trial 1",
                              "Trial_2" = "Trial 2",
                              "Trial_3" = "Trial 3")) +
  theme_bw()
ggsave("~/GitHub/IMI_Chemical_Cue_Detection/plots/sensitivity_analysis.png")


# -----------------------------------------------------------------------------

# derived quantities

final_post <- final$data %>% 
  distinct(Species, treatment, Trial) %>% 
  add_epred_draws(final, re_formula = NA) 

final_post$.row=NULL


final_post %>% 
  median_qi(.epred) %>% 
  write.csv("~/GitHub/IMI_Chemical_Cue_Detection/tables/median_qi_values.csv", row.names = FALSE)


final_post %>% 
  ungroup() %>% 
  select(-.chain, -.iteration) %>%
  distinct() %>% 
  pivot_wider(names_from = treatment, values_from = .epred) %>% 
  mutate(diff_0_01 = `0`-`0.1`,
         diff_0_1 = `0` - `1`,
         diff_0_10 = `0` - `10`,
         diff_01_1 = `0.1` - `1`,
         diff_01_10 = `0.1` - `10`,
         diff_1_10 = `1` - `10`) %>% 
  group_by(Species, Trial) %>% 
  summarize(prob_0_01 = (sum(diff_0_01>0)/length(.draw))*100,
            prob_0_1 = (sum(diff_0_1>0)/length(.draw))*100,
            prob_0_10 = (sum(diff_0_10>0)/length(.draw))*100,
            prob_01_1 = (sum(diff_01_1>0)/length(.draw))*100,
            prob_01_10 = (sum(diff_01_10>0)/length(.draw))*100,
            prob_1_10 = (sum(diff_1_10>0)/length(.draw))*100) %>% 
  write.csv("~/GitHub/IMI_Chemical_Cue_Detection/tables/dq_diff_trt_by_SPandTp.csv", row.names = FALSE)



final_post %>% 
  group_by(Species, treatment) %>% 
  median_qi(.epred) %>% 
  write.csv("~/GitHub/IMI_Chemical_Cue_Detection/tables/median_qi_values_noTp.csv", row.names = FALSE)

  
final_post %>% 
  ungroup() %>% 
  select(-.chain, -.iteration) %>%
  distinct() %>% 
  pivot_wider(names_from = Species, values_from = .epred) %>% 
  mutate(diff_fmt_pt = fmt - painted,
         diff_pt_fmt = painted - fmt) %>% 
  group_by(treatment) %>% 
  summarize(prob_fmt_pt = (sum(diff_fmt_pt>0)/length(.draw))*100,
            prob_pt_fmt = (sum(diff_pt_fmt>0)/length(.draw))*100) %>% 
  write.csv("~/GitHub/IMI_Chemical_Cue_Detection/tables/dq_diff_species_by_trt_noTp.csv", row.names = FALSE)


final_post %>% 
  group_by(Species) %>% 
  median_qi(.epred) %>% 
  write.csv("~/GitHub/IMI_Chemical_Cue_Detection/tables/median_qi_values_SpOnly.csv", row.names = FALSE)

final_post %>% 
  ungroup() %>% 
  select(-.chain, -.iteration) %>%
  distinct() %>% 
  pivot_wider(names_from = treatment, values_from = .epred) %>% 
  mutate(diff_0_01 = `0`-`0.1`,
         diff_0_1 = `0` - `1`,
         diff_0_10 = `0` - `10`,
         diff_01_1 = `0.1` - `1`,
         diff_01_10 = `0.1` - `10`,
         diff_1_10 = `1` - `10`) %>% 
  group_by(Species) %>% 
  summarize(prob_0_01 = (sum(diff_0_01>0)/length(.draw))*100,
            prob_0_1 = (sum(diff_0_1>0)/length(.draw))*100,
            prob_0_10 = (sum(diff_0_10>0)/length(.draw))*100,
            prob_01_1 = (sum(diff_01_1>0)/length(.draw))*100,
            prob_01_10 = (sum(diff_01_10>0)/length(.draw))*100,
            prob_1_10 = (sum(diff_1_10>0)/length(.draw))*100) %>% 
  write.csv("~/GitHub/IMI_Chemical_Cue_Detection/tables/dq_diff_trt_by_species_noTp.csv", row.names = FALSE)



final_post %>% 
  ungroup() %>% 
  select(-.chain, -.iteration) %>%
  distinct() %>% 
  pivot_wider(names_from = treatment, values_from = .epred) %>% 
  mutate(diff_0_01 = `0`-`0.1`,
         diff_0_1 = `0` - `1`,
         diff_0_10 = `0` - `10`,
         diff_01_1 = `0.1` - `1`,
         diff_01_10 = `0.1` - `10`,
         diff_1_10 = `1` - `10`) %>% 
  group_by(Trial) %>% 
  summarize(prob_0_01 = (sum(diff_0_01>0)/length(.draw))*100,
            prob_0_1 = (sum(diff_0_1>0)/length(.draw))*100,
            prob_0_10 = (sum(diff_0_10>0)/length(.draw))*100,
            prob_01_1 = (sum(diff_01_1>0)/length(.draw))*100,
            prob_01_10 = (sum(diff_01_10>0)/length(.draw))*100,
            prob_1_10 = (sum(diff_1_10>0)/length(.draw))*100) %>% 
  write.csv("~/GitHub/IMI_Chemical_Cue_Detection/tables/dq_diff_trt_by_Tp_noSp.csv", row.names = FALSE)


final_post %>% 
  ungroup() %>% 
  select(-.chain, -.iteration) %>%
  distinct() %>% 
  pivot_wider(names_from = Trial, values_from = .epred) %>% 
  mutate(diff_P_1 = Prelim_Trial - Trial_1,
         diff_P_2 = Prelim_Trial - Trial_2,
         diff_P_3 = Prelim_Trial - Trial_3,
         diff_1_2 = Trial_1 - Trial_2,
         diff_1_3 = Trial_1 - Trial_3,
         diff_2_3 = Trial_2 - Trial_3) %>% 
  group_by(Species, treatment) %>% 
  summarize(prob_P_1 = (sum(diff_P_1>0)/length(.draw))*100,
            prob_P_2 = (sum(diff_P_2>0)/length(.draw))*100,
            prob_P_3 = (sum(diff_P_3>0)/length(.draw))*100,
            prob_1_2 = (sum(diff_1_2>0)/length(.draw))*100,
            prob_1_3 = (sum(diff_1_3>0)/length(.draw))*100,
            prob_2_3 = (sum(diff_2_3>0)/length(.draw))*100) %>% 
  write.csv("~/GitHub/IMI_Chemical_Cue_Detection/tables/dq_diff_Trial_by_trt_Sp.csv", row.names = FALSE)



final_post %>% 
  ungroup() %>% 
  select(-.chain, -.iteration) %>%
  distinct() %>% 
  pivot_wider(names_from = Species, values_from = .epred) %>% 
  mutate(diff_fmt_pt = fmt - painted,
         diff_pt_fmt = painted - fmt) %>% 
  summarize(prob_fmt_pt = (sum(diff_fmt_pt>0)/length(.draw))*100,
            prob_pt_fmt = (sum(diff_pt_fmt>0)/length(.draw))*100)
