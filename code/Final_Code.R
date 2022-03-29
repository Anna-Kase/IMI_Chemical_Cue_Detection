
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

# ------------------------------------------------------------------------------------------

# Welcome to the Model Fitting section!


