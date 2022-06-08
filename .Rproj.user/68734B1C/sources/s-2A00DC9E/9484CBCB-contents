
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


