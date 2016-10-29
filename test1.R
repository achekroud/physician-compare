
library(tidyverse); library(doMC); library(stringr)
registerDoMC(cores = detectCores()-1)
setwd("~/Documents/PhD/Projects/medicare-quality/physician-compare")

options(scipen = 100)

states <- read.csv("states.csv", as.is = TRUE)



# raw_PCNDF <- read.csv("Physician_Compare_National_Downloadable_File.csv", as.is = TRUE)
# print(utils:::object.size(raw_PCNDF), units="GB")
# saveRDS(raw_PCNDF, file = "Physician_Compare_National_Downloadable_File.RDS")
# raw_PCNDF <- readRDS("Physician_Compare_National_Downloadable_File.RDS")


raw_individual <- read.csv("Physician_Compare_2014_Individual_EP_Public_Reporting_-_Clinical_Quality_Of_Care.csv", as.is = TRUE)

raw_practice <- read.csv("Physician_Compare_2014_Group_Practice_Public_Reporting_-_Clinical_Quality_Of_Care.csv", as.is = TRUE)


# qplot(raw_CQOC$Getting.a.flu.shot.during.flu.season.)
# qplot(raw_CQOC$Making.sure.older.adults.have.gotten.a.pneumonia.vaccine.)
# qplot(raw_CQOC$Screening.for.depression.and.developing.a.follow.up.plan.)
# qplot(raw_CQOC$Screening.for.tobacco.use.and.providing.help.quitting.when.needed.)
# qplot(raw_CQOC$Prescribing.medicine.to.improve.the.pumping.action.of.the.heart.in.patients.who.have.both.heart.disease.and.certain.other.conditions.)





individual <- raw_individual %>% dplyr::select(-starts_with("Footnote"))

mclapply(individual, function(i) summary(i, useNA="always"))

psych::describe(individual, fast = TRUE)

individual %>% 
    select(5:10) %>%
    summarise_each(funs = c("mean(., na.rm=TRUE)")) %>% t





source("https://raw.githubusercontent.com/briatte/ggcorr/master/ggcorr.R")
plotCorrMat <- ggcorr(individual[5:10],
                      method = c("pairwise", "pearson"),
                      label = TRUE, label_size = 5,
                      mid = "white")
plot(plotCorrMat)
# ggsave(plotCorrMat, 
#        filename = "xxxxxx_corrMat.eps",
#        width = 200, height = 240, units = "mm")




practice <- raw_practice %>% dplyr::select(-starts_with("Footnote"))

practice <- full_join(practice, states, by = c("State" = "Abbreviation")) %>%
    mutate(stateName = str_to_lower(State.y))

mclapply(practice, function(i) summary(i, useNA="always"))


practice %>%
    group_by(State) %>%
    summarise_each(funs = "mean(., na.rm=TRUE)") %>% View

av.practice <- practice %>%
    group_by(State) %>%
    summarise_each(funs = "mean(., na.rm=TRUE)") %>%
    left_join(., states, by = c("State" = "Abbreviation")) %>%
    mutate(region = str_to_lower(State.y.y))

us <- map_data("state")


## Flu Shots

ggplot() + 
    geom_map(data = us, map = us, aes(x=long, y=lat, map_id = region),
             fill = "#ffffff", color = "#ffffff", size = 0.15) + 
    geom_map(data = av.practice, map = us, 
             aes(fill = Getting.a.flu.shot.during.flu.season., map_id = region),
             color = "#ffffff", size = 0.15) + 
    scale_fill_gradientn(colours = c("#2b8cbe", "white", "#d7301f")) + 
    ggtitle("Getting a flu shot during flu season") +
    labs(x=NULL, y=NULL) +
    coord_map("albers", lat0 = 39, lat1 = 45) +
    theme(panel.border = element_blank(),
          panel.background = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5))


# Screening for depression and developing a follow up plan

ggplot() + 
    geom_map(data = us, map = us, aes(x=long, y=lat, map_id = region),
             fill = "#ffffff", color = "#ffffff", size = 0.15) + 
    geom_map(data = av.practice, map = us, 
             aes(fill = Screening.for.depression.and.developing.a.follow.up.plan., map_id = region),
             color = "#ffffff", size = 0.15) + 
    scale_fill_gradientn(colours = c("#2b8cbe", "white", "#d7301f")) + 
    ggtitle("Screening for depression and developing a follow up plan") +
    labs(x=NULL, y=NULL) +
    coord_map("albers", lat0 = 39, lat1 = 45) +
    theme(panel.border = element_blank(),
          panel.background = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5))


# Screening for tobacco use & providing help quitting when needed

ggplot() + 
    geom_map(data = us, map = us, aes(x=long, y=lat, map_id = region),
             fill = "#ffffff", color = "#ffffff", size = 0.15) + 
    geom_map(data = av.practice, map = us, 
             aes(fill = Screening.for.tobacco.use.and.providing.help.quitting.when.needed., map_id = region),
             color = "#ffffff", size = 0.15) + 
    scale_fill_gradientn(colours = c("#2b8cbe", "white", "#d7301f")) + 
    ggtitle("Screening for tobacco use & providing help quitting when needed") +
    labs(x=NULL, y=NULL) +
    coord_map("albers", lat0 = 39, lat1 = 45) +
    theme(panel.border = element_blank(),
          panel.background = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5))


# Screening for colorectal cancer

ggplot() + 
    geom_map(data = us, map = us, aes(x=long, y=lat, map_id = region),
             fill = "#ffffff", color = "#ffffff", size = 0.15) + 
    geom_map(data = av.practice, map = us, 
             aes(fill = Screening.for.colorectal..colon.or.rectum..cancer., map_id = region),
             color = "#ffffff", size = 0.15) + 
    scale_fill_gradientn(colours = c("#2b8cbe", "white", "#d7301f")) + 
    ggtitle("Screening for colorectal cancer") +
    labs(x=NULL, y=NULL) +
    coord_map("albers", lat0 = 39, lat1 = 45) +
    theme(panel.border = element_blank(),
          panel.background = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5))



# Screening for breast cancer

ggplot() + 
    geom_map(data = us, map = us, aes(x=long, y=lat, map_id = region),
             fill = "#ffffff", color = "#ffffff", size = 0.15) + 
    geom_map(data = av.practice, map = us, 
             aes(fill = Screening.for.breast.cancer., map_id = region),
             color = "#ffffff", size = 0.15) + 
    scale_fill_gradientn(colours = c("#2b8cbe", "white", "#d7301f")) + 
    ggtitle("Screening for breast cancer") +
    labs(x=NULL, y=NULL) +
    coord_map("albers", lat0 = 39, lat1 = 45) +
    theme(panel.border = element_blank(),
          panel.background = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5))



