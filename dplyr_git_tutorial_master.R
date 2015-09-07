#dplyr and Git beginner tutorial
#University of Delaware
#Last updated: September 7, 2015
#MASTER
# ____  _     _     _ _        _           _     
# |  _ \(_)   | |   | | |      | |         | |    
#   | |_) |_  __| | __| | | ___  | |     __ _| |__  
# |  _ <| |/ _` |/ _` | |/ _ \ | |    / _` | '_ \ 
# | |_) | | (_| | (_| | |  __/ | |___| (_| | |_) |
# |____/|_|\__,_|\__,_|_|\___| |______\__,_|_.__/       

#GitHub repository URL: https://github.com/IMRambo/biddle-lab.git
#==============================================================================
#GOAL: learn how to use dplyr functions for data wrangling.
#Combine dplyr, tidyr, and ggplot functions.
#Collaborate on this script using Git.
#==============================================================================
library(dplyr)
library(ggplot2)
library(tidyr)
library(vegan)
#==============================================================================
setwd("/Users/imrambo/Documents/Git/biddle-lab/")
#Load the hflights dataset - all flights departing from Houston in 2011 
library(hflights)

#Display header and first four rows of hflights
head(hflights, n = 4)
dim(hflights)
str(hflights)

#Carrier names, some real, most fake and/or goofy.
CarrierName <- c("American Airlines", "Lufthansa", "Fly Like a B6",
                 "Colorado Air", "HushHush Air", "Flying Monkey Air",
                 "Uzbekistan Airlines","US Air", "Windy Air", "Evil Air",
                 "FinnAir", "Soul Plane","Air Morocco",
                 "Xenophobic International", "Air Yugoslavia")
#Unique values of Houston carrier codes; pair with CarrierName vector
UniqueCarrier <- unique(hflights$UniqueCarrier)

#Create a data frame of character vectors containing "airline names" and
#carrier codes.
Carriers <- data.frame(CarrierName, UniqueCarrier) %>%
  transmute(
    CarrierName = as.character(CarrierName),
    UniqueCarrier = as.character(UniqueCarrier))

str(Carriers)

carrierVelocity <- hflights %>% filter(Cancelled == 0) %>%
  rename(DistanceMiles = Distance) %>%
  left_join(Carriers) %>%
  mutate(
    DistanceKm = DistanceMiles/0.62137,
    #Average air velocity, Km/hr
    AvgAirVelocity = DistanceKm/(AirTime/60),
    # %j format gives day of the year
    DayOfYear = as.numeric(strftime(paste(Year,
                               Month,
                               DayofMonth,
                               sep = "-"),
                         format = "%j"))) %>%
  group_by(UniqueCarrier) %>%
  select(DayOfYear, Month, DistanceKm, AvgAirVelocity, UniqueCarrier,
         CarrierName)
  
carrierVelocity %>% group_by(DayOfYear, UniqueCarrier,
                             CarrierName) %>%
  #DMAV - Daily Mean Air Velocity - daily mean air velocity (km/hr) for
  #all flights of a carrier
  #DMAD - Daily Mean Air Distance - daily mean air distance for
  #all flights of a carrier
  summarise(DMAV = mean(AvgAirVelocity),
            DMAD = mean(DistanceKm)) %>% 
  #Select flights for American Airlines, Soul Plane, and Lufthansa
  filter(UniqueCarrier == "AA"|
           UniqueCarrier == "FL"|
           UniqueCarrier == "AS") %>%
  ggplot(aes(x = DayOfYear, y = DMAV,
             shape = factor(CarrierName),
             size = DMAD, color = factor(CarrierName))) +
  geom_point(alpha = 0.55) +
  scale_shape_manual(name = "Carrier", values = c(15, 17, 19)) +
  scale_color_manual(name = "Carrier",
                       values = c("#009E73", "#56B4E9", "#CC79A7")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        legend.key.size = unit(0.6, "cm")) +
  xlab("Day of Year") + ylab("Daily Mean Air Velocity (km/hr)") +
  scale_x_continuous(limits = c(1, 365))
  
#==============================================================================
#Let's work with community population data - Barro Colorado Island tree counts
#from the vegan package

#Load BCI data from the vegan package
data(BCI, package = "vegan")
#Create a character vector containing the 50 sites
BCI$Site <- as.character(paste("Site", 1:50))

#Create a spread, e.g. wide data frame from BCI data. Columns are now
#sites instead of species, with rows containing counts for each species. 
BCI_spread <- BCI %>% gather(Species, Count,
                             Abarema.macradenia:Zuelania.guidonia,
                             convert = TRUE) %>%
  spread(Site, Count)
#------------------------------------------------------------------------------
#Perform Nonmetric Multidimensional Scaling for BCI_spread
ord <- metaMDS(BCI_spread[, 2:ncol(BCI_spread)],
               distance = "bray", autotransform = FALSE, k = 2)

#Extract species scores as numeric vectors
mdsx <- as.numeric(ord$species[, 1])
mdsy <- as.numeric(ord$species[, 2])

#Create a data frame of sites and MDS species
BCI_ord <- data.frame(BCI$Site, mdsx, mdsy) %>%
  rename(Site = BCI.Site)
ShannonH <- diversity(BCI[, 2:(ncol(BCI) - 1)], index = "shannon")
BCI_ord <- cbind(BCI_ord, ShannonH)
BCI_ord$Site <- as.character(BCI_ord$Site)
#------------------------------------------------------------------------------
#Create a tidy data frame of Sites, Species, Counts, MDS species, and ShannonH
BCI_tidy <- BCI %>% gather(Species, Count, Abarema.macradenia:Zuelania.guidonia,
               convert = TRUE) %>% 
  left_join(BCI_ord)

#Filter the tidy data frame for three species, with counts > 0
BCI_tidy %>% filter(Species == "Abarema.macradenia"|
                      Species == "Theobroma.cacao"|
                      Species == "Trema.micrantha", Count > 0) %>%
  #Create a scatterplot of filtered data
  ggplot(aes(x = mdsx, y = mdsy, size = Count,
             shape = Species, color = ShannonH)) +
  geom_point() + theme(panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.background = element_blank(),
                       axis.line = element_line(color = "black")) +
  scale_color_continuous(low = "darkred", high = "dodgerblue")
