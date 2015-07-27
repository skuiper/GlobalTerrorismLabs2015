library(dplyr)
library(readr)

GMdata <- read_csv("finalGM.csv")

GTDdata <- read_csv("FullGTD.csv")

GTDbyCountryYear <- read_csv("GTDbyCountryYear.csv")

regionOptions <- c("All" = "all",
                   "Middle East & North Africa" = "Middle East & N. Africa",
                   "Sub-Saharan Africa" = "Sub-Saharan Africa",
                   "East Asia & Pacific" = "East Asia & Pacific",
                   "South Asia" = "South Asia",
                   "Europe & Central Asia" = "Europe & Central Asia",
                   "North America" = "North America",
                   "Latin America & Caribbean" = "Latin America"  
                   )

attackOptions <- c("All" = "all",
                   "Hostage Taking" = "Hostage Taking",
                   "Assault" = "Assault",
                   "Bombing" = "Bombing",
                   "Assassination" = "Assassination",
                   "Facility Attack" = "Facility Attack",
                   "Hijacking" = "Hijacking",
                   "Unknown" = "Unknown"
                   )

targetOptions <- c("All" = "all",
                   "Armed Forces" = "Armed Forces",
                   "Government" = "Government",
                   "Private Citizens & Property"="Private Citizens & Property",
                   "Business" = "Business",
                   "Infrastructure" = "Infrastructure",
                   "Educational/Religious Organizations"="Educational/Religious",
                   "Other/Unknown" = "Other/Unknown"
                   )

weaponOptions <- c("All" = "all",
                   "CBRN" = "CBRN",
                   "Firearms" = "Firearms",
                   "Explosives/Bombs/Dynamite" = "Explosives",
                   "Melee" = "Melee",
                   "Incendiary" = "Incendiary",
                   "Other/Unknown" = "Other/Unknown"
                   )

religionOptions <- c("All" = "all",
                     "Buddhist" = "Buddhist",
                     "Catholic" = "Catholic",
                     "Christian" = "Christian",
                     "Hindu" = "Hindu",
                     "Jewish" = "Jewish",
                     "Muslim" = "Muslim",
                     "Orthodox Christian" = "Orthodox",
                     "None/Other" = "None/Other"
                     )

facetOptions <- c("Attack Type" = "AttackType",
                  "Target Type" = "TargetType",
                  "Weapon Type" = "WeaponType",
                  "Region" = "Region",
                  "Success" = "Success",
                  "Religion" = "Religion"
                  )

colorOptions <- c("None" = "none",
                  "Region" = "Region",
                  "Religion" = "Religion")


#Custom Colors for graphs
customColors <- c("#a6cee3", "#1f78b4", "#b2df84", "#33a02c",
                  "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00")

#For Y-axis
GTDOptions <-c("Incidents" = "Incidents",
               "Fatalities" = "Fatalities",
               "Wounded" = "Wounded")

#For X-axis
GMOptions <- c("Population (millions)" = "PopulationInMillions",
               "GDP per Capita" = "GDPPerCapita",
               "Life Expectancy" = "LifeExpectancy",
               "Unemployment Rate (Female)" = "FemaleUnemploymentRate",
               "Labor Rate" = "LaborRate",
               "Children per Woman" = "ChildrenPerWoman",
               "Electricity per Capita" = "ElectricityPerCapita",
               "Child Mortality Rate" = "ChildMortalityRate")

  
