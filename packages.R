library(conflicted)

library(targets)
library(tarchetypes)
library(tidyverse)
library(here)
library(kableExtra)
library("scales")
library("rmarkdown")

library("sf")
library("mapview")
library("rnaturalearth")
library("rnaturalearthdata")


conflict_prefer("filter", "dplyr")
