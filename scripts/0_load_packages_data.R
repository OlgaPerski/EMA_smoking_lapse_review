# install required packages -----------------------------------------------

if (!require("pacman")) install.packages("pacman")

pkgs <- c("here",
          "tidyverse",
          "stringr",
          "ggplot2",
          "readxl",
          "gtsummary",
          "metafor",
          "arsenal",
          "robumeta",
          "dmetar",
          "cowplot")

pacman::p_load(pkgs, character.only=T)

# load data ---------------------------------------------------------------

data_descriptives <- read_xlsx(here("data","Smoking Lapse Review Data Extraction.xlsx"), sheet = "additional_descriptives")

lapse_coding <- read_xlsx(here("data","Smoking Lapse Review Data Extraction.xlsx"), sheet = "lapse_coding")

relapse_coding <- read_xlsx(here("data","Smoking Lapse Review Data Extraction.xlsx"), sheet = "relapse_coding")

theory_coding <- read_xlsx(here("data","Smoking Lapse Review Data Extraction.xlsx"), sheet = "theory_coding")

data_meta_analysis <- read_xlsx(here("data","Smoking Lapse Review Data Extraction.xlsx"), sheet = "meta_analysis_clean")
