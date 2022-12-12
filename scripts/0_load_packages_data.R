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
          "cowplot",
          "flextable")

pacman::p_load(pkgs, character.only=T)

# load data ---------------------------------------------------------------

updated_search_descriptives_clean <- read_xlsx(here("data","Smoking Lapse Review.xlsx"), sheet = "updated_search_descriptives_cle")

data_descriptives <- read_xlsx(here("data","Smoking Lapse Review.xlsx"), sheet = "additional_descriptives")

lapse_coding <- read_xlsx(here("data","Smoking Lapse Review.xlsx"), sheet = "lapse_coding")

relapse_coding <- read_xlsx(here("data","Smoking Lapse Review.xlsx"), sheet = "relapse_coding")

theory_coding <- read_xlsx(here("data","Smoking Lapse Review.xlsx"), sheet = "theory_coding")

data_meta_analysis <- read_xlsx(here("data","Smoking Lapse Review.xlsx"), sheet = "meta_analysis_clean")
