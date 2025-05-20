## |= Setup ====================================================================
## |- Loading packages ---------------------------------------------------------
library(tidyverse)
library(haven)
library(janitor)
library(VIM)
library(survey)
library(latex2exp)
library(future.apply)
library(dplyr)
library(readr)
library(httr)
library(ggplot2)
library(tidyr)

## |- Theme formatting ---------------------------------------------------------
theme_set(theme_bw())

theme_update(
  plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
  text = element_text(size = 9),
  axis.text = element_text(size = 9),
  panel.spacing = unit(.5, "lines"),
  legend.position = 'bottom',
  strip.placement = "outside",
  strip.text = element_text(size = 9, face = "bold"),
  legend.margin = margin(0, 0, 0, 0)
) 
