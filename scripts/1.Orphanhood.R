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


## |= Data =====================================================================
## |- Opening the EPS data -----------------------------------------------------
# Selected variables
# folio: individual identification code
# factor: expansion factor
# orden: allows identifying the people inside the respondent's household
# where an order equal to 1 corresponds to the respondent.
# ip4: sex
# ip5: age
# vp1: type of relationship with mother or tutor
# vp2: Is your mother alive?
# vp7c: Mother's educational attainment
# vp7t: Mother's type education
# vp12: Is your father alive?
# vp17c: Father's educational attainment
# vp17t: Father's type education


# expansion factor database 
expfactor2002 <- read_dta("data/factor_EPS02.dta")  %>%
  rename(folio = folio_n20)

# parental information data base
eps2002.1 <- read_dta("data/base1.dta") %>%
  select(gse,factor, vp1, vp2, vp7c, vp7t, vp12,
         vp17c, vp17t, folio_n20) %>%
  rename(mrelation = vp1,
         malive = vp2,
         meduat = vp7c,
         medutype = vp7t,
         falive = vp12,
         feduat = vp17c,
         fedutype = vp17t,
         folio = folio_n20)

# individual information database
eps2002.2 <- read_dta("data/base2.dta") %>%
  select(factor, ip4, ip5, ip9c, ip9t, folio_n20, orden, iip9) %>% 
  rename(sex = ip4,
         age = ip5,
         edu = ip9c,
         edutype = ip9t,
         folio = folio_n20,
         order = orden,
         occup = iip9)

## |- Merging and selecting answers from the interviewed person ----------------
data_2002 <-
  full_join(
    # order == 1 is the person that answer the survey in the hh
    eps2002.2 %>% filter(order==1),             
    eps2002.1 %>% select(-factor), 
    # folio represents the household, 
    by = "folio"
  )

# checking folio is unique, not repeating the same person in the data base
data_2002 %>% 
  count(folio, order) %>% 
  pull(n) %>% 
  unique() 

## |- Groups of education ------------------------------------------------------
## Education categories
#1        Educación Parvularia o Preescolar
#2                             Preparatoria
#3                         Educación Básica
#4                    Educación Diferencial
#5                              Humanidades
#6               Media Científico-Humanista
#7 Técnica,Comercial,Industrial, Normalista
#8                Media Técnica Profesional
#9   Centro de formación Técnica incompleta
#10     Centro de formación Técnica completa
#11         Instituto Profesional incompleta
#12           Instituto Profesional completa
#13       Educación Universitaria incompleta
#14         Educación Universitaria completa
#15               Universitaria de Postgrado
#16                                  Ninguno
#17                                  No Sabe

edu_categories <- 
  data_2002 %>% 
  mutate(
    mothers_edu = factor(
      case_when(
        as.numeric(medutype) %in% 1:2 ~ 1,    # low
        as.numeric(medutype) %in% 3:15 ~ 2,   # high
        as.numeric(medutype) %in% 16 ~ 1,     # low
        as.numeric(medutype) %in% 17 ~ 3      # unknown
      ),
      levels = c(1, 2, 3), labels = c("low", "high", "unknown"), ordered = TRUE
    ),
    children_edu = factor(
      case_when(
        as.numeric(edutype) %in% 1:4 ~ 1,    # low
        as.numeric(edutype) %in% 5:15 ~ 2,   # high
        as.numeric(edutype) %in% 16 ~ 1      # low
      ),
      levels = c(1, 2), labels = c("low", "high"), ordered = TRUE
    ), 
    edu = toupper(paste0(substr(mothers_edu, 1, 1), substr(children_edu, 1, 1)))
  )

## |- Age groups ---------------------------------------------------------------
# filtering only children that answer if the mother is alive 
# or not and age range (25-55)
edu_nona <- 
  edu_categories %>%
  filter(!is.na(malive), 
         age >= 15,       
         age <= 55)

# create age groups

edu_nona <-
  edu_nona %>%
  mutate(
    age_group = cut(
      age, 
      breaks = seq(0, 100, by = 5), 
      labels = paste(seq(0, 95, by = 5), "-", seq(5, 100, by = 5)),
      right = FALSE
    )
  )


## |- Mothers variables --------------------------------------------------------
edu_nona <-
  edu_nona %>%
  # Filter only biological mothers
  filter(mrelation == 1) %>% 
  # Indicator variable for mothers alive
  mutate(malive = ifelse(malive == 1, 1, 0))

education_variables <- edu_nona %>% 
  select(children_edu, mothers_edu, edu) %>% 
  distinct() 

## |- Chilean life tables ------------------------------------------------------
# Vector with nax to use to estimate e0 later
chile_ax <- read_table("data/fltper_5x1.txt", skip = 1) %>% 
  filter(Year == 1992) %>%  
  slice(1:22)  %>% 
  pull(ax)


# From 1992 to 2020
chile_lt1992to2020 <- read_table("data/fltper_5x1.txt", skip = 1)
chile_lt1992to2020 <- chile_lt1992to2020 %>% 
  mutate(age_group = rep(c(0,1,seq(5,110,by=5)),29)) %>% 
  select(year=Year, age_group, lx) %>% 
  mutate(lx = lx/1e5)

# From 1989 to 1991
chile_lt1989to1991 <- read_csv("data/fltper_5x1_1989to1991.csv")

chile_lt <- rbind(chile_lt1989to1991, chile_lt1992to2020)

chile_lx_25plus <- chile_lt %>% 
  filter(age_group >= 25) %>% 
  mutate(n = as.integer(age_group / 5)+1) %>% 
  mutate(
    l25plusn_s = if_else(n <= n(), lx[n], NA_real_),
    np25_s = l25plusn_s / first(lx),
    .by = year
  ) %>% 
  rename(lx_s = lx) %>% 
  select(-n)

chile_lx_25plus <- chile_lx_25plus %>% 
  filter(age_group == 25) %>% 
  select(year, lx_s) %>% 
  rename(l25_s=lx_s) %>% 
  right_join(chile_lx_25plus) 

## |= Functions ================================================================
## |- Generic functions --------------------------------------------------------
orphanhood.method <- function(age_group_numeric, prop_living, prop_living_previous, m) {
  coefs <- tibble(
    N = c(10,      15,      20,      25,      30,      35,      40,      45,      50),
    A = c(-0.2894, -0.1718, -0.1513, -0.1808, -0.2511, -0.3644, -0.5181, -0.6880, -0.8054),
    B = c(0.00125, 0.00222, 0.00372, 0.00586, 0.00885, 0.01287, 0.01795, 0.02343, 0.02721),
    C = c(1.2559,  1.1123,  1.0525,  1.0267,  1.0219,  1.0380,  1.0753,  1.1276,  1.1678)
  )
  
  A <- coefs$A[match(age_group_numeric, coefs$N)]
  B <- coefs$B[match(age_group_numeric, coefs$N)]
  C <- coefs$C[match(age_group_numeric, coefs$N)]
  
  np25 <- A + B*m + C * prop_living_previous
  
  np25[np25>1] <- NA 
  np25[np25<0] <- NA
  
  n_half <- (age_group_numeric/2)
  ln_prop <- log(sqrt(prop_living*prop_living_previous))
  
  time <- n_half*(1-1/3*ln_prop+1/3*log((80-m-age_group_numeric)/(80-m)))
  
  tibble(np25, time)
}

indirect.nqx <- function(year_s, alpha, n=45, x=15){
  
  if(is.na(alpha) | is.na(year_s)) return(NA)
  
  lxs <- chile_lt %>% filter(year == year_s)
  
  lxs_l <- lxs %>% filter(age_group == x) %>% pull(lx)
  lxs_u <- lxs %>% filter(age_group == x+n) %>% pull(lx)
  
  ys_l <- .5*log((1-lxs_l)/lxs_l)
  ys_u <- .5*log((1-lxs_u)/lxs_u)
  
  nqx <- 1 - ( 1+exp(2*(alpha+ys_l)) ) / ( 1+exp(2*(alpha+ys_u)) ) 
  
  return(nqx)
}

ey.fromnqx <- function(age, nqx, y=0, ax=chile_ax){
  n   <- c(diff(age))
  n <- c(n,n[length(n)])
  
  nqx          <- c(nqx[-(length(nqx))], 1)
  nqx[nqx > 1] <- 1
  
  npx <- 1 - nqx
  lx <- cumprod(c(1, npx))
  ndx <- -diff(lx)
  lxpn <- lx[-1]
  nLxpn <- n * lxpn + ndx * ax
  nLx <- c(nLxpn[-length(nLxpn)], lx[length(lx)-1]*ax[length(lx)-1])
  Tx <- rev(cumsum(rev(nLx)))
  lx <- lx[1:length(age)]
  ex <- Tx/lx
  
  ey <- ex[age==y]
  
  return(ey)
}

indirect.ey <- function(year_s, alpha, y){
  tryCatch(
    expr = {
      if(is.na(alpha) | is.na(year_s)) return(NA)
      
      ages <- c(0,1,seq(5,100,5)) 
      
      nqx <- mapply(
        indirect.nqx, 
        year_s = year_s, 
        alpha  = alpha, 
        n = c(1,4,rep(5,20)), 
        x = ages,
        SIMPLIFY = TRUE
      )
      
      nqx[22] <- 1
      nqx <- unlist(nqx)
      
      ex <- ey.fromnqx(age = ages, nqx = nqx, y=y) 
      
      return(ex)
    }, 
    error = function(e) return(NA)
  )
}

## |- Functions for CI with bootstrapping --------------------------------------
bs.orphanhood <- function(data, ex_at = 60, indirect_nqx_x = 15, indirect_nqx_n = 45){
  data <- data |> 
    slice_sample(
      n = nrow(data), replace = TRUE
    ) |> 
    mutate(
      age_group_numeric = as.numeric(substr(age_group, 1, 2))
    ) |>
    arrange(age_group_numeric) |>
    summarise(
      malive = sum(malive*factor), # Expansion factor
      total = sum(factor),
      prop_living = malive/total,
      .by = c(edu, age_group_numeric)
    ) |> 
    mutate(
      prop_living_previous = lag(prop_living, order_by = age_group_numeric),
      .by = edu
    ) |>  
    mutate(
      m = 28,  ##mean age at childbearing
      orphanhood.method(age_group_numeric, prop_living, prop_living_previous, m),
      year = 2002-time,
      year_round = round(year)
    ) 
  
  data <- data |> 
    mutate(year_round = if_else(year_round <= 1989, 1989, year_round))
  
  data <- left_join(
    data, chile_lx_25plus, 
    join_by(year_round==year, age_group_numeric==age_group)
  )
  
  data <- data |> 
    mutate(
      alpha = -1/2*log(
        1 + ( (np25/l25plusn_s) - (1/l25_s) )/ (1-np25) 
      )
    ) 
  
  data |> 
    mutate(
      normalized_nqx = map2_dbl(
        year_round, alpha, 
        ~ indirect.nqx(.x, .y, n=indirect_nqx_n, x=indirect_nqx_x)),
      ex = map2_dbl(year_round, alpha, ~ indirect.ey(.x, .y, ex_at))
    ) |>
    select(
      edu, age_group_numeric, 
      malive, total, 
      prop_living, np25, year, 
      alpha, normalized_nqx, ex
    )
}

## |- Descriptives ------------------------------------

# Sample with missing for survival of the mothers

data_2002 %>% #create age groups
  mutate(
    age_group = cut(
      age, 
      breaks = seq(0, 100, by = 5), 
      labels = paste(seq(0, 95, by = 5), "-", seq(5, 100, by = 5)),
      right = FALSE
    )
  ) %>%
  group_by(malive) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(percentage = count / sum(count) * 100)

# Sample with missing for survival of the fathers

data_2002 %>% #create age groups
  mutate(
    age_group = cut(
      age, 
      breaks = seq(0, 100, by = 5), 
      labels = paste(seq(0, 95, by = 5), "-", seq(5, 100, by = 5)),
      right = FALSE
    )
  ) %>%
  group_by(falive) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(percentage = count / sum(count) * 100)

# Sample by the type of the relation with the mother

data_2002 %>% #create age groups
  mutate(
    age_group = cut(
      age, 
      breaks = seq(0, 100, by = 5), 
      labels = paste(seq(0, 95, by = 5), "-", seq(5, 100, by = 5)),
      right = FALSE
    )
  ) %>%
  group_by(mrelation) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(percentage = count / sum(count) * 100)

# Children age

age_summary <- edu_nona %>%
  filter(age >= 20 & age <= 45) %>% 
  group_by(age_group) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(percentage = count / sum(count) * 100)

age_summary_factor <- edu_nona %>%
  filter(age >= 20 & age <= 45) %>%
  group_by(age_group) %>%
  summarise(count = sum(factor), .groups = "drop") %>%  
  mutate(percentage = count / sum(count) * 100)

# Mother education

edu_mothers_summary <- edu_nona %>%
  filter(age >= 20 & age <= 45) %>% 
  group_by(mothers_edu) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(percentage = count / sum(count) * 100)

edu_mothers_summary_factor <- edu_nona %>%
  filter(age >= 20 & age <= 45) %>% 
  group_by(mothers_edu) %>%
  summarise(count = sum(factor), .groups = "drop") %>% # use sum(factor) for weighted count
  mutate(percentage = count / sum(count) * 100)

# Children education

edu_children_summary <- edu_nona %>%
  filter(age >= 20 & age <= 45) %>% 
  group_by(children_edu) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(percentage = count / sum(count) * 100)

edu_children_summary <- edu_nona %>%
  filter(age >= 20 & age <= 45) %>% 
  group_by(children_edu) %>%
  summarise(count = sum(factor), .groups = "drop") %>%
  mutate(percentage = count / sum(count) * 100)

# Calculate percentage distribution for combined education type

edu_summary <- edu_nona %>%
  filter(age >= 20 & age <= 45) %>% 
  group_by(mothers_edu, children_edu) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(percentage = count / sum(count) * 100)

edu_summary_factor <- edu_nona %>%
  filter(age >= 20 & age <= 45) %>% 
  group_by(mothers_edu, children_edu) %>%
  summarise(count = sum(factor), .groups = "drop") %>%
  mutate(percentage = count / sum(count) * 100)

## |= Estimating mother's survival measures using bootstrapping ================
set.seed(420)
n_simulations <- 10000

start_time <- Sys.time()
# Set up parallel backend
plan(multisession, workers = 20) # If running local can use detectCores() - 1

# Run parallel simulation
data_bs <- future_replicate(
  n_simulations, 
  bs.orphanhood(edu_nona, ex_at = 60, indirect_nqx_x = 30), # here you choose with LE, I am choosing 50
  simplify =FALSE
)

# Reset to sequential mode
plan(sequential)
end_time <- Sys.time()

data_bs_binded <- map2_dfr(
  .x = data_bs, .y = seq_along(data_bs), ~mutate(.x, simulation_id = .y)
)

s_bs <- data_bs_binded %>% 
  summarise(
    bs_year = round(mean(year, ra.rm=TRUE)), 
    bs_prop_living = mean(prop_living),
    bs_prop_living_l = quantile(prop_living,.025, na.rm=TRUE),
    bs_prop_living_u = quantile(prop_living,.975, na.rm=TRUE),
    bs_np25 = mean(np25, na.rm=TRUE),
    bs_np25_se = sd(np25, na.rm=TRUE),
    bs_np25_l = quantile(np25,.025, na.rm=TRUE), 
    bs_np25_u = quantile(np25,.975, na.rm=TRUE),
    bs_30q30 = mean(normalized_nqx, na.rm=TRUE),
    bs_30q30_l = quantile(normalized_nqx,.025, na.rm=TRUE), 
    bs_30q30_u = quantile(normalized_nqx,.975, na.rm=TRUE),
    bs_alpha = mean(alpha, na.rm = TRUE),
    bs_alpha_l = quantile(alpha, .025, na.rm = TRUE),
    bs_alpha_u = quantile(alpha, .975, na.rm = TRUE),
    bs_ex = mean(ex, na.rm=TRUE),
    bs_ex_l = quantile(ex, .025, na.rm=TRUE), 
    bs_ex_u = quantile(ex, .975, na.rm=TRUE),
    .by = c(edu, age_group_numeric)
  )

s_bs <- left_join(s_bs, education_variables) %>% 
  mutate(children_edu = factor(children_edu, levels = c("high","low")))

s_bs2 <- data_bs_binded %>% 
  filter(between(age_group_numeric,25,50)) %>% 
  mutate(age_group2 = if_else(age_group_numeric <= 30, 30, 35)) %>% 
  summarise(
    np25 = mean(np25),
    ex = mean(ex), 
    .by = c(edu, age_group2, simulation_id)
  ) %>% 
  summarise(
    bs_np25 = mean(np25, na.rm=TRUE),
    bs_np25_se = sd(np25, na.rm=TRUE),
    bs_np25_l = quantile(np25,.025, na.rm=TRUE), 
    bs_np25_u = quantile(np25,.975, na.rm=TRUE),
    bs_ex = mean(ex, na.rm=TRUE),
    bs_ex_l = quantile(ex, .025, na.rm=TRUE), 
    bs_ex_u = quantile(ex, .975, na.rm=TRUE),
    .by = c(edu, age_group2)
  )

s_bs2 <- left_join(s_bs2, education_variables) %>% 
  mutate(children_edu = factor(children_edu, levels = c("high","low")))

## |- np25 - Survival probability ----------------------------------------------

# Figure 1

s_bs %>%
  filter(between(age_group_numeric, 25, 50)) %>% 
  ggplot(aes(y = factor(age_group_numeric + 25), 
             group = children_edu, color = children_edu)) + 
  geom_point(aes(x = bs_np25),
             position = position_dodge(width = 0.35),
             size = 1.5) + 
  geom_errorbar(
    aes(xmin = bs_np25_l, xmax = bs_np25_u),
    width = 0, position = position_dodge(width = 0.35), linewidth = 0.75) +
  scale_color_viridis_d(
    name = "Children's Education", 
    direction = -1,
    labels = c("low" = "Low", "high" = "High", "dknow" = "Unknown")) +
  scale_y_discrete(
    name = "Mother's age",
    labels = function(x) paste0(x)) +
  labs(x = TeX("Conditional Survival Probabilities from age 25 to Mother's age")) +
  coord_cartesian(xlim = c(.4,1)) + 
  facet_wrap(~mothers_edu, labeller = as_labeller(c(
    "low" = "Low educated mothers", 
    "high" = "High educated mothers",
    "dknow" = "Unknown")))

ggsave("outputs/np25_4.pdf",
       width = 16, height = 10, units = "cm")

ggsave("outputs/np25_4.jpeg",
       width = 16, height = 10, units = "cm")


## |- Life Expectancy at 60 ----------------------------------------------------
## e60 facet by mother's edu, without "unknown" and e60 for total population from WPP

s_bs_wpp <-
  s_bs %>%
  filter(between(age_group_numeric, 25, 50), mothers_edu != "dknow") %>% 
  mutate(cohort = case_when(
    age_group_numeric == 25 ~ 1955,
    age_group_numeric == 30 ~ 1950,
    age_group_numeric == 35 ~ 1945,
    age_group_numeric == 40 ~ 1940,
    age_group_numeric == 45 ~ 1935,
    age_group_numeric == 50 ~ 1930,
    TRUE ~ NA_real_
  )) %>% 
  mutate(WPP = case_when(
    cohort == 1955 ~ 25.3,
    cohort == 1950 ~ 24.7,
    cohort == 1945 ~ 24.4,
    cohort == 1940 ~ 23.6,
    cohort == 1935 ~ 22.4,
    cohort == 1930 ~ 21.2,
    TRUE ~ NA_real_
  ))


# Figure 2

ggplot(data = s_bs_wpp, aes(x = cohort, group = children_edu, color = children_edu)) +
  facet_wrap(~mothers_edu, labeller = as_labeller(c(
    "low" = "Low educated mothers", 
    "high" = "High educated mothers"))) + 
  geom_point(aes(y = bs_ex), position = position_dodge(width = 2), size = 2) + 
  geom_errorbar(
    aes(ymin = bs_ex_l, ymax = bs_ex_u),
    width = 0, position = position_dodge(width = 2), linewidth = 0.75) + 
  geom_point(aes(x = cohort, y = WPP, shape = "WPP (female population)"), 
             size = 2, inherit.aes = FALSE) + 
  scale_color_manual(
    name = "Children's education", 
    values = viridis::viridis_pal(direction = -1)(length(unique(s_bs_wpp$children_edu))),
    labels = c("low" = "Low", "high" = "High")) +   
  scale_shape_manual(name = NULL, values = c("WPP (female population)" = 21)) +
  scale_x_continuous(
    breaks = seq(1930, 1955, 5),
    labels = paste0(seq(1930, 1955, 5), rep("\n",6), rep("(",6), seq(45,20,-5),rep(")",6))
  ) +
  scale_y_continuous(breaks = seq(20,50,5)) +
  labs(x = "Mother's Mean Year of Birth \n(Children's age group)", y = TeX("Mother's Life Expectancy at age 60")) +
  theme(panel.grid.minor.x = element_blank()) + 
  coord_flip()

ggsave("outputs/e60_WPP3.pdf",
       width = 16, height = 10, units = "cm")

ggsave("outputs/e60_WPP3.jpeg",
       width = 16, height = 10, units = "cm")

## Difference in e60 facet by mother's edu, without "unknown"

data_bs_binded2 <-
  data_bs_binded %>%
  filter(between(age_group_numeric, 25, 50)) %>% 
  mutate(cohort = case_when(
    age_group_numeric == 25 ~ 1955,
    age_group_numeric == 30 ~ 1950,
    age_group_numeric == 35 ~ 1945,
    age_group_numeric == 40 ~ 1940,
    age_group_numeric == 45 ~ 1935,
    age_group_numeric == 50 ~ 1930,
    TRUE ~ NA_real_
  ))

# Figure 3

data_bs_binded2 %>% 
  left_join(education_variables) %>% 
  filter(mothers_edu != "unknown") %>% 
  select(simulation_id, children_edu, cohort, mothers_edu, ex) %>% 
  pivot_wider(names_from = children_edu, values_from = ex) %>% 
  mutate(
    diff = high - low
  ) %>% 
  summarise(
    diff_ex = mean(diff, na.rm = TRUE),
    diff_ex_l = quantile(diff, .025, na.rm = TRUE),
    diff_ex_u = quantile(diff, .975, na.rm = TRUE),
    .by = c(mothers_edu, cohort)
  ) %>%
  ggplot(aes(x = cohort)) + 
  facet_wrap(~mothers_edu, labeller = as_labeller(c(
    "low" = "Low educated mothers", 
    "high" = "High educated mothers"))) + 
  geom_point(
    aes(y = diff_ex), position = position_dodge(width = 2), 
    size = 2) + 
  geom_errorbar(
    aes(ymin = diff_ex_l, ymax = diff_ex_u),
    width = 0, position = position_dodge(width = 2), linewidth = 0.75) + 
  scale_x_continuous(
    breaks = seq(1930, 1955, 5),
    labels = paste0(seq(1930, 1955, 5), "\n(", seq(45, 20, -5), ")")
  ) +
  scale_y_continuous() +
  labs(
    x = "Mother's Mean Year of Birth\n(Children's age group)", 
    y = TeX("Difference in Mother's $e_{60}$ by children's education (high - low)")
  ) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  theme(panel.grid.minor.x = element_blank()) + 
  coord_flip()
  
ggsave(
  "outputs/diff_e60_3.pdf",
  width = 16, height = 10, units = "cm")

ggsave(
  "outputs/diff_e60_3.jpeg",
  width = 16, height = 10, units = "cm")