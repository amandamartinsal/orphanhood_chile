## |- Descriptive  =============================================================

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
    labels = c("low" = "Low", "high" = "High", "unknown" = "Unknown")) +
  scale_y_discrete(
    name = "Mother's age",
    labels = function(x) paste0(x)) +
  labs(x = TeX("Conditional Survival Probabilities from age 25 to Mother's age")) +
  coord_cartesian(xlim = c(.4,1)) + 
  facet_wrap(~mothers_edu, labeller = as_labeller(c(
    "low" = "Low educated mothers", 
    "high" = "High educated mothers",
    "unknown" = "Unknown")))

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
    labels = c("low" = "Low", "high" = "High", "unknown" = "Unknown")) +
  scale_y_discrete(
    name = "Mother's age",
    labels = function(x) paste0(x)) +
  labs(x = TeX("Conditional Survival Probabilities from age 25 to Mother's age")) +
  coord_cartesian(xlim = c(.4,1)) + 
  facet_wrap(~mothers_edu, labeller = as_labeller(c(
    "low" = "Low educated mothers", 
    "high" = "High educated mothers",
    "unknown" = "Unknown")))

ggsave("outputs/np25_4.pdf",
       width = 16, height = 10, units = "cm")

ggsave("outputs/np25_4.jpeg",
       width = 16, height = 10, units = "cm")


## |- Life Expectancy at 60 ----------------------------------------------------
## e60 facet by mother's edu, without "unknown" and e60 for total population from WPP

s_bs_wpp <-
  s_bs %>%
  filter(between(age_group_numeric, 25, 50), mothers_edu != "unknown") %>% 
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

## |- Difference in e60 ----------------------------------------------------
## Facet by mother's edu, without "unknown"

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
  scale_y_continuous(limits = c(-15,17), breaks = seq(-15,15,5)) +
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