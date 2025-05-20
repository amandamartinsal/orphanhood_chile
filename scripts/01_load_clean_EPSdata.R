## |= EPS Data  =================================================================

## |- Source -----------------------------------------------------

#EPS 2002 data used in this study can be requested at:
#https://previsionsocial.gob.cl/datos-estadisticos/condiciones-bases-de-datos-eps/

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

# Important: You must manually download the file and place it in "data"

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
