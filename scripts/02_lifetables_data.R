## |= Chilean Life Tables ======================================================

##  |- Life Tables from 1989–1991======================================
# Source: Human Life Table (www.lifetable.de)

# List of file information
file_info <- list(
  list(
    url = "https://www.lifetable.de/File/GetDocument/data/CHL/CHL000019861989CU1.txt",
    use_year2 = TRUE,       # Use Year2 column for this data set
    filter_ageint = TRUE    # Apply special filtering for age intervals (to fix duplication)
  ),
  list(
    url = "https://www.lifetable.de/File/GetDocument/data/CHL/CHL000019901990AU1.txt",
    use_year2 = TRUE,
    filter_ageint = FALSE
  ),
  list(
    url = "https://www.lifetable.de/File/GetDocument/data/CHL/CHL000019911992AU1.txt",
    use_year2 = FALSE,      # Use Year1 column for this data set
    filter_ageint = FALSE
  )
)

# Function to download and clean each file

process_file <-
  function(url, use_year2 = TRUE, filter_ageint = FALSE) {
  df <- read_csv(url, show_col_types = FALSE)
  
  year_val <- if (use_year2) df$Year2[1] else df$Year1[1]
  
  # Keep only female data (Sex == 2)
  df <- df %>% filter(Sex == 2)
  
  if (filter_ageint) {
    df <- df %>%
      filter(
        (Age == 0 & AgeInt == 1) |
          (Age == 1 & AgeInt == 4) |
          (Age >= 2 & AgeInt == 5)
      )
  }
  
  # Remove duplicate Age values ans select age and lx
  df <- df %>% distinct(Age, .keep_all = TRUE)
  
  df %>%
    select(Age, `l(x)`) %>%
    mutate(
      `l(x)` = as.numeric(`l(x)`) / 100000,
      Year = as.integer(year_val)
    )
}

# Process all three datasets and bind them together
chile_lt1989to1991 <-
  bind_rows(lapply(file_info, function(info) {
    process_file(info$url, info$use_year2, info$filter_ageint)
  }))

# Remove ages 2–4 specifically for year 1991
chile_lt1989to1991 <- chile_lt1989to1991 %>%
  filter(!(Year == 1991 & Age %in% 2:4))

## |-  Life Tables from 1992–2020=======================================
# Source: Human Mortality Database (https://www.mortality.org)

# Important: You must manually download the file and place it in "data/fltper_5x1.txt"

# Read HMD file and clean
chile_lt1992to2020 <- read_table("data/fltper_5x1.txt", skip = 1)

# Vector with nax to use to estimate e0 later
chile_ax <- chile_lt1992to2020 %>% 
  filter(Year == 1992) %>%  
  slice(1:22)  %>% 
  pull(ax)

# Add age group
chile_lt1992to2020 <- chile_lt1992to2020 %>% 
  mutate(age_group = rep(c(0, 1, seq(5, 110, by = 5)), 29)) %>% 
  select(year = Year, age_group, lx) %>% 
  mutate(lx = lx / 1e5) 

## |- Merge Both Data sets==============================================

# Rename column to match before binding
chile_lt1989to1991 <- chile_lt1989to1991 %>% 
  rename(age_group = Age, lx = `l(x)`, year = Year)

# Merge data sets (1989–1991 + 1992–2020)
chile_lt <- bind_rows(chile_lt1989to1991, chile_lt1992to2020)

## |- Compute np25_s (survival probabilities from age 25+)==============

chile_np25 <- chile_lt %>% 
  filter(age_group >= 25) %>% 
  mutate(n = as.integer(age_group / 5) + 1) %>% 
  mutate(
    l25plusn_s = if_else(n <= n(), lx[n], NA_real_),
    np25_s = l25plusn_s / first(lx),
    .by = year
  ) %>% 
  rename(lx_s = lx) %>% 
  select(-n)

# Keep only the rows for age 25 and join back survival ratio
chile_np25 <- chile_np25 %>% 
  filter(age_group == 25) %>% 
  select(year, lx_s) %>% 
  rename(l25_s = lx_s) %>% 
  right_join(chile_np25, by = "year")