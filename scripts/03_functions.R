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
  
  np25
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
      np25 = orphanhood.method(
        age_group_numeric, 
        prop_living, 
        prop_living_previous, 
        m
      )
    ) 
  
  data <- data %>% 
    left_join(
      chile_np25, 
      join_by(age_group_numeric==age_group),
      relationship = "many-to-many"
    ) %>% 
    mutate(diff = abs(np25_s - np25)) %>% 
    filter(diff == min(diff), .by = c(edu, age_group_numeric)) %>% 
    select(-c(diff))
  
  data <- data |> 
    mutate(
      alpha = -1/2*log(
        1 + ( (np25/l25plusn_s) - (1/l25_s) )/ (1-np25) 
      )
    ) 
  
  data |> 
    mutate(
      normalized_nqx = map2_dbl(
        year, alpha, 
        ~ indirect.nqx(.x, .y, n=indirect_nqx_n, x=indirect_nqx_x)),
      ex = map2_dbl(year, alpha, ~ indirect.ey(.x, .y, ex_at))
    ) |>
    select(
      edu, age_group_numeric, 
      malive, total, 
      prop_living, np25, year,
      alpha, normalized_nqx, ex
    )
}
