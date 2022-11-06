# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# INTRO ================================
metadatar <- list(script_starttime = Sys.time(), 
                  script_det = list(version_dt = as.Date("2021-09-21"), 
                                    author = "corb", 
                                    proj_name = "ncwclc", 
                                    script_type = "eda", 
                                    notepad = paste0("explore eda")), 
                  seed_set = 6)
metadatar
# ^ ====================================
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# LOAD LIBRARIES ------------------------------------------------------------
R.version.string
Sys.info()
getwd()
library(lobstr)
library(rlang)
library(tidyverse)
library(tidylog)
# library(readxl)
# library(janitor)
# library(lubridate)
library(scales)
library(gt)
# library(DataExplorer)
# library(reshape2)
# library(grid)
# library(caTools)
library(patchwork)
# library(modelr)
# library(broom)
set.seed(metadatar$seed_set[1])
options(digits = 4, max.print = 99, warnPartialMatchDollar = TRUE, 
        tibble.print_max = 30, scipen = 999, nwarnings = 5)
mem_used()
# ^ -----

# basic helper functions -------------------------------------------------

# function to print object size
sizer <- function(x) {
  aaa <- format(object.size(x), "MB")
  return(aaa)}

# function to quickly run garbage collection
trash <- function(x) {
  gc(verbose = TRUE)}

# function to quickly view a sample of a df
viewer <- function(x) {
  if (is.data.frame(x) == FALSE) {
    print("Error, insert a dataframe")
  } else {
    if(nrow(x) < 95) {
      View(x[sample(1:nrow(x), floor(nrow(x) * 0.5)), ])
    } else {
      View(x[sample(1:nrow(x), 100), ])
    }}}

# a function to make a quick data dictionary of a data frame
data_dictionary <- function(aa) {
  dd <- data.frame(column_order = seq(1, ncol(aa)), 
                   column_name_text = colnames(aa), 
                   column_class = sapply(aa, class, simplify = TRUE), 
                   column_nacount = sapply(lapply(aa, is.na), 
                                           sum, simplify = TRUE), 
                   row_01 = sapply(aa[1, ], as.character, simplify = TRUE), 
                   row_02 = sapply(aa[2, ], as.character, simplify = TRUE),
                   row_03 = sapply(aa[3, ], as.character, simplify = TRUE),
                   row_04 = sapply(aa[4, ], as.character, simplify = TRUE),
                   row_05 = sapply(aa[5, ], as.character, simplify = TRUE),
                   row.names = NULL)
  return(dd)}

# start the clock timer
clockin <- function() {
  aa <- Sys.time()
  clock_timer_start <<- aa
  return(aa)}

# end the clock timer
clockout <- function(x) {
  aa <- clock_timer_start
  bb <- Sys.time()
  cc <- bb - aa
  return(cc)}

# helps turn a character dollar variable into numeric
#   requires stringr
cash_money <- function(x) {
  zz <- ifelse(x == "", "$0.0", x)
  aa <- str_sub(zz, start = 2, end = str_length(x))
  bb <- str_replace_all(aa, pattern = ",", replacement = "")
  cc <- as.numeric(bb)
  dd <- ifelse(is.na(cc), 0, cc)
  return(dd)}

# ^ -----

# data loaders -----------------------------------------------------

# load a rds file
loader_path1 <- paste0(getwd(), "/etl/ingot", "/dataframe.rds")
clockin()
xx <- readRDS(loader_path1)
clockout()
dim(xx)
head(xx)

xx$metadata_tag[1]

df <- xx %>% select(-metadata_tag)

# ^ -----

# visualize 1 -----------------------------------------------

plt0 <- df %>% filter(industry != 'F-Class') %>% 
  mutate(lc_cap = ifelse(loss_cost > 10, 10, loss_cost)) %>% 
  ggplot(aes(x = lc_effective_year, y = lc_cap, 
             group = lc_effective_year)) + 
  geom_boxplot(color = 'blue', alpha = 0.25) + 
  theme_minimal() + labs(y = 'Capped Loss Cost', x = '')

plt1 <- df %>% filter(industry != 'F-Class') %>% 
  group_by(lc_effective_year) %>% 
  summarise(lc_mean = mean(loss_cost), 
            lc_med = median(loss_cost)) %>% 
  ggplot(aes(x = lc_effective_year, y = lc_mean)) + 
  geom_line() + geom_point() + 
  geom_label(aes(label = round(lc_mean, digits = 2))) + 
  theme_minimal() + labs(y = 'Mean Loss Cost', x = '')

plt2 <- df %>% filter(industry != 'F-Class') %>% 
  group_by(lc_effective_year) %>% 
  summarise(lc_mean = mean(loss_cost), 
            lc_med = median(loss_cost)) %>% 
  ggplot(aes(x = lc_effective_year, y = lc_med)) + 
  geom_line(color = 'red') + geom_point(color = 'red') + 
  geom_label(aes(label = round(lc_med, digits = 2)), 
             color = 'red') + 
  theme_minimal() + labs(y = 'Median Loss Cost', x = '')

plt0+ plt1 + plt2
rm(plt1, plt2, plt0)

# box plot of how loss costs have mostly come down in successive years
df %>% mutate(lc_cap = ifelse(loss_cost > 15, 15, loss_cost)) %>% 
  # filter(loss_cost < 20) %>% 
  # filter(industry != "F-Class") %>% 
  ggplot(aes(x = lc_cap, color = as.factor(lc_effective_year))) + 
  geom_boxplot(alpha = 0.25) + 
  facet_wrap(vars(industry), scales = 'free') + 
  theme_minimal() + theme(legend.position = "bottom") + 
  labs(color = "Loss Cost Effective Year", x = "Loss Cost", 
       subtitle = "Class-Level Loss Cost Distros by Industry Over Time", 
       caption = "Capped LC @ $15")

# trend line of loss costs (mean/median) over time
df %>% 
  # filter(loss_cost < 20) %>% 
  # filter(industry != "F-Class") %>% 
  # mutate(lc_effective_year = as.factor(lc_effective_year)) %>% 
  group_by(industry, lc_effective_year) %>% 
  summarise(lc_mean = mean(loss_cost), 
            lc_med = median(loss_cost)) %>% 
  ggplot(aes(x = lc_effective_year, y = lc_med, color = industry)) + 
  geom_line() + geom_point() + 
  geom_label(aes(label = lc_med), size = 3) + 
  facet_wrap(vars(industry), scales = "free_y") + 
  theme_minimal() + theme(legend.position = "top") + 
  labs(y = "Median Loss Cost", x = "Loss Cost Effective Year",  
       subtitle = "Class-Level Median Loss Cost by Industry Over Time", 
       caption = "")
  
# ^ -----

# aggro ----------------------------------------------------------

fun_delta_type <- function(vvv) {
  return_me <- case_when(is.na(vvv) ~ 'ERROR', 
                         vvv == 0 ~ 'NO CHANGE', 
                         vvv > 0.2 ~ '+20%', 
                         vvv > 0.1 ~ '10-20%', 
                         vvv > 0.05 ~ '5-10%', 
                         vvv > 0 ~ '0-5%',
                         vvv < -0.2 ~ '-20%', 
                         vvv < -0.1 ~ '-10-20%', 
                         vvv < -0.05 ~ '-10-20%', 
                         vvv < 0 ~ '-0-5%', 
                         TRUE ~ 'ERROR')
  return(return_me)}

fun_code_type <- function(yr1, yr2) {
  return_me <- case_when(is.na(yr1) & !is.na(yr2) ~ 'NEW CODE', 
                         !is.na(yr1) & is.na(yr2) ~ 'RETIRED CODE', 
                         is.na(yr1) & is.na(yr2) ~ 'ANCIENT CODE', 
                         !is.na(yr1) & !is.na(yr2) ~ 'LIVE CODE', 
                         TRUE ~ 'ERROR')
  return(return_me)}

fun_wider_tbl <- function(df_func) {
  return_me <- df_func %>% 
    mutate(lc_eff_yr = as.factor(lc_effective_year)) %>% 
    group_by(lc_eff_yr, industry, 
             class_code_fct, special_class) %>% 
    summarise(loss_cost = min(loss_cost)) %>% 
    pivot_wider(names_from = lc_eff_yr, values_from = loss_cost, 
                names_prefix = 'yr_') %>% 
    mutate(delta_18_19 = yr_2019 / yr_2018 - 1, 
           delta_19_20 = yr_2020 / yr_2019 - 1, 
           delta_20_21 = yr_2021 / yr_2020 - 1, 
           delta_21_22 = yr_2022 / yr_2021 - 1, 
           delta_22_23 = yr_2023 / yr_2022 - 1) %>% 
    mutate(type_22_23 = fun_delta_type(delta_22_23), 
           class_code_type = fun_code_type(yr_2022, yr_2023))
  return(return_me)}

# write the aggregated data out
# filename <- paste0(getwd(), "/etl/ingot/agg_classcode_df.csv")
# clockin()
# write.csv(df %>% fun_wider_tbl(), file = filename, row.names = FALSE)
# clockout()

# ^ -----

# plt aggro ------------------------------------------------------

df %>% fun_wider_tbl() %>% 
  mutate(yr_2023 = ifelse(yr_2023 > 10, 10, yr_2023)) %>%
  ggplot() + 
  geom_point(aes(x = yr_2023, y = delta_22_23, 
                 color = industry), 
             alpha = 0.5) + 
  geom_hline(aes(yintercept = 0), 
             linetype = 2) + 
  theme_minimal() + theme(legend.position = 'none') + 
  facet_wrap(vars(industry), scales = 'free')

df %>% fun_wider_tbl() %>% 
  mutate(yr_2023 = ifelse(is.na(yr_2023), 0, yr_2023), 
         yr_2022 = ifelse(is.na(yr_2022), 0, yr_2022)) %>% 
  mutate(yr_2023 = ifelse(yr_2023 > 5, 5, yr_2023)) %>% 
  mutate(yr_2022 = ifelse(yr_2022 > 5, 5, yr_2022)) %>%
  ggplot() + 
  geom_abline(slope = 1, intercept = 0, linetype = 2) + 
  geom_point(aes(x = yr_2022, y = yr_2023, 
                 color = industry), 
             alpha = 0.5) + 
  theme_minimal() + theme(legend.position = 'none') + 
  facet_wrap(vars(industry), scales = 'free')


# ^ -----
