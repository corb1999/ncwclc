# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
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
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

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

# basic helper functions ------------------------------------------------------------

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

df <- xx %>% select(-metadata_tag)

# ^ -----

# visualize 1 -----------------------------------------------

# box plot of how loss costs have mostly come down in successive years
df %>% filter(loss_cost < 10) %>% filter(industry != "F-Class") %>% 
  ggplot(aes(x = loss_cost, color = as.factor(lc_effective_year))) + 
  geom_boxplot() + 
  facet_wrap(vars(industry)) + 
  theme_minimal() + theme(legend.position = "bottom") + 
  labs(color = "Loss Cost Effective Year", x = "Loss Cost", 
       subtitle = "Class-Level Loss Cost Distributions by Industry Over Time", 
       caption = "Excludes F-Class and classes with LC over $10")

# trend line of loss costs (mean/median) over time
df %>% filter(loss_cost < 10) %>% filter(industry != "F-Class") %>% 
  # mutate(lc_effective_year = as.factor(lc_effective_year)) %>% 
  group_by(industry, lc_effective_year) %>% 
  summarise(lc_mean = mean(loss_cost), 
            lc_med = median(loss_cost)) %>% 
  ggplot(aes(x = lc_effective_year, y = lc_med, color = industry)) + 
  geom_line() + geom_point() + 
  facet_wrap(vars(industry), scales = "free_y") + 
  theme_minimal() + theme(legend.position = "top") + 
  labs(y = "Median Loss Cost", x = "Loss Cost Effective Year",  
       subtitle = "Class-Level Median Loss Cost by Industry Over Time", 
       caption = "Excludes F-Class and classes with LC over $10")
  
# ^ -----
