# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# INTRO ================================
metadatar <- list(script_starttime = Sys.time(), 
                  script_det = list(version_dt = as.Date("2021-09-21"), 
                                    author = "corb", 
                                    proj_name = "ncwclc", 
                                    script_type = "etl", 
                                    notepad = paste0("clean, combine, 
                                                 the loss cost excel files")), 
                  seed_set = 6)
metadatar
# ^ ====================================
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# LOAD LIBRARIES -----------------------------------------------------
R.version.string
Sys.info()
getwd()
library(lobstr)
library(rlang)
library(tidyverse)
library(tidylog)
library(readxl)
library(janitor)
library(lubridate)
library(scales)
# library(gt)
# library(DataExplorer)
# library(reshape2)
library(grid)
# library(caTools)
# library(patchwork)
# library(modelr)
# library(broom)
set.seed(metadatar$seed_set[1])
options(digits = 4, max.print = 99, warnPartialMatchDollar = TRUE, 
        tibble.print_max = 30, scipen = 999, nwarnings = 5)
mem_used()
# ^ -----

# basic helper functions ---------------------------------------------

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

# unionizer helper snipper ------------------------------------------

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# identify dropzone where files are stored and vector the filenames
filepath_prefix_payload <- paste0(getwd(), "/etl/ore")
# put the files list into a dataframe
payload <- data.frame(file_nm = 
                        list.files(path = filepath_prefix_payload)) %>% 
  mutate(file_nm_full = paste0(filepath_prefix_payload, "/", 
                               file_nm))

# test +++++++++++++++++++++++++++
dim(payload)
# payload[1, 1]
# payload[1, 2]

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# set this to the year of the earliest LC file minus 1. This way
#   the following function can tag the file as coming from a certain 
#   year. Not elegant I know, but it should get the job done. 
#   Will only work as long as the files are sorted alphbetically
#  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
lc_year_setter <- 2017
#  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# write a function to read each file the same way into r
fun_readfiles <- function(filepaths) {
  # read the files, but just the cells with the loss costs in them
  xx <- read_excel(path = filepaths, sheet = 1, 
                   range = "B7:N1000", col_names = TRUE)
  # drop any extra rows and drop unwanted columns
  xx <- xx %>% select(1:3, COST) %>% filter(!is.na(CODE)) 
  # fix the column names, prefix delete the ones we dont want
  colnames(xx) <- c("class_code", "special_class", 
                    "industry", "loss_cost")
  # adding the loss cost year
  lc_year_setter <<- lc_year_setter + 1
  xx <- xx %>% mutate(lc_effective_year = lc_year_setter)
  return(xx)}

# test +++++++++++++++++++++++++++
# fun_readfiles(payload[5, 2]) %>% View()

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# execute the file reading purrr, and time it
clockin()
payload_list <- lapply(payload[, 2], FUN = fun_readfiles)
clockout()

# test +++++++++++++++++++++++++++
# payload_list[[2]]
# dim(payload_list[[1]])

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# run checks and validation on the loaded files
payload_stats <- data.frame(col_num_chk = map_dbl(payload_list, 
                                                  ncol), 
                            row_num_chk = map_dbl(payload_list, 
                                                  nrow))

fun_colnames_chk <- function(x) {
  aa <- colnames(x)
  bb <- reduce(aa, paste0)
  return(bb)}

# perform checks !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
payload_stats
length(unique(payload_stats[, 1])) == 1
length(unique(map_chr(payload_list, fun_colnames_chk))) == 1

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# combine into a single dataframe, as long as checks pass
clockin()
df <- map_dfr(payload_list, rbind)
clockout()

# test +++++++++++++++++++++++++++
dim(df)
# viewer(df)

# perform checks !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
sum(payload_stats$row_num_chk) == nrow(df)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# execute any additional filter and manipulation before writing

df <- df %>% 
  mutate(loss_cost = ifelse(loss_cost == "–", NA, loss_cost)) %>% 
  filter(!is.na(loss_cost)) %>% 
  mutate(loss_cost = round(as.numeric(loss_cost), digits = 2), 
         special_class_ind = ifelse(is.na(special_class), FALSE, TRUE), 
         class_code_fct = str_pad(as.character(class_code), 4), 
         class_code_fct = str_replace_all(class_code_fct, 
                                          ' ', '0'), 
         class_code_fct = as_factor(class_code_fct))

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# create and append a metadata tag
metadata_tag <- paste0("unionizer script metadata; ", 
                       "files consumed = ", 
                       nrow(payload_stats), 
                       "; runtime = ", Sys.time(), 
                       "; nrow = ", nrow(df), 
                       "; ncol = ", ncol(df))
metadata_tag
df <- df %>% mutate(metadata_tag = "NA")
df[1, "metadata_tag"] <- metadata_tag


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# write the unionized dataframe out
# write to rds
filename <- paste0(getwd(), "/etl/ingot", "/dataframe.rds")
clockin()
saveRDS(df, file = filename)
clockout()

# ^ -----
