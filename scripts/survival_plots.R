#samuel ahuno
#survival plots for the crc_c2i project 
# date - 08/16/2023
#synopsis: what follow-up interval is associated with the best survival?
#create combinations of follow-up intervals 

#load requierd packages
library(tidyverse)
library(readxl)
library(ggpubr)
library(ggrepel)
library(here)
library(survival)
library(janitor)
library(ggsurvfit)
library(ggsurvfit)
library(survminer)

#set working directory to the root of the project
setwd(here::here())


#load data files
resp_data <-
  read_excel(path = "data/New_results_visualization_20230320.xlsx", n_max = 40)
df_rect <-
  read_xlsx(path = "data/MSKCC_CRC_c2_results_20220930_v2.xlsx", sheet =
              2)

resp_data <- resp_data %>% as.data.frame()
# View(resp_data)
resp_data <- resp_data %>% clean_names()

make.true.NA <- function(x)
  if (is.character(x) || is.factor(x)) {
    is.na(x) <- x %in% c("NA", "<NA>")
    x
  } else {
    x
  }

resp_data[] <- lapply(resp_data, make.true.NA)


# resp_data <- resp_data %>%
#   mutate(interval = case_when(interval == "NA" ~ NA))


# is.na(resp_data$interval)
# resp_data %>% dplyr::filter(is.na(interval))
# resp_data %>% filter(interval == "NA")
names(resp_data)

resp_data <-
  resp_data %>% mutate(time_to_event_yrs = round(time_to_event_yrs, 1)) %>%
  tidyr::separate_wider_delim(time_to_event_yrs,
                              ".",
                              names = c("year", "month"),
                              too_few = "align_start") %>% as.data.frame() %>%
  dplyr::mutate(month = as.numeric(month), year = as.numeric(year))  %>%
  dplyr::mutate(month = replace_na(month, 0),
                time_to_event_yearmonth = (year * 12) + month)






########################
#####make a function to convert years to months
df <-
  data.frame(pid = c("p1", "p2", "p3", "p4"),
             ym = c(2.45, 2.9561644, NA, 3.1))
df <-
  structure(list(
    pid = c("p1", "p2", "p3", "p4"),
    ym = c(2.5, 3,
           NA, 3.1)
  ),
  class = "data.frame",
  row.names = c(NA,-4L))

df2 <- df %>% mutate(ym = round(ym, 1)) %>%
  tidyr::separate_wider_delim(ym, ".", names = c("year", "month"),
                              too_few = "align_start") %>% as.data.frame() %>%
  dplyr::mutate(month = as.numeric(month), year = as.numeric(year))  %>%
  dplyr::mutate(month = replace_na(month, 0),
                time_to_event_yearmonth = (year * 12) + month)

#df %>% mutate(ym = round(ym, 1), yrs_months(ym))


################################
###############################################################
fittedSurvInterval <-
  survfit2(Surv(time_to_event_yearmonth, dfs_event) ~ interval, data = resp_data) %>%
  ggsurvfit(linewidth = 1) +
  add_confidence_interval() +
  add_risktable() + labs(title = "Interval", x = "Time/(months)")
#+ theme(axis.title.y = element_blank())


fittedSurvFollowUp <-
  survfit2(Surv(time_to_event_yearmonth, dfs_event) ~ follow_up, data = resp_data) %>%
  ggsurvfit(linewidth = 1) +
  add_confidence_interval() +
  add_risktable() + labs(title = "Follow-up") + theme(axis.title.y = element_blank())

fittedSurvRestaging <-
  survfit2(Surv(time_to_event_yearmonth, dfs_event) ~ restaging, data = resp_data) %>%
  ggsurvfit(linewidth = 1) +
  add_confidence_interval() +
  add_risktable() + labs(title = "Restaging") + theme(axis.title.y = element_blank())

survAll <-
  cowplot::plot_grid(
    fittedSurvInterval,
    fittedSurvFollowUp,
    fittedSurvRestaging,
    align = "h",
    ncol = 3,
    rel_widths = c(1, 1, 1),
    scale = 0.85,
    label_y = "Percentage Survival"
  )
ggsave(survAll, filename = "figures/DSF_Survival.pdf")





fit_new <-
  survfit2(Surv(time_to_event_yearmonth, dfs_event) ~ restaging, data = resp_data)
my_times <- c(12, 24, 36)

summary(fit_new)
df <- data.frame(times = my_times,
                 probs = summary(fit_new, my_times)$surv)

# data_in %>% dplyr::select({{time_to_event_yearmonth}})

# function argument not found in function
df_test <-
  resp_data %>% dplyr::select(time_to_event_yearmonth, dfs_event, restaging, follow_up) %>% sample_n(6)
df_test <- dput(df_test)


plt_survive <- function(data_in, time_var, event_var, group_var) {
  # Require the required packages inside the function
  requireNamespace("ggplot2", quietly = TRUE)
  requireNamespace("survival", quietly = TRUE)
  requireNamespace("survminer", quietly = TRUE)
  
  fit <-
    survfit2(Surv(get(time_var), get(event_var)) ~ get(group_var), data = data_in) #%>%
  
  my_times <- c(12, 24, 36)
  
  df <- data.frame(times = my_times,
                   probs = summary(fit, my_times)$surv)
  #print(df)
  p <- fit %>% ggsurvfit(linewidth = 1) +
    add_censor_mark() +
    add_confidence_interval() +
    add_risktable() +
    labs(title = group_var, x = "Time(months)")
  
  plot <- p +
    geom_segment(data = df,
                 aes(
                   x = times,
                   y = 0,
                   xend = times,
                   yend = probs
                 ),
                 linetype = "dashed") +
    geom_segment(data = df,
                 aes(
                   x = 0,
                   y = probs,
                   xend = times,
                   yend = probs
                 ),
                 linetype = "dashed")
  
  surv_fit_plus_plots <- list(surv_objects = fit, surv_plots = plot)
  return(surv_fit_plus_plots)
}

#call the function
survival_restaging <-
  plt_survive(
    data_in = resp_data,
    time_var = "time_to_event_yearmonth",
    event_var = "dfs_event",
    group_var = "restaging"
  )
surv_median(survival_restaging$surv_objects)


ggsave(
  survival_restaging$surv_plots,
  file = "survival_restating_different_time_points.pdf",
  width = 9,
  height = 7
)

# result
######################################################################################
####### different time points
######################################################################################
# p <- survfit2(Surv(time, status) ~ surg, data = df_colon) |>
#   ggsurvfit(linewidth = 1) +
#   add_confidence_interval() +
#   add_risktable() +
#   add_quantile(y_value = 0.6, color = "gray50", linewidth = 0.75)


##survival follow up combninations
######################################################################################

OPRA_ctDNA_noPHI <-
  read_excel(path = "data/OPRA_ctDNA_noPHI.xlsx",
             n_max = 40,
             sheet = 1)
dim(OPRA_ctDNA_noPHI)


#clean names
OPRA_ctDNA_noPHI <- OPRA_ctDNA_noPHI %>% as.data.frame()
OPRA_ctDNA_noPHI <- OPRA_ctDNA_noPHI %>% clean_names()

make.true.NA <- function(x)
  if (is.character(x) || is.factor(x)) {
    is.na(x) <- x %in% c("NA", "<NA>")
    x
  } else {
    x
  }

OPRA_ctDNA_noPHI[] <- lapply(OPRA_ctDNA_noPHI, make.true.NA)



head(OPRA_ctDNA_noPHI)
names(OPRA_ctDNA_noPHI)
OPRA_ctDNA_noPHI$c2_test_pf1

#convert  all to numeric
OPRA_ctDNA_noPHI <-
  OPRA_ctDNA_noPHI %>% mutate(across(contains("_test_"), ~ as.numeric(.x)))
#convert years to months
OPRA_ctDNA_noPHI <-
  OPRA_ctDNA_noPHI %>% mutate(time_to_event_yrs = round(time_to_event_yrs, 1)) %>%
  tidyr::separate_wider_delim(time_to_event_yrs,
                              ".",
                              names = c("year", "month"),
                              too_few = "align_start") %>% as.data.frame() %>%
  dplyr::mutate(month = as.numeric(month), year = as.numeric(year))  %>%
  dplyr::mutate(month = replace_na(month, 0),
                time_to_event_yearmonth = (year * 12) + month)

OPRA_ctDNA_noPHI$time_to_event_yearmonth


#create new categories
# OPRA_ctDNA_noPHI <- OPRA_ctDNA_noPHI %>%
#   mutate(c2_test_pf1_pf2 = case_when(c2_test_pf1 == 1 | c2_test_pf2 == 1 ~ 1, TRUE ~ 0),
#          c2_test_pf1_pf2_pf3 = case_when(c2_test_pf1 == 1 | c2_test_pf2 == 1 ~ 1, TRUE ~ 0)
#   )

names(OPRA_ctDNA_noPHI)
df1 <-
  dput(OPRA_ctDNA_noPHI %>% dplyr::select(starts_with("c2_test_pf")))
#df_in=df1

#function to make new combs
make_detection_matrix <-
  function(df_in,
           combs = TRUE,
           n_combs = 2,
           cond = NULL) {
    if (combs == TRUE & n_combs >= 1) {
      detection_combs_ls <- combn(x = df_in,
                                  m = n_combs,
                                  simplify = FALSE)
    } else{
      detection_combs_ls = df_in
    }
    
    #####function to test if any detection was done
    any_detection_fup <- function(x) {
      nr = NROW(x) #extact number of
      tmp_values = matrix(ncol = 1, nrow = nr)
      x[is.na(x)] <- -1
      for (i in 1:nr) {
        if (cond == "all") {
          #detected = all(x[i,]==1)
          allX1 <- all(x[i,] == 1)
          allXmin1 <- all(x[i,] == -1)
          if (allX1) {
            detected = 1
          } else if (allXmin1) {
            detected = NA
          } else{
            detected = 0
          }
          
        } else if (cond == "any") {
          #detected = any(x[i,]==1)
          condAny_allX1 <- any(x[i,] == 1)
          condAny_allXmin1 <- all(x[i,] == -1)
          if (condAny_allX1) {
            detected = 1
          } else if (condAny_allXmin1) {
            detected = NA
          } else{
            detected = 0
          }
        }
        tmp_values[i, 1] = as.numeric(detected)
      }
      if (cond == "any") {
        header = paste(names(x), collapse = "_or_") #extract columnn names
      } else if (cond == "all") {
        header = paste(names(x), collapse = "_and_") #extract columnn names
      }
      colnames(tmp_values) <- header #add combination header
      return(tmp_values)
    }
    
    
    #apply the function
    if (combs == TRUE & n_combs >= 1) {
      #lapply(detection_combs_ls, any_detection_fup(ls_df2[[1]]))
      c2_pf_combinations <-
        lapply(detection_combs_ls, any_detection_fup)
      df_out <- do.call(cbind, c2_pf_combinations)
      #df_out
      return(df_out)
    }
    else {
      return(any_detection_fup(detection_combs_ls))
    }
  }


hdf1 <- head(df1)

nCombs <- seq(from = 2, to = 10)
comb_any_holder_ls <- lapply(nCombs, function(x) {
  ls_combs <- make_detection_matrix(
    df_in = df1,
    combs = TRUE,
    n_combs = x,
    cond = "any"
  )
  return(ls_combs)
})
comb_all_holder_ls <- lapply(nCombs, function(x) {
  ls_combs <- make_detection_matrix(
    df_in = df1,
    combs = TRUE,
    n_combs = x,
    cond = "all"
  )
  return(ls_combs)
})

#add names for easy identification
names(comb_any_holder_ls) <- paste0("comb_any_", nCombs)
names(comb_all_holder_ls) <- paste0("comb_all_", nCombs)
#ttdf <- comb_any_holder_ls[["comb_any_3"]] #sanity checks
#redcuce  everything to data for esay mergig
combs_data_cbind_any_df <-
  as.data.frame(Reduce(cbind, comb_any_holder_ls))
combs_data_cbind_all_df <-
  as.data.frame(Reduce(cbind, comb_all_holder_ls))
class(combs_data_cbind_any_df)
dim(combs_data_cbind_any_df)
dim(combs_data_cbind_any_df)

compl_combs_data_df <-
  as.data.frame(Reduce(
    cbind,
    list(combs_data_cbind_any_df, combs_data_cbind_all_df)
  ))
#compl_combs_data_df <- cbind(as.data.frame(combs_data_cbind_any_df), as.data.frame(combs_data_cbind_all_df)) #substituents

dim(compl_combs_data_df)
#class(cbind_any) #sanity checcks
#make various. combinations
results_c2_pf_comb_2s_any <-
  make_detection_matrix(
    df_in = df1,
    combs = TRUE,
    n_combs = 2,
    cond = "any"
  )
results_c2_pf_comb_3s_any <-
  make_detection_matrix(
    df_in = df1,
    combs = TRUE,
    n_combs = 3,
    cond = "any"
  )
results_c2_pf_comb_2s_all <-
  make_detection_matrix(
    df_in = df1,
    combs = FALSE,
    n_combs = 2,
    cond = "all"
  )
results_c2_pf_comb_3s_all <-
  make_detection_matrix(
    df_in = df1,
    combs = FALSE,
    n_combs = 3,
    cond = "all"
  )
combs_df_ls <-
  list(
    results_c2_pf_comb_2s_any,
    results_c2_pf_comb_3s_any,
    results_c2_pf_comb_2s_all,
    results_c2_pf_comb_3s_all
  )
combs_df_merged <- do.call(cbind, combs_df_ls)
dim(combs_df_merged)
# results_c2_pf_comb_2s_all <- make_detection_matrix(df_in = df1, combs=TRUE, n_combs = 2,cond="all")
# results_c2_pf_comb_3s_all <- make_detection_matrix(df_in = df1, combs=FALSE, n_combs = 3, cond="all")
# results_c2_pf_comb_3s_all <- make_detection_matrix(df_in = df1, combs=FALSE, n_combs = 3, cond="all")


# df_test <- dput(OPRA_ctDNA_noPHI %>% dplyr::select(c(c2_test_pf3, c2_test_pf4)) %>% head())
#
# any_valid_detection <- function(x){
#   nr <- NROW(x)
#   comb_holder <- matrix(data=-1, nrow = nr, ncol = 1)
#   x[is.na(x)] <- -1
#
# for(i in 1:nr){
#
#   if(any(x[i,]==1)){
#     detected = 1
#   }else if(all(x[i,]== -1)){
#     detected = NA
#   }else{
#     detected = 0
#
#   }
#   comb_holder[i,1] <- detected
# }
#   return(comb_holder)
#
# }
# any_valid_detection(x = df_test)
# #which(x == 0 & x == NA)
# any(c(-1, 0) == 1)
# any(c(-1, 1) == 1)
# any(c(0, 0) == 1)
# all(c(-1, -1) == -1) #if all is na

data_surv <-
  cbind((OPRA_ctDNA_noPHI %>% dplyr::select(
    c(subject, time_to_event_yearmonth, dfs_event)
  )), compl_combs_data_df)

#remove strnage letters in names and correct duplicated names
data_surv <- data_surv %>% janitor::clean_names()

# library(data.table)
# hdf1_dt <- copy(hdf1)
# hdf1_dt_cl <- hdf1_dt %>% janitor::clean_names()
# class(hdf1_dt_cl)
#data_surv %>% mutate(across(!c(subject, time_to_event_yearmonth, dfs_event), ~case_when(.x == 1, ~ "Detected", .x == 0, ~ "Not_Detected")))
#recode detections
# duplicated(names(data_surv))
# names(data_surv)[length(names(data_surv))]

data_surv_recode_detection_vals <-
  data_surv %>% mutate(across(
    !c(subject, time_to_event_yearmonth, dfs_event),
    ~ case_when(.x == 1 ~ "Detected", .x == 0 ~ "Not_Detected")
  ))

#call the function
# survival_pf_combs <-
#   plt_survive(
#     data_in = data_surv,
#     time_var = "time_to_event_yearmonth",
#     event_var = "dfs_event",
#     group_var = "c2_test_pf1_or_c2_test_pf2"
#   )
# length(names(data_surv_recode_detection_vals))
cols_2plot <- names(data_surv_recode_detection_vals)[-c(1:3)]
plots_comb_ls <-
  lapply(cols_2plot[1:200], function(x) {
    plt_surv <- plt_survive(
      data_in = data_surv_recode_detection_vals,
      time_var = "time_to_event_yearmonth",
      event_var = "dfs_event",
      group_var = x
    )
  })

#names(plots_comb_ls) <- cols_2plot
# names(plots_comb_ls)

##extract ssurvival fitted objects; and get mean survival
fitted_surv_ls <-
  lapply(plots_comb_ls, function(x)
    x[["surv_objects"]])
# plots_only_surv_ls <- lapply(plots_comb_ls, function(x) x[["surv_plots"]])
median_surv_df <- surv_median(fitted_surv_ls, combine = TRUE)
write_tsv(median_surv_df, file = "median_dfs_surv_c2i_FollowUp1-10_combinations.tsv")

#notes: why there is a lot of NA in the survival median and lowe and upper bound
#the median survival time and its 95% confidence interval. For this data, the median is missing (NA) because the survival function never reaches 0.50.
#ref: https://www.bookdown.org/rwnahhas/RMPH/survival-km.html

# median_surv_ls <- lapply(plots_comb_ls, function(x) surv_median(x[["surv_objects"]]))
# surv_median(plots_comb_ls[[8]][["surv_objects"]])

# names(plots_comb_ls[[1]])
# names(plots_comb_ls[[2]])
# names(plots_comb_ls[[3]])

# save plots
pdf("survival_plots_combinations_of_follow_up_tests1_10.pdf")
lapply(plots_comb_ls, function(x)
  x[["surv_plots"]])
dev.off()

