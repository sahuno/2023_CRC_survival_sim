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
library(data.table)
#install.packages("survminer")
#set working directory to the root of the project
#setwd(here::here())


#load data files
resp_data <- read_excel(path = "data/New_results_visualization_20230320.xlsx", n_max = 40)
df_rect <- read_xlsx(path = "data/MSKCC_CRC_c2_results_20220930_v2.xlsx", sheet = 2)

#clean up the data
resp_data <- resp_data %>% as.data.frame()
resp_data <- resp_data %>% janitor::clean_names()

#corce na's to be true NA's
make.true.NA <- function(x)
  if (is.character(x) || is.factor(x)) {
    is.na(x) <- x %in% c("NA", "<NA>")
    x
  } else {
    x
  }

resp_data[] <- lapply(resp_data, make.true.NA)


#convert time to event from years to months
resp_data <-
  resp_data %>% mutate(time_to_event_yrs = round(time_to_event_yrs, 1)) %>%
  tidyr::separate_wider_delim(time_to_event_yrs,
                              ".",
                              names = c("year", "month"),
                              too_few = "align_start") %>% as.data.frame() %>%
  dplyr::mutate(month = as.numeric(month), year = as.numeric(year))  %>%
  dplyr::mutate(month = replace_na(month, 0),
                time_to_event_yearmonth = (year * 12) + month)







######################################################################################
## custom function to plot survival curves for different groups and events
plt_survive <- function(data_in, time_var, event_var, group_var) {
  # Require the required packages inside the function
  requireNamespace("ggplot2", quietly = TRUE)
  requireNamespace("survival", quietly = TRUE)
  requireNamespace("survminer", quietly = TRUE)
  
  #create a survfit object
  fit <-
    survfit2(Surv(get(time_var), get(event_var)) ~ get(group_var), data = data_in) #%>%
  
  #set time intervals to add to the plot
  my_times <- c(12, 24, 36)
  sv <- summary(fit, my_times, extend = TRUE)
  grp <- rep(levels(sv$strata), each = length(my_times))

  surv_custom_intervals_df <- data.table(times = my_times,
                   probs = sv$surv,
                   strata = grp)

#create ggplot2 obect of the survfit object
  p <- fit %>% ggsurvfit(linewidth = 1) +
    add_censor_mark() +
    add_confidence_interval() +
    add_risktable() +
    labs(title = group_var, x = "Time(months)")
  
  plot <- p +
    geom_segment(data = surv_custom_intervals_df,
                 aes(
                   x = times,
                   y = 0,
                   xend = times,
                   yend = probs
                 ),
                 linetype = "dashed") +
    geom_segment(data = surv_custom_intervals_df,
                 aes(
                   x = 0,
                   y = probs,
                   xend = times,
                   yend = probs
                 ),
                 linetype = "dashed")
  
  surv_fit_plus_plots <- list(surv_objects = fit, surv_plots = plot, surv_custom_intervals = surv_custom_intervals_df)
  return(surv_fit_plus_plots)
}


###### test survival function
#call the function
# survival_restaging <-
#   plt_survive(
#     data_in = resp_data,
#     time_var = "time_to_event_yearmonth",
#     event_var = "dfs_event",
#     group_var = "restaging"
#   )

# surv_median(survival_restaging$surv_objects)


# ggsave(
#   survival_restaging$surv_plots,
#   file = "survival_restating_different_time_points.pdf",
#   width = 9,
#   height = 7
# )





######################################################################################
####### Analysis two; create different combinations of follow-up intervals
######################################################################################

#load data
OPRA_ctDNA_noPHI <-
  read_excel(path = "data/OPRA_ctDNA_noPHI.xlsx",
             n_max = 40, #uncomment this if excel file has more than 40 rows; this safety measure is to avoid reading non-data rows
             sheet = 1)
#dim(OPRA_ctDNA_noPHI)

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


#convert all to numeric for downstream analysis
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

# OPRA_ctDNA_noPHI$time_to_event_yearmonth


#select only the columns with test results; may change if looking at different tests like baseline or others
df1 <- OPRA_ctDNA_noPHI %>% dplyr::select(starts_with("c2_test_pf"))



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


# hdf1 <- head(df1)

nCombs <- seq(from = 2, to = 10) #this is the number of follow-up intervals to combine
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


#merge test combinations with main data for survival analysis
data_surv <-
  cbind((OPRA_ctDNA_noPHI %>% dplyr::select(
    c(subject, time_to_event_yearmonth, dfs_event)
  )), compl_combs_data_df)

#remove strnage letters in names and correct duplicated names
data_surv <- data_surv %>% janitor::clean_names()

#recode detection values to detected and not detected
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
cols_2plot <- cols_2plot#[1:100]
create_surv_objs_n_plots_combo_ls <-
  lapply(cols_2plot, function(x) {
    plt_surv <- plt_survive(
      data_in = data_surv_recode_detection_vals,
      time_var = "time_to_event_yearmonth",
      event_var = "dfs_event",
      group_var = x
    )
  })

#set names for easy identification
names(create_surv_objs_n_plots_combo_ls) <- cols_2plot 
length(create_surv_objs_n_plots_combo_ls)
# Error in findrow(fit[i], times, extend) : 
#   no points selected for one or more curves, consider using the extend argument


#names(plots_comb_ls) <- cols_2plot
# names(plots_comb_ls)

##extract ssurvival fitted objects; and get mean survival
fitted_surv_ls <-
  lapply(create_surv_objs_n_plots_combo_ls, function(x)
    x[["surv_objects"]])

names(fitted_surv_ls)

# plots_only_surv_ls <- lapply(create_surv_objs_n_plots_combo_ls, function(x) x[["surv_plots"]])
median_surv_df <- surv_median(fitted_surv_ls, combine = TRUE)
head(median_surv_df)

dir.create(file.path("data", "processed"), showWarnings = FALSE) #ceate processed folder to save data
write_tsv(median_surv_df, file = "data/processed/median_dfs_surv_c2i_FollowUp1-10_combinations.tsv")

#notes: why there is a lot of NA in the survival median and lowe and upper bound
#the median survival time and its 95% confidence interval. For this data, the median is missing (NA) because the survival function never reaches 0.50.
#ref: https://www.bookdown.org/rwnahhas/RMPH/survival-km.html


# save plots
pdf("figures/survival_plots_combinations_of_follow_up_tests1_10.pdf")
lapply(create_surv_objs_n_plots_combo_ls, function(x)
  x[["surv_plots"]])
dev.off()


##### surv times at custom intervals: fish for the good ones
surv_at_custom_intervals_ls <-
  lapply(create_surv_objs_n_plots_combo_ls, function(x)
    x[["surv_custom_intervals"]])
length(surv_at_custom_intervals_ls)
#
surv_at_custom_intervals_df <-  data.table::rbindlist(surv_at_custom_intervals_ls,  idcol="test", fill=TRUE)
fwrite(surv_at_custom_intervals_df, file = "data/processed/surv_at_custom_intervals_c2i_FollowUp1-10_combinations.tsv", sep = "\t")

surv_filter_dt <- surv_at_custom_intervals_df[,`:=`(combo_groups = fcase(str_detect(test, "_or_"), "any_pos_detect", str_detect(test, "_and_"), "all_pos_detect"))][strata == "get(group_var)=Detected",.(max_surv_prob=max(probs)), by=.(combo_groups, test, times)]#[order(times, -max_surv_prob)]
#head(surv_at_custom_intervals_df, 10)
#surv_at_custom_intervals_df %>% dplyr::filter(is.na(strata)) %>% head()
surv_filter_dt_top <- surv_filter_dt%>% group_by(combo_groups, times) %>%   arrange(max_surv_prob, .by_group = TRUE) %>% top_n(1) %>% ungroup()
#surv_filter_dt$top_label <- surv_filter_dt_top

plt_box_surv <- ggplot(surv_filter_dt, aes(x=as.factor(times), y=max_surv_prob)) + geom_boxplot() + geom_jitter(width = 0.2) +  
                #geom_label_repel(data=surv_filter_dt_top, aes(x=as.factor(times), y=max_surv_prob, label=test), min.segment.length = 0,position = position_dodge(width = 1)) + 
                facet_wrap(~combo_groups, scales = "free") +
                labs(x = "Time (months)", y = "Survival probability (Detected group)", title = "DF Survival at intervals for follow-up test") 
ggsave("figures/plt_box_surv.pdf", plt_box_surv, width = 9, height = 7)
#names(surv_at_custom_intervals_ls)

