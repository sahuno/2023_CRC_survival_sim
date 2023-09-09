

strcapture('(\\d+).(\\d+)', 2.9561644, proto = list(year = integer(), month = integer()))
conv <- function(x){
  result <- transform(strcapture('(\\d+).(\\d+)', as.character(x), 
                                 proto = list(year = integer(), month = integer())), 
                      #month = round(month/100, 1),
                      time_to_event_yearmonth = (year * 12) + month)#$time_to_event_yearmonth
  return(result)
}





df <- df %>% mutate(ym = round(ym, 1)) %>% dput()
df %>% mutate(ym = round(ym, 1), conv(ym))
df %>% mutate(ym = round(ym, 1)) %>% mutate(transform(strcapture('(\\d+)\\.(\\d+)', as.character(ym), 
                                                                 proto = list(year = integer(), month = integer()))
), time_to_event_yearmonth = (year * 12) + month) 


df %>% tidyr::separate_wider_delim(ym, ".", names = c("year", "time")) 






str(resp_data$time_to_event_yrs)
#first make it into 2 decimal places
#resp_data <- resp_data %>% mutate(time_to_event_yrs = round(time_to_event_yrs, 1),
#  time_to_event_yearmonth = function())
x <- "2.5"
x <- 3.1
x <- c("2Y3M", "3Y0M")
ym <- as.yearmon(x, "%YY%mM")

ym <- as.yearmon(x, "%Y.%M")
class(ym)
12 * as.numeric(ym) + 1

yrs_months <- function(data){
  require(zoo)
  data %>% mutate(ym = 12 * as.numeric(as.yearmon(ym, format="%Y.%m")) + 1)
}

df_c <- df %>% mutate(ym1 = round(ym, 1), ym2 = as.character(ym1), ym_con = (12 * as.numeric(as.yearmon(ym2, "%Y.%m")) + 1))
str(df_c)

df_c <- df %>% mutate(ym = round(ym, 1), ym_con = (12 * as.numeric(as.yearmon(ym, "%Y.%M")) + 1))

df_c <- df %>% mutate(ym = round(ym, 1), ym_con = (12 * as.integer(as.yearmon(ym, "%Y.%M")) + cycle(ym)))


%>%replace_na(list(time = 0))
as.data.frame() %>%
  mutate(time = case_when(is.na(time)) ~ 0, (TRUE ~ time))
df2 %>% mutate(time_months = case_when((!is.na(year) & is.na(time)) ~ 0, (TRUE ~ NA_integer_)))

str(df2)
df2 %>% mutate(time_months = case_when((!is.na(year) & is.na(time)) ~ 0, (!is.na(year) & !is.na(time)) ~ time) )
#TRUE ~ time
#         (!is.na(year) & !is.na(time)) ~ time)
