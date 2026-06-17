###################################PRISMA#######################################
library(readr)
file_path <- "C:\\Users\\aanjum\\Box\\BoxPHI-PHMR Projects\\Data\\Prisma Health\\Infectious Disease EHR\\Weekly Data\\RSV\\01_07_2026\\Prisma_Health_Weekly_RSV_State_dx_cond_lab_Burden.csv"

data1 <- read_csv(file_path)
colnames(data1)

file_path2 <- "C:\\Users\\aanjum\\Box\\BoxPHI-PHMR Projects\\Data\\Prisma Health\\Infectious Disease EHR\\Weekly Data\\RSV\\01_14_2026\\Prisma_Health_Weekly_RSV_State_dx_cond_lab_Burden.csv"

data2 <- read_csv(file_path2)

file_path3 <- "C:\\Users\\aanjum\\Box\\BoxPHI-PHMR Projects\\Data\\Prisma Health\\Infectious Disease EHR\\Weekly Data\\RSV\\01_21_2026\\Prisma_Health_Weekly_RSV_State_dx_cond_lab_Burden.csv"

data3 <- read_csv(file_path3)

file_path4 <- "C:\\Users\\aanjum\\Box\\BoxPHI-PHMR Projects\\Data\\Prisma Health\\Infectious Disease EHR\\Weekly Data\\RSV\\01_28_2026\\Prisma_Health_Weekly_RSV_State_dx_cond_lab_Burden.csv"

data4 <- read_csv(file_path4)

file_path5 <- "C:\\Users\\aanjum\\Box\\BoxPHI-PHMR Projects\\Data\\Prisma Health\\Infectious Disease EHR\\Weekly Data\\RSV\\11_24_2025\\Prisma_Health_Weekly_RSV_State_dx_cond_lab_Burden.csv"
data5 <- read_csv(file_path5)

file_path6 <- "C:\\Users\\aanjum\\Box\\BoxPHI-PHMR Projects\\Data\\Prisma Health\\Infectious Disease EHR\\Weekly Data\\RSV\\12_01_2025\\Prisma_Health_Weekly_RSV_State_dx_cond_lab_Burden.csv"
data6 <- read_csv(file_path6)


file_path7 <- "C:\\Users\\aanjum\\Box\\BoxPHI-PHMR Projects\\Data\\Prisma Health\\Infectious Disease EHR\\Weekly Data\\RSV\\12_10_2025\\Prisma_Health_Weekly_RSV_State_dx_cond_lab_Burden.csv"
data7 <- read_csv(file_path7)


file_path8 <- "C:\\Users\\aanjum\\Box\\BoxPHI-PHMR Projects\\Data\\Prisma Health\\Infectious Disease EHR\\Weekly Data\\RSV\\12_17_2025\\Prisma_Health_Weekly_RSV_State_dx_cond_lab_Burden.csv"
data8 <- read_csv(file_path8)


file_path9 <- "C:\\Users\\aanjum\\Box\\BoxPHI-PHMR Projects\\Data\\Prisma Health\\Infectious Disease EHR\\Weekly Data\\RSV\\02_04_2026\\Prisma_Health_Weekly_RSV_State_dx_cond_lab_Burden.csv"

data9 <- read_csv(file_path18)























colnames(data8)

max(data1$Week)
max(data2$Week)
max(data3$Week)
max(data4$Week)
max(data5$Week)
max(data6$Week)
max(data7$Week)
max(data8$Week)


max(data18$Week)















library(dplyr)
library(tidyr)

dataset_names <- paste0("data", 1:9)

combined_data <- lapply(dataset_names, function(df_name) {
  
  df <- get(df_name)
  report_date <- max(df$Week, na.rm = TRUE)
  
  df_clean <- df %>%
    select(State, Week, Weekly_Diagnoses) %>%
    mutate(Report_Week = as.character(report_date))
  
  return(df_clean)
}) %>% 
  bind_rows() 
delay_triangle <- combined_data %>%
  group_by(Week, Report_Week) %>%
  summarise(Weekly_Diagnoses = sum(Weekly_Diagnoses, na.rm = TRUE), .groups = "drop") %>%
  
  pivot_wider(
    names_from = Report_Week, 
    values_from = Weekly_Diagnoses
  ) %>%
  arrange(Week)

print(delay_triangle)




library(dplyr)
library(tidyr)

dataset_names <- paste0("data", 1:9)

combined_data <- lapply(dataset_names, function(df_name) {
  df <- get(df_name)
  report_date <- max(df$Week, na.rm = TRUE)
  
  df_clean <- df %>%
    select(State, Week, Weekly_Diagnoses) %>%
    mutate(Report_Week = as.character(report_date))
  
  return(df_clean)
}) %>% 
  bind_rows()

filtered_combined_data <- combined_data %>%
  mutate(Week = as.Date(Week)) %>%
  filter(Week >= as.Date("2025-11-29"))

delay_triangle <- filtered_combined_data %>%
  group_by(Week, Report_Week) %>%
  summarise(Weekly_Diagnoses = sum(Weekly_Diagnoses, na.rm = TRUE), .groups = "drop") %>%
  
  pivot_wider(
    names_from = Report_Week, 
    values_from = Weekly_Diagnoses
  ) %>%
  arrange(Week)

print(delay_triangle)

















library(dplyr)
library(tidyr)
library(purrr)

find_reporting_week <- function(row_data) {
  week_val <- row_data[[1]]
  vals <- as.numeric(row_data[-1])
  col_names <- names(row_data)[-1]
  
  valid_idx <- !is.na(vals)
  vals <- vals[valid_idx]
  col_names <- col_names[valid_idx]
  
  if(length(vals) == 0) return(NA_character_)
  
  final_value <- tail(vals, 1)
  
  stable_index <- which(vals == final_value)[1]
  
  return(col_names[stable_index])
}

result_df <- delay_triangle %>%
  mutate(reporting_week = purrr::pmap_chr(., function(...) {
    row_data <- list(...)
    find_reporting_week(row_data)
  })) %>%
  select(Week, reporting_week, everything())

print(result_df)














library(dplyr)

result_df <- result_df %>%
  mutate(
    Week = as.Date(Week),
    reporting_week = as.Date(reporting_week)
  )

final_delay_df <- result_df %>%
  mutate(
    reference_week = case_when(
      Week == as.Date("2025-12-20") ~ as.Date("2026-01-03"),
      Week == as.Date("2025-12-27") ~ as.Date("2026-01-03"),
      TRUE                          ~ Week
    ),
    
    weeks_delay = as.numeric(reporting_week - reference_week) / 7
  ) %>%
  
  select(Week, reporting_week, reference_week, weeks_delay, everything())

print(final_delay_df)

















library(dplyr)

result_df <- result_df %>%
  mutate(
    Week = as.Date(Week),
    reporting_week = as.Date(reporting_week)
  )

final_delay_df <- result_df %>%
  mutate(
    reference_week = case_when(
      Week == as.Date("2025-12-20") ~ as.Date("2026-01-03"),
      Week == as.Date("2025-12-27") ~ as.Date("2026-01-03"),
      TRUE                          ~ Week
    ),
    
    weeks_delay = as.numeric(reporting_week - reference_week) / 7,
    
    weeks_delay = case_when(
      Week == as.Date("2025-12-13") ~ 1,
      TRUE                          ~ weeks_delay
    )
  ) %>%
  
  select(Week, reporting_week, reference_week, weeks_delay, everything())

print(final_delay_df)



library(dplyr)

calculate_underreporting_clean <- function(vals) {
  
  valid_vals <- vals[!is.na(vals)]
  
  
  if (length(valid_vals) < 2) return(0)
  
  initial_val <- valid_vals[1]
  final_val   <- tail(valid_vals, 1)
  
  if (final_val == 0) return(0)
  
  pct <- ((final_val - initial_val) / final_val) * 100
  return(round(pct, 2))
}

final_analysis_df <- final_delay_df %>%
  rowwise() %>% 
  mutate(
    
    Underreported_Pct = calculate_underreporting_clean(c_across(matches("^20")))
  ) %>%
  ungroup() %>% 
  
  
  select(Week, reporting_week, reference_week, weeks_delay, Underreported_Pct, everything())

print(final_analysis_df)


















install.packages("NobBS")
library(NobBS)

long_data_for_nowcast <- final_delay_df %>%
    pivot_longer(
    cols = matches("^20"),
    names_to = "report_date",
    values_to = "n"
  ) %>%
  mutate(
    Week = as.Date(Week),
    report_date = as.Date(report_date)
  ) %>%
  filter(!is.na(n)) # NA ভ্যালুগুলো বাদ দেওয়া

nowcast_result <- NobBS(
  data = long_data_for_nowcast,
  now = as.Date("2026-01-31"),       
  onset_date = "Week",               
  report_date = "report_date",       
  cases = "n",                       
  moving_window = 21,                
  max_delay = 3                      
)


print(nowcast_result$estimates)
