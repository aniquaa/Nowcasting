###################################
# RSV NOWCASTING WITH EPINOWCAST
# (consolidated pipeline, same variable names as original script)
###################################

library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(epinowcast)

## ── STEP 0: Load weekly snapshot files ───────────────────────────────────────

file_path  <- "C:\\Users\\aanjum\\Box\\BoxPHI-PHMR Projects\\Data\\Prisma Health\\Infectious Disease EHR\\Weekly Data\\RSV\\01_07_2026\\Prisma_Health_Weekly_RSV_State_dx_cond_lab_Burden.csv"
file_path2 <- "C:\\Users\\aanjum\\Box\\BoxPHI-PHMR Projects\\Data\\Prisma Health\\Infectious Disease EHR\\Weekly Data\\RSV\\01_14_2026\\Prisma_Health_Weekly_RSV_State_dx_cond_lab_Burden.csv"
file_path3 <- "C:\\Users\\aanjum\\Box\\BoxPHI-PHMR Projects\\Data\\Prisma Health\\Infectious Disease EHR\\Weekly Data\\RSV\\01_21_2026\\Prisma_Health_Weekly_RSV_State_dx_cond_lab_Burden.csv"
file_path4 <- "C:\\Users\\aanjum\\Box\\BoxPHI-PHMR Projects\\Data\\Prisma Health\\Infectious Disease EHR\\Weekly Data\\RSV\\01_28_2026\\Prisma_Health_Weekly_RSV_State_dx_cond_lab_Burden.csv"
file_path5 <- "C:\\Users\\aanjum\\Box\\BoxPHI-PHMR Projects\\Data\\Prisma Health\\Infectious Disease EHR\\Weekly Data\\RSV\\11_24_2025\\Prisma_Health_Weekly_RSV_State_dx_cond_lab_Burden.csv"
file_path6 <- "C:\\Users\\aanjum\\Box\\BoxPHI-PHMR Projects\\Data\\Prisma Health\\Infectious Disease EHR\\Weekly Data\\RSV\\12_01_2025\\Prisma_Health_Weekly_RSV_State_dx_cond_lab_Burden.csv"
file_path7 <- "C:\\Users\\aanjum\\Box\\BoxPHI-PHMR Projects\\Data\\Prisma Health\\Infectious Disease EHR\\Weekly Data\\RSV\\12_10_2025\\Prisma_Health_Weekly_RSV_State_dx_cond_lab_Burden.csv"
file_path8 <- "C:\\Users\\aanjum\\Box\\BoxPHI-PHMR Projects\\Data\\Prisma Health\\Infectious Disease EHR\\Weekly Data\\RSV\\12_17_2025\\Prisma_Health_Weekly_RSV_State_dx_cond_lab_Burden.csv"
file_path9 <- "C:\\Users\\aanjum\\Box\\BoxPHI-PHMR Projects\\Data\\Prisma Health\\Infectious Disease EHR\\Weekly Data\\RSV\\02_04_2026\\Prisma_Health_Weekly_RSV_State_dx_cond_lab_Burden.csv"

data1 <- read_csv(file_path)
data2 <- read_csv(file_path2)
data3 <- read_csv(file_path3)
data4 <- read_csv(file_path4)
data5 <- read_csv(file_path5)
data6 <- read_csv(file_path6)
data7 <- read_csv(file_path7)
data8 <- read_csv(file_path8)
data9 <- read_csv(file_path9)

## ── STEP 1: Combine snapshots into long "delay triangle" form ───────────────

dataset_names <- paste0("data", 1:9)

combined_data <- lapply(dataset_names, function(df_name) {
  df <- get(df_name)
  report_date <- max(df$Week, na.rm = TRUE)

  df %>%
    select(State, Week, Weekly_Diagnoses) %>%
    mutate(Report_Week = as.character(report_date))
}) %>%
  bind_rows()

## ── STEP 2: Reshape combined_data directly into epinowcast's long format ────
## epinowcast wants: reference_date (when the event occurred),
## report_date (when it was reported as of), confirm (cumulative count as of
## that report_date). We build this straight from combined_data instead of
## going through the wide delay_triangle, since epinowcast handles the long
## format natively.

long_data <- combined_data %>%
  group_by(Week, Report_Week) %>%
  summarise(confirm = sum(Weekly_Diagnoses, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    reference_date = as.Date(Week),
    report_date    = as.Date(Report_Week),
    confirm        = as.integer(confirm)
  ) %>%
  filter(!is.na(confirm)) %>%
  filter(report_date >= reference_date) %>%
  select(reference_date, report_date, confirm) %>%
  arrange(reference_date, report_date)

## ── STEP 3: Fill in any missing weekly report AND reference weeks ────────────
## epinowcast needs BOTH reference_date and report_date to be evenly spaced
## (7-day timestep). Missing weeks can show up on either axis — e.g. a
## snapshot file that skipped a state/week, or a holiday week with no rows.
## We build complete weekly sequences for both and fill in the gaps.

all_report_dates <- seq(
  from = min(long_data$report_date),
  to   = max(long_data$report_date),
  by   = "week"
)

all_reference_dates <- seq(
  from = min(long_data$reference_date),
  to   = max(long_data$reference_date),
  by   = "week"
)

long_data_filled <- long_data %>%
  complete(
    reference_date = all_reference_dates,
    report_date    = all_report_dates
  ) %>%
  filter(report_date >= reference_date) %>%
  arrange(reference_date, report_date) %>%
  group_by(reference_date) %>%
  fill(confirm, .direction = "down") %>%   # carry forward last known cumulative count
  mutate(confirm = if_else(is.na(confirm), 0L, as.integer(confirm))) %>% # nothing reported yet -> 0
  ungroup()

## Sanity check: gaps should now all be exactly 7 days on BOTH axes
long_data_filled %>%
  distinct(report_date) %>%
  arrange(report_date) %>%
  mutate(gap = as.numeric(report_date - lag(report_date)))

long_data_filled %>%
  distinct(reference_date) %>%
  arrange(reference_date) %>%
  mutate(gap = as.numeric(reference_date - lag(reference_date)))

## ── STEP 4: Set "now" and cache location ─────────────────────────────────────

enw_set_cache(
  tools::R_user_dir(package = "epinowcast", "cache"),
  type = c("session", "persistent")
)

now_date <- max(long_data_filled$report_date)
cat("Nowcasting as of:", as.character(now_date), "\n")

## ── STEP 5: Preprocess ────────────────────────────────────────────────────────

preprocessed <- enw_preprocess_data(
  obs       = long_data_filled,
  by        = NULL,
  max_delay = 2,
  timestep  = "week"
)

preprocessed

## ── STEP 6: Fit the model ─────────────────────────────────────────────────────

rsv_nowcast <- epinowcast(
  data        = preprocessed,
  expectation = enw_expectation(~ 1, data = preprocessed),
  report      = enw_report(~ 1, data = preprocessed),
  reference   = enw_reference(~ 1, data = preprocessed),
  obs         = enw_obs(family = "negbin", data = preprocessed),
  fit = enw_fit_opts(
    save_warmup   = FALSE,
    pp            = TRUE,
    chains        = 2,
    iter_warmup   = 500,
    iter_sampling = 1000,
    adapt_delta   = 0.95
  )
)

## ── STEP 7: Inspect results ───────────────────────────────────────────────────

# Summary of fitted model
summary(rsv_nowcast)

# Extract nowcast estimates (posterior draws summarized by reference_date)
nowcast_summary <- summary(rsv_nowcast, type = "nowcast")
print(nowcast_summary)

# Plot the nowcast against reported data
plot(rsv_nowcast, type = "nowcast")

