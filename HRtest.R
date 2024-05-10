
# Packages and Globals ----------------------------------------------------------------

library(readxl)
library(tidyverse)
library(lubridate)

YEAR_OF_SERVICE_CALC_DATE <- lubridate::as_date("2024-07-01")
HR_TEST_XLSX_FILEPATH <- "C:/Users/iancj/Documents/Excel_test_HRAnalyst.xlsx"
# Import Data -------------------------------------------------------------

tasks <- readxl::read_excel(HR_TEST_XLSX_FILEPATH, sheet = 'tasks')
hr_data <- readxl::read_excel(HR_TEST_XLSX_FILEPATH, sheet = 'HR DATA')
addl_data <- readxl::read_excel(HR_TEST_XLSX_FILEPATH, sheet = 'ADDL INFO')

# make the `Salary (pro-rated for FTE and Term)` term easier to deal with in script,
# store the name to reapply at the end.
new_sal_col_name <- "sal_prorated"
orig_sal_name <- "Salary (pro-rated for FTE and Term)"
# make sure that dply looks up the right col names...
addl_data <- rename(addl_data, !!new_sal_col_name  := .data[[orig_sal_name]])

# Task 1 - get the max salary employee
# filter to which record's salary is equal to the max in the set of all salaries
max_sal_ee <- filter(addl_data,
                     sal_prorated == max(sal_prorated)) %>%
  select(EmplID, !!sal_prorated)
print(max_sal_ee)

# Task 2 - join Addl Info to HR Data
# similar sql type, joins,  combine the datasets using EmplID as the key
hr_data_all <- left_join(hr_data, addl_data, by = "EmplID")

# Task 6 - calculate years of service. Use basic lubridate functions to determine
# the interval in years, then find the floor of the years. (nobody respects .95 yos hah)
hr_data_all <- mutate(hr_data_all,
                      years_of_service = interval(`Hire Date`, YEAR_OF_SERVICE_CALC_DATE) / years(1)) %>%
  mutate(years_of_service = floor(years_of_service))

# Tasks 3, 4, 5, 7 - calculate raise based on predetermined metrics
# Convert character 'Term' column to numeric
hr_data_all <- mutate(hr_data_all, term_numeric = as.numeric(`Term (Months per year)`))

# Define the function here. fast and dirty way of applying these... could  pull
# that data from an 'official spreadsheet' to avoid hardcoding %s

#' Calculate the percent raise for an employee based on union status,
#' FY, years_of_service and term length
#'
#' @param union Character Union code. One of NU, SU1, U1, U2, or U3.
#' @param fy Numeric Fiscal Year. One of 2025 or 2026.
#' @param years_of_service Numeric integer representing employee years of service.
#' @param term Numeric employee term (Months per year)
calc_raise_perc <- function(union, fy, years_of_service, term) {
  # validate input parameters
  stopifnot(fy %in% c(2025, 2026))
  stopifnot(union %in% c("NU", "SU1", "U1", "U2", "U3"))
  stopifnot(term %in% c(9, 10, 11, 12))

  # barebones dirty way of doing this. At this risk of additional complexity,
  # defining each union in it's own vectorized function would make this easier to
  # understand use going forward.
  raise_perc <- case_when(
      # Define  SU1 Raise Amounts
      union == "SU1" & fy == 2025 ~ .05,
      union == "SU1" & fy == 2026 ~ .04,
      # Define U1, U2, U3 Raise amounts
      union %in% c("U1", "U2", "U3") & fy == 2025 & term %in% c(9, 10) ~ .01,
      union %in% c("U1", "U2", "U3") & fy == 2025 & term %in% c(11, 12) ~ .03,
      union %in% c("U1", "U2", "U3") & fy == 2026 & term %in% c(9, 10) ~ .02,
      union %in% c("U1", "U2", "U3") & fy == 2026 & term %in% c(11, 12) ~ .035,
      #Define NU Raise Amounts
      union == "NU" & fy == 2025 & years_of_service < 1 ~ .02,
      union == "NU" & fy == 2025 & between(years_of_service, 1, 3) ~ .04,
      union == "NU" & fy == 2025 & between(years_of_service, 4, 9) ~ .055,
      union == "NU" & fy == 2025 & years_of_service >= 10 ~ .065,
      union == "NU" & fy == 2026 & years_of_service < 1 ~ .03,
      union == "NU" & fy == 2026 & between(years_of_service, 1, 3) ~ .05,
      union == "NU" & fy == 2026 & between(years_of_service, 4, 9) ~ .06,
      union == "NU" & fy == 2026 & years_of_service >= 10 ~ .0675,
      TRUE ~ NA_real_  # default case if no conditions are met
  )


  return(raise_perc)
}


#apply the YoS calc to  the dataset. Calc the expected FY Salaries from the raise percs
hr_data_all <- mutate(hr_data_all,
                      FY25_raise_perc = calc_raise_perc(Union,
                                                        2025,
                                                        years_of_service,
                                                        term_numeric),
                      FY26_raise_perc = calc_raise_perc(Union,
                                                        2026,
                                                        years_of_service,
                                                        term_numeric),
                      FY25_Salary_Prorated = `Salary (pro-rated for FTE and Term)` * (1 + FY25_raise_perc),
                      FY26_Salary_Prorated = FY25_Salary_Prorated * (1 + FY26_raise_perc))

#TODO: use an existing template for formatting. Insert Raise Calc excel formulas
#
opa::write_report(hr_data_all, "./", "HRTest_supplemented")

