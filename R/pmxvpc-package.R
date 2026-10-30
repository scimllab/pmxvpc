#' pmxvpc: Pharmacometrics VPC Plot Generator
#'
#' @keywords internal
#' @importFrom rlang .data
#' @importFrom stats setNames
#' @importFrom utils head
"_PACKAGE"

# Column names referenced via non-standard evaluation inside dplyr/
# ggplot2 verbs (dplyr::group_by(), dplyr::summarise(), aes(), etc.) --
# not real global variables, but R CMD check's static analysis can't
# tell the difference. Declaring them here silences that false
# positive without affecting behavior.
utils::globalVariables(c(
  "Time_Bin", "lower", "median", "upper",
  "p05_CI_lower", "p05_CI_upper",
  "median_CI_lower", "median_CI_upper",
  "p95_CI_lower", "p95_CI_upper",
  "p05_median", "p95_median",
  ".vpc_time_bin", ".vpc_endpoint"
))
