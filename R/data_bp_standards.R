#' Blood pressure reference standards (2017 Chinese standard)
#'
#' A data frame storing the 2017 Chinese sex-/age-/height-specific blood
#' pressure reference values used internally by the package. This dataset is
#' provided for reproducibility and is not exported to the top-level
#' namespace. Users should call it with `CNChildBP::bp_standards` if needed.
#'
#' @docType data
#' @name bp_standards
#' @format A data.frame with columns including (but not limited to):
#' \describe{
#'   \item{Sex}{character: "男" / "女"}
#'   \item{Age}{integer: whole years 3..17}
#'   \item{Height_Lower, Height_Upper}{integer: height interval bounds (cm)}
#'   \item{SBP_P90, SBP_P95, SBP_P99}{numeric: systolic BP percentiles}
#'   \item{DBP_P90, DBP_P95, DBP_P99}{numeric: diastolic BP percentiles}
#' }
#' @keywords data
#' @examples
#' data("bp_standards", package = "CNChildBP")
NULL
