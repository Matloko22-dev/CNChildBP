#' @title Blood pressure evaluation for Chinese children and adolescents (ages 3-17)
#' @name evaluate_bp
#'
#' @description
#' Evaluate pediatric blood pressure according to the 2017 Chinese
#' sex-/age-/height-specific reference standards. The function includes
#' robust input parsing for non-standard age formats and performs height
#' rounding and percentile-based classification.
#'
#' @details
#' Robust age parsing logic:
#' \itemize{
#'   \item Mixed years and months (e.g. "3 years 5 months") are parsed as \eqn{years + months/12}.
#'   \item Pure numeric values are interpreted as years if <= 18, otherwise as months (divided by 12).
#'   \item Parsed ages are floored to whole years for table lookup.
#' }
#'
#' Height matching:
#' \itemize{
#'   \item Heights are rounded using "round half up": \code{floor(x + 0.5)} before matching table intervals.
#' }
#'
#' Evaluation categories:
#' \itemize{
#'   \item Normal: SBP < P90 and DBP < P90
#'   \item High-normal: P90 ≤ BP < P95, or BP ≥ 120/80 mmHg (but below hypertension thresholds)
#'   \item Stage 1 Hypertension: P95 ≤ BP < P99 + 5 mmHg
#'   \item Stage 2 Hypertension: BP ≥ P99 + 5 mmHg
#' }
#'
#' @param data A data.frame containing measurements to evaluate.
#' @param sex_col Character; name of the sex column (default: "sex").
#' @param age_col Character; name of the age column (default: "age").
#'   Supported formats: numeric, "10", "10.5", "75months", "3y5m", etc.
#' @param height_col Character; name of the height column in cm (default: "height").
#' @param sbp_col Character; name of systolic BP column (default: "sbp").
#' @param dbp_col Character; name of diastolic BP column (default: "dbp").
#' @param language Character; one of "chinese" (default) or "english" for output labels.
#'
#' @return The input data.frame with an added `BP_Evaluation` column.
#'
#' @import dplyr
#' @import stringr
#' @export
#'
#' @examples
#' # 1. Basic usage
#' df_basic <- data.frame(
#'   性别 = c("男", "女"),
#'   年龄 = c(10, 12),
#'   身高 = c(140, 150),
#'   收缩压 = c(110, 130),
#'   舒张压 = c(70, 85)
#' )
#' # evaluate_bp(df_basic)
#'
#' # 2. Handle mixed age formats
#' df_complex <- data.frame(
#'   sex = c("male", "male", "female"),
#'   age = c("75months", "3y5m", "120"), # 6.25y, 3.41y, 10y
#'   height = c(120.5, 98, 140),
#'   sbp = c(110, 90, 130),
#'   dbp = c(70, 60, 85)
#' )
#' # evaluate_bp(df_complex,
#' #             sex_col = "sex", age_col = "age", height_col = "height",
#' #             sbp_col = "sbp", dbp_col = "dbp")
#'
#' # 3. Use English labels
#' # evaluate_bp(df_basic, language = "english")
evaluate_bp <- function(data,
                        sex_col = "\u6027\u522b",
                        age_col = "\u5e74\u9f84",
                        height_col = "\u8eab\u9ad8",
                        sbp_col = "\u6536\u7f29\u538b",
                        dbp_col = "\u8212\u5f20\u538b",
                        language = c("chinese", "english")) {

  # 0. match language argument
  language <- match.arg(language)

  # define labels (Chinese/English)
  labels <- if (language == "chinese") {
    list(
      normal = "\u6b63\u5e38",
      high_normal = "\u6b63\u5e38\u9ad8\u503c",
      stage1 = "1\u671f\u9ad8\u8840\u538b",
      stage2 = "2\u671f\u9ad8\u8840\u538b",
      missing = "\u7f3a\u5c11",
      out_of_range = "\u65e0\u6cd5\u8bc4\u4ef7(\u5e74\u9f84/\u8eab\u9ad8\u8d85\u51fa\u8303\u56f4)"
    )
  } else {
    list(
      normal = "Normal",
      high_normal = "High-normal",
      stage1 = "Stage 1",
      stage2 = "Stage 2",
      missing = "Missing",
      out_of_range = "N/A"
    )
  }

  # 1. check required columns exist
  required_cols <- c(sex_col, age_col, height_col, sbp_col, dbp_col)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("\u6570\u636e\u4e2d\u7f3a\u5c11\u4ee5\u4e0b\u5217: ", paste(missing_cols, collapse = ", "))
  }

  # ========================================================================
  # internal helper: robust age parsing
  # ========================================================================
  parse_smart_age <- function(x) {
    # convert to character and remove whitespace
    x_str <- stringr::str_remove_all(as.character(x), "\\s+")
    # numeric pattern (allow decimals)
    num_pattern <- "\\d+(\\.\\d+)?"

    sapply(x_str, function(s) {
      if (is.na(s) || s == "NA" || s == "") return(NA_real_)

      val <- NA_real_

      # pattern1: mixed years and months (e.g. "3y5m")
      if (stringr::str_detect(s, "\u5c81") && stringr::str_detect(s, "\u6708")) {
        parts <- stringr::str_match_all(s, num_pattern)[[1]][,1]
        if (length(parts) >= 2) {
          # years + months/12
          val <- as.numeric(parts[1]) + as.numeric(parts[2]) / 12
        }
      }
      # pattern2: years only (e.g. "6y" / "6.5y")
      else if (stringr::str_detect(s, "\u5c81")) {
        num <- stringr::str_extract(s, num_pattern)
        val <- as.numeric(num)
      }
      # pattern3: months only (e.g. "75months")
      else if (stringr::str_detect(s, "\u6708")) {
        num <- stringr::str_extract(s, num_pattern)
        val <- as.numeric(num) / 12
      }
      # pattern4: pure numeric strings ("6", "6.5", "74")
      else {
        num_val <- as.numeric(s)
        if (!is.na(num_val)) {
          # 智能阈值判断:
          # 3-17岁是量表区间。18岁以上通常不适用此表。
          # 如果数值 > 18，极大概率是“月龄”(如74月)
          # 如果数值 <= 18，默认为“岁龄”
          if (num_val > 18) {
            val <- num_val / 12
          } else {
            val <- num_val
          }
        }
      }
      return(val)
    })
  }

  # 2. prepare and clean data
  # copy data and add temporary id to preserve row order
  work_data <- data
  work_data$..temp_id.. <- 1:nrow(work_data)

  work_data <- work_data %>%
    dplyr::rename(
      Sex_ = !!dplyr::sym(sex_col),
      Age_Raw_ = !!dplyr::sym(age_col),
      Height_ = !!dplyr::sym(height_col),
      SBP_ = !!dplyr::sym(sbp_col),
      DBP_ = !!dplyr::sym(dbp_col)
    ) %>%
    dplyr::mutate(
      # step A: parse age
      Age_Parsed_ = parse_smart_age(Age_Raw_),

      # step B: standardize
      # age: floor to whole years for table lookup
      Age_Final_ = floor(Age_Parsed_),

      # height: round half up (floor(x + 0.5)) for table matching
      Height_Final_ = floor(Height_ + 0.5)
    )

  # 3. load reference standards
  standards <- CNChildBP::bp_standards

  # 4. matching logic (table lookup)
  matched <- work_data %>%
    # filter out rows with unparsed age/height (will be marked as out-of-range)
    dplyr::filter(!is.na(Age_Final_), !is.na(Height_Final_)) %>%
    # join by sex and age
    dplyr::left_join(standards, by = c("Sex_" = "Sex", "Age_Final_" = "Age")) %>%
    # filter rows within height interval
    dplyr::filter(Height_Final_ >= Height_Lower & Height_Final_ <= Height_Upper)

  # 5. evaluation (based on P90, P95, P99+5)
  results_calculated <- matched %>%
    dplyr::mutate(
      # --- systolic BP (SBP) evaluation ---
      sbp_status = dplyr::case_when(
        is.na(SBP_) ~ labels$missing,
        # 2期: >= P99 + 5 mmHg
        SBP_ >= (SBP_P99 + 5) ~ labels$stage2,
        # 1期: P95 ~ P99 + 5
        SBP_ >= SBP_P95 ~ labels$stage1,
        # 正常高值: P90 ~ P95 或 >= 120 (即使小于P90)
        SBP_ >= SBP_P90 | SBP_ >= 120 ~ labels$high_normal,
        TRUE ~ labels$normal
      ),

      # --- diastolic BP (DBP) evaluation ---
      dbp_status = dplyr::case_when(
        is.na(DBP_) ~ labels$missing,
        DBP_ >= (DBP_P99 + 5) ~ labels$stage2,
        DBP_ >= DBP_P95 ~ labels$stage1,
        DBP_ >= DBP_P90 | DBP_ >= 80 ~ labels$high_normal,
        TRUE ~ labels$normal
      ),

      # --- combined evaluation (take the more severe of SBP/DBP) ---
      BP_Evaluation = dplyr::case_when(
        sbp_status == labels$stage2 | dbp_status == labels$stage2 ~ labels$stage2,
        sbp_status == labels$stage1 | dbp_status == labels$stage1 ~ labels$stage1,
        sbp_status == labels$high_normal | dbp_status == labels$high_normal ~ labels$high_normal,
        sbp_status == labels$missing | dbp_status == labels$missing ~ labels$missing,
        TRUE ~ labels$normal
      )
    ) %>%
    dplyr::select(..temp_id.., BP_Evaluation)

  # 6. merge results back to original data
  final_result <- data
  final_result$..temp_id.. <- 1:nrow(final_result)

  final_result <- final_result %>%
    dplyr::left_join(results_calculated, by = "..temp_id..") %>%
    dplyr::select(-..temp_id..)

  # fill unmatched rows (age/height out of range or missing)
  final_result$BP_Evaluation[is.na(final_result$BP_Evaluation)] <- labels$out_of_range

  return(final_result)
}


## Declare globals to satisfy R CMD check NOTES about undefined globals
 
