# Demo: language switching and saving Chinese output
# Purpose: show how to run evaluate_bp() with Chinese/English labels and save UTF-8 output

library(CNChildBP)

# Example data (use English column names to match package examples)
df <- data.frame(
  sex = c("男", "女"),
  age = c(10, 12),
  height = c(140, 150),
  sbp = c(110, 130),
  dbp = c(70, 85),
  stringsAsFactors = FALSE
)

# 1) Evaluate with Chinese labels
res_cn <- evaluate_bp(
  data = df,
  sex_col = "sex",
  age_col = "age",
  height_col = "height",
  sbp_col = "sbp",
  dbp_col = "dbp",
  language = "chinese"
)

print(res_cn$BP_Evaluation)

# 2) Evaluate with English labels
res_en <- evaluate_bp(
  data = df,
  sex_col = "sex",
  age_col = "age",
  height_col = "height",
  sbp_col = "sbp",
  dbp_col = "dbp",
  language = "english"
)

print(res_en$BP_Evaluation)

# 3) Save Chinese-labeled result to CSV with UTF-8 encoding
out_path <- file.path("inst", "examples", "demo_output_cn.csv")
write.csv(res_cn, out_path, row.names = FALSE, fileEncoding = "UTF-8")
cat("Saved Chinese-labeled results to:", out_path, "\n")
