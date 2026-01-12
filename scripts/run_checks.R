# 一键运行包开发检查脚本
# 目的：安装开发依赖、加载包源码、运行 testthat 测试、执行 devtools::check()，并把输出写入 logs

options(repos = c(CRAN = "https://cran.rstudio.com"))

required_pkgs <- c("devtools", "testthat", "rcmdcheck")
inst <- required_pkgs[!required_pkgs %in% installed.packages()[, "Package"]]
if (length(inst) > 0) {
  message("Installing missing packages: ", paste(inst, collapse = ", "))
  install.packages(inst)
}

library(devtools)
library(testthat)

log_dir <- "checks_logs"
if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)

log_out <- file(file.path(log_dir, "run_output.log"), open = "wt")
log_msg <- file(file.path(log_dir, "run_message.log"), open = "wt")

sink(log_out, type = "output")
sink(log_msg, type = "message")

cat("--- Start run_checks.R ---\n")
cat("Time:", Sys.time(), "\n")

res_status <- 0

tryCatch({
  cat("Loading package with devtools::load_all()...\n")
  devtools::load_all()

  cat("Running tests with devtools::test()...\n")
  devtools::test()

  cat("Running devtools::check()... this may take several minutes.\n")
  check_res <- devtools::check()

  saveRDS(check_res, file.path(log_dir, "check_result.rds"))
  cat("devtools::check() finished. Results saved to check_result.rds\n")
}, error = function(e) {
  res_status <<- 1
  cat("ERROR:", conditionMessage(e), "\n")
}, finally = {
  cat("End time:", Sys.time(), "\n")
  cat("--- End run_checks.R ---\n")
  sink(type = "message")
  sink(type = "output")
  close(log_out)
  close(log_msg)
})

# If run non-interactively, set exit code accordingly
if (!interactive()) {
  q(status = res_status)
}
