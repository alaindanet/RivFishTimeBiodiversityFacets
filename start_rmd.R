knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  cache = FALSE,
  out.width = "100%",
  dev = c("png", "pdf"),
  pdf.options(encoding = "ISOLatin9.enc")
)

source(here::here("R", "misc.R"))
source(here::here("R", "variable_shortcut.R"))
source_dir(here::here("R"))
source(here::here("packages.R"))
theme_set(theme_bw())

tar_config_set(store = here::here("_targets"))


