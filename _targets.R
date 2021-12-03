source("./packages.R")

## Load your R files
lapply(list.files(here("R"), full.names = TRUE), source)

## tar_plan supports drake-style targets and also tar_target()
tar_plan(

# tar_target(target2, function_to_make2(arg)) ## targets style
  tar_target(
    raw_data_file,
    here("inst", "extdata", "GlobalTimeSeries_database_1232021.csv"),
    format = "file"),
  tar_target(timeseries, load_time_series_data(raw_data_file)),


)
