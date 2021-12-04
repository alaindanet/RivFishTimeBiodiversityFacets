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
  tar_target(site_desc_loc, get_site_desc_loc(ts_data = timeseries)),
  tar_target(abun_rich_op, get_abun_rich_op(ts_data = timeseries)),
  tar_target(species_number_site,
    get_species_number_site(
      ts_data = timeseries,
      species_number_by_op = abun_rich_op,
      op_protocol = op_protocol)),
  tar_target(species_status, get_species_status(ts_data = timeseries)),
  tar_target(op_protocol, get_op_protocol(ts_data = timeseries)),
  tar_target(site_protocol_quali, get_site_quali_quanti_protocol(op_data = op_protocol, type = "quali")),
  tar_target(site_protocol_quanti, get_site_quali_quanti_protocol(op_data = op_protocol, type = "quanti")),
  tar_target(measurement, get_abundance_biomass_data(ts_data = timeseries)),
  tar_target(toy_dataset, get_toy_dataset(
  measurement = measurement,
  op_protocol = op_protocol,
  site_protocol_quanti = site_protocol_quanti,
  seed = 123)),


  tar_render(explo, here("vignettes/intro.Rmd")),
  tar_render(report, here("doc/aa-research-questions.Rmd")),

)
