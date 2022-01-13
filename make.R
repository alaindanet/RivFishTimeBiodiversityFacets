library(targets)
library(tarchetypes)

tar_make()

tar_meta()
tar_visnetwork()

tar_load(c(filtered_dataset, measurement))

ti <- filtered_dataset$measurement %>%
    nest_by(siteid) %>%
    ungroup()
debugonce(avg_first_year_measurement)

for(i in seq(nrow(ti))) {
  print(i)
  avg_first_year_measurement(ti$data[[i]])
}


test <- furrr::future_map(ti$data, function(x) {
  avg_first_year_measurement(
    x = x,
    nb_sampling_to_average = 3
  )
},
.progress = TRUE
)
