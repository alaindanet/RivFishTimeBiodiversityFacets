library(targets)
library(tarchetypes)

tar_make()

tar_meta()
tar_visnetwork()

tar_load(c(filtered_dataset, measurement))

tar_load(c(site_desc_loc, filtered_op_protocol, op_protocol))

site_desc_loc %>%
    filter(country == "AUS")

filtered_op_protocol %>%
    left_join(site_desc_loc, by = "siteid") %>%
    filter(country == "AUS")

op_protocol %>%
    left_join(site_desc_loc, by = "siteid") %>%
    filter(country == "AUS")

# Test filtering
opt <- filter_op(
      op_protocol = op_protocol,
      selected_protocol = NULL,
      selected_abun_unit = NULL,
      nb_sampling = 10,
      extent_month = 1.5,
      convert_month_to_date = TRUE,
      return_no_filtered = TRUE 
    )

ti <- opt %>%
    left_join(site_desc_loc, by = "siteid") %>%
    filter( country == "AUS", month_check, quarter_check)

map_dbl(ti, ~sum(is.na(.x)))

output <- ti %>%
    nest_by(siteid, year) %>%
    mutate(
	data = list(
	    data[choose_from_multiple(
		x = data[["month_duration"]],
		quarter = data[["quarter"]],
		mode_quarter = data[["mode_quarter"]]
		), ]
	)
	) %>%
    unnest(cols = c(data)) %>%
    ungroup()

summary_sampling <- output %>%
    group_by(siteid) %>%
    summarise(n = n())
filter(summary_sampling, n >= 5)

summary_sampling <- ti %>%
    group_by(siteid) %>%
    summarise(
	sp = max(year) - min(year) + 1,
	n = n()
    )
filter(summary_sampling, n >= 5, )
filter(summary_sampling, sp >= 10, n >= 5)

mask_nb_sampling <- output$siteid %in%
    summary_sampling[summary_sampling$n >= 10, ]$siteid

output <- output[mask_nb_sampling, ]
