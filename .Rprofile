library(conflicted)
library(targets)
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("clean_names", "janitor")

# For brms
#conflict_prefer("chisq.test", "stats")
#conflict_prefer("fisher.test", "stats")
conflict_prefer("col_factor", "scales")
conflict_prefer("ar", "brms")
conflict_prefer("col_factor", "scales")
conflict_prefer("discard", "scales")
conflict_prefer("extract", "tidyr")
conflict_prefer("group_rows", "dplyr")
conflict_prefer("lag", "dplyr")
#conflict_prefer("inset", "magrittr")
#conflict_prefer("rescale", "scales")
#conflict_prefer("run", "future")
#conflict_prefer("simplify", "purrr")
#conflict_prefer("src", "dplyr")
#conflict_prefer("stamp", "lubridate")

options(
    clustermq.scheduler = "ssh",
    clustermq.ssh.host = "bi1ahd@sharc", # use your user and host, obviously
    clustermq.ssh.log = "~/cmq_ssh.log" # log for easier debugging
)
