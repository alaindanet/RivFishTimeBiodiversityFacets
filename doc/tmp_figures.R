tar_load(world_site_sf)
tar_load(p_clust_prop)
tar_load(c(site_cl_rm, site_cl_0))
tar_load(c(bp_cl_dist, p_re_sd))

bp_cl_dist + theme(legend.position = "top")

cl_label <- c(
  "1: No change",
  "2: High turnover",
  "3: Increase of species richness",
  "4: Decrease of species richness",
  "5: Decrease of abundance",
  "6: Increase of abundance"
)

main_map +
  scale_color_discrete(
    name = "Cluster",
    breaks = seq_len(6),
    labels = cl_label
  )




library(cowplot)

rivers10 <- ne_download(
  scale = 10,
  type = 'rivers_lake_centerlines',
  category = 'physical',
  returnclass = "sf")

world_site_sf$cl <- world_site_sf$site %>%
    left_join(site_cl_rm[, c("siteid", "cl")], by = "siteid") %>%
    mutate(cl = relevel(as.factor(cl), ref = 1))

main_map <- ggplot(world_site_sf$world) +
  geom_sf(color = NA) +
  theme_void() +
  geom_sf(data = rivers10, colour = "lightblue", alpha = .8) +
  geom_sf(data = world_site_sf$cl %>%
    filter(!is.na(cl)),
  aes(color = cl))

zoom_map <- list(
  north_am = "USA",
  south_eu = c("FRA", "GBR", "HUN", "ESP", "BEL"),
  north_eu = c("FIN", "NOR", "SWE"),
  aus = "AUS"
)

zoom_map_coord <- map(zoom_map,
  ~world_site_sf$cl %>%
    filter(country %in% .x) %>%
    st_buffer(1) %>%
    st_bbox()
)
zoom_map_coord$north_am[["xmin"]] <- -98

zoom_map_coord_df <- map_dfr(zoom_map_coord,
  ~setNames(as.numeric(.x), names(zoom_map_coord[[1]])) %>%
    enframe(.),
  .id = "zoom") %>%
pivot_wider(names_from = "name", values_from = "value") %>%
mutate(label = LETTERS[seq_len(4)]) 


main_map_rect <- main_map +
  geom_rect(
    data = zoom_map_coord_df,
  aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax),
    fill = NA,
    colour = "black",
    size = 0.6) +
  geom_text(
    data = zoom_map_coord_df,
    aes(x = xmin, y = ymax, label = label),
    vjust = 1, hjust = 1, size = 15
  ) +
theme(legend.position = "none") +
  coord_sf(expand = FALSE) +
  scale_color_discrete(
    name = "Cluster",
    breaks = seq_len(6),
    labels = cl_label
  )

p_zoom_map <- map2(zoom_map_coord, zoom_map_coord_df$label,
  ~main_map_rect +
    coord_sf(
      xlim = .x[c("xmin", "xmax")],
      ylim = .x[c("ymin", "ymax")],
      expand = FALSE) +
    annotate("text", x = .x["xmin"], y = .x["ymax"],
      label = .y, size = 15, vjust = 1, hjust = 0) +
    theme(legend.position = "none",
      panel.background = element_rect(fill = "white"))
)
p_zoom_map[[1]]

# Make the full map
full_map <- ggdraw(main_map_rect) +
  draw_plot({p_zoom_map[[1]]},
    # The distance along a (0,1) x-axis to draw the left edge of the plot
    x = -.1,
    # The distance along a (0,1) y-axis to draw the bottom edge of the plot
    y = .75,
    # The width and height of the plot expressed as proportion of the entire
    # ggdraw object
    width = 0.35, height = 0.25) +
  draw_plot({p_zoom_map[[2]]},
    x = .2, y = .75,
    width = 0.35, height = 0.25) +
  draw_plot({p_zoom_map[[3]]},
    x = .45, y = .5,
    width = 0.35, height = 0.25) +
  draw_plot({p_zoom_map[[4]]},
    x = .6, y = .35,
    width = 0.35, height = 0.25) +
  draw_plot({make_custom_boxplot(bp_cl_dist)},
    x = 0.1, y = -0.03,
    width = 0.8, height = 0.25
  )


p_re_sd2 <- p_re_sd +
  theme(
    panel.background = element_rect(colour = "black", size = 1),
    legend.position = c(.8, .04)
  )
# Legend cluster:
legend_cluster <- get_legend(
  main_map_rect + theme(legend.position = "right"))

full_map +
  draw_plot({p_re_sd2},
    x = .00, y = 0.25,
    width = 0.26, height = 0.25) +
  draw_plot({legend_cluster},
    x = .89, y = 0.48,
    width = 0.1, height = 0.25) 



######################
#  Heat map cluster  #
######################
tar_load(c(clust_var, site_cl_rm))


te <- expand.grid(
  list(
    cluster = c(seq_len(6)),
    response = clust_var
  )
)

avg_cl_var <- site_cl_rm %>%
  select(-riv_str_rc1, -hft_c9309_scaled_no_center) %>%
  pivot_longer(-c(siteid, cl),
    names_to = "response",
    values_to = "value") %>%
  group_by(cl, response) %>%
  summarise(value = median(value), .groups = "drop") %>%
  rename(cluster = cl)

# Ajouter moyenne par cluster
te <- te %>%
  left_join(avg_cl_var, by = c("cluster", "response"))

heat_map_df <- te %>%
  pivot_wider(names_from = "response", values_from = "value")

heat_cl <- te %>%
  ggplot(aes(
      x = get_var_replacement()[response],
      y = str_replace_all(as.character(cluster),
        setNames(cl_label, seq_len(6))),
      fill = value)) +
  geom_tile() +
  hrbrthemes::theme_ipsum(base_family = "Helvetica") +
  coord_fixed() +
  scale_fill_gradient2(
    high = muted("red"),
    mid = "white",
    low = muted("blue"),
    midpoint = 0,
    space = "Lab") +
  labs(x = "", y = "") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

## Boxplot:

bp_theme = theme_minimal( ) +
  theme(legend.position="top", legend.title=element_blank(), panel.grid.major.x=element_blank())

save_plot(p_bp_cl_dist)
