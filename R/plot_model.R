#' Compare the fixed coefficients of models
#'
#' @param model_list list of model
#' 
plot_model_comp_coeff <- function(model_list = NULL) {

  comp_intercept <- compare_models(model_list, effects = "fixed")
  comp_intercept %>%
    as_tibble() %>%
    select(Parameter, starts_with("CI_"), starts_with("Coefficient")) %>%
    pivot_longer(-Parameter, names_to = "names", values_to = "values") %>%
    separate(col = names, into  = c("type", "response"), sep = "\\.") %>%
    pivot_wider(names_from = "type", values_from = "values") %>%
    filter(!Parameter %in% "(Intercept)", !str_detect(Parameter, "unitabundance")) %>%
    filter(!response %in% c("species_nb", "log_species_nb", "total_abundance", "total_abundance_scaled")) %>%
    ggplot(
      aes(y = Parameter, x = Coefficient, color = response, xmin = CI_low,
        xmax = CI_high)) +
    geom_pointrange(position = position_dodge(width = .2))
}