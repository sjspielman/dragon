fit_linear_model <- function(response, predictor, use_mineral_nodes)
{

  ## Prep model -----------------------------------------------------
  if(predictor == cluster_ID_str){
    # rename to match tibble
    predictor <- "cluster_ID" 
    
    # remove all clusters with <3 minerals
    use_mineral_nodes %>%
      dplyr::group_by(cluster_ID) %>% 
      dplyr::mutate(n = n()) %>%
      dplyr::ungroup() %>%
      dplyr::filter(n>=3) %>%
      dplyr::select(-n) -> use_mineral_nodes 
  } 
  response_string <- paste0("`", response, "`")
  predictor_string <- paste0("`", predictor, "`")
  fit_string <- paste(response_string, "~", predictor_string)

  ## Build model ------------------------------------------------
  model_fit <- lm(stats::as.formula(fit_string), data = use_mineral_nodes, na.action = na.omit )
  
  
  # Run a Tukey as needed -------------------------------------------
  tukey_fit_table <- NULL   ## need to return qqch even if not running Tukey
  tukey_ok_variance <- TRUE ## TRUE unless shown FALSE below
  if(predictor == "cluster_ID")
  {
    ## Test for variance assumption and provide warning if not met ----------------------------------------------
    test_variance_pvalue <- stats::bartlett.test(stats::as.formula(fit_string), data = use_mineral_nodes, na.action = na.omit)$p.value
    if (test_variance_pvalue <= 0.05) tukey_ok_variance <- FALSE

    stats::TukeyHSD(aov(model_fit)) %>%
      broom::tidy() %>%
      dplyr::select(-term) %>%
      dplyr::mutate(comparison  = stringr::str_replace_all(comparison, "-", " - "),
                    estimate    = round(estimate, 6),
                    conf.low    = round(conf.low, 6),
                    conf.high   = round(conf.high, 6),
                    adj.p.value = round(adj.p.value, 6)) %>%
      dplyr::rename("Cluster Comparison" = comparison, 
                    "Estimated effect size difference" = estimate,
                    "95% CI Lower bound" = conf.low,
                    "95% CI Upper bound" = conf.high,
                    "Adjusted P-value" = adj.p.value) -> tukey_fit_table
  }
  broom::tidy(model_fit) %>%
    dplyr::mutate(term  = stringr::str_replace(term, "cluster_ID", paste0(cluster_ID_str, " ")),
                  term  = stringr::str_replace_all(term, "`", ""),
                  estimate = round(estimate, 6),
                  std.error = round(std.error, 6),
                  statistic = round(statistic, 6),
                  p.value   = round(p.value, 6)) %>%
    dplyr::rename("Coefficient"          = term, 
                  "Coefficient estimate" = estimate,
                  "Standard error"       = std.error,
                  "t-statistic"          = statistic,
                  "P-value"              = p.value) -> model_fit_table
  return(list("model_fit" = model_fit_table,  ## tibble
              "tukey_fit" = tukey_fit_table,  ## tibble
              "tukey_ok_variance" = tukey_ok_variance ## logical
             )
        )

}



plot_linear_model <- function(response, predictor, use_mineral_nodes, logx, logy, point_color, bestfit, bestfit_color, cluster_colors)
{
  if(predictor == cluster_ID_str)
  {
    ## Build the strip plot output for models with cluster as predictor ------------------------------------
    ggplot2::ggplot(use_mineral_nodes) + 
      ggplot2::aes(x = cluster_ID, 
                   y = !!sym(response), 
                   color = cluster_ID) + 
      ggplot2::xlab(cluster_ID_str) + 
      ggplot2::ylab(response) +
      ggplot2::geom_jitter(size=3, width=0.1) + 
      ggplot2::scale_color_manual(values = cluster_colors, name = predictor) +
      ggplot2::stat_summary(geom="errorbar", width=0, color = "grey30", size=1)+
      ggplot2::stat_summary(geom="point", color = "grey30", size=3.5) + 
      ggplot2::theme(legend.text  = ggplot2::element_text(size=12), 
                     legend.title = ggplot2::element_text(size=13)) -> fitted_model_plot
  } else {
    ## Build the scatterplot for models that do NOT HAVE cluster as predictor ------------------------------------
    ggplot2::ggplot(use_mineral_nodes) + 
      ggplot2::aes(x = !!sym(predictor), 
                   y = !!sym(response)) +
      ggplot2::xlab(predictor) + 
      ggplot2::ylab(response) + 
      ggplot2::geom_point(size=2, color = point_color) -> fitted_model_plot
  }
  if (logx) fitted_model_plot <- fitted_model_plot + ggplot2::scale_x_log10()
  if (logy) fitted_model_plot <- fitted_model_plot + ggplot2::scale_y_log10()
  if (bestfit) fitted_model_plot <- fitted_model_plot + ggplot2::geom_smooth(method = "lm", color = bestfit_color)
  
  return( fitted_model_plot )  
}
