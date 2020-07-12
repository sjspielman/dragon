
#' Fit linear regression given a user-specified response and predictor variable on mineral data in network
#'
#' @param response   Variable to be used as regression response
#' @param predictor  Variable to be used as regression predictor
#' @param mineral_nodes  Tibble containing data to be modeled
#'
#' @returns Named list containing a tidied fitted model tibble ('model_fit'), a tidied fitted Tukey test tibble if performed ('tukey_fit'), a logical indicating if equal or unequal variance across groups associated with Tukey ('tukey_ok_variance')
#' @noRd
fit_linear_model <- function(response, predictor, mineral_nodes)
{

  ## Baseline: if cluster_ID, some may be removed. we want to remember which are being modeled
  keep_clusters <- NA


  ## Prep model -----------------------------------------------------
  response_string <- paste0("`", response, "`")
  predictor_string <- paste0("`", predictor, "`")
  
  if(predictor %in% categorical_model_variables){ 
    # rename to match tibble
    predictor <- "cluster_ID" 
    predictor_col <- as.symbol(predictor)
    
    # remove all clusters with <3 minerals
    mineral_nodes %>%
      dplyr::group_by({{predictor_col}}) %>% 
      dplyr::mutate(num = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::filter(num>=3) %>%
      dplyr::select(-num) -> mineral_nodes
    keep_clusters <- sort( unique(mineral_nodes$cluster_ID) ) ## SORT
    
    # Ensure factor
    predictor_string <- paste0("factor(`", predictor, "`)")
  } 
  fit_string <- paste(response_string, "~", predictor_string)

  ## Build model ------------------------------------------------
  model_fit <- stats::lm(stats::as.formula(fit_string), data = mineral_nodes, na.action = stats::na.omit )
  
  
  # Run a Tukey as needed -------------------------------------------
  tukey_fit_table <- NULL   ## need to return qqch even if not running Tukey
  tukey_ok_variance <- TRUE ## TRUE unless shown FALSE below
  if(predictor == "cluster_ID")
  {
    ## Test for variance assumption and provide warning if not met ----------------------------------------------
    test_variance_pvalue <- stats::bartlett.test(stats::as.formula(fit_string), data = mineral_nodes, na.action = stats::na.omit)$p.value
    if (test_variance_pvalue <= 0.05) tukey_ok_variance <- FALSE

    stats::TukeyHSD(stats::aov(model_fit)) %>%
      broom::tidy() -> tidy_tukey_raw
    # Broom packages aren't well managed (?) for some of the CRAN checks In an INSANE twist of events, column is either `comparison` or `contrast`, and sometimes there is a null.value column.
    # Make the name `contrast`, and be sure to NOT select `null.value`
    raw_names <- names(tidy_tukey_raw)
    if ("comparison" %in% raw_names) tidy_tukey_raw %<>% dplyr::rename(contrast = comparison)
    tidy_tukey_raw %>%    
      dplyr::mutate(contrast  = stringr::str_replace_all(contrast, "-", " - "),
                    estimate    = round(estimate, 6),
                    conf.low    = round(conf.low, 6),
                    conf.high   = round(conf.high, 6),
                    adj.p.value = round(adj.p.value, 6)) %>%
      dplyr::select(contrast, estimate, conf.low, conf.high, adj.p.value) %>%
      dplyr::rename("Cluster Comparison" = contrast, 
                    "Estimated effect size difference" = estimate,
                    "95% CI Lower bound" = conf.low,
                    "95% CI Upper bound" = conf.high,
                    "Adjusted P-value" = adj.p.value) -> tukey_fit_table

    rsquared <- NA
    rsquared.pvalue <- NA
  } else {   
    broom::glance(model_fit) -> glanced
    rsquared <- glanced$r.squared
    rsquared.pvalue <- glanced$p.value
  }
  
  broom::tidy(model_fit) %>%
    dplyr::mutate(term  = stringr::str_replace(term, "cluster_ID", paste0(cluster_ID_str, " ")),
                  term  = stringr::str_replace_all(term, "`", ""),
                  term  = stringr::str_replace_all(term, "factor\\(", ""),
                  term  = stringr::str_replace_all(term, "\\)", ""),
                  estimate = round(estimate, 6),
                  std.error = round(std.error, 6),
                  statistic = round(statistic, 6),
                  p.value   = round(p.value, 6)) %>%
    dplyr::rename("Coefficient"          = term, 
                  "Coefficient estimate" = estimate,
                  "Standard error"       = std.error,
                  "t-statistic"          = statistic,
                  "P-value"              = p.value) -> model_fit_table

  return(list("keep_clusters" = keep_clusters,  
              "model_fit" = model_fit_table,  ## tibble
              "rsquared" = c(rsquared, rsquared.pvalue), # array
              "tukey_fit" = tukey_fit_table,  ## tibble
              "tukey_ok_variance" = tukey_ok_variance ## logical
             )
        )

}


#' Plot results from linear regression, specifically where community cluster is the predictor variable
#' 
#' @param response   Variable used as regression response
#' @param keep_clusters Array of which community clusters were used in modeling and should be plotted
#' @param mineral_nodes  Tibble containing data to visualize
#' @param cluster_colors  Array of pre-specified colors to use across categories in plot
#' @param plot_type  String indicating which type of plot to make, one of 'strip', 'sina', 'violin', or 'boxplot'
#' @param flip_coord Logical indicating if plot coordinates should be flipped. Coordinates are flipped if TRUE, otherwise predictor variable is on the x-axis
#' @param show_mean_se  Logical indicating if mean and standard error per group should be displayed in the plot. Displayed if TRUE.
#' @param show_legend  Logical indicating if the legend should be displayed. Displayed if TRUE. Default: FALSE. 
#' @param point_size  Numeric used as point size for either strip or sina plots. Argument is ignored for boxplot and violin plot.
#' @param show_grid  Logical indicating if a background grid should be displayed in the plot. Displayed if TRUE. Default: FALSE. 
#'
#' @returns ggplot object to be displayed
#' @noRd
plot_linear_model_cluster <- function(response, keep_clusters, mineral_nodes, cluster_colors, plot_type, flip_coord, show_mean_se, show_legend, point_size, show_grid)
{

  stopifnot(all( keep_clusters == sort(keep_clusters) ))
  use_cluster_colors <- cluster_colors[keep_clusters]
  resp <- as.symbol(response)
  ## Build the baseline plot output for models with cluster as predictor ------------------------------------
  mineral_nodes %>%
    dplyr::filter(cluster_ID %in% keep_clusters) %>%
    dplyr::mutate(cluster_ID = factor(cluster_ID)) %>%
    ggplot2::ggplot() + 
      ggplot2::aes(x = cluster_ID, y = {{resp}}) +
      ggplot2::xlab(cluster_ID_str) + 
      ggplot2::ylab(response) +
      ggplot2::theme(legend.text  = ggplot2::element_text(size=12), 
                     legend.title = ggplot2::element_text(size=13)) -> fitted_model_plot
  
  ## Add geom and color/fill ------------------------------------------------------------------------------
  if (plot_type == "strip")
  {
    fitted_model_plot <- fitted_model_plot + 
                  ggplot2::aes(color = cluster_ID) + 
                  ggplot2::geom_jitter(size=point_size, width=0.1) + 
                  ggplot2::scale_color_manual(values = use_cluster_colors, name = cluster_ID_str)
  }
  if (plot_type == "sina")
  {
    fitted_model_plot <- fitted_model_plot + 
                  ggplot2::aes(color = cluster_ID) + 
                  ggforce::geom_sina(size=point_size) +  
                  ggplot2::scale_color_manual(values = use_cluster_colors, name = cluster_ID_str)
  }
  if (plot_type == "violin")
  {
    fitted_model_plot <- fitted_model_plot + 
                  ggplot2::aes(fill = cluster_ID) + 
                  ggplot2::geom_violin() +  
                  ggplot2::scale_fill_manual(values = use_cluster_colors, name = cluster_ID_str)
  }
  if (plot_type == "boxplot")
  {
    fitted_model_plot <- fitted_model_plot + 
                  ggplot2::aes(fill = cluster_ID) +  ## Should we add alpha?
                  ggplot2::geom_boxplot() +  
                  ggplot2::scale_fill_manual(values = use_cluster_colors, name = cluster_ID_str)
  }


  ## Add mean and se -----------------------------------------------------------------------
  if (show_mean_se) 
  {
    fitted_model_plot <- fitted_model_plot + 
                  ggplot2::stat_summary(geom="errorbar", width=0, color = "black", size=1)+
                  ggplot2::stat_summary(geom="point", color = "black", size=3.5)
  }


  ## Flip coordinates -----------------------------------------------------------------------
  if (flip_coord)  fitted_model_plot <- fitted_model_plot + ggplot2::coord_flip()

  ## Legend ---------------------------------------------------------------------------------
  if (!(show_legend))  fitted_model_plot <- fitted_model_plot + ggplot2::theme(legend.position = "none")
  
  ## Grid ------------------------------------------------------------
  if (show_grid)  fitted_model_plot <- fitted_model_plot + cowplot::background_grid()
     
  
  return( fitted_model_plot )
}




#' Plot results from linear regression as a scatterplot
#' 
#' @param response   Variable used as regression response
#' @param predictor   Variable used as regression predictor
#' @param rsquared_info   Array containing the regression's R^2 and p-value (in that order)
#' @param mineral_nodes  Tibble containing data to visualize
#' @param logx Logical indicating if x-axis should be displayed on log10-scale 
#' @param logy Logical indicating if y-axis should be displayed on log10-scale 
#' @param point_color String indicating what color should be used for points in the scatterplot
#' @param point_size Numeric indicating the point size in the scatterplot
#' @param bestfit Logical indicating if regression line should be displayed with 95% confidence interval
#' @param bestfit_color String indicating what color should be used for the regression line. Ignored if bestfit = FALSE.
#' @param show_grid  Logical indicating if a background grid should be displayed in the plot. Displayed if TRUE. Default: FALSE. 
#'
#' @returns ggplot object to be displayed
#' @noRd
plot_linear_model_scatter <- function(response, predictor, rsquared_info, mineral_nodes, logx, logy, point_color, point_size, bestfit, bestfit_color, show_grid)
{

  resp <- as.symbol(response)
  pred <- as.symbol(predictor)
  rsq <- round(rsquared_info[[1]], 3)
  rsqp <- round(rsquared_info[[2]], 3)

  ## Build the scatterplot for models that do NOT HAVE cluster as predictor ------------------------------------
  ggplot2::ggplot(mineral_nodes) + 
    ggplot2::aes(x = {{pred}}, y = {{resp}}) +
    ggplot2::xlab(predictor) + 
    ggplot2::ylab(response) + 
    ggplot2::geom_point(size = point_size, color = point_color) +
    ggplot2::labs(subtitle = bquote(R^2 == .(rsq) ~ "(P = " ~ .(rsqp) ~ ")" )) +
    ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0)) -> fitted_model_plot

  if (logx)      fitted_model_plot <- fitted_model_plot + ggplot2::scale_x_log10()
  if (logy)      fitted_model_plot <- fitted_model_plot + ggplot2::scale_y_log10()
  if (bestfit)   fitted_model_plot <- fitted_model_plot + ggplot2::geom_smooth(method = "lm", color = bestfit_color)
  if (show_grid) fitted_model_plot <- fitted_model_plot + cowplot::background_grid()
  
  return( fitted_model_plot )  
}
