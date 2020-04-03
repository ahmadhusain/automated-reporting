barplot_report <- function(data, xname, title = NULL, sub = NULL, caption = NULL){
  
  library(ggplot2)
  library(rlang)
  
  xname <- sym(xname)
  
  # visualize
  
  
  ggplot(data_viz, aes(x = !!xname, y = n)) +
    geom_col(aes(fill = n)) +
    geom_text(aes(y = n + max(n) * 0.05, label = label)) +
    scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
    guides(fill = FALSE) +
    labs(
      title = title,
      subtitle = sub,
      caption = caption,
      x = NULL,
      y = NULL
    ) +
    theme_minimal()
  
}



densityplot_report <- function(data, xname){
  
  library(ggplot2)
  library(rlang)
  
  xcol <- sym(xname)
  
  xname <- str_replace(xname, pattern = "_", replacement = " ") %>% str_to_title()
  
  # visualize
  ggplot(data, aes(x = !!xcol)) +
    geom_density(fill = "red", alpha = 0.5, colour = FALSE) +
    scale_x_continuous(
      expand = expand_scale(mult = c(0, 0)),
      labels = dollar_format(scale = 1e-3, suffix = "K")
    ) +
    scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
    labs(
      title = paste(xname, "distribution"),
      subtitle = "estimated using kernel density function",
      caption = "Source: IBM Watson",
      x = NULL,
      y = NULL
    ) +
    theme_minimal()
  
}