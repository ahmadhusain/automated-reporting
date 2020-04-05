barplot_report <- function(data, xname,
                           title = NULL,
                           sub = NULL, caption = NULL, 
                           flip = FALSE){
  
  library(ggplot2)
  
  xname <- sym(xname)
  
  plot <- ggplot(data_viz, aes(x = !!xname, y = n)) +
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
  
  # visualize
  
  if(flip = FALSE) {
    
      plot
    
  } else {
    
      plot + coord_flip()
    
  }
  

  
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


get_narative_model <- function(model, target){
  
  
  tidy_estimate <- tidy(model) %>% 
    mutate(term = gsub(term, pattern = "([[:upper:]])", replacement = ' \\1') %>% 
             str_remove(pattern = "[[:punct:]]") %>% 
             str_remove_all(pattern = "_") %>% 
             str_squish())
  
  text <- paste0(
    "We fitted a logistic regression to predict ", 
    target, 
    ".",
    "",
    " The model Intercepet is at ", 
    round(tidy_estimate$estimate[1], digits = 2),
    ". Within this model: <br>"
  )
  
  for (i in 2:nrow(tidy_estimate)) {
    
    text[i] <- paste0(
      i-1,
      ". The effect of ", 
      tidy_estimate$term[i], 
      " is ", 
      ifelse(tidy_estimate$estimate[i] > 0, "positive", "negative"), 
      " with value: ", 
      round(tidy_estimate$estimate[i], digits = 2), 
      "<br>"
    ) 
  }
  
  return(text)
  
}


get_narative_cor <- function(x, y, xname, yname){
  
  temp <- cor.test(x, y)
  paste0(
    "The Pearson's product-moment correlation between ",
    xname, 
    " and ", 
    yname, 
    " is ", 
    ifelse(temp$estimate > 0, "positive ", "negative "), 
    ifelse(temp$p.value < 0.05, "significant", "but not significant enough"), 
    " with a value ", 
    round(temp$estimate, digits = 2)
  )
  
}

string_norm <- function(data){
  
  temp <- data
  
  colnames(temp) <- str_replace_all(colnames(temp), pattern = "_", replacement = " ") %>% 
    str_to_title() %>% 
    str_replace_all(pattern = " ", replacement = "_")
  
  temp <- temp %>% 
    mutate_if(is.character, (~ str_replace_all(pattern = "_", replacement = " ", .) %>% str_to_title() %>% str_replace_all(pattern = " ", replacement = "_", .)))
  
  return(temp)
  
}


kable_report <- 