barplot_report <- function(data, xname, yname,
                           title = NULL,
                           sub = NULL, caption = NULL, 
                           flip = FALSE, fillby = NULL, 
                           label = NULL,
                           showlegend = FALSE){
  
  library(ggplot2)
  
  xname <- sym(xname)
  yname <- sym(yname)
  

  
  geomcol <- if(is.null(fillby)) {
    
    geom_col(aes(fill = !!yname)) 
    
  } else {
    
    fillby <- sym(fillby)
    
    geom_col(aes(fill = !!fillby), position = "fill") 
    
  } 
  
  geomtext <- if(!is.null(label)) {
    
    geom_text(aes(y = !!yname + max(!!yname) * 0.075, label = label), size = 3) 
    
  } else {}
  
  legend <- if(showlegend) {
    
    theme(legend.position = "top")
    
  } else {
    
    theme(legend.position = "none")
    
  }
  
  plot <- ggplot(data, aes(x = !!xname, y = !!yname)) +
    geomcol +
    geomtext +
    scale_y_continuous(
      expand = expand_scale(mult = c(0, 0.1)),
      label = number_format(accuracy = 2, big.mark = ",")
    ) +
    labs(
      title = title,
      subtitle = sub,
      caption = caption,
      x = NULL,
      y = NULL,
      fill = fillby
    ) +
    ggthemes::theme_pander() +
    legend
  
  # visualize
  
  if(flip == FALSE) {
    
    plot
    
  } else {
    
    plot + coord_flip()
    
  }
  
  
  
}


densityplot_report <- function(data, xname, 
                               title = NULL, sub = NULL,
                               caption = NULL, fillby = NULL,
                               showlegend = FALSE,
                               axisxcurrency = TRUE){
  
  library(ggplot2)
  
  xcol <- sym(xname)
  
  
  
  geomdensity <- if(is.null(fillby)) {
    
    geom_density(fill = "red", alpha = 0.5, colour = FALSE)
    
  } else {
    
    fillby <- sym(fillby)
    
    geom_density(aes(fill = !!fillby), alpha = 0.5, colour = FALSE)
    
  } 
  
  scalex <- if(axisxcurrency){
    
    scale_x_continuous(
      expand = expand_scale(mult = c(0, 0)),
      labels = dollar_format(big.mark = ",")
    ) 
    
  } else {
    
    scale_x_continuous(
      expand = expand_scale(mult = c(0, 0)),
      labels = number_format(big.mark = ",")
    ) 
    
  }
  
  legend <- if(showlegend) {
    
    theme(legend.position = "top")
    
  } else {
    
    theme(legend.position = "none")
    
  }
  
  # visualize
  ggplot(data, aes(x = !!xcol)) +
    geomdensity +
    scalex +
    scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
    labs(
      title = title,
      subtitle = sub,
      caption = caption,
      x = NULL,
      y = NULL
    ) +
    ggthemes::theme_pander() +
    legend
  
}


scatterplot_report <- function(data, xname, yname,
                           title = NULL, sub = NULL,
                           caption = NULL, fillby = NULL,
                           showlegend = FALSE,
                           axisxcurrency = TRUE,
                           axisycurrency = TRUE){
  
  library(ggplot2)
  
  xcol <- sym(xname)
  ycol <- sym(yname)
  
  
  
  geompoint <- if(is.null(fillby)) {
    
    geom_point(colour = "blue")
    
  } else {
    
    fillby <- sym(fillby)
    
    geom_point(aes(colour = !!fillby)) 
    
  } 
  
  legend <- if(showlegend) {
    
    theme(legend.position = "top")
    
  } else {
    
    theme(legend.position = "none")
    
  }
  
 scaley <- if(axisycurrency){
   
   scale_y_continuous(labels = dollar_format(big.mark = ","))
   
 } else {
   
   scale_y_continuous(labels = number_format(big.mark = ","))
   
 }
 
 scalex <- if(axisxcurrency){
   
   scale_x_continuous(labels = dollar_format(big.mark = ","))
   
 } else {
   
   scale_x_continuous(labels = number_format(big.mark = ",", accuracy = 2))
   
 }
  
  
  ggplot(data, aes(x = !!xcol, y = !!ycol)) +
    geompoint +
    geom_smooth(method = "loess", se = FALSE, colour ="dark red") +
    scalex +
    scaley +
    labs(
      title = title,
      subtitle = sub,
      caption = caption,
      x = NULL,
      y = NULL
    ) +
    ggthemes::theme_pander() +
    legend
  
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
