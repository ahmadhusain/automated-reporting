library(shiny)

# UI ----------------------------------------------------------------------

setwd(getwd())

data_avocado <- read.csv("data-avocados.csv")


ui <- fluidPage(
  
  selectInput(
    inputId = "region", 
    label = "Region",
    choices = levels(data_avocado$region), 
    selected = "Albani"
  ),
  
  sliderInput(
    inputId = "year", 
    label = "Year",
    min = 2015,
    max = 2018, 
    step = 1,
    value = 2016
  ),
  
  selectInput(
    inputId = "type", 
    label = "Type",
    choices = levels(data_avocado$type),
    selected = "organic"
  ),
  
  radioButtons(
    inputId = "format", 
    label = "Choose output format", 
    choices = c("html", "pdf", "word", "all"), 
    selected = "html"
  ),
  
  downloadButton(
    outputId = "report",
    label = "Generate report"
  )
  
)


# server ------------------------------------------------------------------



server <- function(input, output, session) {
  
  format <- reactive({
    switch (
      input$format,
      "html" = "html_document",
      "word" = "word_document",
      "pdf" = "pdf_document",
      "all" = c("html_document", "word_document", "pdf_document")
    )
  })
  
  
  output$report <- downloadHandler(
    
    filename = paste0(input$region, input$type, "in", input$year, collapse = "-"),
    
    content = function(file) {
      
      tempReport <- file.path("temp/template-elasticity.Rmd")
      file.copy(from = "template-elasticity.Rmd", to = tempReport, overwrite = TRUE)

      
      paramslist <- list(
        avocado_region = input$region,
        avocado_type = input$type,
        avocado_year = input$year
      )
      

      
      rmarkdown::render(
        output_file = c(paste(input$region, input$type, "in", input$year, sep = "-"),
                        paste(input$region, input$type, "in", input$year, sep = "-"),
                        paste(input$region, input$type, "in", input$year, sep = "-")),
        input = tempReport,
        params = paramslist,
        envir = new.env(parent = globalenv()), 
        output_format = format(),
        output_dir = getwd()
      )
      
    }
    
  )
  
}

shinyApp(ui, server)

