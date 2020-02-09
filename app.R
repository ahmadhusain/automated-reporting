library(shiny)


# UI ----------------------------------------------------------------------



ui <- fluidPage(
  
  sliderInput(
    inputId = "slider", 
    label = "Slider",
    min = 1,
    max = 100,
    value = 50
  ),
  
  textInput(
    inputId = "filename", 
    label = "File name, .pdf or .html", 
    value = "report"
  ), 
  

  radioButtons(
    inputId = "format", 
    label = "Choose output format", 
    choices = c("html", "pdf"), 
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
      "pdf" = "pdf_document"
    )
  })
  

  output$report <- downloadHandler(
    
    filename = input$filename,
    
    content = function(file) {
      
      tempReport <- file.path("temp/report.Rmd")
      file.copy(from = "report.Rmd", to = tempReport, overwrite = TRUE)
      
      # set up parameters to pas to Rmd document
      
      paramslist <- list(n = input$slider)
      
      # knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app)
      
      rmarkdown::render(
        input = tempReport,
        output_file = input$filename,
        params = paramslist,
        envir = new.env(parent = globalenv()), 
        output_format = format(),
        output_dir = getwd(),
        knit_root_dir = getwd()
      )
      
    }
    
  )
  
}

shinyApp(ui, server)

