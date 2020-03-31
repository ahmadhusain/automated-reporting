library(shiny)


# UI ----------------------------------------------------------------------



ui <- fluidPage(
  
  selectInput(
    inputId = "gender", 
    label = "Gender",
    choices = c("female", "male")
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
      
      tempReport <- file.path("temp/template.Rmd")
      file.copy(from = "templated.Rmd", to = tempReport, overwrite = TRUE)
      
      # set up parameters to pas to Rmd document
      
      paramslist <- list(gender = input$gender)
      
      # knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app)
      
      rmarkdown::render(
        input = tempReport,
        output_file = input$filename,
        params = paramslist,
        envir = new.env(parent = globalenv()), 
        output_format = format(),
        output_dir = getwd()
      )
      
    }
    
  )
  
}

shinyApp(ui, server)

