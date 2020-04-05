library(shiny)


# UI ----------------------------------------------------------------------



ui <- fluidPage(
  
  selectInput(
    inputId = "gender", 
    label = "Gender",
    choices = c("Female", "Male")
  ),
  
  selectInput(
    inputId = "marital", 
    label = "Gender",
    choices = c("Single", "Divorced", "Married")
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

      
      paramslist <- list(gender = input$gender, marital = input$marital)
      

      
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

