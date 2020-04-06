library(shiny)

# UI ----------------------------------------------------------------------

setwd(getwd())

data_avocado <- read.csv("data-avocados.csv")

ui <- fluidPage(

  titlePanel("Avocado Price Report"),
  
  sidebarLayout(
    
    sidebarPanel(
      
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
        selected = "html",
        inline = TRUE
      ),
      
      downloadButton(
        outputId = "report",
        label = "Generate report"
      )
      
    ),
    
    mainPanel(
      
      h3("Main Idea: "),
      
      h4("price elasticity of demand: Price elasticity is used to understand how supply or demanad
        change given changes in price to understand the pattern of sales. For instance, some goods
        are very inelastic, that is, their price do not change very much given changes in supply or
        demand. For example: People need to buy gasoline to get to work, and so if oil price rise,
        people will likely still to buy just the same amount of gas. On the other hand some goods 
        are very elastic, their price moves cause substantial changes in its demand or its supply.
        This report will try to formulated the relation of price and quantity in demand of avocado.")
      
    )
      
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
  
  renderoutput <- reactive({
    
    c(paste0(paste(input$region, input$type, "in", input$year, sep = "-"), ".docx"),
      paste0(paste(input$region, input$type, "in", input$year, sep = "-"), ".pdf"),
      paste0(paste(input$region, input$type, "in", input$year, sep = "-"), ".html"))
    
  })
  
  output$report <- downloadHandler(
    
    filename = function(){
      
      paste0(paste(input$region, input$type, "in", input$year, sep = "-"), switch(
        
        input$format, pdf = ".pdf", html = ".html", word = ".docx", all = ".zip"
        
      ))
      
    },
    
    content = function(file) {
      
      if(input$format == "all") {
        
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
          params = paramslist, clean = TRUE,
          envir = new.env(parent = globalenv()), 
          output_format = format(),
          output_dir = getwd()
        )
        
        zip::zipr(file, renderoutput())
        
        if(file.exists(renderoutput())) {file.remove(renderoutput())}
        
      } else {
        
        tempReport <- file.path("temp/template-elasticity.Rmd")
        file.copy(from = "template-elasticity.Rmd", to = tempReport, overwrite = TRUE)
        
        
        paramslist <- list(
          avocado_region = input$region,
          avocado_type = input$type,
          avocado_year = input$year
        )
        
        
        
        rmarkdown::render(
          output_file = c(paste(input$region, input$type, "in", input$year, sep = "-")),
          input = tempReport,
          params = paramslist, clean = TRUE,
          envir = new.env(parent = globalenv()), 
          output_format = format(),
          output_dir = getwd()
        )
        
      }
      
      
      
    }, 
    contentType = c("application/zip", "application/docx")
  )
  
}

shinyApp(ui, server)

