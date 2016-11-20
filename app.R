library(shiny)
library(foreign)
library(haven)
library(readr)
library(feather)

ingest_function <- function(data_type, data_path) {

  # mapping between input filetype and read function
  switch(data_type,
    ".arff" = read.arff(data_path),
    ".dbf" = read.dbf(data_path),
    ".dta" = read_dta(data_path),
    ".epiinfo" = read.epiinfo(data_path),
    ".mtp" = read.mtp(data_path),
    ".octave" = read.octave(data_path),
    ".rds" = read_rds(data_path),
    ".sas7bdat" = read_sas(data_path),
    ".sav" = read_sav(data_path))
  
  # conditionals for additional args required by read functions, eventually
  
} 

output_function <- function(data_type, target, data_path) {
  
  switch(data_type,
    "csv" = write_csv(target, data_path),
    "dta" = write_dta(target, data_path),
    "feather" = write_feather(target, data_path),
    "rds" = write_rds(target, data_path),
    "sas7bdat" = write_sas(target, data_path),
    "sav" = write_sav(target, data_path))

}

ui <- fluidPage(
  # Application title
  titlePanel("this2that", windowTitle = "no one expects the inquisition."),
  includeCSS("www/simplex.css"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(width = 3,

      selectizeInput("infile_type", label = "which format do you have?",
        choices = c(".arff", ".dbf", ".dta", ".epiinfo", ".mtp", 
          ".octave", ".rds", ".sas7bdat", ".sav"),
        options = list(
        placeholder = 'Select an input data type:',
        onInitialize = I('function() { this.setValue(""); }'))
      ),
      fileInput("infile", label = "upload your data"),
      selectizeInput("outfile_type", label = "which format do you want?",
       choices = c("csv", "dta", "feather", "rds", "sas7bdat", "sav"),
       options = list(
        placeholder = 'Select an output data type:',
        onInitialize = I('function() { this.setValue(""); }'))
       ),
      downloadButton("download")
      ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h2("preview:"),
      dataTableOutput("data_sample")
      )
    )
  )


server <- function(input, output) {

  # ingest file ----------------------------------------------------------------
  imported_data <- eventReactive(input$infile, {
    ingest_function(input$infile_type, input$infile$datapath)
  })
  
  infile_name <- eventReactive(input$infile, {
    sub(x = input$infile$name, pattern = "\\..+$", replacement = "")
  })
  
  output$data_sample <- renderDataTable(imported_data())

  output$download <- downloadHandler(
    filename = function() {
      paste0(infile_name(), ".", input$outfile_type)
    },
    content = function(file) {
      output_function(input$outfile_type, imported_data(), file)
    }
  )

}
  shinyApp(ui, server)