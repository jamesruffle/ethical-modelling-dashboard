library(shiny)

# Define UI for data upload app ----
ui <- fluidPage(
  
  # Application title
  titlePanel("Prototype: The neuroradiology segmentation model ethical calibration dashboard | (c) James Ruffle July 2023"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),
      
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head"),
      
      
      sliderInput("bins",
                  "Histogram: number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      
      sliderInput("dice_range", "Dice Range:",
                  min = 0, max = 1,
                  value = c(0.5,1)),
      
      sliderInput("age_range", "Age range:",
                  min = 18, max = 90,
                  value = c(18,90)),
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      tableOutput("contents"),
      plotOutput("distPlot"),
      plotOutput("scatterplot_dice_age")
      
    )
    
  )
)




server <- shinyServer(function(input, output, session) {
    # added "session" because updateSelectInput requires it


  df <- reactive({ 
    
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    
    df['Dice'] = runif(nrow(df), 0.5, 1)
    df['Precision'] = runif(nrow(df), 0.5, 1)
    df['Recall'] = runif(nrow(df), 0.5, 1)

    return(df)
  })

  output$contents <- renderTable({
    
    if(input$disp == "head") {
      return(head(df()))
    }
    else {
      return(df())
    }
    
  })
  
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x = as.numeric(unlist(df()['Age_at_scan_years']))
    #x = runif(nrow(df()), 0.5, 1)
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white',
         xlab = 'Age (years)',
         main = 'Histogram of patient age at presentation',
    )
    
  })
  
  output$scatterplot_dice_age <- renderPlot({
    # generate bins based on input$bins from ui.R
    plot(unlist(df()['Dice']),unlist(df()['Age_at_scan_years']),
         pch=21,  bg="lightgreen",cex = 1,
         xlab = 'Tumour segmentation dice coefficient',
         ylab= 'Age (years)',
         main = 'Histogram of lesion segmentation performance',
         xlim = input$dice_range,
         ylim = input$age_range,
    )
  })


})


# Run the application 
shinyApp(ui = ui, server = server)