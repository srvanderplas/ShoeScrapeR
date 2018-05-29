library(shiny)
library(DBI)
library(pool) # Multithread db connection

con <- dbPool(drv = odbc::odbc(), dsn = "shoefeatures-connector")

full_im_dir <- "../processed/toslice"
slice_dir <- "../processed/slices"

ui <- fluidPage(
  titlePanel("Shoe Tread Classification"),
  fluidRow(
    column(
      width = 3,
      wellPanel(
        textInput(inputId = "user", label = "Name"),
        
      )
    ),
    column(
      width = 8,
      wellPanel(
        # Feature ID here
      )
    ),
    column(
      width = 1,
      wellPanel(
        actionButton(inputId = "submit", label = "Submit")
      )
    )
  ),
  fluidRow(
    column(
      width = 6,
      h3("Context Images")
    ),
    column(
      width = 6,
      h3("Images to Evaluate")
    )
  ),
  fluidRow(
    column(
      width = 6,
      imageOutput("edgeFull"), 
      imageOutput("colorFull")
    ),
    column(
      width = 3, 
      imageOutput("edgeSlice")
    ),
    column(
      width = 3,
      imageOutput("colorSlice")
    )
  )
)

server <- function(input, output) {
  
  newFile <- reactive({
    z <- dbSendQuery(con, "SELECT * FROM files WHERE image NOT IN (SELECT image FROM ratings) ORDER BY RAND() LIMIT 1;")
    dbFetch(z)
  })
  
  ## Need to write code to serve up each image
  
  ## Need to write code to submit ratings to database
  
  ## Need to add comment field to rating
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

