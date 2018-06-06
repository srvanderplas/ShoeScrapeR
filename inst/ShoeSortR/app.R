library(shiny)
library(stringr)
library(tidyverse)
library(bsplus) # Handy bootstrappyness
library(DBI)
library(pool) # Multithread db connection

full_im_dir <- "../processed/toslice480"
slice_dir <- "../processed/slices480"
addResourcePath("photos", "../photos")
addResourcePath("processed", "../processed")

chunksize <- 50

feature_def_table <- readLines("feature-def-table.txt")

con <- dbPool(drv = odbc::odbc(), dsn = "shoefeatures-connector")
onStop(function() {
  poolClose(con)
})


ui <- fluidPage(
  wellPanel(
    fluidRow(
      column(
        width = 3,
        use_bs_tooltip(),
        use_bs_popover(),
        textInput(inputId = "user", label = "ID") %>%
          shinyInput_label_embed(
            shiny_iconlink() %>%
              bs_embed_tooltip(title = "Your name or an anonymous nickname", placement = "bottom")
          )
      ),
      column(
        width = 5,
        selectizeInput(
          inputId = "features",
          label = "Features in image chunk",
          choices = c("Circle/Ellipse", "Parallel Lines", "Chevron", 
                      "Triangle", "Star", "Hexagon", "Pentagon", 
                      "Square/Diamond", "Rectangle/Rounded Rectangle",
                      "Perimeter Lug", "Crepe", "Logo", "Contour Lines",
                      "Other"),
          multiple = T
        ) %>%
          shinyInput_label_embed(
            bs_button("Help", button_type="info", button_size = "extra-small") %>%
            bs_attach_modal(id_modal = "help-modal")
          ) ,
        bs_modal(id = "help-modal", title = "Feature help", body = HTML(feature_def_table), size = "large")
      ),
      column(
        width = 2, 
        # tags$label(HTML("&nbsp;")),
        class = "form-group",
        align = "center",
        textInput(
          inputId = "comment",
          label = "Comment"
        )
      ),
      column(
        width = 2,
        class = "form-group",
        align =  "right",
        tags$label(HTML("&nbsp;")),
        tags$br(),        
        actionButton(inputId = "submit", label = "Submit", class = "btn btn-info")
      )
    ),
    fluidRow(
      column(
        width = 12,
        align = "center",
        div(
          style = "display: inline-block;margin-right: 5px;margin-left:auto;",
          checkboxInput(
            inputId = "nofeatures",
            label = "No Useful Features Present/Not a Sole", 
            value = FALSE
          )
        ), 
        div(
          style = "display: inline-block;margin-left: 5px;margin-right: auto;", 
          checkboxInput(
            inputId = "bwfeatures",
            label =  "Features not identifiable in BW version",
            value = FALSE
          )
        )
      )
    )
  ),
  fluidRow(
    column(
      width = 6,
      align = "center",
      imageOutput("edgeSlice", height = "256px"),
      br(),
      h3("Context: "),
      imageOutput("edgeFull")
    ),
    column(
      width = 6, 
      align = "center",
      imageOutput("colorSlice", height = "256px"),
      br(),
      h3(HTML("&nbsp")),
      imageOutput("colorFull")
    )
  ),
  # fluidRow(
  #   column(
  #     width = 12,
  #     textOutput("filePaths")
  #   )
  # )
  NULL
)

server <- function(input, output, session) {
  
  updates <- reactiveValues(n = 0, s = 0)
  
  observe({
    updates$n
    isolate({
      updateSelectizeInput(session, inputId = "features", selected = "")
      updateCheckboxInput(session, inputId = "nofeatures", value = F)
      updateCheckboxInput(session, inputId = "bwfeatures", value = F)
      updateTextInput(session, inputId = "comment", value = "")
    })
  })
  
  newSet <- reactive({
    updates$s
    conn <- poolCheckout(con)
    q <- sprintf("SELECT image, slice, size, flip, crop 
                  FROM slices 
                  WHERE (image, slice, size) NOT IN (SELECT image, slice, size FROM ratings) 
                  AND (image) NOT IN (SELECT  image FROM ratings)
                  AND (size != '64')
                  AND RAND()<=0.0001 LIMIT %d;", chunksize)
    z <- dbSendQuery(conn, q)
    zz <- dbFetch(z)
    poolReturn(conn)
    zz
  })
  
  newFile <- reactive({
    idx <- (updates$n %% chunksize) + 1
    newSet()[idx,]
  })
  
  # filepath <- reactive({
  #   c(edgeSlicePath(), colorSlicePath(), edgeFullPath(), colorFullPath())
  # })
  # 
  # filesexist <- reactive({
  #   sum(file.exists(filepath()))
  # })
  # 
  # observe({
  #   if (filesexist() < 4) {
  #     updates$n <- updates$n + 1
  #     updates$s <- floor(updates$n/chunksize)
  #   }
  # })
  
  edgeSlicePath <- reactive({
    nf <- newFile()
    filename <- sprintf("%s%s_edge_crop%s_sz%d_%03d.png",
                        nf$image,
                        ifelse(nf$flip == 1, "_flip", ""),
                        nf$crop,
                        nf$size,
                        nf$slice)
    fp <- file.path("..", "processed", "slices", filename)
    fp
  })
  
  colorSlicePath <- reactive({
    nf <- newFile()
    filename <- sprintf("%s%s_crop%s_sz%d_%03d.png",
                        nf$image,
                        ifelse(nf$flip == 1, "_flip", ""),
                        nf$crop,
                        nf$size,
                        nf$slice)
    fp <- file.path("..", "processed", "slices", filename)
    fp
  })
  
  edgeFullPath <- reactive({
    nf <- newFile()
    filename <- sprintf("%s%s_edge_crop%s.png",
                        nf$image,
                        ifelse(nf$flip == 1, "_flip", ""),
                        nf$crop)
    fp <- file.path("..", "processed", "toslice", filename)
    fp
  })
  
  colorFullPath <- reactive({
    nf <- newFile()
    filename <- sprintf("%s%s_crop%s.png",
                        nf$image,
                        ifelse(nf$flip == 1, "_flip", ""),
                        nf$crop)
    fp <- file.path("..", "processed", "toslice", filename)
    fp
  })
  
  output$edgeSlice <- renderImage({
    res <- list(src = edgeSlicePath(), contentType = "image/png", 
         width = 256, height = 256,  
         style = "background-color: grey;padding:3px; margin:5px;",
         alt = "Slice of edge-detected shoe outsole image")
    res
  })
  
  output$colorSlice <- renderImage({
    res <- list(src = colorSlicePath(), contentType = "image/png", 
         width = 256, height = 256,  
         style = "background-color: grey;padding:3px; margin:5px;",
         alt = "Slice of color shoe outsole image")
    res
  })
  
  output$edgeFull <- renderImage({
    res <- list(src = edgeFullPath(), contentType = "image/png", 
                width = "95%", height = "auto",
                alt = "Edge-detected shoe outsole image")
    res
  })
  
  output$colorFull <- renderImage({
    res <- list(src = colorFullPath(), contentType = "image/png", 
                width = "95%", height = "auto",
                alt = "Color shoe outsole image")
    res
  })
  
  # output$filePaths <- renderPrint({
  #   filepath()
  # })
  
  writeDB <- reactive({
    if (nchar(input$user) == 0) {
      return("Please add a nickname/user ID")
    }
    if (!(length(input$features) > 0 | input$nofeatures == TRUE)) {
      return("Please select some features")
    }
    
    if (input$nofeatures) {
      featurelist <- "None"
    } else {
      featurelist <- input$features
    }
    
    writedf <- data_frame(
      image = newFile()$image,
      size = as.numeric(newFile()$size),
      crop = newFile()$crop,
      flip = as.numeric(newFile()$flip),
      slice = as.numeric(newFile()$slice),
      rater = input$user,
      feature = featurelist,
      colorOnly = as.numeric(input$bwfeatures),
      comment = input$comment
    ) 
    
    # print(writedf)
    
    conn <- poolCheckout(con)
    res <- dbWriteTable(conn, "ratings", writedf, append = T)
    poolReturn(conn)
    
    if (length(res) > 0) {
      updates$n <- updates$n + 1
      updates$s <- floor(updates$n/chunksize)
      "Database updated!"
    } else {
      "Something went wrong"
    }
  })
  
  observeEvent(input$submit, {
    showNotification(writeDB(), duration = 5)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


