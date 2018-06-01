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
              bs_embed_tooltip(title = "Your name or an anonymous nickname")
          ),
        NULL
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
            #   bs_embed_tooltip(title = "Select features which make up at least 25% of the area of the image.", placement = "botttom")
          ) ,
        bs_modal(id = "help-modal", title = "Feature help", body = HTML(feature_def_table), size = "large"),
        # a("Feature definitions and examples", href = "#", role = "button") %>%
        # bs_attach_modal(id_modal = "help-modal")
        div(
          style = "display: inline-block;margin-right: 5px;",
          checkboxInput(
            inputId = "nofeatures",
            label = "No Features Present", 
            value = FALSE
          )
        ), 
        div(
          style = "display: inline-block;margin-right: 5px;", 
          checkboxInput(
            inputId = "bwfeatures",
            label =  "Features not identifiable in BW version",
            value = FALSE
          )
        )
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
    )
  ),
  fluidRow(
    column(
      width = 6,
      align = "center",
      imageOutput("edgeSlice"),
      br(),
      h3("Context: "),
      imageOutput("edgeFull")
    ),
    column(
      width = 6, 
      align = "center",
      imageOutput("colorSlice"),
      br(),
      h3(HTML("&nbsp")),
      imageOutput("colorFull")
    )
  )
)

server <- function(input, output, session) {
  
  update <- reactiveValues(n = 0, s = 0)
  
  observe({
    update$n
    isolate({
      updateSelectizeInput(session, inputId = "features", selected = "")
      updateCheckboxInput(session, inputId = "nofeatures", value = F)
      updateCheckboxInput(session, inputId = "bwfeatures", value = F)
    })
  })
  
  newSet <- reactive({
    update$s
    conn <- poolCheckout(con)
    q <- sprintf("SELECT image, slice, size, flip, crop 
                  FROM slices 
                  WHERE (image, slice, size) NOT IN (SELECT image, slice, size FROM ratings) 
                  AND RAND()<=0.0001 LIMIT %d;", chunksize)
    z <- dbSendQuery(conn, q)
    zz <- dbFetch(z)
    poolReturn(conn)
    # print(zz)
    zz
  })
  
  newFile <- reactive({
    idx <- (update$n %% chunksize) + 1
    newSet()[idx,]
  })

  output$edgeSlice <- renderImage({
    nf <- newFile()
    filename <- sprintf("%s%s_edge_crop%s_sz%d_%03d.png",
                        nf$image,
                        ifelse(nf$flip == 1, "_flip", ""),
                        nf$crop,
                        nf$size,
                        nf$slice)
    # print(filename)
    fp <- file.path("../", "processed", "slices", filename)
    
    res <- list(src = fp, contentType = "image/png", 
         width = 256, height = 256,  
         style = "background-color: grey;padding:3px; margin:5px;",
         alt = "Slice of edge-detected shoe outsole image")
    # print(res)
    res
  })
  
  output$colorSlice <- renderImage({
    nf <- newFile()
    filename <- sprintf("%s%s_crop%s_sz%d_%03d.png",
                        nf$image,
                        ifelse(nf$flip == 1, "_flip", ""),
                        nf$crop,
                        nf$size,
                        nf$slice)
    
    fp <- file.path("../", "processed", "slices", filename)
    
    res <- list(src = fp, contentType = "image/png", 
         width = 256, height = 256,  
         style = "background-color: grey;padding:3px; margin:5px;",
         alt = "Slice of color shoe outsole image")
    # print(res)
    res
  })
  
  output$edgeFull <- renderImage({
    
    nf <- newFile()
    filename <- sprintf("%s%s_edge_crop%s.png",
                        nf$image,
                        ifelse(nf$flip == 1, "_flip", ""),
                        nf$crop)
    fp <- file.path("../", "processed", "toslice", filename)
    
    res <- list(src = fp, contentType = "image/png", 
                width = "95%", height = "auto",
                alt = "Edge-detected shoe outsole image")
    # print(res)
    res
  })
  
  output$colorFull <- renderImage({
    nf <- newFile()
    filename <- sprintf("%s%s_crop%s.png",
                        nf$image,
                        ifelse(nf$flip == 1, "_flip", ""),
                        nf$crop)
    fp <- file.path("../", "processed", "toslice", filename)
    
    res <- list(src = fp, contentType = "image/png", 
                width = "95%", height = "auto",
                alt = "Color shoe outsole image")
    # print(res)
    res
  })
  
  ## Need to write code to submit ratings to database
  writeDB <- reactive({
    
    if (nchar(input$user) == 0) {
      return("Please add a nickname/user ID")
    }
    if (length(input$features) > 0 | input$nofeatures == TRUE) {
      return("Please select some features")
    }
    
    featurelist <- ifelse(input$nofeatures, "None", input$features)
    
    writedf <- data_frame(
      image = newFile()$image,
      slice = newFile()$slice,
      rater = input$user,
      feature = featurelist,
      colorOnly = as.numeric(input$bwfeatures),
      comment = input$comment
    )
    
    conn <- poolCheckout(con)
    res <- dbWriteTable(conn, "ratings", writedf, append = T)
    poolReturn(conn)
    
    if (length(res) > 0) {
      update$n <- update$n + 1
      update$s <- floor(update$n/chunksize)
      # print(update$n)
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


