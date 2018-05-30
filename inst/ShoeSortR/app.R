library(shiny)
library(stringr)
library(tidyverse)
library(bsplus) # Handy bootstrappyness
library(DBI)
library(pool) # Multithread db connection

con <- dbPool(drv = odbc::odbc(), dsn = "shoefeatures-connector")

full_im_dir <- "../processed/toslice"
slice_dir <- "../processed/slices"
addResourcePath("photos", "../photos")
addResourcePath("processed", "../processed")

feature_def_table <- readLines("feature-def-table.txt")

ui <- fluidPage(
  wellPanel(
    fluidRow(
      column(
        width = 3,
        use_bs_tooltip(),
        use_bs_popover(),
        textInput(inputId = "user", label = "Name") %>%
          shinyInput_label_embed(
            shiny_iconlink() %>%
              bs_embed_tooltip(title = "Your name")
          ),
        NULL
      ),
      column(
        width = 5,
        selectizeInput(
          inputId = "features",
          label = "Features in 64x64 chunk",
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
        checkboxInput(
          inputId = "nofeatures",
          label = "No Features Present", 
          value = FALSE
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
      imageOutput("edgeSlice", height = "256px"),
      br(),
      h3("Context: "),
      imageOutput("edgeFull", width = "95%", height = "auto")
    ),
    column(
      width = 6, 
      align = "center",
      imageOutput("colorSlice", height = "256px"),
      br(),
      h3(HTML("&nbsp")),
      imageOutput("colorFull", width = "95%", height = "auto")
    )
  )
)

server <- function(input, output) {
  print(getwd())
  
  newFile <- reactive({
    conn <- poolCheckout(con)
    z <- dbSendQuery(conn, "SELECT image, slice, flip, crop FROM slices 
                                WHERE (image, slice) NOT IN (
                                    SELECT image, slice FROM ratings
                                ) 
                            ORDER BY RAND() LIMIT 1;")
    zz <- dbFetch(z)
    poolReturn(conn)
    zz
  })
  
  ## Need to write code to serve up each image
  
  edge_slice <- reactive({
    filename <- sprintf("%s%s_edge_crop%s_sz64_%03d.png",
                        newFile()$image,
                        ifelse(newFile()$flip == 1, "_flip", ""),
                        newFile()$crop,
                        newFile()$slice)
    
    file.path("../", "processed", "slices", filename)
  })
  
  color_slice <- reactive({
    filename <- sprintf("%s%s_crop%s_sz64_%03d.png",
                        newFile()$image,
                        ifelse(newFile()$flip == 1, "_flip", ""),
                        newFile()$crop,
                        newFile()$slice)
    
    file.path("../", "processed", "slices", filename)
  })
  
  edge_full <- reactive({
    filename <- sprintf("%s%s_edge_crop%s.png",
                        newFile()$image,
                        ifelse(newFile()$flip == 1, "_flip", ""),
                        newFile()$crop)
    file.path("../", "processed", "toslice", filename)
  })
  
  color_full <- reactive({
    filename <- sprintf("%s%s_crop%s.png",
                        newFile()$image,
                        ifelse(newFile()$flip == 1, "_flip", ""),
                        newFile()$crop)
    file.path("../", "processed", "toslice", filename)
  })
  
  output$edgeSlice <- renderImage({
    list(src = edge_slice(), contentType = "image/png", 
         width = 256, height = 256,  
         style = "background-color: grey;padding:3px; margin:5px;",
         alt = "64x64 slice of edge-detected shoe outsole image")
  })
  
  output$colorSlice <- renderImage({
    list(src = color_slice(), contentType = "image/png", 
         width = 256, height = 256,  
         style = "background-color: grey;padding:3px; margin:5px;",
         alt = "64x64 slice of color shoe outsole image")
  })
  
  output$edgeFull <- renderImage({
    dims <- str_extract_all(isolate(newFile()$crop), "\\d{2,}", simplify = T) %>% as.numeric()
    aratio <- dims[1]/dims[2]
    list(src = edge_full(), contentType = "image/png", 
         # width = aratio*256, height = 256,  
         alt = "Edge-detected shoe outsole image")
  })
  
  output$colorFull <- renderImage({
    dims <- str_extract_all(isolate(newFile()$crop), "\\d{2,}", simplify = T) %>% as.numeric()
    aratio <- dims[1]/dims[2]
    list(src = color_full(), contentType = "image/png", 
         # width = aratio*256, height = 256,  
         alt = "Color shoe outsole image")
  })
  
  ## Need to write code to submit ratings to database
  
  # observe({
  #   validate(need(length(input$user) > 0, "Please input your name"))
  #   
  #   writedf <- data_frame(
  #     image = newFile()$image,
  #     slice = newFile()$slice,
  #     rater = input$user,
  #     feature = input$features,
  #     comment = input$comment
  #   )
  #   
  #   conn <- poolCheckout(con)
  #   dbWriteTable(conn, "ratings", writedf, append = T)
  #   poolReturn(conn)
  # })
  
  ## Need to add comment field to rating
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)


