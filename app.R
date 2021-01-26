library(shiny)
library(shinyMobile)
library(shinyWidgets)
library(dplyr)
library(shinyalert)
library(emo)
library(DT)
library(gfonts)
library(beepr)

options(DT.options = list(
  pageLength = 90,
  dom = "t",
  order = list(
    list(2, "desc"),
    list(1, "desc")
  )
))

# setup fonts

# setup_font(
#   id = "pacifico",
#   output_dir = "~/slammer/WWW"
# )


# background_colors <- c("Aqua", "LightSeaGreen", "steelBlue",
#                        "Indigo", "IndianRed", "LightCoral",
#                        "HoneyDew", "AntiqueWhite", "PeachPuff",
#                        "LemonChiffon", "SeaShell")

ui <- fluidPage(
  
  theme = "bootstrap.min.css",
  titlePanel("El Slammer"),  
  setBackgroundColor("GhostWhite", gradient = "linear", direction = "top"),
  br(),
  useShinyalert(),
  actionButton("group1", emo::ji("poop"), style = "padding:2px; font-size:80px"),
  align = "center",
  actionButton("group2", emo::ji("droplet"), style = "padding:2px; font-size:80px"), 
  actionButton("group3", emo::ji("v"), style = "padding:2px; font-size:80px"), 
  br(),
  br(),
  actionButton("delete_btn", "Delete"),
  
  

  dataTableOutput("table"),
)


server <- function(input, output, session) {
  values <- reactiveValues()
  
  values$df <- data.frame(move = NA, date_time = NA)

  messages <- c(
    paste0("El baño excellente ", emo::ji("nerd")),
    paste0("Get low ", emo::ji("fire")),
    paste0("Good dog! ", emo::ji("winner")), 
    paste0("Awesome! ", emo::ji("ILY")),
    paste0("Drop it like it's hot ", emo::ji("fire")),
    paste0("¡Felicidades! ", emo::ji("clap")), 
    paste0("Who's a good dog?! ", emo::ji("winner")), 
    paste0("Yassss! ", emo::ji("sparkle")), 
    paste0("¡Muy bien! ", emo::ji("dancing")),
    paste0("Who want's a treat? ", emo::ji("drumstick")), 
    paste0("Really good stuff! ", emo::ji("contest")), 
    paste0("Oh Yeaaah! ", emo::ji("perfect")), 
    paste0("Do ya thang! ", emo::ji("nail")), 
    paste0("SLAM! ", emo::ji("boom")),
    paste0("Huzzah! ", emo::ji("hooray"))
  )


  observeEvent(input$group1, {
    shinyalert(sample(messages, 1), emo::ji("dog"))
    insertUI(selector = "#group1",
             where = "afterEnd",
             ui = tags$audio(src = "sound.mp3", type = "audio/mp3", autoplay = NA, controls = NA, style = "display:none;")
    )    
    newLine <- c("Poop", date())
    isolate(values$df <- rbind(values$df, newLine))
    saveData(values$df)
  })

  observeEvent(input$group2, { 
    shinyalert(sample(messages, 1), emo::ji("dog"))
    beep(0)
    newLine <- c("Pee", date())
    isolate(values$df <- rbind(values$df, newLine))
    saveData(values$df)
  })
  
  observeEvent(input$group3, { 
    shinyalert(sample(messages, 1), emo::ji("dog"))
    beep(0)
    newLine <- c("Double Feature", date())
    isolate(values$df <- rbind(values$df, newLine))
    saveData(values$df)
  })
  
  observeEvent(input$delete_btn, {
    tmpshot <- fileSnapshot("~/slammer/responses")
    file_name <- rownames(tmpshot$info[which.max(tmpshot$info$mtime),])
    mistake_file <- paste0("~/slammer/responses/",file_name)
    file.remove(mistake_file)
    session$reload()
  })
  

  output$table <- renderDataTable({
    rownames = FALSE
    values$df
    loadData()
  })

  outputDir <- "responses"

  saveData <- function(data) {
    data <- as.data.frame(data)
    # Create a unique file name
    fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
    # Write the file to the local system
    write.csv(
      x = data,
      file = file.path(outputDir, fileName),
      row.names = FALSE, quote = TRUE
    )
  }
  
  
  loadData <- function() {
    # Read all the files into a list
    files <- list.files(outputDir, full.names = TRUE)
    data <- lapply(files, read.csv)
    # Concatenate all data together into one data.frame
    data <- do.call(rbind, data)
    distinct(data)
  }
}

shinyApp(ui, server)
