library(shiny)
library(dplyr)
library(ggplot2)
library(bslib)
library(arrow)
library(plotly)
library(tuneR)
library(scales)
library(gganimate)
library(tidyverse)
library(purrr)
library(sonify)
library(pastecs)
library(seewave)
library(signal)
library(soundgen)
library(base64enc)
library(shinyjs)


# set wave player - Note this is for mac. 
#setWavPlayer("afplay")

data <- arrow::read_parquet("data/sharing/all_pwqn_data.parquet")
sites <- unique(data$site)
parameters <- unique(data$parameter)

#walk(list.files(path = "src/", full.names = T), source)


ui <- page_navbar(
  title = "Water Data Sonification",
  nav_panel(
    title = "Data Exploration",
    layout_sidebar(
      sidebar = sidebar(
        title = "Select Data",
        selectInput("site", "Site:", choices = sites, selected = "cottonwood", multiple = T),
        selectInput("parameter", "Parameter:", choices = parameters, selected = "DO", multiple = T),
        dateRangeInput("dateRange", "Date Range:", 
                       min = as.Date("2023-01-01"),
                       max = as.Date("2024-12-01"), start = "2023-01-01", end = "2023-12-01" 
        ),
        selectInput("timestep", "Timestep:", choices = c("15 minute", "1 hour", "6 hour", "12 hour", 
                                                         "1 day", "1 week", "1 month"), 
                    selected = "1 hour"),
        actionButton("plotBtn", "Plot Data", class = "btn-primary"),
      ),
      # Main content area with plot card in the middle and sonification on the right
      layout_columns(
        # Plot card takes up more space (width 8/12)
        col_widths = c(8, 4),
        
        # Main plot card
        card(
          card_header("Data Plot"),
          plotlyOutput("dataPlot")
        ),
        
        # Sonification card on the right
        card(
          card_header("Sonification Parameters"), 
          numericInput("totalSeconds", "Total Duration (seconds):", value = 120, min = 10, max = 300),
          numericInput("noteLength", "Note Length (seconds):", value = 0.2, min = 0.1, max = 1, step = 0.1),
          #numericInput("refFreq", "Reference Frequency (Hz):", value = 220, min = 100, max = 500),
          numericInput("octave", "Central Octave:", value = 3, min = 0, max = 7),
          numericInput("octaves", "Octave Range:", value = 3, min = 1, max = 5),
          selectInput("key", "Key:", 
                      choices = c("Ab","A","Bb","B","C","C#","D","Eb","E","F","F#","G"),
                      selected = "A"),
          selectInput("waveType", "Wave Type:",
                      choices = c("sine", "square", "triangle", "sawtooth"),
                      selected = "triangle"),
          selectInput("scale", "Musical Scale:",
                      choices = c("major", "minor"),
                      selected = "minor"), 
          selectInput("feedback_opt", "Feedback:", choice = c(TRUE,FALSE), selected = FALSE),
          actionButton("playBtn", "Play Audio", class = "btn-info"), 
          uiOutput("audioPlayer"),
          downloadButton("downloadBtn", "Download Wave")
        )
      )
    )
  )
)

server <- function(input, output, session){

  
  # Filtered data based on user selection
  filtered_data <- eventReactive(input$plotBtn, {
    req(data, input$site, input$parameter, input$dateRange, input$timestep)
    
    data %>%
      dplyr::filter(
        site %in% input$site,
        parameter %in% input$parameter,
        DT_round >= input$dateRange[1],
        DT_round <= input$dateRange[2]
      ) %>%
      arrange(DT_round)%>%
      #adjust for user timestep
      mutate(DT_round = round_date(DT_round,unit = input$timestep))%>%
      group_by(DT_round, site, parameter) %>%
      summarise(clean_mean = mean(clean_mean, na.rm = TRUE), .groups = 'drop') %>%
      ungroup()
      
  })
  
  # Plot the data
  output$dataPlot <- renderPlotly({
    req(filtered_data())
    
    #Don't show anything if there is no data
    if(nrow(filtered_data()) == 0) {
      return(NULL)
    }
    
    p <- ggplot(filtered_data(), aes(x = DT_round, y = clean_mean, color = site)) +
      geom_line() +
      geom_point() +
      facet_wrap(~ parameter, scales = "free_y") +
      theme(strip.placement = "outside", strip.background = element_blank())+
      labs(
        x = "Date",
        y = ""
      ) +
      theme_bw()
    
    ggplotly(p)
  })
  
  
  #create conditional UI so that playBtn can only be clicked if filtered data contains data

  observe({
    if (nrow(filtered_data()) == 0) {
      shinyjs::disable("playBtn")
      shinyjs::disable("downloadBtn")
    } else {
      shinyjs::enable("playBtn")
      shinyjs::enable("downloadBtn")
    }
  })
  

  #when playBtn is clicked, play the generated sound

  observeEvent(input$playBtn, {
    
    cat("Memory before:", pryr::mem_used(), "\n")
    
    sounds <- sonify_data2(filtered_data()$clean_mean,
                           note_length = input$noteLength,
                           to_plot = FALSE,
                           total_seconds = input$totalSeconds,
                           #ref_freq = input$refFreq,
                           key = input$key,
                           octave = input$octave,
                           octaves = input$octaves,
                           wave_type = input$waveType,
                           scale = input$scale, 
                           feedback = input$feedback_opt,
                           to_play = FALSE
    )
    
    # Convert Wave object to base64
    temp_file <- tempfile(fileext = ".wav")
    tuneR::writeWave(sounds[[1]], temp_file)
    
    # Read file and encode to base64
    audio_data <- base64enc::base64encode(temp_file)
    
    # Clean up temp file
    file.remove(temp_file)
    
    # Create HTML5 audio element and trigger play
    audio_html <- paste0(
      '<audio id="audioPlayer" controls autoplay>',
      '<source src="data:audio/wav;base64,', audio_data, '" type="audio/wav">',
      'Your browser does not support the audio element.',
      '</audio>'
    )
    
    # Update UI with audio player
    output$audioPlayer <- renderUI({
      HTML(audio_html)
    })
    
    # Force garbage collection after processing
    gc()
    cat("Memory after:", pryr::mem_used(), "\n")
  })
  
  
  
  output$downloadBtn <- downloadHandler(
    
    filename = function(){
      paste0(input$site, "_",input$parameter, "_", input$dateRange[1], "_", input$dateRange[2], ".wav")
    },
    content = function(file){
      
      sound <- sonify_data2(filtered_data()$clean_mean,
                            note_length = input$noteLength,
                            to_plot = FALSE,
                            total_seconds = input$totalSeconds,
                            #ref_freq = input$refFreq,
                            key = input$key,
                            octave = input$octave,
                            octaves = input$octaves,
                            wave_type = input$waveType,
                            scale = input$scale, 
                            feedback = input$feedback_opt,
                            to_play = FALSE)[1]
      
      writeWave(object = sound[[1]], filename = file) 
    }
  )
}

shinyApp(ui = ui, server = server)

  