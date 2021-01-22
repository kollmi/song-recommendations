#R Shiny App for Recommendations
library(shiny)
library(dplyr)
library(shinythemes)
library(spotifyr)
library(DT)

id <- #redacted
secret <- #redacted
Sys.setenv(SPOTIFY_CLIENT_ID = id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = secret)
access_token <- get_spotify_access_token()

genres <- c("acoustic", "afrobeat","alt-rock", "alternative", "ambient", "anime", "black-metal","bluegrass","blues","bossanova",
            "brazil","breakbeat","british","cantopop","chicago-house","children","chill", "classical","club", "comedy","country",
            "dance","dancehall","death-metal","deep-house", "detroit-techno","disco","disney","drum-and-bass","dub","dubstep",
            "edm","electro","electronic","emo","folk","forro","french","funk","garage","german","gospel","goth","grindcore",
            "groove","grunge","guitar","happy","hard-rock","hardcore","hardstyle","heavy-metal","hip-hop","holidays","honky-tonk",
            "house","idm","indian","indie","indie-pop","industrial","iranian","j-dance","j-idol","j-pop","j-rock","jazz","k-pop",
            "kids","latin","latino","malay","mandopop","metal","metal-misc","metalcore","minimal-techno","movies","mpb","new-age",
            "new-release","opera","pagode","party","philippines-opm","piano","pop","pop-film","post-dubstep","power-pop",
            "progressive-house","psych-rock","punk","punk-rock","r-n-b","rainy-day","reggae","reggaeton","road-trip","rock",
            "rock-n-roll","rockabilly","romance","sad","salsa","samba","sertanejo","show-tunes","singer-songwriter","ska",
            "sleep","songwriter","soul","soundtracks","spanish","study","summer","swedish","synth-pop","tango","techno",
            "trance","trip-hop","turkish","work-out","world-music")

ui <- fluidPage(theme = shinytheme("sandstone"),
  # App title ----
  h2("Track Recommendations"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Inputs
      textInput("artist", "Artist"),
      #selectInput("a.choices", "Artist Choices", ""),
      textInput("track", "Track"),
      #selectInput("t.choices", "Track Choices", ""),
      selectInput("genre", "Genre", choices = genres, multiple = TRUE, selectize = TRUE),
      sliderInput("valence", "Happiness", min = 0, max = 1, value = c(0,1)),
      sliderInput("energy", "Energy", min = 0, max = 1, value = c(0,1)),
      sliderInput("dance", "Danceability", min = 0, max = 1, value = c(0,1)),
      #actionButton("create", "Create Playlist"),
      actionButton("request", "Get Recommendations!", style="color: #fff; background-color: #228B22; border-color: #2e6da4")
      #textInput("newname", "New Playlist Name"),
      #actionButton("addto", "Add to Current Playlist"),
      #textInput("currentname", "Current Playlist Name")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output
      dataTableOutput(outputId = "results"),
      verbatimTextOutput(outputId = "playlist"),
      tags$head(tags$style("#text1{color: red;
                                 font-size: 20px;
                           font-style: italic;
                           }"
                         )
      )
      
    )
  )
)

server <- function(input, output, session) {
  
  tree <- eventReactive(input$request, {
    artists <- if(input$artist == "") NULL else {search_spotify(input$artist, type = "artist", limit = 1) %>% select(id)}
    tracks <- if(input$track == "") NULL else {search_spotify(input$track, type = "track", limit = 1) %>% select(id)}
  
    if (is.null(artists) == TRUE){
    seed <- get_recommendations(seed_tracks = tracks, seed_genres = input$genre,
                                min_valence = input$valence[1], max_valence = input$valence[2],
                                min_energy = input$energy[1], max_energy = input$energy[2],
                                min_danceability = input$dance[1], max_danceability = input$dance[2])}
    
    if (is.null(tracks) == TRUE){
      seed <- get_recommendations(seed_artists = artists, seed_genres = input$genre,
                                  min_valence = input$valence[1], max_valence = input$valence[2],
                                  min_energy = input$energy[1], max_energy = input$energy[2],
                                  min_danceability = input$dance[1], max_danceability = input$dance[2])}
    else {
      seed <- get_recommendations(seed_tracks = tracks, seed_artists = artists, seed_genres = input$genre,
                                      min_valence = input$valence[1], max_valence = input$valence[2],
                                      min_energy = input$energy[1], max_energy = input$energy[2],
                                      min_danceability = input$dance[1], max_danceability = input$dance[2])}
    
    for (i in 1:length(seed$artists)){
      seed$artist.name[[i]] <- paste(unlist(seed$artists[[i]][3]), collapse = ", ")
    }
    
    seed %>% select(name, artist.name, album.name)
  })
  
  search <- reactive({
   artists <- search_spotify(req(input$artist), type = "artist", limit = 1) %>% select(id)
   tracks <- search_spotify(req(input$track), type = "track", limit = 1) %>% select(id)
    
    seed <- get_recommendations(seed_artists = artists, seed_tracks = tracks,
                                min_valence = input$valence[1], max_valence = input$valence[2],
                                min_energy = input$energy[1], max_energy = input$energy[2],
                                min_danceability = input$dance[1], max_danceability = input$dance[2])
    
    for (i in 1:length(seed$artists)){
      seed$artist.name[[i]] <- paste(unlist(seed$artists[[i]][3]), collapse = ", ")
    }
    
    )
    
  })
  
  output$results <- renderDataTable(tree())
  

}

shinyApp(ui = ui, server = server)
