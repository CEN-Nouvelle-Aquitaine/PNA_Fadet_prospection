

ui <- navbarPage("Inscription aux prospections pour le PNA 'Fadet des laîches'", id="main",
                 tabPanel("Carte", style= "margin-top:-1.5em; margin-left:-1em; margin-right:-1em;", 
                          useShinyjs(), 
                          actionButton("telechargement", "Téléchargement des données", style="margin-top: 25px; margin-left: 75px; position:absolute;z-index:1;"),
                          uiOutput("conditionalInput2"),
                          leafletOutput("map") %>% withSpinner(color="#000000"),
                          tags$link(
                            rel = "stylesheet",
                            href = "https://cdnjs.cloudflare.com/ajax/libs/jquery-contextmenu/2.8.0/jquery.contextMenu.min.css"
                          ),
                          tags$script(
                            src = "https://cdnjs.cloudflare.com/ajax/libs/jquery-contextmenu/2.8.0/jquery.contextMenu.min.js"
                          ),
                          tags$script(HTML(js)),
                          tags$style(HTML("html,body {margin: 0; overflow: hidden;}"))
                 ))