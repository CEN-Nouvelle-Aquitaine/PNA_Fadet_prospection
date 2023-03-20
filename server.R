

server <- function(input, output, session){
  
  #rajouter légende maille et signe permettant de discriminer les mailles avec inscriptions des autres
  
  maille <- reactivePoll(intervalMillis = 4000, session, checkFunc = function(){
    st_read("mailles.geojson")
  }, valueFunc = function(){
    st_read("mailles.geojson")
  }
  )
  
  centroides_mailles <- reactive({
  
  centroides_maille <- st_centroid(maille()) %>% group_by(id_maille) %>% 
    summarise(total_count=n()-1,
              .groups = 'drop')
  
  centroides_maille = extract(centroides_maille, geometry, into = c('Lon', 'Lat'), '\\((.*),(.*)\\)', conv = T)
  centroides_maille
  })
  
  hide(selector = "#main li a[data-value=Carte]")
  
  output$map <- renderLeaflet({
    
    myPal <- colorRampPalette(c("#51a635", "#d93c2e"))
    factpal.Div <- colorFactor(myPal(length(maille()$info_mailles)), maille()$info_mailles)
    
    myPal2 <- colorFactor(c("#51a635","#d93c2e"), domain = maille()$info_mailles)
    
    leaflet() %>%
      setView(lng = 0.1, lat= 44.5, zoom = 8) %>%
      addTiles(group = "Fond de carte standard (OSM)") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Imagerie satelitte (ESRI)") %>%
      addPolygons(data = maille(),
                  layerId = maille()$id_maille,
                  color = "black",
                  fillColor = ~factpal.Div(maille()$info_mailles),
                  fillOpacity = 0.5,
                  weight = 0.5,
                  group = "Maille",
                  label = paste("ID maille :", maille()$id_maille),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "10px",
                    direction = "auto")) %>%
      addLabelOnlyMarkers(data = centroides_mailles(),
                                lng = ~Lon, lat = ~Lat, label = ifelse(centroides_mailles()$total_count == 0, " ", centroides_mailles()$total_count),
                                labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE
                                # ,style=list('color'="white", 'fontSize'="15px")                            
                                                            )) %>%
      addLegend(pal = myPal2, values = maille()$info_mailles, group = "Maille",
                title = "Légende", position = "topright") %>%
      addPolygons(
        data = region_NA,
        layerId = region_NA$code,
        fillOpacity = 0,
        weight = 1,
        opacity = 1,
        color = "black",
        options = pathOptions(clickable = FALSE)) %>%
      addLayersControl(
        overlayGroups = c("Maille"),
        baseGroups = c("Fond de carte standard (OSM)", "Imagerie satelitte (ESRI)"),
        options = layersControlOptions(collapsed = FALSE))
  })
  
  
  observeEvent(input$map_shape_click, {
    showModal(modalDialog(title = "Prospection PNA Papillons de jour",
                          tags$head(tags$style(".butt3{background-color:#16881B;} .butt3{color: #e6ebef;}")),
                          fluidRow(align ="center", 
                                   actionButton(inputId = "acces_inscriptions",label = "Consulter les inscriptions pour cette maille", class="butt3")),
                          fluidRow(align ="center", 
                          actionButton(inputId = "faire_inscriptions",label = "S'inscrire à une période de prospection sur cette maille", class="butt3")),
                          size = "l",
                          easyClose = TRUE, 
                          fade = TRUE,
                          footer = tagList(modalButton("Annuler"))
                          ))
    
  })
  
  
  
  
  observeEvent(input$faire_inscriptions, {
    ### This is the pop up board for input a new row
    showModal(modalDialog(title = "Inscription à une période de prospection PNA Papillons de jour",
                          fluidRow(align ="center", 
                                   shinyjs::useShinyjs(),
                          textInput("nom", "Nom", ""),
                          textInput("prenom", "Prénom", ""),
                          textInput("pseudo", "Veuillez renseigner un pseudonyme qui s'affichera à la place de votre nom dans l'application", ""),
                          #structure optionnelle: 
                          textInput("structure", "Structure"),
                          ## Possibilité de cocher les 3 dates :
                          checkboxGroupInput(
                            inputId = "dates_passages",
                            label = "Périodes de prospections", 
                            choices = c("Entre le 20/05 et le 04/06", "Entre le 05/06 et le 25/06", "Entre le 26/06 et le 20/07"),
                            selected = "Entre le 20/05 et le 04/06"
                          ),
                          textInput("contact", "Email", ""),
                          tags$head(tags$style(".butt3{background-color:#16881B;} .butt3{color: #e6ebef;}")),
                          actionButton(inputId = "save_BDD",label = "Enregistrer", class="butt3", style = "width:250px")),
                          size = "l",
                          easyClose = TRUE, 
                          fade = TRUE,
                          footer = tagList(modalButton("Annuler"))
    ))
    
  })
  
  
# 
#   observe({
#     if(!is.null(input$nom) || !is.null(input$prenom) || input$nom == "" || input$prenom == "" ||
#        (!input$confidentialite && (!is.null(input$pseudo) || input$pseudo == ""))) {
#       disable("save_BDD")
#     } else {
#       enable("save_BDD")
#     }
#   })
#   
  
  observeEvent(input$activate_passage2, {
    if(input$activate_passage2 == F){
      shinyjs::disable("passage2")
    } else {
      shinyjs::enable("passage2")
    }
  })
  
  observeEvent(input$activate_passage3, {
    if(input$activate_passage3 == F){
      shinyjs::disable("passage3")
    } else {
      shinyjs::enable("passage3")
    }
  })
  

  dataset<-reactive({ 
    subset(maille(), id_maille == input$map_shape_click$id & nchar(nom) > 0)[, c(5, 6, 8, 9, 10)]})

  
  
  # observeEvent(input$save_BDD, {
  #   shinyalert(title = paste("Votre inscription à la maille ",input$map_shape_click$id, " a bien été enregistrée"), type = "success")
  #   removeModal()
  # })
  

  output$dt = DT::renderDataTable({
    datatable(colnames = c("Pseudo" = 1, "Structure" = 2, "Période de prospection n°1" = 3, "Période de prospection n°2" = 4, "Période de prospection n°3" = 5),
              dataset(), selection = 'single', rownames= FALSE, escape = FALSE,
              options = list(
                columnDefs = list(list(className = 'dt-center', targets = "_all"), list(visible=FALSE, targets=c(5))),
                language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json'),
              scrollY = T, scrollX = T, 
              # scroller = TRUE,
              autoWidth=TRUE,
              columnDefs = list(list(className = 'dt-center', targets = '_all')),initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#609e72', 'color': '#fff'});",
                "}"))) })
  

  observeEvent(input$acces_inscriptions, {
    click <- input$map_shape_click
    showModal(modalDialog(title = "Consulter les inscriptions en cours pour les prospections PNA Papillons de jour",
                          fluidRow(align ="center",
                                   p(paste(length(dataset()$id_maille), " personnes inscrites pour la maille '", input$map_shape_click$id, "' :")),
                                   tags$style(type = "text/css", ".dataTables_wrapper { margin: 20px; }"),
                                   DT::DTOutput("dt")),
                                size = "l",
                                easyClose = TRUE, 
                                fade = TRUE,
                                footer = tagList(modalButton("Annuler"))
    ))
  })




observeEvent(input$save_BDD, {
  
  req(input$map_shape_click$id)
  

  if(input$nom == "" || input$prenom == "" || input$contact == "" || input$pseudo == ""){
    shinyalert(title = paste("Veuillez renseigner votre nom, prénom , pseudonyme et email pour vous inscrire à une maille !"), type = "error")
    removeModal()}
  else {  
    shinyalert(title = paste("Votre inscription à la maille ",input$map_shape_click$id, " a bien été enregistrée"), type = "success")
    removeModal()
  
    
  
  maille_subset <- maille()[maille()$id_maille == input$map_shape_click$id, ]
  isolate({
  newLine <- data.frame(
    id_maille = input$map_shape_click$id,
    info_mailles = maille_subset$info_mailles,
    nom = input$nom,
    prenom = input$prenom,
    pseudonyme = input$pseudo,
    structure = input$structure,
    contact = input$contact,
    passage_1 = as.character(input$dates_passages[1]),
    passage_2 = as.character(input$dates_passages[2]),
    passage_3 = as.character(input$dates_passages[3]),
    geometry = maille_subset$geometry
 
  )})
  

  print(newLine)
  print("TEST")
  print(head(newLine,1))
  
  # append the new row to the geojson
  st_write(head(newLine,1), "mailles.geojson", driver = "GeoJSON", append = TRUE)
  }
}) 

observeEvent(input$telechargement, {

shinyalert(html = TRUE, text = tagList(
  textInput("password", "Mot de passe ?"),
))
  
})

output$conditionalInput2 <- renderUI({
  if(!is.null(input$password) && input$password == "adminPY64"){
    downloadButton("download_geojson", "Télécharger le geojson", style="margin-top: 60px; margin-left: 85px; position:absolute;z-index:1;")}
  })


output$download_geojson <- downloadHandler(
  filename = "prospections_mailles.geojson",
  content = function(file) {
    st_write(maille(), file, driver = "GeoJSON")
  }
)


  
}
  