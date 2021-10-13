library(shiny)
library(googlesheets4)
library(dplyr)
library(shinydashboard)
library(shinyWidgets)
library(googledrive)
library(DT)
library(googleway)
library(ggplot2)
library(ggthemes)
library(magrittr)
library(shinycssloaders)
library(tidyr)
library(stringr)
library(shinyWidgets)

#Améliorations dans le code
#use googlesheets4 api to prevent client error OK
#dans plot8 retirer autres programmes
#régler erreurs json
#régler erreurs % offres acceptées seulement
#rendertext to uioutput
# isolate()
#https://gargle.r-lib.org/articles/get-api-credentials.html
#https://googlesheets4.tidyverse.org/articles/articles/auth.html
#password input

#lien google sheets
 #gs4_auth_configure(api_key = "AIzaSyAzH9wJtVYLlaClDx758cRvJhfsbvI8iYo")
 #gs4_deauth()
 
drive_auth(path = "secret/clientsecret.json")
gs4_auth(path = "secret/clientsecret.json")


datazu <- read_sheet("1WFewCMyHGIhMgSNVD6vfElNVoEvY_duGKipdkK97K0Q",sheet="Export")
dataprogs <- read_sheet("1WFewCMyHGIhMgSNVD6vfElNVoEvY_duGKipdkK97K0Q",sheet="progs")

# nombre de programmes
choixprog=unique(dataprogs$programme)

#nbr candidatures totales brutes
valeur1 <- datazu %>% 
  distinct(nomcompagnie,programme) %>% 
  summarize(total=n())%>% 
  pull()

#nbr incubés total
valeur2 <- datazu %>% 
  distinct(nomcandidat,programme,statut) %>% 
  filter(statut=="accepted") %>% 
  summarize(total=n())%>% 
  pull()

#candidatures totales nettes
valeur3 <- 
  datazu %>% 
  distinct(nomcomplet,totalapps) %>% 
  group_by(nomcomplet) %>% 
  slice(which.max(totalapps)) %>% 
  ungroup() %>% 
  summarize(total2=n()) %>% 
  pull()

#candidatures moyenne par compagnie
valeur4 <- 
  datazu %>% 
  distinct(nomcomplet,totalapps) %>% 
  group_by(nomcomplet) %>% 
  slice(which.max(totalapps)) %>% 
  ungroup() %>% 
  summarize(total2=mean(totalapps)) %>% 
  pull() %>% 
  round(.,2)

#value box pour créer boîte info sans shiny dashboard
valueBox <- function(value, subtitle, icon, color) {
  div(class = "col-lg-3 col-md-6",
      div(class = "panel panel-primary",
          div(class = "panel-heading", style = paste0("background-color:", color),
              div(class = "row",
                  div(class = "col-xs-3",
                      icon(icon, "fa-5x")
                  ),
                  div(class = ("col-xs-9 text-right"),
                      div(style = ("font-size: 56px; font-weight: bold;"),
                          textOutput(value)
                      ),
                      div(subtitle)
                  )
              )
          ),
          div(class = "panel-footer",
              div(class = "clearfix")
          )
      )
  )
}

# Set up the application ui
ui <- (navbarPage("Zu Stat",
                   
# define the tabs to be used in the app ----------------------------------------
# introduction splash
tabPanel("Intro",
                h1("Zustat 0.1"),
                p("Cet outil fonctionne à partir de données compilées dans un ",
                  span(tags$a(href="https://docs.google.com/spreadsheets/d/1_Abtl3UhlMCKvLt5xUj-oF_vtbii6Fl-hf8F16AF-FQ/edit?usp=sharing", "document Google Sheets."))
                  ,"Il recense les candidatures depuis 2019."),
         br(),
         br(),
         fluidRow(
           
           
          
          # nbr d'entreprises incubées
           valueBox(value="boitetot2",subtitle="Total d'entreprises incubées depuis 2019",icon="cog",color="red"
           ), 
           
          # nbr de candidatures reçues 
           valueBox(value="boitetot",subtitle="Candidatures totales reçues",icon="tachometer",color="green"
           )),
         
         fluidRow(
         
         
         # nbr de compagnies qui ont soumis
           valueBox(value="boitetot3",subtitle="Compagnies distinctes ont postulé",icon="building",color="blue"
         ),
         
         valueBox(value="boitetot4",subtitle="Candidatures moyenne par compagnie",icon="door-closed",color="darkblue"
         ),
         )

),

  #paramètres
  tabPanel("Filtrer",
  h5("Choisir programme")
  ,pickerInput("prog", 
              choices=choixprog, selected=NULL,
              options = list(
                `deselect-all-text` = "Aucune",
                `select-all-text` = "Tous", 
                `none-selected-text` = "Tous",
                `selected-text-format` = paste0("count > ", length(choixprog) -1),`count-selected-text` = "Plusieurs"),
              multiple = T)
  ,h5("Offres acceptées seulement")
  #,checkboxInput("incub", "Oui", value = FALSE)
  ,materialSwitch(inputId = "incub", label = "Oui",value=FALSE)
  ,submitButton("Soumettre")
  ,hr()),


# indicateurs de base
tabPanel("Stats de base",
         
         fluidRow(column(12,align="left",
                         "Note : ces indicateurs sont filtrés selon les critères de l'onglet Filtrer"
                         ) 
                  ),
         fluidRow(
           
             #graphique nombre candidatures
             column(6,h3("Nombre de candidatures"),
               shinycssloaders::withSpinner(
               plotOutput("plot1", height = 500))
                  ),
             
             column(6, h3("Stats additionnelles"),
                  tags$head(tags$style("
                    #container * {  
                     display: inline;}")
                    ),
                  br(),
                  br(),
                  p(
                    #style="text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px",

                    tags$ul(
                      tags$li(
                          div(id="container",
                              h5(textOutput("texte2"), 
                                 paste(":"),
                                 textOutput("texte1")))),
                      tags$li(
                        div(id="container",
                            h5(paste("Nombre d'entreprises affichées:"),
                               textOutput("texte3"), 
                               ))),
                      
                      tags$li(
                        div(id="container",
                            h5(paste("Délai moyen entre inscription et candidature:"),
                               textOutput("texte4"), paste("jours.")
                            )))
                      
                )
                ))
               ),
         
         fluidRow(
           #graphique industrie
           box(
             title="Industrie",
             h6("Plus d'un choix possible. Les catégories de moins de 1% sont omises."),
             shinycssloaders::withSpinner(
             plotOutput("plot2", height = 500))
           )
           
          #graphique techno
           ,box(
             title="Technologies employées",
             h6("Plus d'un choix possible. Les catégories de moins de 1% sont omises."),
             shinycssloaders::withSpinner(
             plotOutput("plot3", height = 500))
           )
         
      

)),

# indicateurs plus poussés
tabPanel("Indicateurs supp.",
         
         fluidRow(
           column(12,align="center",
           #graphique homme-femme
           h3("Ratio H/F parmi les candidatures"),
           shinycssloaders::withSpinner(
             plotOutput("plot4", width=400, height = 350)),
           
           h3("Âge des candidats"),
           shinycssloaders::withSpinner(
             plotOutput("plot7", width=400, height = 350)),
           
           #graphique moment déposé
           
           h3("Moment du dépôt des candidatures"),
           h6("La ligne rouge indique une date limite de soumission pour un programme"),
           shinycssloaders::withSpinner(
             plotOutput("plot6", height = 500)),
           
           h3("Dépôts cumulatif"),
           h6("La ligne rouge indique la date limite de candidature"),
           shinycssloaders::withSpinner(
             plotOutput("plot8", height = 500))
         ))
),

# tab source des candidatures
tabPanel("Source des candidatures",
         
         fluidRow(
           column(12,align="center",
                  #graphique homme-femme
                  h3("Source des candidatures"),
                  h6("Pour les programmes 2019-2020. Les startups pouvaient sélectionner de multiples sources."),
                  shinycssloaders::withSpinner(
                    plotOutput("plot5", width=600, height = 350)),
                  
                  h3("Source des candidatures"),
                  h6("Pour les programmes à partir de 2021. Les startups ne pouvaient choisir qu'une source"),
                  shinycssloaders::withSpinner(
                    plotOutput("plot9", width=600, height = 350))
                  
           )),
         
         fluidRow(
           box(
             title="Détails provenance Word of mouth",
             h6("En nombres absolus. Pour programmes à partir de 2021."),
             shinycssloaders::withSpinner(
               plotOutput("plot10", height = 300))
           )
           
           ,box(
             title="Détails provenance social media",
             h6("En nombres absolus. Pour programmes à partir de 2021."),
             shinycssloaders::withSpinner(
               plotOutput("plot11", height = 300))
           )),
         
         fluidRow(
           box(
             title="Détails provenance Zu activity",
             h6("En nombres absolus. Pour programmes à partir de 2021."),
             shinycssloaders::withSpinner(
               plotOutput("plot12", height = 300))
           )
           
           ,box(
             title="Détails Provenance online source",
             h6("En nombres absolus. Pour programmes à partir de 2021."),
             shinycssloaders::withSpinner(
               plotOutput("plot13", height = 300))
           ))
         
         ),





# table de données à explorer
tabPanel("Fichier source",
         h1("La source des données"),
         p("Voir la table de données",
           ),
         DTOutput("datadisplay")
        
),

tabPanel("MAJ",
         h3("20 mai 2021"),
         p(
           tags$ul(
             tags$li("Remaniement des onglets supérieurs"),
             tags$li("Nettoyage sources provenance 2021-"),
             tags$li("Ajout graphique sources 2021-"),
             tags$li("Correction noms des programmes"),
             tags$li("Ajout graphiques sources détaillées 2021-"))),
         
         h3("Prochaines étapes à coder"),
         p(
           tags$ul(
             tags$li("Candidatures pré-2019"),
             tags$li("Ajout des candidatures brouillon"),
             tags$li("Division en fonction de la thèse d'investissement"),
             tags$li("Ajout de la date d'ouverture des candidatures dans le graphique du temps"),
             tags$li("Origine géographique"),
             tags$li("Nuage de mots avec description"),
             tags$li("Ajout de stats du portfolio")
           ),
           br(),
           p("Ce module a été codé en R à l'aide du package Shiny. OSH 2020-2021.")
         )
         
)
                                      
# close the UI definition
))


server <- function(input, output) {

  #jeu de données résumé des candidatures
  resume <- reactive ({
    datazu %>% 
    distinct(nomcompagnie,programme,statut) %>% 
    left_join(.,dataprogs,by="programme") %>% 
    { if (!is.null(input$prog)) filter(.,programme %in% input$prog) else . } %>%
    { if (input$incub==TRUE) filter(., statut=="accepted") else . } %>%
    count(programme,statut,annee,ordre,name="total") %>% 
    arrange(ordre)
  })
  
  #jeu de données filtré selon input
  datazu2 <- reactive ({
    datazu %>% 
      { if (!is.null(input$prog)) filter(.,programme %in% input$prog) else . } %>% 
      { if (input$incub==TRUE) filter(., statut=="accepted") else . }
  })
  
  #jeu de données résumé filtré selon input (total compagnies résumé par programme)
  resume2 <- reactive ({
      resume() %>% 
      summarize(total2=sum(total)) %>% 
      pull()
  
  })
  

  #boîte total incubés
  output$boitetot2<- renderText({ 
    valeur2
  })
  
  #boîte candidatures totales brutes
  output$boitetot<- renderText({ 
    valeur1
  })
  
  #boîte candidatures totales nettes
  output$boitetot3<- renderText({ 
    valeur3
  })

  #boîte candidatures moyennes par compagnie
  output$boitetot4<- renderText({ 
    valeur4
  })
  
  #boîte candidatures de nouvelles compagnies
  output$texte1<- renderText({ 
      datazu %>%
      distinct(nomcomplet,totalapps,programme) %>%
      { if (!is.null(input$prog)) filter(.,programme %in% input$prog) else . } %>%
      group_by(nomcomplet) %>%
      slice(which.max(totalapps)) %>%
      filter(totalapps==1) %>%
      ungroup() %>%
      summarize(total2=(sum(totalapps)/resume2())) %>%
      round(.,2) %>% 
      "*"(100) %>% 
      paste0(.,"%")
  })
  
  output$texte2<- renderText({ 
    if (is.null(input$prog)) { 
      paste("Pourcentage d'entreprises qui ont postulé 1 fois") } 
    else
      paste("Pourcentage de nouveaux candidats") 
  })
  
  output$texte3<- renderText({ 
    resume2()
  })

  # délai entre création compte et candidature 
  output$texte4 <- renderText({ 
    datazu %>%
      distinct(nomcomplet,totalapps,programme,datesoumission,datecreation) %>%
      { if (!is.null(input$prog)) filter(.,programme %in% input$prog) else . } %>%
      mutate(delai=difftime(strptime(datesoumission,format = "%Y-%m-%d"),strptime(datecreation, format = "%Y-%m-%d"),units="days")) %>% 
      group_by(programme) %>%
      summarize(moyenne=round(mean(delai))) %>% 
      summarize(moyennegen=as.character(round(mean(moyenne)))) %>% 
      pull()
    
  })
  
  #graphique candidatures par programme
  output$plot1 <- renderPlot({
    resume <- resume()
    
    resume$programme=factor(resume$programme,levels=unique(resume$programme))
    
    resume %>% 
      select(programme,total,ordre) %>% 
      group_by(programme,ordre) %>% 
      arrange(ordre) %>% 
      summarize(total=sum(total)) %>% 
      ggplot(aes(x=programme,y=total))+
      geom_col(aes(fill = factor(programme)))+
      scale_x_discrete(guide = guide_axis(n.dodge=2))+
      labs(x="",y="Nombre")+
      geom_label(aes(label=total))+
        guides(fill=FALSE)+
      theme_gdocs()
    
  })
  
  #graphique industrie
  output$plot2 <- renderPlot({
    datazu2 <- datazu2()
    
    datazu2 %>% 
      distinct(nomcomplet,programme,industrie) %>% 
      select(industrie) %>% 
      drop_na(industrie) %>% 
      group_by(industrie) %>% 
      summarize(total=n()) %>% 
      mutate(percent=(total/sum(total)*100)) %>% 
      filter(percent>1) %>% 
      ggplot(aes(x=percent,y=reorder(industrie,percent)))+geom_col()+
      geom_label(aes(label=round(percent)), hjust=0.5)+
      labs(title="",x="Pourcentage",y="")+
      theme_gdocs()
  })
  
  #graphique techno
  output$plot3 <- renderPlot({
    datazu2 <- datazu2()
    
    datazu2 %>% 
      distinct(nomcomplet,programme,techno1,techno2) %>% 
      select("techno1","techno2") %>% 
      unlist(recursive=FALSE) %>% as.character() %>% 
      tibble %>% 
      set_colnames("techno") %>% 
      drop_na() %>% 
      group_by(techno) %>% 
      summarize(total=n()) %>% 
      mutate(percent=(total/sum(total)*100)) %>% 
      filter(percent>1) %>% 
      #arrange(desc(percent)) %>% 
      ggplot(aes(x=percent,y=reorder(techno,percent)))+geom_col()+
      geom_label(aes(label=round(percent)), hjust=0.5)+
      labs(title="",x="Pourcentage",y="")+
      theme_gdocs()
    
  })
  
  #graphique sexe H-F
  output$plot4 <- renderPlot({
    datazu2 <- datazu2()
    
    datazu2 %>% 
      select(sexe) %>% 
      drop_na(sexe) %>% 
      group_by(sexe) %>% 
      summarize(total=n()) %>% 
      mutate(percent=(total/sum(total)*100)) %>% 
      ggplot(aes("",percent,fill=sexe))+
      geom_bar(width=1,stat="identity")+
      scale_fill_discrete(name = "",
                          labels = c("Femmes", "Hommes"))+
      labs(title="",x="",y="Pourcentage")+
      geom_text(aes(label=round(percent)),position = position_stack(vjust = 0.5,reverse = FALSE))+
      theme_gdocs()
    
  })
  
  #graphique source candidature 2019-2020
  output$plot5 <- renderPlot({
    datazu2 <- datazu2()
    
    datazu2 %>% 
      select("source1","source2","source3","source4","source5") %>% 
      unlist(recursive=FALSE) %>% as.character() %>% 
      tibble %>% 
      set_colnames("source") %>% 
      filter(!grepl("NULL",source)) %>% 
      drop_na() %>% 
      mutate(new=str_replace_all(source,"Facebook|Instagram|LinkedIn","Réseaux sociaux")) %>% 
      group_by(new) %>% 
      summarize(total=n()) %>% 
      mutate(percent=(total/sum(total)*100)) %>% 
      #arrange(desc(percent)) %>% 
      ggplot(aes(x=percent,y=reorder(new,percent)))+
      geom_col()+
      geom_label(aes(label=round(percent)), hjust=0.5)+
      labs(title="",x="Pourcentage",y="")+
      theme_gdocs()
  
  })
  
  #graphique source candidature 2021-
  output$plot9 <- renderPlot({ 
    datazu2 <- datazu2()
    
    datazu2 %>% 
      distinct(nomcomplet,programme,newsource) %>% 
      select(newsource) %>% 
      drop_na(newsource) %>% 
      group_by(newsource) %>% 
      summarize(total=n()) %>% 
      mutate(percent=(total/sum(total)*100)) %>% 
      filter(percent>1) %>% 
      ggplot(aes(x=percent,y=reorder(newsource,percent)))+geom_col()+
      geom_label(aes(label=round(percent)), hjust=0.5)+
      labs(title="",x="Pourcentage",y="")+
      theme_gdocs()
  })
  
  #graphique provenance 1
  output$plot10 <- renderPlot({ 
    datazu2 <- datazu2()
    datazu2 %>% 
      distinct(nomcomplet,programme,wordofmouth) %>% 
      select(wordofmouth) %>% 
      drop_na(wordofmouth) %>% 
      group_by(wordofmouth) %>% 
      summarize(total=n()) %>%
      ggplot(aes(x=total,y=reorder(wordofmouth,total)))+geom_col()+
      geom_label(aes(label=round(total)), hjust=0.5)+
      labs(title="",x="Nombre",y="")+
      theme_gdocs()
  })
  
  #graphique provenance2
  output$plot11 <- renderPlot({ 
    datazu2 <- datazu2()
    datazu2 %>% 
      distinct(nomcomplet,programme,socialmedia) %>% 
      select(socialmedia) %>% 
      drop_na(socialmedia) %>% 
      group_by(socialmedia) %>% 
      summarize(total=n()) %>%
      ggplot(aes(x=total,y=reorder(socialmedia,total)))+geom_col()+
      geom_label(aes(label=round(total)), hjust=0.5)+
      labs(title="",x="Nombre",y="")+
      theme_gdocs()
  })
  
  #graphique provenance3
  output$plot12 <- renderPlot({ 
    datazu2 <- datazu2()
    datazu2 %>% 
      distinct(nomcomplet,programme,zuactivity) %>% 
      select(zuactivity) %>% 
      drop_na(zuactivity) %>% 
      group_by(zuactivity) %>% 
      summarize(total=n()) %>%
      ggplot(aes(x=total,y=reorder(zuactivity,total)))+geom_col()+
      geom_label(aes(label=round(total)), hjust=0.5)+
      labs(title="",x="Nombre",y="")+
      theme_gdocs()
  })
  
  #graphique provenance4
  output$plot13 <- renderPlot({ 
    datazu2 <- datazu2()
    datazu2 %>% 
      distinct(nomcomplet,programme,onlinesource) %>% 
      select(onlinesource) %>% 
      drop_na(onlinesource) %>% 
      group_by(onlinesource) %>% 
      summarize(total=n()) %>%
      ggplot(aes(x=total,y=reorder(onlinesource,total)))+geom_col()+
      geom_label(aes(label=round(total)), hjust=0.5)+
      labs(title="",x="Nombre",y="")+
      theme_gdocs()
  })
  
  #graphique âge des candidats
  output$plot7 <- renderPlot({
    datazu2 <- datazu2()
    
      datazu2%>% 
      select(nomcomplet,naissance) %>% 
      filter(!is.na(naissance)) %>% 
      group_by(naissance) %>% 
      summarize(total=n()) %>% 
      mutate(percent=(total/sum(total)*100)) %>% 
      ggplot(aes(x=percent,y=naissance))+
      geom_col()+
      geom_label(aes(label=round(percent)), hjust=0.5)+
      labs(title="",x="Pourcentage",y="")+
      theme_gdocs()
    
  })
  
  #graphique moment déposé
  output$plot6 <- renderPlot({
    datazu2 <- datazu2()
    
    datelimite <- 
      dataprogs %>% 
      { if (!is.null(input$prog)) filter(.,programme %in% input$prog) else . } %>%
      pull(datelimite) %>% as.Date 
    
    datazu2 %>% 
      distinct(nomcompagnie,programme,datesoumission) %>% 
      mutate(moment=as.Date(datesoumission)) %>% 
      group_by(moment,programme) %>% 
      summarize(total=n()) %>% 
      ggplot(aes(moment,total))+
      theme(axis.text.x=element_text(angle=60, hjust=1)) +
      geom_point((aes(colour = factor(programme),size=total)))+
      geom_line()+
      geom_vline(xintercept=as.numeric(as.Date(datelimite)),color="red")+
      labs(title="",x="",y="nombre de candidatures")+
      { if (!is.null(input$prog)) 
        scale_x_date(date_breaks = "weeks" , date_labels = "%d-%b-%y")
        else 
      scale_x_date(date_breaks = "months" , date_labels = "%b-%y") } +
      #il serait possible d'activer la légende plus bas
      guides(size = FALSE,colour=FALSE)+
      theme_gdocs()
    
  })
  
  #graphique dépôts cumulatifs
  output$plot8 <- renderPlot({
    datazu2 <- datazu2()
    
    datazu_limite <- 
      full_join(datazu2, dataprogs, by ="programme")
    
    datazu_limite %>% 
      distinct(nomcompagnie,programme,datesoumission,datelimite) %>% 
      mutate(delai=difftime(strptime(datesoumission,format = "%Y-%m-%d"),strptime(datelimite, format = "%Y-%m-%d"),units="weeks")) %>% 
      mutate(delai2=as.numeric(delai)) %>% 
      group_by(delai2,programme) %>% 
      summarize(total=n()) %>% 
      ungroup() %>% 
      arrange(programme) %>% 
      group_by(programme) %>% 
      mutate(cumu = cumsum(total)) %>% 
      mutate(pourcentage= cumu *100 /sum(total)) %>% 
      ggplot(aes(x = delai2, y = pourcentage, colour = factor(programme),size=cumu)) + 
      geom_line(size = 1)+
      geom_point() +
      geom_vline(xintercept=0,color="red")+
      labs(title="",x="délai en semaines",y="pourcentage déposé")+
      theme_gdocs()+
      guides(size = FALSE)
    
    
  })
  
  
  #table données
  output$datadisplay <- renderDT ({
    
    # datatable(hikedata2(), options=list(columnDefs = list(list(visible=FALSE, targets=c(8:18)))))
    datatable(datazu2()[,c("nomcandidat","programme","prenom","nomfamille","ville","naissance")])
    
  })
  
}

shinyApp(ui = ui, server = server)



