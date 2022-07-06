library(pacman)
p_load("gapminder", "ggforce", "gh", "globals", "openintro", "profvis", 
       "RSQLite", "shiny", "shinycssloaders", "shinyFeedback", 
       "shinythemes", "testthat", "thematic", "tidyverse", "vroom", 
       "waiter", "xml2", "zeallot", "ggthemes", "readxl", "DT")
options(scipen = 999)


Liga_MX <- read_excel("01_datos/Liga MX.xlsx") %>% 
  arrange(Equipo) %>% 
  filter(!is.na(Equipo)) %>% 
  mutate() %>% 
  filter(Equipo %in%  c("América", "Atlas", "León", "Tigres", "Santos", 
                        "Toluca", "Puebla", "Cruz Azul", "Monterrey", "Guadalajara",
                        "UNAM", "Atlético de San Luis", "Mazatlán", "Necaxa", "Pachuca",
                        "Juàrez", "Querétaro", "Tijuana"))

Promedio <- Liga_MX %>% 
  group_by(Equipo) %>% 
  summarize(sum = sum(Pts.),
            PJ = sum(PJ),
            G = sum(G),
            E = sum(E),
            P = sum(P),
            GF = sum(GF),
            GC = sum(GC)
            )  %>% 
  ungroup()

Campeonatos <- Liga_MX %>% 
  mutate(Campeón = ifelse(is.na(`Es campeón`),0,1) ) %>% 
  group_by(Equipo) %>% 
  summarize(campeonatos = sum(Campeón))  %>% 
  ungroup() 


ui <- fluidPage(
  titlePanel(h1(id="Titulo", "Desempeño equipos futbol mexicano")),  # Add a title panel
  tags$style(HTML("#Titulo{font-size:42px; weight:bold}")),
  sidebarLayout(  # Make the layout a sidebarLayout
    sidebarPanel(
      selectInput(inputId = "select", label = "seleccionar equipo", choices = unique(Liga_MX$Equipo), selectize = TRUE, selected = Liga_MX$Equipo[2]),
      selectInput(inputId = "selectb", label = "seleccionar equipo", choices = unique(Liga_MX$Equipo), selectize = TRUE, selected = Liga_MX$Equipo[2]),
      p("Selecciona los dos equipos que quieres comparar"),
      h2("Campeonatos"),
      DT::dataTableOutput("Champions")
    ),
    mainPanel(
      plotOutput("plot"),
      DT::dataTableOutput("Tabla")
    )  # Inside the sidebarLayout, add a mainPanel
  )
)

server <- function(input, output, session) {

  output$plot <- renderPlot(
      Liga_MX %>% 
        filter(Equipo == input$select | Equipo == input$selectb) %>% 
      ggplot(aes(Temporada, Pos., group = Equipo, color = Equipo)) +
        geom_point(size = 3)+
        geom_smooth(span = 0.25, se = FALSE, aes(colour= Equipo))+
        scale_y_reverse(limits = c(20, 1)) +
        labs(title = (paste(input$select, "vs", input$selectb, collapse ="")),
             subtitle = 'La línea azul es la tendencia atenuada a lo largo del tiempo',
             caption = "Desarrollado por @elcerebrohabla\nFuente: Wikipedia",
             x = "Años",
             y = "Posición en la tabla")+
        #scale_y_continuous(breaks = c(1,2), limits = c(1, 2))+
        theme_fivethirtyeight()+
        theme(
          plot.title = element_text(size = 26, face = "bold"),
          legend.title = element_text(size = 18),
          legend.text = element_text(size=18),
          #legend.position = "none",
          plot.subtitle = element_text(size = 16),
          plot.caption = element_text(size = 12),
          axis.text = element_text(size = 9),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
      
    )
  
  filterData <- reactive({
    Promedio[which(Promedio$Equipo == input$select | Promedio$Equipo == input$selectb),]
  }) 
  
  filterCamp <- reactive({
    Campeonatos[which(Campeonatos$Equipo == input$select | Campeonatos$Equipo == input$selectb),]
  }) 
  

  

  
  
  output$Tabla = DT::renderDataTable({
    DT::datatable(filterData(),selection="single",rownames = F, options = list(dom = 't'))
  })
  
  output$Champions = DT::renderDataTable({
    DT::datatable(filterCamp(),selection="single",rownames = F, options = list(dom = 't'))
  })



}


shinyApp(ui, server)
