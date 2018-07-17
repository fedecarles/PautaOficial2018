# ' Pauta Oficial - 1er Semestre 2018
# ' 16-JUL-2018
# ' Simple app the Shiny para graficar el gasto de publicidad oficial del gobierno argentino
# ' en los medios de comunicación.

# Paquetes requeridos
library(shiny)
library(dplyr)
library(ggplot2)
library(scales)
library(DT)

# Carga de datos
pauta <-  read.csv("pauta-oficial-primer-semestre-2018.csv", header = TRUE)
pauta <-  pauta[, c(
  "rubro",
  "medio",
  "provincia_medio",
  "descripcion_anunciante",
  "campania",
  "total_neto",
  "total_con_iva"
)]

ui <- fluidPage(# Application title
  titlePanel("Pauta Oficial - 1er Semestre 2018"),
  h5("Simple app the Shiny para graficar el gasto de publicidad oficial del gobierno argentino
             en los medios de comunicación durante el primer semestre de 2018."),
  helpText(a("Datos Completos", href="http://infra.datos.gob.ar/catalog/jgm/dataset/1/distribution/1.7/download/pauta-oficial-primer-semestre-2018.csv")),
  helpText(a("Portal de Datos Argentina", href="http://datos.gob.ar/")),
  
    # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        "rubro",
        "Rubro",
        choiceValues = unique(as.character(pauta$rubro)),
        choiceNames = unique(as.character(pauta$rubro)),
        selected = unique(as.character(pauta$rubro))
      ),
      checkboxInput('all_rubro', 'Todos/Ninguno', value = TRUE),
      checkboxGroupInput(
        "provincia",
        "Provincia",
        choiceValues = unique(as.character(pauta$provincia_medio)),
        choiceNames = unique(as.character(pauta$provincia_medio)),
        selected = unique(as.character(pauta$provincia_medio))
      ),
      checkboxInput('all_provincia', 'Todos/Ninguno', value = TRUE),
      width = 3
    ),
    
    # Show a plot of the generated distribution
    mainPanel(tabsetPanel(
      type = "tabs",
      tabPanel(
        "Gráficos",
        plotOutput("rubro_plot"),
        plotOutput("pcia_plot")
      ),
      tabPanel("Datos", DT::dataTableOutput("results"))
    ))
  ))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  filtered <- reactive({
    pauta %>%
      filter(rubro %in% input$rubro,
             provincia_medio %in% input$provincia)
  })
  
  output$results <- DT::renderDataTable({
    filtered()
  })
  
  output$pcia_plot <- renderPlot({
    filtered() %>%
      ggplot(aes(
        x = reorder(provincia_medio, -total_neto),
        y = filtered()$total_neto
      )) +
      stat_summary(fun.y = sum,
                   geom = "bar",
                   fill = "skyblue") +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      scale_y_continuous(labels = dollar)
  })
  
  output$rubro_plot <- renderPlot({
    filtered() %>%
      
      ggplot(aes(
        x = reorder(rubro,-total_neto),
        y = filtered()$total_neto
      )) +
      stat_summary(fun.y = sum,
                   geom = "bar",
                   fill = "skyblue") +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      scale_y_continuous(labels = dollar)
  })
  
  observe({
    updateCheckboxGroupInput(
      session,
      'rubro',
      choices = unique(as.character(pauta$rubro)),
      selected = if (input$all_rubro)
        unique(as.character(pauta$rubro))
    )
  })
  
  observe({
    updateCheckboxGroupInput(
      session,
      'provincia',
      choices = unique(as.character(pauta$provincia_medio)),
      selected = if (input$all_provincia)
        unique(as.character(pauta$provincia_medio))
    )
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)

