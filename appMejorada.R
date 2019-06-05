#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# AUTORA: Carolina Gimenez Arias

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(dplyr)
library(ggplot2)
library(plotly)

#load("PrediccionesMaquina.Rdata")

ui <- navbarPage( id = "AppMaster", a(img(src = "https://image.flaticon.com/icons/png/512/806/806287.png", height = "30px"), "App Master Ciencia de Datos", href="http://oceano.uv.es:3838/users/jgxbos/MasterAppEjercicio/"), theme = shinytheme("sandstone"),
                  tabPanel("Selección de la máquina",
                           sidebarLayout(
                             sidebarPanel( 
                               h4("Máquina", style="text-align: center;"),
                               fileInput("DatosFichero", "Selecciona un fichero", accept = NULL),
                               uiOutput("Selmaq"), width = 3
                             ),
                             mainPanel(
                               h3("Probabilidad de orden", style="text-align: center;"), 
                               plotlyOutput("plot1"),
                               h4("Descripción", style="font-weight: bold;"),
                               h5(textOutput("Descripcion1")) , width = 9
                             )
                           )
                  ),
                  navbarMenu("Estado de la máquina",
                             tabPanel("Evolución Temporal de las alarmas",
                                      sidebarLayout(
                                          sidebarPanel(h4("Alarmas", style="text-align: center;"),
                                          uiOutput('Buttons'), width = 3
                                          ),
                                          mainPanel(
                                            h3("Evolución temporal de alarmas", style="text-align: center;"), 
                                            plotlyOutput("plot2"),
                                            h4("Descripción", style="font-weight: bold;"),
                                            h5(textOutput("Descripcion2")), width = 9
                                          )
                                      )
                              ),
                    
                             tabPanel("Registros de la máquina",
                                        sidebarLayout(
                                          sidebarPanel(
                                            h4("Alarmas", style="text-align: center;"),
                                            uiOutput("Selmaq2"),
                                            br(),
                                            h5("Exportar registros a formato csv:"),
                                            br(), 
                                            downloadButton(outputId = "download_data", label = "Download", style="display: block; margin: 0 auto; background: green;"), width = 3
                                          ),
                                      mainPanel(
                                        h3("Registros de la máquina seleccionada",
                                        style="text-align: center;"), 
                                          fluidRow(
                                            column(12, dataTableOutput('tbl'))
                                          ),
                                        width = 9)
                                      )
                             )
                  ),
                  tabPanel("Estadísticas Globales Temporales",
                           sidebarLayout(
                             sidebarPanel( 
                                h5("Seleccione la alarma:", style="text-align: center;"),
                                uiOutput("Selal"), width = 3
                              ),
                             
                              mainPanel(
                                  tabsetPanel(type = "tabs",
                                              tabPanel("Histograma",
                                                       plotOutput(outputId ="plothist"),
                                                       dropdownButton(
                                                         tags$h3("Opciones del histograma"),
                                                         
                                                         dateRangeInput("RangoFechas", "Selecciona el período", 
                                                                        start = "2016-01-01", end = NULL, min = NULL, 
                                                                        max = NULL, format = "yyyy-mm-dd", weekstart = 1,
                                                                        language = "es", separator = "a"),
                                                          
                                                         sliderInput("histbin", "Ancho del bin del histograma", min = 1, max = 50, value = 25),
                                                         
                                                         circle = TRUE, status = "danger", icon = icon("gear"), 
                                                         tooltip = tooltipOptions(title = "Ajustes del histograma")
                                                ),
                                              br(), 
                                              h5("Built with",
                                                 img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                                                "by",
                                                 img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"), 
                                                ".",style="text-align: right;")
                                              ),
                                              
                                              tabPanel("Boxplots", 
                                                       plotlyOutput(outputId ="plotbox"),
                                                       prettySwitch("Caja1", "Ver todas las máquinas", value = FALSE, width = NULL, status = "success"),
                                                       br(),
                                                       h5("Built with",
                                                          img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                                                          "by",
                                                          img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
                                                          ".",style="text-align: right;")
                                              )
                                        
                             ), width = 9
                           )
                          )
                  )
)

server <- function(input, output) {
  DATOS_R <- reactive({
    a <- input$DatosFichero
    nombre_variable<-load(a$datapath)   
    Datos1<-eval(parse(text = nombre_variable)) #Esto es para evaluar el dataframe 
    vars <- list()
    vars$DataFrameDatos <- Datos1
    vars$MatriculasUnicas <- as.list(unique(Datos1$matricula))
    vars$AlarmasUnicas <- names(select(Datos1, starts_with("a")))
    return(vars)
  })
  
  output$Selmaq <- renderUI({
    selectInput("Maquina","Tipos de máquina",DATOS_R()$MatriculasUnicas)
    })
  
  output$Buttons<-renderUI({
    selectInput("Alarma","Variables en el conjunto",DATOS_R()$AlarmasUnicas)
  })
  
  output$Selmaq2 <- renderUI({
    awesomeCheckboxGroup("Alarmascheck","Tipos de alarma",DATOS_R()$AlarmasUnicas, inline = TRUE)
  })
  
  output$Selal <- renderUI({
    selectInput("SelAlarma","Alarma",DATOS_R()$AlarmasUnicas)
  })
  
  #Gráfico de p_orden
  output$plot1 <- renderPlotly({
    ggplotly({
    plot_ly(data = filter(DATOS_R()$DataFrameDatos, matricula == input$Maquina), x = ~dia, y = ~p_orden, color = ~p_orden, mode = 'lines+markers', name="Valor")  %>% layout(xaxis = list(range = c(as.numeric(as.Date("2015-12-01")) * 24 * 60 * 60 * 1000, as.numeric(as.Date("2017-01-31")) * 24 * 60 * 60 * 1000)))
    })
  })
  
  # Descripcion grafico p-orden
  output$Descripcion1 <- renderText({
    paste("La gráfica representa los valores de p_orden de la máquina", input$Maquina, "a lo largo de los años.")
  })
  
  # Gráfico con el perfil de alarmas de la maquina que hemos filtrado
  output$plot2 <- renderPlotly({
    ggplotly({
      ggplot(data = filter(DATOS_R()$DataFrameDatos, matricula == input$Maquina), aes_string(x="dia", y=input$Alarma)) + geom_line(color = "gray49") + geom_point(color = "firebrick4") + theme_bw() 
    })
  })
  
  # Descripcion grafico p-orden
  output$Descripcion2 <- renderText({
    paste("La gráfica representa la progresión del perfil de la alarma", input$Alarma, "de la máquina", input$Maquina, "a lo largo de los años.")
  })

  
  # Histograma
  output$plothist <- renderPlot({
    ggplot(DATOS_R()$DataFrameDatos %>% filter(dia>input$RangoFechas[1] & dia<input$RangoFechas[2]), aes_string(input$SelAlarma)) + geom_histogram(binwidth = input$histbin,  fill = "cornflowerblue") + theme(axis.title.x = element_text(size=14), 
                                                                                                                                                                                                               axis.title.y = element_text(size=14), 
                                                                                                                                                                                                               axis.text.x = element_text(size=12),
                                                                                                                                                                                                               axis.text.y = element_text(size = 12))
    })
  
  #Boxplot para todas las maquinas
  output$plotbox <- renderPlotly({
    ggplotly({
    if (input$Caja1 == FALSE) {
      ggplot(DATOS_R()$DataFrameDatos %>% filter(dia>input$RangoFechas[1] & dia<input$RangoFechas[2]) %>% filter(matricula == input$Maquina), aes_string(x = "matricula", y =input$SelAlarma, fill="matricula")) + geom_boxplot() + theme(legend.position="none", axis.text.x = element_text(color="coral4")) 

      } else { 
      ggplot(DATOS_R()$DataFrameDatos %>% filter(dia>input$RangoFechas[1] & dia<input$RangoFechas[2]), aes_string(x = "matricula", y =input$SelAlarma, fill="matricula")) + geom_boxplot() + theme(legend.position="none", 
                                                                                                                                                                                                  axis.title.x = element_text(size=11), 
                                                                                                                                                                                                  axis.title.y = element_text(size=11),
                                                                                                                                                                                                  axis.text.x = element_text(color="coral4", 
                                                                                                                                                                                                                             size=7, 
                                                                                                                                                                                                                             angle=45,
                                                                                                                                                                                                                             vjust=0.5))
      }
    }) 
  })
  
  #Tabla
  output$tbl <- renderDataTable({DATOS_R()$DataFrameDatos %>% filter (matricula == input$Maquina) %>% select(matricula, dia, input$Alarmascheck, p_orden)})
  
  #Boton guardado
  output$download_data <- downloadHandler(
    filename = "registros_data.csv",
    content = function(file) {
      data <- DATOS_R()$DataFrameDatos %>% filter (matricula == input$Maquina) %>% select(matricula, dia, input$Alarmascheck, p_orden)
      write.csv(data, file, row.names = FALSE)
    }
  )
  }

shinyApp(ui, server)

