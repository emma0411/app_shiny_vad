#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(rsconnect)
library(stringr)
library(ggplot2)
library(dplyr)
library(shinythemes)
library(plotly)
library(shinydashboard)
library(emo) 
library(devtools)
library(fresh)
library(highcharter)
library(shinyWidgets)


ui <-fluidPage( 
                 theme = shinytheme('journal'),
                 tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "mq.css")),
    
    navbarPage(title = paste('App Master Ciencia de datos', ji('computer'), sep = ' '), id = 'TabApp',
                 
                 header = tagList(
                     useShinydashboard()
                 ),
                 
                 tabPanel(paste("Home",ji('person_gesturing_OK_medium_skin_tone')),
                           includeHTML("home.html")
                     
                 ),
                 
                 tabPanel('Selección de máquina', 
                          fluidRow(
                                    column(3,
                                           br(),br(),
                                         box(title=(paste('Fichero')),
                                             width = 12, status = "primary", solidHeader = T, collapsible = T,collapsed = F,
                                             
                                             fileInput("DatosFichero", paste("Selecciona el fichero de datos", ji('book'), sep = ' '), accept = NULL),
                                                     ),
                                         box(title=code('MÁQUINA'),
                                             width = 12, status = "warning", solidHeader = T, collapsible = T,collapsed = F,
                                                     uiOutput('selectmaquina'),
                                                     br(),
                                                     textOutput('active')
                                        )),
                                    column(8,
                                                 h3(strong('Probabilidad de orden')),
                                                  br(),
                                                  box(title="",
                                                      width = 12, status = "success", solidHeader = F, collapsible = F,collapsed = F,
                                                  highchartOutput("proborden")
                                                  )
                                        )
                          )),
                 navbarMenu('Estado de la máquina',
                            tabPanel(paste('Evolución temporal alarmas', ji('five'), sep = ' '),
                                     fluidRow(
                                         column(3,
                                             dropdown(style = "unite", icon = icon("info"),
                                                      status = "success", width = "600px",
                                                       h6('Para esta máquina no están activas las siguientes alarmas:'),
                                                       textOutput('evtempalarmasnoactives'),
                                               tooltip = tooltipOptions(title = "Pulse para conocer alarmas no activas!"),
                                               animate = animateOptions(
                                                   enter = animations$fading_entrances$fadeInLeftBig,
                                                   exit = animations$fading_exits$fadeOutLeftBig
                                                     )
                                                ),
                                             br(),
                                                box(title=code(paste('ALARMAS', ji('alarm'), sep = ' ')),
                                                    width = 12, status = "primary", solidHeader = T, collapsible = F,collapsed = F,
                                                uiOutput('selectalarmas'))
                                         ),
                                         column(8,
                                                br(),br(),br(),br(),
                                                box(title="",
                                                    width = 12, status = "danger", solidHeader = F, collapsible = F,collapsed = F,
                                             highchartOutput('evtempalarmas')))
                                             )
                                    ),
                            tabPanel(paste('Registros de la máquina', ji('chart increasing'), sep = ' '), 
                                     fluidRow(
                                         column(3,
                                                   box(title = code(paste('ALARMAS', ji ('alarm'))),
                                                       width = 12, status = "danger", solidHeader = T, collapsible = F,collapsed = F,
                                                                uiOutput('selectcheckbos')
                                                   )),
                                         column(8,
                                                box(title = 'Registros de la máquina seleccionada',
                                                    width = 12, status = "success", solidHeader = T, collapsible = F,collapsed = F,
                                                    dataTableOutput('registrocheck')
                                                  ))
                                     ))
                 ),
                 tabPanel('Estadísticas globales temporales', 
                          
                          fluidRow(
                              column(4,
                                     br(),br(),
                                     box(title = paste('PERIODO Y ESTADÍSTICAS', ji('chart increasing'), sep = ' '),  
                                         width = 12, status = "danger", solidHeader = T, collapsible = T,collapsed = F,
                                         
                                                     prettyRadioButtons(inputId = 'selecthistbox', label = NULL, choices =c("Histograma"="hist", "Boxplot"="box"), inline = TRUE, 
                                                                  status = "danger",
                                                                  fill = TRUE),
                                                     dateRangeInput(inputId = 'rangofecha', label = 'Selecciona el periodo', 
                                                                    start = '2016-01-01', end = '2016-12-31', format = "dd-mm-yyyy", 
                                                                    separator= "a", language = 'es'),
                                                     uiOutput('seleccionalarma2')
                                        ),
                                     box(title = code('HISTOGRAMA'),  
                                         width = 12, status = "primary", solidHeader = T, collapsible = T,collapsed = F,
                                         sliderInput(inputId = 'selectbins', label = 'Número de bins del histograma', 
                                                     min = 1, max = 50, value = 10),
                                     ),
                                     box(title = code('BOXPLOT'),  
                                         width = 12, status = "success", solidHeader = T, collapsible = T,collapsed = F,
                                         prettySwitch(inputId = 'selecttot', label = 'Todas las alarmas', value = FALSE, status = "success",
                                                      fill = TRUE)
                                     )
                                     ),
                              column(8,
                                h3(strong('Aqui mostraremos determinados estadísticos en un periodo temporal')),
                                    box(width = 12, status = "warning", solidHeader = F, collapsible = F,collapsed = F,
                                                  plotlyOutput('elegirentre'),
                                                  #plotOutput('histogramaalarma'),
                                                  #strong('Boxplot de la alarma seleccionada'),
                                                  #plotOutput('boxplotalarma')
                                    )
                                        )
                          ))
)
)

server <- function(input, output){
    
    Tipos_R <- reactive({
        req(input$DatosFichero)
        a <- input$DatosFichero
        req(a) # evita el error que se produce cuando aún no se ha cargado el fichero
        nombre_variable <- load(a$datapath) # nombre del dataframe dentro del fichero
        vars <- eval(parse(text=nombre_variable)) # evalua 'names' del dataframe
        return(vars)
    })
    output$selectmaquina <- renderUI({
        Datos <- Tipos_R()
        selectInput(inputId = 'selectmaq', label = 'Selecciona máquina',  
                    choices = unique(Datos$matricula))
    })
    datosactives <- reactive({
        Datos <- Tipos_R()
        datosactive <- Datos %>% filter( matricula == input$selectmaq)
        names <- colnames(datosactive)[str_detect(colnames(datosactive), '^a[[:digit:]]')]
        datetes <- na.omit(datosactive)
        actives <- c()
        j <- 1
        for (i in names){
            m <- (datetes[,i] != 0)
            if(length(m[m== TRUE]) != 0){
                actives[j] <- i
                j <- j + 1
            }
        }
        actives
    })
    output$selectalarmas <- renderUI({
        awesomeRadio(inputId = "radiobuttons1", label = "Selecciona la alarma a visualizar", status = "warning",
                     choices = datosactives())#colnames(Datos)[str_detect(colnames(Datos), '^a[[:digit:]]')])
    })
    output$selectcheckbos <- renderUI({
        Datos <- Tipos_R()
        multiInput(inputId = "checkboxinput1", 
                   label = "Selecciona las alarmas para ver en la tabla",
                   choices = datosactives())
       #checkboxGroupInput(inputId = "checkboxinput1", 
       #                   label = "Selecciona las alarmas para ver en la tabla",
       #                   choices = datosactives())
        #colnames(Datos)[str_detect(colnames(Datos), '^a[[:digit:]]')])
    })
    output$seleccionalarma2 <- renderUI({
        Datos <- Tipos_R()
        pickerInput(inputId = 'selectalarm', label = 'Alarma',  
                    choices = colnames(Datos)[str_detect(colnames(Datos), '^a[[:digit:]]')], options = list(
                        style = "btn-danger"))
        
    })
    datos1 <- reactive({
        Datos <- Tipos_R()
        Datos %>%
            filter( matricula == input$selectmaq) %>% filter( dia > '2016-01-01' & dia < '2016-12-31')
    })
    datos2 <- reactive({
        Datos <- Tipos_R()
        datosactive <- Datos %>% filter( matricula == input$selectmaq)
        names <- colnames(datosactive)[str_detect(colnames(datosactive), '^a[[:digit:]]')]
        datetes <- na.omit(datosactive)
        noactives <- c()
        j <- 1
        for (i in names){
            m <- (datetes[,i] != 0)
            if(length(m[m== TRUE]) == 0){
                noactives[j] <- i
                print(i)
                j <- j + 1
            }
        }
        noactives
    })
    output$active <- renderText({
        Datos <- Tipos_R()
        names <- colnames(Datos)[str_detect(colnames(Datos), '^a[[:digit:]]')]
        paste('Número alarmas no activas:', length(datos2()), 
              'Número de alarmas activas:', (length(names)-length(datos2())))       
    })
    
    output$proborden <- renderHighchart({ 
        data <- datos1()
        hc <- data %>% 
            hchart(
                "line", 
                hcaes(x = dia, y = p_orden), color = c('#A3E4D7')) %>% hc_title(text = paste('Máquina', input$selectmaq, sep = ': '))  
        
        hc
    })
    
    output$evtempalarmasnoactives <- renderText({
        paste(datos2(), '-')
    })
    
    output$evtempalarmas <- renderHighchart({
        yin <- input$radiobuttons1
        df <- select(datos1(), dia, yin)
        colnames(df) <- c("dia", "alarma")
        hc <- df %>% 
            hchart(
                "line", 
                hcaes(x = dia, y = alarma ), color = c('#A3E4D7')) %>% hc_title(text = paste("Evolución temporal", yin, sep ='-'))  
        
        hc
    })
    datos3 <- reactive({
        Datos <- Tipos_R()
        df3 <- select(Datos, matricula, dia, input$checkboxinput1, p_orden) 
        df3 <- df3 %>% filter(matricula == input$selectmaq)
        return(df3)
    })
    output$registrocheck <- renderDataTable(
        datos3(), options = list( pageLength = 15)
    )
    datos4 <- reactive({
        Datos <- Tipos_R()
        Datos %>% 
            filter(dia > input$rangofecha[1] & dia < input$rangofecha[2])
    })
    output$elegirentre <- renderPlotly({
        if(input$selecthistbox == 'hist'){
            yin1 <- input$selectalarm
            bins <- input$selectbins
            matriculah <- datos4() %>% filter(matricula == input$selectmaq)
            m <- ggplot(data = matriculah, aes(matriculah[,yin1])) +  geom_histogram(binwidth = bins, fill = 'cyan3') + ylab('count') +
                xlab(yin1)+ theme_minimal() + ggtitle('Histograma')
            ggplotly(m)
        }else{
            todas <- input$selecttot
            yin1 <- input$selectalarm
            if (todas == FALSE){
                matricula1 <- datos4() %>% filter(matricula == input$selectmaq)
                u <- ggplot(data = matricula1, aes(matricula, matricula1[,yin1])) +
                    geom_boxplot(fill = 'cyan3') +  ylab('eval(ALA)') + xlab(input$selectmaq) + theme_minimal()+ ggtitle('Boxplot')
                ggplotly(u)
            }else{
                w <- ggplot(data = datos4(), aes(matricula, datos4()[,yin1])) +
                    geom_boxplot(aes(fill = matricula)) + ylab('eval(ALA)')+ theme_minimal() + theme(axis.text.x = element_text(angle = 45),legend.position = "none") +
                     ggtitle('Boxplot')
                ggplotly(w)
            }
        }
    })
}

shinyApp(ui = ui, server = server)  

