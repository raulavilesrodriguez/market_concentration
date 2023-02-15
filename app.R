library(readxl)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(shiny)
library(rsconnect)
library(DT)

# import the data

lines_sma <- read_excel('1.1.1-Lineas-activas-por-servicio_y_Densidad_Dic-2022.xlsx', sheet = "Líneas por servicio")

lines_sma
lines_sma <- lines_sma[-(1:7),]
lines_sma <- lines_sma[-(172:198),-c(18,19)]
lines_sma[1,(3:5)]='CONECEL S.A.'
lines_sma[1,(8:10)]='OTECEL S.A.'
lines_sma[1,(13:15)]='CNT EP'
lines_sma[2, which(is.na(lines_sma[2, ]))] = ''
new_names <- sapply(lines_sma[1:2, ], paste, collapse=" ")
colnames(lines_sma) <- new_names
lines_sma <- lines_sma[-(1:2),]
lines_sma[is.na(lines_sma)] <- '0'
# transform of character to numeric
lines_sma[,-1] <- lapply(lines_sma[,-1], as.numeric)
lines_sma <- lines_sma|> mutate(`MES/AÑO `=as.character(as.Date(as.numeric(`MES/AÑO `), origin="1899-12-30")))
sapply(lines_sma, class)

# concentración de mercados tibble
df <- lines_sma[,-c(2:5, 7:10, 12:15)]
df <- df[-(1:127), ]
colnames(df) <- c('mes', 'conecel', 'otecel', 'cnt', 'total')

df |> ggplot(aes(x=mes, y=conecel)) + geom_line() +
  geom_smooth() +
  theme(axis.text.x = element_text(angle = 90))


trimestres <- c()
for(i in seq(1, nrow(df), 3)){
  j <- i + 2
  trim <- sapply(df[i:j, 1], paste, collapse = " ")
  trimestres <- c(trimestres, trim[[1]])
}

sumar_trim <- function(df, x){
  vectore <- c()
  for(i in seq(1, nrow(df), 3)){
    val <- df[i, x] + df[i+1, x] + df[i+2, x]
    vectore <- c(vectore, val[1,1])
  }
  vectore
}

sum_conecel <- sumar_trim(df, 2)
sum_otecel <- sumar_trim(df, 3)
sum_cnt <- sumar_trim(df, 4)
sum_total <- sumar_trim(df, 5)

df_mercado <- tibble(trimestre=trimestres,
                     conecel=sum_conecel,
                     otecel=sum_otecel,
                     cnt=sum_cnt,
                     total=sum_total)
df_mercado <- df_mercado |> mutate(porce_conecel=conecel/total*100,
                                   porce_otecel=otecel/total*100,
                                   porce_cnt=cnt/total*100)
df_mercado <- df_mercado |> mutate(pago_conecel= ifelse(porce_conecel >=30 & porce_conecel <=34.99, 0.5,
                                                 ifelse(porce_conecel >=35 & porce_conecel <=44.99, 1,
                                                 ifelse(porce_conecel >=45 & porce_conecel <=54.99, 3,
                                                 ifelse(porce_conecel >=55 & porce_conecel <=64.99, 5,
                                                 ifelse(porce_conecel >=65 & porce_conecel <=74.99, 7,
                                                 ifelse(porce_conecel >=75, 9, 0)))))))
df_mercado <- df_mercado |> mutate(pago_otecel= ifelse(porce_otecel >=30 & porce_otecel <=34.99, 0.5,
                                                 ifelse(porce_otecel >=35 & porce_otecel <=44.99, 1,
                                                 ifelse(porce_otecel >=45 & porce_otecel <=54.99, 3,
                                                 ifelse(porce_otecel >=55 & porce_otecel <=64.99, 5,
                                                 ifelse(porce_otecel >=65 & porce_otecel <=74.99, 7,
                                                 ifelse(porce_otecel >=75, 9, 0)))))))
df_mercado <- df_mercado |> mutate(pago_cnt= ifelse(porce_cnt >=30 & porce_cnt <=34.99, 0.5,
                                             ifelse(porce_cnt >=35 & porce_cnt <=44.99, 1,
                                             ifelse(porce_cnt >=45 & porce_cnt <=54.99, 3,
                                             ifelse(porce_cnt >=55 & porce_cnt <=64.99, 5,
                                             ifelse(porce_cnt >=65 & porce_cnt <=74.99, 7,
                                             ifelse(porce_cnt >=75, 9, 0)))))))

df_mercado |> ggplot(aes(porce_otecel, fill = "grey")) +
  geom_density(alpha = 0.2, bw = 0.75, position = "stack") +
  theme(axis.text.x = element_text(angle = 90))

#Calculations indicators_______________
indicators <- df_mercado |> summarise(var_pago_conecel = var(pago_conecel),
                        var_pago_otecel = var(pago_otecel),
                        sd_pago_conecel = sd(pago_conecel),
                        sd_pago_otecel = sd(pago_otecel))

#df$mes <- lapply(df$mes, as.character) #transform dato to character
datos <- list(df, df_mercado)
df_mercado[vapply(df_mercado, is.numeric, logical(1))]


#Shiny_____________
myApp <- function(){
  ui <- fluidPage(
    theme = bslib::bs_theme(bootswatch = "united"),
    HTML(r"(
         <h1 style="text-align:center">ANÁLISIS DE CONCENTRACIÓN DE MERCADO</h1>
         <h6 style="color:#609EA2;">
         <a style="text-decoration: none"
         target="_blank"
         href="https://raulaviles.netlify.app/">
         Designed by: Byron Raúl Avilés Rodríguez
         </a>
         </h6>
    )"),
    sidebarLayout(
      sidebarPanel(
        selectInput('setdata', 'Escoge los Datos:',
                    choices = c("Líneas SMA"="1",
                                "Participaciones de Mercado"="2")),
        selectInput("var", "Variable:", choices = NULL),

      ),
      mainPanel(
        plotOutput("plot_1"),
      )
    ),
    fluidRow(
      titlePanel("INDICADORES"),
      column(12, tableOutput("indicadores"))
    ),
    fluidRow(
      column(12,titlePanel(textOutput("titulo_tabla"))),
      column(12, DT::dataTableOutput("my_tabla"))
    )
  )

  server <- function(input, output, session) {
    data <- reactive({
      req(input$setdata) #que sea conocido
      tibble(datos[[as.numeric(input$setdata)]])
    })
    #<< tables
    output$indicadores <- renderTable(indicators)
    output$my_tabla <- DT::renderDataTable(data())
    observeEvent(data(), {
      updateSelectInput(session, "var", choices = colnames(data())[-1])
    })
    #<< Name Table
    string <- reactive(
      if(as.numeric(input$setdata)==1){
        paste0("Tabla de LINEAS ACTIVAS SMA")
      }
      else{
        paste0("Tabla de PARTICIPACIONES DE MERCADO")
      }
    )

    output$titulo_tabla <- renderText(string())

    #<< Graphs
    thematic::thematic_shiny()
    output$plot_1 <- renderPlot({
      req(input$var)
      req(data())
      x_1 <- colnames(data())[1]
      ggplot(data(), aes(.data[[x_1]], .data[[input$var]], group = 1)) +
        geom_line() + geom_smooth() +
        theme(axis.text.x = element_text(angle = 90))
    }, res = 96)
  }

  shinyApp(ui, server)
}

myApp()




