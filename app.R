setwd("C:/Users/carlo/Documents/Faculdade/Topicos/Shinny-Topicos/Topicos")

#install.packages("openxlsx")


library(shiny)
library(leaflet)
library(openxlsx)
library(tidyverse)

# Base 1: Matriz de Leontief
base1 <- read.xlsx("BASES-EMPREGO-E-RENDA.xlsx",sheet=6)

# Base 2: RAIS compilada IBGE
base2 <- read.xlsx("BASES-EMPREGO-E-RENDA.xlsx",sheet=7)

# Base 3: Distancias entre municipios
base3 <- read.xlsx("BASES-EMPREGO-E-RENDA.xlsx",sheet=8)


setor_produtivo <- base1$Setores
UF<- c("Acre", "Alagoas", "Amapa", "Amazonas", "Bahia", 
       "Ceara", "Distrito Federal", "Espirito Santo", "Goias", "Maranhao", 
       "Mato Grosso", "Mato Grosso do Sul", "Minas Gerais", "Para", "Paraiba", 
       "Parana", "Pernambuco", "Piaui", "Rio de Janeiro", "Rio Grande do Norte", 
       "Rio Grande do Sul", "Rondonia", "Roraima", "Santa Catarina", "Sao Paulo", 
       "Sergipe", "Tocantins")



ui <- fluidPage(
    
    sidebarLayout(
        
    sidebarPanel(fluidRow(column(12, wellPanel(
        numericInput('investimento',"Investimento em reais (R$)",value=NULL,min=1),
        selectInput("setor_produtivo","Setor Produtivo", 
                    choices = setor_produtivo,selected = NULL),
        selectInput("UF","Estado", 
                    choices = UF,selected = NULL),
       
         selectInput("mun","Município", 
                    choices = c("Brasília"),selected = NULL)))),),
    mainPanel(
    
    titlePanel("Trabalho Topicos "),
    navbarPage("Emprego e Renda", position = 'static-top'),
    theme=shinythemes::shinytheme('cosmo'),
    fixedRow(
        column(6, 
            fluidRow(column(12, wellPanel(
                leafletOutput("map",height = 600)))),
            fluidRow(column(12,wellPanel(
                textOutput('fonte')
            )))),
         column(4,
             fluidRow(column(6,wellPanel(
                            tableOutput('tabela'))),
                      column(6,wellPanel(
                            tableOutput('tabela2')))),
             fluidRow(column(12, wellPanel(
                textOutput('indicador')))
               )
        )
    )
)))

server <- function(input, output,session) {

    
    output$map <- renderLeaflet({
        leaflet() %>%
            addTiles(
                urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
            ) %>%
            setView(lng = -52.4704, lat = -12.3829, zoom = 4)
    })
    UF<-reactive(imput$UF)
    name <- c("Brasil","Argentina","Venezuela","Alemanha","Inglaterra","China","Japão","Australia","Russia","Canada")
    posi <- c(1,2,3,4,5,6,7,8,9,10)
    tab <- data.frame(name,posi)
    output$tabela <- renderTable({
        tab
    })
    output$tabela2<- renderTable({
        tab
    })
    output$indicador <- renderText({
        "Indicador sobre o impacto do setor que recebeu o investimento em cadeias produtivas anteriores ou posteriores."
    })
    output$fonte <- renderText({
        "Indicação das bases de dados utilizadas com as respectivas fontes."
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


