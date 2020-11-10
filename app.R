

library(shiny)
library(leaflet)




setor_produtivo <- list("Agricultura, inclusive o apoio à agricultura e a pós-colheita
                        ","Pecuária, inclusive o apoio à pecuária
                        ","Produção florestal; pesca e aquicultura
                        ","Extração de carvão mineral e de minerais não metálicos
                        ","Extração de petróleo e gás, inclusive as atividades de apoio
                        ","Extração de minério de ferro, inclusive beneficiamentos e a aglomeração
                        ","Extração de minerais metálicos não ferrosos, inclusive beneficiamentos
                        ","Abate e produtos de carne, inclusive os produtos do laticínio e da pesca
                        ","Fabricação e refino de açúcar
                        ","Outros produtos alimentares
                        ","Fabricação de bebidas
                        ","Fabricação de produtos do fumo
                        ","Fabricação de produtos têxteis
                        ","Confecção de artefatos do vestuário e acessórios
                        ","Fabricação de calçados e de artefatos de couro
                        ","Fabricação de produtos da madeira
                        ","Fabricação de celulose, papel e produtos de papel
                        ","Impressão e reprodução de gravações
                        ","Refino de petróleo e coquerias
                        ","Fabricação de biocombustíveis
                        ","Fabricação de químicos orgânicos e inorgânicos, resinas e elastômeros
                        ","Fabricação de defensivos, desinfestantes, tintas e químicos diversos
                        ","Fabricação de produtos de limpeza, cosméticos/perfumaria e higiene pessoal
                        ","Fabricação de produtos farmoquímicos e farmacêuticos
                        ","Fabricação de produtos de borracha e de material plástico
                        ","Fabricação de produtos de minerais não metálicos
                        ","Produção de ferro gusa/ferroligas, siderurgia e tubos de aço sem costura
                        ","Metalurgia de metais não ferosos e a fundição de metais
                        ","Fabricação de produtos de metal, exceto máquinas e equipamentos
                        ","Fabricação de equipamentos de informática, produtos eletrônicos e ópticos
                        ","Fabricação de máquinas e equipamentos elétricos
                        ","Fabricação de máquinas e equipamentos mecânicos
                        ","Fabricação de automóveis, caminhões e ônibus, exceto peças
                        ","Fabricação de peças e acessórios para veículos automotores
                        ","Fabricação de outros equipamentos de transporte, exceto veículos automotores
                        ","Fabricação de móveis e de produtos de indústrias diversas
                        ","Manutenção, reparação e instalação de máquinas e equipamentos
                        ","Energia elétrica, gás natural e outras utilidades
                        ","Água, esgoto e gestão de resíduos
                        ","Construção
                        ","Comércio por atacado e varejo
                        ","Transporte terrestre
                        ","Transporte aquaviário
                        ","Transporte aéreo
                        ","Armazenamento, atividades auxiliares dos transportes e correio
                        ","Alojamento
                        ","Alimentação
                        ","Edição e edição integrada à impressão
                        ","Atividades de televisão, rádio, cinema e  gravação/edição de som e imagem
                        ","Telecomunicações
                        ","Desenvolvimento de sistemas e outros serviços de informação
                        ","Intermediação financeira, seguros e previdência complementar
                        ","Atividades imobiliárias
                        ","Atividades jurídicas, contábeis, consultoria e sedes de empresas 
                        ","Serviços de arquitetura, engenharia, testes/análises técnicas e P & D
                        ","Outras atividades profissionais, científicas e técnicas
                        ","Aluguéis não imobiliários e gestão de ativos de propriedade intelectual
                        ","Outras atividades administrativas e serviços complementares
                        ","Atividades de vigilância, segurança e investigação
                        ","Administração pública, defesa e seguridade social
                        ","Educação pública
                        ","Educação privada
                        ","Saúde pública
                        ","Saúde privada
                        ","Atividades artísticas, criativas e de espetáculos
                        ","Organizações associativas e outros serviços pessoais
                        ","Serviços domésticos")

ui <- fluidPage(
    navbarPage("Emprego e Renda", position = 'static-top'),
    theme=shinythemes::shinytheme('cosmo'),
    fixedRow(
        column(2, 
            fluidRow(column(12, wellPanel(
                        numericInput('investimento',"Investimento em reais (R$)",value=NULL,min=1),
                        selectInput("setor_produtivo","Setor Produtivo", 
                                    choices = setor_produtivo,selected = NULL),
                        selectInput("UF","UF", 
                                    choices = ("Distrito Federal"),selected = NULL),
                        selectInput("mun","Município", 
                                    choices = c("Brasília"),selected = NULL)))),
            fluidRow(column(12, wellPanel( 
                        selectInput("setor_produtivo","Setor Produtivo", 
                                    choices = setor_produtivo,selected = NULL),
                        selectInput("UF","UF", 
                                    choices = ("Distrito Federal"),selected = NULL),
                        selectInput("mun","Município", 
                                    choices = c("Brasília"),selected = NULL))))),
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
)

server <- function(input, output,session) {

    
    output$map <- renderLeaflet({
        leaflet() %>%
            addTiles(
                urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
            ) %>%
            setView(lng = -52.4704, lat = -12.3829, zoom = 4)
    })
    
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


##############teste Carlo


##########segundo teste


####### teste

#### streaming