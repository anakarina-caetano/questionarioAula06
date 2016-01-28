library(shiny)
# https://github.com/AnalytixWare/ShinySky
library(shinyBS)

temas <- c('Análise de textos' = 'texto',
           'Outros modelos: LASSO, modelo aditivo, GAMLSS' = 'modelos_plus',
           'Gráficos em HTML e D3 com HTMLWidgets' = 'htmlwidgets',
           'Otimização e ajuste de modelos usando funções do base no R' = 'justiniano',
           'Sobre paralelização, trabalhar com grandes bancos, etc.' = 'bigdata',
           'Mais análise descritiva: mapas, kaplan-meier, gráficos de multivariada, etc.' = 'descritiva_plus',
           'Web crawling e web scraping (trabalhar com arquivos html e baixar dados da web)' = 'webscraping',
           'R Open CPU (criação de APIs com o R)' = 'opencpu')
fluidPage(

  h2("Questionário sobre a aula 06"),
  tags$hr(),
  h4('Na aula 06 daremos 2 ou 3 temas de interesse da maioria dos alunos. Os temas com maior número de votos serão abordados de maneira introdutória.'),
  tags$hr(),

  wellPanel(fluidRow(
    textInput('nome', 'Nome', value = ''),
    textInput('email', 'Email', value = '')
  )),

  wellPanel(fluidRow(
    checkboxGroupInput('q1',
                       'Selecione entre 1 e 3 temas que você gostaria de ver na Aula 08',
                       temas,
                       inline = F),
    tags$p('OBS: Se você deseja ver algo que não está nessa lista fale conosco!')

  )),

  actionButton('salvar', 'Salvar!'),
  tags$br(), tags$br(),
  bsAlert('salvou'),

  tags$br(),
  tags$span('Esse questionário foi feito utilizando R e Shiny. Código disponível em',
            tags$a('https://github.com/curso-r/questionarioAula06',
                   href = 'https://github.com/curso-r/questionarioAula06',
                   target = 'blank')),
  tags$hr(),

  plotOutput('resultados')

)

