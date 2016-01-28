library(shiny)
library(shinysky)
library(dplyr)
library(stringr)
library(lubridate)
library(RMySQL)
library(ggplot2)

shinyServer(function(input, output, session) {

  valida <- reactive({
    if (!is.null(input$q1) & !is.null(input$email)) {

      val <- (length(input$q1) %in% c(1, 2, 3)) & str_detect(input$email, fixed('@'))

      return(val)
    } else {
      return(0)
    }
  })

  dados <- reactive({
    d <- data_frame(datetime = as.character(now()),
                    email = input$email,
                    q1 = paste(input$q1, collapse = '@')) %>%
      as.data.frame()
    return(d)
  })

  salva <- reactive({
    res <- FALSE
    try({
      drv <- dbDriver("MySQL")
      con <- dbConnect(drv,
                       host = host,
                       dbname = dbname,
                       user = username,
                       password = password,
                       port = 3306)
      # tabela_atual <- dbReadTable(con, 'questionario')
      d <- dados()
      if (dbExistsTable(con, 'questionarioAula06')) {
        res <- dbWriteTable(con, 'questionarioAula06', d,
                            append = T, row.names = F, overwrite = F)
      } else {
        res <- dbWriteTable(con, 'questionarioAula06', d, row.names = F)
      }
      dbDisconnect(con)
    })
    return(res)
  })

  observe({
    aux <- input$salvar
    isolate({
      val <- valida()
      res <- FALSE

      if (aux > 0 & val) {
        res <- salva()
      }

      if (aux > 0 & val & res) {
        createAlert(session, 'salvou', content = 'Salvou!!', style = 'success')
      } else if (aux == 0) {
        createAlert(session, 'salvou', content = 'Clique em salvar!', style = 'warning')
      } else if (!val) {
        createAlert(session,'salvou',
                    content = 'Existem campos obrigatórios que não foram preenchidos corretamente',
                    style = 'danger')
      } else if (!res) {
        createAlert(session,'salvou', content = 'Ocorreu um erro na base de dados.', style = 'danger')
      }

    })
  })

  output$resultados <- renderPlot({
    aux <- input$salvar
    try({
      drv <- dbDriver("MySQL")
      con <- dbConnect(drv,
                       host = host,
                       dbname = dbname,
                       user = username,
                       password = password,
                       port = 3306)
      if (dbExistsTable(con, 'questionarioAula06')) {
        d <- dbReadTable(con, 'questionarioAula06')
        d %>%
          tidyr::separate(q1, c('a', 'b', 'c'), sep = '@', fill = 'left') %>%
          tidyr::gather(key, tema, a, b, c) %>%
          filter(!str_detect(email, 'trecenti|^asa@$')) %>%
          filter(!is.na(tema)) %>%
          ggplot(aes(x = tema, fill = tema)) +
          geom_bar() +
          guides(fill = F) +
          theme_bw()
      } else {
        ggplot(mtcars) +
          theme_bw()
      }
    })
  })

})
