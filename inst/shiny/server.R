shinyServer(function(input, output) {

  output$ui <- renderUI({

    lapply(1:input$input_number, function(i) {
      output[[paste0('b', i)]] <- renderUI({
        textInput(paste0("v_",i), paste0("Name of variable ",i), value = paste0("var ",i))
      })
    })

  })

  output$ui2 <- renderUI({
    choices = list("1/9","1/8","1/7","1/6","1/5","1/4","1/3","1/2","1","2","3","4","5","6","7","8","9")

    ll = list()
    c = 1

    for(i in 1:input$input_number){
      for(j in 1:input$input_number){
        if(j>i){
          print(paste0("input$v_",i))
          ll[[c]] = selectInput(paste0(i,"-",j), label = paste0(input[[paste0("v_",i)]], " : ", input[[paste0("v_",j)]]),
                                choices = choices, selected = 1)
          c = c + 1
        }
      }
    }

    ll
  })

})
