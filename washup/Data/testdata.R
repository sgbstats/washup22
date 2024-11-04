input=list()
input$group1="Ward"
input$prevvoters1=T
input$year1="L24"
input$ward1="Didsbury West"
input$votehistory1_24="L23"
input$regcycle1=unique((Data %>% arrange(regcycle))$regcycle) %>% as.character()

input$group2="Shuttle2"
input$prevvoters2=T
input$year2="L24"
input$ward2="Didsbury West"
input$votehistory2_24="L23"
input$regcycle2=unique((Data %>% arrange(regcycle))$regcycle) %>% as.character()
input$shuttle2=c(T,F)
input$voters2=c(T,F)

input$group3="Shuttle2"
input$prevvoters3=T
input$year3="L24"
input$ward3="Didsbury West"
input$votehistory2_23="L23"
input$regcycle3=unique((Data %>% arrange(regcycle))$regcycle) %>% as.character()
input$shuttle3=c(T,F)
input$voters3=c(T,F)


input$group5="Shuttle2"
input$prevvoters5=T
input$year5="L24"
input$ward5="Didsbury West"
input$votehistory2_25="L23"
input$regcycle5=unique((Data %>% arrange(regcycle))$regcycle) %>% as.character()
input$shuttle5=c(T,F)
input$voters5=c("In Person", "Postal")
input$leftreg5=F
input$election5_24="L23"
input$election5_23="L22"
