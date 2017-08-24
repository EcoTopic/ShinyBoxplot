source("boxplotte.R")


shinyServer(function(input, output, session) {
    observe({
      query <- parseQueryString(session$clientData$url_search)
      for (i in 1:(length(reactiveValuesToList(input)))) {
        nameval = names(reactiveValuesToList(input)[i])
        valuetoupdate = query[[nameval]]
        
        if (!is.null(query[[nameval]])) {
          if (is.na(suppressWarnings(as.numeric(valuetoupdate)))) {
            updateTextInput(session, nameval, value = valuetoupdate)
          }
          else {
            updateTextInput(session, nameval, value = as.numeric(valuetoupdate))
          }
        }
      }
    })
  
#   output$cropControl <- renderUI({
#     if(input$country=="RSA"){
#       selectInput("crop", 
#                   label="Crop",
#                   choices=c("maize","sorghum","bean","cowpea","soybean"),
#                   selected="maize")
#     }
#     else if(input$country=="KENYA"){
#       selectInput("crop", 
#                   label="Crop",
#                   choices=c("maize","sorghum","soybean"),
#                   selected="maize")
#     }
#     else if(input$country=="BOT"){
#       selectInput("crop", 
#                   label="Crop",
#                   choices=c("maize","sorghum","cowpea"),
#                   selected="maize")
#     }
#     else if(input$country=="MAUR"){
#       selectInput("crop", 
#                   label="Crop",
#                   choices=c("maize","bean"),
#                   selected="maize")
#     }
#   }) 
   
    output$map <- renderPlot({
      if(!is.null(input$crop)){
      # dataInput<-reactive({
      #   fetchData(input$crop,input$country,input$metGrid)
      # })
        seasonSelect<-input$seasonClass
        soilSelect<-input$soil
        cropSelect<-input$crop
        countrySelect<-input$country
        metGridSelect<-input$metGrid
        dataInput<-fetchData(cropSelect,countrySelect,metGridSelect,soilSelect,seasonSelect)
      # dat0=dataInput()
        dat0=dataInput  
      
      dat0$classSeason <- factor(dat0$rainClass, levels=c("BN","N","AN"))
      
      if(cropSelect=="soybean"){
        dat0$cultivar<- factor(dat0$cultivar,levels=c("VeryEarly","Early","Medium","Late"))
        xlabels= c("VeryEarly","Early","Medium","Late")
        ylabels="Soybean yield (kg/ha)"
        dat1=subset(dat0,soil==soilSelect)
        ymin=suppressWarnings(min(dat1$cropYield))
        ymax=suppressWarnings(max(dat1$cropYield))
#         par(mfrow=c(1,3), pty="s")
#         for(i in sort(unique(dat1$classSeason))){
#           dat3=subset(dat1,classSeason==i)
#           plotGraph2(dat3,xlabels,ylabels,input$soil,i,ymin,ymax)
#         }
        #dat2=subset(dat1,classSeason==caption)
        par(mfrow=c(1,1), pty="s")
        plotGraph2(dat1,xlabels,ylabels,soilSelect,seasonSelect,ymin,ymax)
        }
      else if(cropSelect=="cowpea"){
       dat0$cultivar<- factor(dat0$cultivar,levels=c("Early", "Medium", "Late"))
       xlabels= c("Early","Medium","Late")
       ylabels="Cowpea yield (kg/ha)"
       dat1=subset(dat0,soil==soilSelect)
       ymin=suppressWarnings(min(dat1$cropYield))
       ymax=suppressWarnings(max(dat1$cropYield))
       #dat2=subset(dat1,classSeason==seasonSelect)
       par(mfrow=c(1,1), pty="s")
       plotGraph2(dat1,xlabels,ylabels,soilSelect,seasonSelect,ymin,ymax)
       }
      else if(cropSelect=="maize"){
       dat0$cultivar<- factor(dat0$cultivar,levels=c("early", "medium", "late"))
       dat0$fertiliser<-factor(dat0$fertiliser, levels=c("5kgN","20kgN","30kgN","40kgN","50kgN"))
       xlabels= c("5","20","30","40","50")
       ylabels="Maize yield (kg/ha)"
       dat1=subset(dat0,soil==soilSelect)
       #dat2=subset(dat1,classSeason==caption)
       ymin=suppressWarnings(min(dat1$cropYield))
       ymax=suppressWarnings(max(dat1$cropYield))
       par(mfrow=c(1,3), pty="s")
       for(i in sort(unique(dat1$cultivar))){
         dat3=subset(dat1,cultivar==i)
         plotGraph(dat3,xlabels,ylabels,dat3$classSeason,soilSelect,i,ymin,ymax)
       }
      }
      else if(cropSelect=="sorghum"){
        dat0$cultivar<- factor(dat0$cultivar,levels=c("early", "medium", "late"))
        dat0$fertiliser<-factor(dat0$fertiliser, levels=c("10kgN","20kgN","30kgN","40kgN","50kgN"))
        xlabels= c("10","20","30","40","50")
        ylabels="Sorghum yield (kg/ha)"
        dat1=subset(dat0,soil==soilSelect)
        #dat2=subset(dat1,classSeason==caption)
        ymin=suppressWarnings(min(dat1$cropYield))
        ymax=suppressWarnings(max(dat1$cropYield))
        par(mfrow=c(1,3), pty="s")
        for(i in sort(unique(dat1$cultivar))){
          dat3=subset(dat1,cultivar==i)
          plotGraph(dat3,xlabels,ylabels,dat3$classSeason,soilSelect,i,ymin,ymax)
        }
      }
      else if(cropSelect=="bean"){
        if(input$country=="RSA"){
          dat0=subset(dat0,fertiliser=="10kgN")
          dat0$cultivar<- factor(dat0$cultivar,levels=c("early", "medium", "late"))
        }
        else{
          dat0$cultivar<- factor(dat0$cultivar,levels=c("Early", "Medium", "Late"))
        }
        xlabels= c("Early","Medium","Late")
        ylabels="Bean yield (kg/ha)"
        dat1=subset(dat0,soil==soilSelect)
        ymin=suppressWarnings(min(dat1$cropYield))
        ymax=suppressWarnings(max(dat1$cropYield))
        dat2=subset(dat1,classSeason==seasonSelect)
        par(mfrow=c(1,1), pty="s")
        plotGraph2(dat2,xlabels,ylabels,soilSelect,seasonSelect,ymin,ymax)
      }
      
      
      } # end of if (!isnul...)      
    })

  })


