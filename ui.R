source("boxplotte.R")

# function listOfMetFiles has been updated, so that it uses the table metGrids. 
# This table needs to be regenerated each time new data is uploaded with this statement in
# MYSQL (add new table names...: 
#   create table metGrids select distinct metGrid from (select metGrid from maizeRSA union select metGrid from maizeBOT union select metGrid from maizeKENYA union select metGrid from maizeMAUR union select metGrid from maizeZWE union select metGrid from maizeMWI union select metGrid from maizeTZA  union select metGrid from maizeZMB) t;
#
# add the country code to line 26: choices=c("RSA","BOT", ....
#
# Potententially add columns country and crop. 


listOfMetGrids = listOfMetFiles()

shinyUI(fluidPage(
  killDbConnections(),
  #plotOutput("map", "100%","200px"),
  #plotOutput("map","100%", "300px"),
  #titlePanel("cropDST"),
  #fluidRow(
  #  column(12,
         conditionalPanel(
           condition ="1==2",
             selectInput("country",
                         label="Country",
                         choices=c("RSA","BOT","KENYA","MAUR","ZWE","TZA","ZMB","MWI","LSO","SWZ","NAM","MDG","AGO"),
                         selected="KENYA"),
             selectInput("crop",
                         label="Crop",
                         choices=c("maize","sorghum","soybean","bean","cowpea"),
                         selected="maize"),
             selectInput("metGrid", 
                         label = "Chosen metGrid",
                         #choices = c("226_420","226_421","226_422","171_430"),
                         choices = listOfMetGrids,
                         selected = listOfMetGrids[1]),
             selectInput("soil", 
                         label = "Choose a soil to display",
                         choices = seq(1,27),
                         selected = 1),
             selectInput("seasonClass", 
                           label = "Choose a rainfall catergory to display",
                           choices = c("BN", "N","AN"),
                           selected = "N")
         ),
  plotOutput("map","100%", "300px")
  )
)

#)
#)