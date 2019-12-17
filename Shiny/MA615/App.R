library(leaflet)
library(shiny)
library(tidyverse)
library(stringr)

tem = read.csv("TEM.csv")
pl.da = dplyr::select(tem,-c(X,business_id,city,state,review_count,is_open,categories,
                             China.p,India.p,Italy.p,Japan.p,France.p,Mexico.p))
sel = colnames(pl.da)
sel = sel[-c(2,3,4,5,6,7)]

ui = navbarPage("Restaurants Distribution", id = "nav",
           
           tabPanel("Distribution Map",
                    div(class = "outer",
                        
                        tags$head(
                          includeCSS("styles.css")
                        ),
                        
                        leafletOutput("map",width = 1280,height = 720),
                        absolutePanel(id = "control", class = "panel panel-default",fixed = TRUE,
                                      draggable = TRUE, top = 60,left = "auto",right = 30,bottom = "auto",
                                      width = 400,height = "auto",
                                      
                                      h2("Feature Explorer"),
                                      
                                      selectInput("yase","Select an attributes to color the map", sel),
                                      plotOutput("Distribution")
                                      
                                      ),
                                      tags$div(id="cite",
                                               'Data compiled for ', tags$em('Coming Apart: The State of White America, 1960-2010'), ' by Charles Murray (Crown Forum, 2012).'
                              )
                        )
                    ),
           tabPanel("Data Explorer",
                    fluidRow(
                      column(3,selectInput("Postal_code","Select Postal Code",c("All Districts"="",unique(pl.da$postal_code)),multiple = TRUE)),
                      column(3,selectInput("sentiments","Select Sentiments",c("All Sentiments"="",unique(pl.da$sentiments)),multiple = TRUE)),
                      column(3,selectInput("star","Select Star Ratings",c("All"="",unique(pl.da$stars)),multiple = TRUE)),
                      column(3,
                             textOutput("Explain")
                             ),
                    ),
                    hr(),
                    tableOutput("table")
                    ),
           
           conditionalPanel("false", icon("crosshair"))
           )

server <- function(input, output,session) {
  output$map = renderLeaflet({
    lef = function(x){
      att = x
      getColor = function(x){
        a = att
        b = which(colnames(x)==a)
        se = x[b] %>% as.data.frame()
        n = length(unique(se[,1]))
        set.seed(2019)
        co = colors()
        co.in = sample(x = 1:length(co),size = n)
        pal <- colorFactor(
          palette = co[co.in],
          domain = se[,1])
        return(list(value = se[,1],color = pal(se[,1])))
      }
      
      re = getColor(pl.da)
      
      plot = leaflet(data = pl.da) %>%
        setView(mean(pl.da$longitude), mean(pl.da$latitude), zoom = 12) %>% 
        addProviderTiles(providers$CartoDB.Positron, group = "Map") %>%
        addProviderTiles("Esri.WorldImagery", group = "Satellite")%>% 
        addProviderTiles("Esri.WorldShadedRelief", group = "Relief")%>%
        addCircles(lng =~longitude,lat = ~latitude,label=~name,group = "attributes",color= ~re$color)%>%
        addScaleBar(position = "bottomleft") %>%
        #addLegend(group = "bottomright", pal = re$color, values = as.numeric(re$value),title = att,opacity = 1)%>%
        addLayersControl(
          baseGroups = c("Map", "Satellite", "Relief"),
          overlayGroups = c("attributes","bottomright"),
          options = layersControlOptions(collapsed = T)
        )
      re = data.frame(value = re$value,color = re$color) %>% distinct()
      return(list(plot = plot,legend = re))
    }
    
    # "sentiments"  "name"        "address"     "postal_code" "latitude"    "longitude"   "stars"  
    
    re = lef(input$yase)
    re$plot
  })
  
  output$Distribution = renderPlot({
    tme = pl.da %>% dplyr::select(stars,input$yase)
    colnames(tme) = c("stars","attributes")
    inp = input$yase
    if(inp == "sentiments"){na = inp}else{
      a = str_split(string = inp,pattern = "[.]",simplify = T)
      na = a[1,2]
    }
    tme = tme %>% group_by(attributes) %>% summarise(stars = mean(stars))
    ggplot(tme) + geom_col(aes(x = factor(attributes),y = stars,fill = factor(attributes))) + ylab("Average Star Ratings") + 
      labs(fill = na) + 
      xlab(na)
  })
  
  observeEvent(input$Postal_code,{
    ls.sen = pl.da %>% filter(postal_code %in% input$Postal_code) %>% `$`(sentiments) %>% unique() %>% sort()
    updateSelectInput(session,inputId = "sentiments",choices = ls.sen)
  })
  
  observeEvent(c(input$sentiments,input$Postal_code),{
    ls.st = pl.da %>% filter(postal_code %in% input$Postal_code & sentiments %in% input$sentiments) %>%
      `$`(stars) %>% unique() %>% sort()
    updateSelectInput(session,inputId = "star",choices = ls.st)
  })
  
  output$table = renderTable({
    df = pl.da %>% filter(postal_code %in% input$Postal_code & 
                            sentiments %in% input$sentiments &
                            stars %in% input$star)
    df
  })
  
  output$Explain = renderText({
    "Selection support muliple selection, if no results are shown, it means no restaurants satisfy, please select more."
  })
 
}
shinyApp(ui = ui, server = server)
