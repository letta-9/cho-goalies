library(shiny)
library(shinydashboard)
library(readr)
library(DT)
library(dplyr)
library(tidyr)
library(plyr)
library(shinyBS)
library(ggplot2)
library(ggpubr)
library(png)
library(ggforce)


# Read CSV file with goal data
ga <- read.csv('qu_ga.csv')


img <- readPNG("www/goalie.png", native=TRUE)


##################
# USER INTERFACE #
##################


ui <- fluidPage(
  
  titlePanel(title = 'QUINNIPIAC HOCKEY GOALTENDERS'),
  sidebarLayout(
    sidebarPanel(
      img(src='bobcat.png', width='50%'),
      br(),
      selectInput('goalie','Goaltender', choices = unique(ga$goalie)),
      selectInput('opp','Opponent', choices = c('All', 'Boston College (10/7/22)',unique(ga$opp)), selected = 'All'),
      br(),
      
      checkboxGroupInput("per", "Period",
                         c("1" = "first",
                           "2" = "sec",
                           "3" = "third",
                           "OT" = "ot"), selected = c('first','sec','third','ot')),
      
      checkboxGroupInput("crunch", "Crunch Time (<5 min)",
                         c("Yes" = "yes",
                           "No" = "no"), selected = c('yes','no')),
      
      checkboxGroupInput("str", "Strength",
                         c("Full Strength" = "full",
                           "Man Down" = "kill",
                           "2 Men Down" = "doubkill",
                           "Shorthanded" = "short"), selected = c('full','kill','doubkill','short')),
      
      checkboxGroupInput("hand", "Hand",
                         c("Right Handed" = "R",
                           "Left Handed" = "L"), selected = c('R','L')),      
      
      
      checkboxGroupInput("shot_type", "Shot Type",
                         c("Slap Shot" = "slap",
                           "Wrist Shot" = "wrst",
                           "Snap Shot" = "snap",
                           "Backhand" = "back",
                           "Tip" = "tip"), selected = c('slap','wrst','snap','back','tip')),
      
      checkboxGroupInput("dist", "Ice Location",
                         c("High-Danger Zone" = "HDZ",
                           "Medium-Danger Zone" = "MDZ",
                           "Low-Danger Zone" = "LDZ"), selected = c('HDZ','MDZ','LDZ')),
      
      checkboxGroupInput("misc", "Misc",
                         c("N/A" = "none",
                           "One Timer" = "onet",
                           "Deflection" = "defl",
                           "Rebound" = "reb",
                           "Breakaway" = "brk"), selected = c('none','onet','defl','reb','brk')),
      
    
      
      br(),
      checkboxInput('all', 'Select All'),
      width = 3
    ),
    mainPanel(
      column(6, plotOutput('heatmap')),
      column(6, plotOutput('scatter')),
      textOutput('update')
    )
  )
)



##########
# SERVER #
##########


server <- function(input, output, session){
  
  
  observe({
    updateCheckboxGroupInput(session, 'hand', selected = if(input$all) c('R','L'))
    updateCheckboxGroupInput(session, 'shot_type', selected = if(input$all) c('slap','wrst','snap','back','tip'))
    updateCheckboxGroupInput(session, 'dist', selected = if(input$all) c('high','mid','low'))
    updateCheckboxGroupInput(session, 'misc', selected = if(input$all) c('none','onet','defl','reb','brk'))
    updateCheckboxGroupInput(session, 'per', selected = if(input$all) c('first','sec','third','ot'))
    updateCheckboxGroupInput(session, 'str', selected = if(input$all) c('full','kill','doubkill','short'))
    updateCheckboxGroupInput(session, 'crunch', selected = if(input$all) c('yes','no'))
  })

  
  filtered_data = reactive({
    if (input$opp != 'All'){
      ga <- ga %>% filter(opp == input$opp)
    }
    
    # checkboxGroup outputs a vector of char of the boxes that are checked
    # Here filter df if column is %in% said vector
    
    ga <- ga %>% filter(goalie == input$goalie) %>%
      filter(hand %in% input$hand) %>%
      filter(type %in% input$shot_type) %>%
      filter(dist %in% input$dist) %>% 
      filter(misc %in% input$misc) %>%
      filter(per %in% input$per) %>%
      filter(crunch %in% input$crunch) %>%
      filter(strength %in% input$str)
    
    })
  
  net_loc = reactive({
  
    loc <- data.frame(loc = c(1:9))
    
    # Counts the amount of goals per location
    n <- filtered_data() %>% dplyr::count(loc)
    
    # Combine two dataframes by location
    loc <- merge(loc, n, by= 'loc', all=TRUE)
    loc$n[is.na(loc$n)] <- 0
    
    # Create x and y coordinates
    loc$x <- c('X3','X3','X3','X2','X2','X2','X1','X1','X1')
    loc$y <- c('Y1','Y2','Y3','Y1','Y2','Y3','Y1','Y2','Y3')
    
    loc <- as.data.frame(loc)
    
  })
    
  
  
  
  output$heatmap <- renderPlot({
    # Plot Heatmap
    goals_hm <- ggplot(net_loc(), aes(y, x)) +
      geom_tile(aes(fill = n)) +
      scale_fill_gradient(low="white",high="red") +
      geom_text(aes(label = n)) +
      ggtitle(paste0('GOALS BY NET LOCATION: ', input$goalie)) + 
      background_image(img)
    
    goals_hm
  })
  
  output$scatter <- renderPlot({
    goal_loc <- ggplot(filtered_data(), aes(x=x,y=y)) +
      geom_point(shape = 8, colour = 'blue', size = 4) +
      xlim(0,85) +
      ylim(0, 64) + 
      geom_hline(yintercept=64, color = 'blue', size = 2) +         #Blue Line
      geom_hline(yintercept=0, color = 'red', size = 2) +           #Red Line
      annotate("rect", xmin=20.5, xmax=64.5, ymin=22, ymax= 37,     #High Danger Zone
               fill=NA, colour="orange", size = 2) +
      annotate("rect", xmin=35.5, xmax=49.5, ymin=0, ymax= 22,      #Medium Danger Zone Large Rect
               fill=NA, colour="red", size = 2) +
      annotate("rect", xmin=35.5, xmax=49.5, ymin=37, ymax= 50,     #Medium Danger Zone Small Rect
               fill=NA, colour="orange", size = 2) + 
      annotate("segment", x = 20.5, xend = 35.5, y = 22, yend = 0,  #Medium Danger Zone Left Seg
               colour = "orange", size = 2) +
      annotate("segment", x = 64.5, xend = 49.5, y = 22, yend = 0,  #Medium Danger Zone Right Seg
               colour = "orange", size = 2) + 
      annotate("segment", x=3.5, xend=5.5, y=24, yend= 24,          #Top Left Hash, Left Circle
               colour="black") +
      annotate("segment", x=3.5, xend=5.5, y=20, yend= 20,          #Bottom Left Hash, Left Circle
               colour="black") +
      annotate("segment", x=35.5, xend=37.5, y=24, yend= 24,        #Top Right Hash, Left Circle
               colour="black") +
      annotate("segment", x=35.5, xend=37.5, y=20, yend= 20,        #Bottom Right Hash, Left Circle
               fill=NA, colour="black") +
      annotate("segment", x=47.5, xend=49.5, y=24, yend= 24,        #Top Left Hash, Right Circle
               colour="black") +
      annotate("segment", x=47.5, xend=49.5, y=20, yend= 20,        #Bottom Left Hash, Right Circle
               colour="black") +
      annotate("segment", x=79.5, xend=81.5, y=24, yend= 24,        #Top Right Hash, Right Circle
               colour="black", size = 1) +
      annotate("segment", x=79.5, xend=81.5, y=20, yend= 20,        #Bottom Right Hash, Right Circle
               fill=NA, colour="black", size = 1) +
      geom_circle(aes(x0=20.5, y0=22, r=15),                        #Left Circle
                  inherit.aes=FALSE) +
      geom_circle(aes(x0=64.5, y0=22, r=15),                        #Right Circle
                  inherit.aes=FALSE) +
      geom_circle(aes(x0=42.5, y0=0, r=6),                          #Crease
                  inherit.aes=FALSE) +
      annotate(geom="text", x=42.5, y=11, label="High-Danger Zone", color = 'red', size = 3.8,
               colour="red") +
      annotate(geom="text", x=42.5, y=30, label="Medium-Danger Zone", color = 'orange',
               colour="red") +
      annotate(geom="text", x=42.5, y=55, label="Low-Danger Zone", color = 'black',
               colour="red") +
      ggtitle(paste0('GOALS BY ICE LOCATION: ', input$goalie))
    
    goal_loc
  })
  
  output$update <- renderText('*Updated through 10/15/22 games')
}

shinyApp(ui, server)


