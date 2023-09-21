setwd("/Users/ellisschwartz/Desktop/MLB Draft League")
install.packages("shiny")
library(shiny)
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("shinydashboard")
library(shinydashboard)
install.packages("readxl")
library(readxl)
install.packages("knitr")
library(knitr)
install.packages("tidyr")
library(tidyr)
gamelogs <- read.csv("/Users/ellisschwartz/Desktop/MLB Draft League/SeasonDataBothHalves.csv")
gamelogs$date <- as.Date(gamelogs$date,"%Y-%m-%d")
thunderbats <- gamelogs%>%
  unite(col='count',c(balls,strikes),sep='-')%>%
  filter(gamelogs$batterteam == "TRE_THU")

header <- dashboardHeader(title = "Trenton Thunder Hitter Reports",titleWidth = 450)
sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("Batted Ball", tabName = "BB"),
      menuItem("Swing Decisions",tabName = "SD"),
      menuItem("Count Control",tabName = "CC"),
      menuItem("Leaderboard",tabName = "LB")
    )
)

body <- dashboardBody(
  selectInput("player", "Select Player", sort(unique(thunderbats$batter))),
    tabItems(
      tabItem(tabName = "BB",
        fluidRow(
        box(dataTableOutput("TopEV"),width = 5, height = 5),
        box(dataTableOutput("Barrels"),width = 5, height = 5),
        )
              ),
      tabItem(tabName = "SD",
        fluidRow(
          box(selectInput("pitchtype", "Pitch Type", sort(unique(thunderbats$taggedpitchtype))),
                 selectInput("hcount","Count",sort(unique(thunderbats$count))),
            plotOutput("swd")))
              ),
      tabItem(tabName = "CC",
              fluidRow(
                box(dataTableOutput("ppa")),
                box(dataTableOutput("pwt"))
              )),
      tabItem(tabName = "LB",
              fluidRow(
                box(dataTableOutput("nine")),
                box(dataTableOutput("kbb"))),
              fluidRow(
                box(dataTableOutput("cp")),
                box(dataTableOutput("unlucky"))
              ),
              fluidRow(
                box(dataTableOutput("bvv"))
              )
              )
      

    ))

  
  # Put them together into a dashboardPage
  ui <- dashboardPage(
    header,
    sidebar,
    body
  )
         

server <- function(input,output){
  output$TopEV <- renderDataTable({
    thunderbats%>%
      filter(batter==input$player,!is.na(exitspeed),pitchcall == "InPlay")%>%
      summarize(utcdate,exitspeed,angle,taggedhittype,playresult)
  })
    
   output$Barrels <- renderDataTable({
     thunderbats%>%
     filter(batter == input$player,!is.na(exitspeed),pitchcall=="InPlay")%>%
       summarize(barrels = sum(exitspeed >95 & angle > 10 & angle < 35),barrel_percentage = round(barrels/n(),2))
   }) 
 output$swd <- renderPlot({
   thunderbats%>%
     filter(batter==input$player,taggedpitchtype==input$pitchtype,count==input$hcount)%>%
     ggplot(aes(x = platelocside, y = platelocheight,color = pitchcall))+xlim(-3,3) + ylim(0,5) + labs(color = "", title = "Pitch Location") +
     geom_rect(aes(xmin = -.83, xmax = .83, ymin = 1.5, ymax = 3.5), alpha = 0, size = 1, color = "black") +
     geom_segment(aes(x = -0.708, y = 0.15, xend = 0.708, yend = 0.15), size = 1, color = "black") + 
     geom_segment(aes(x = -0.708, y = 0.3, xend = -0.708, yend = 0.15), size = 1, color = "black") + 
     geom_segment(aes(x = -0.708, y = 0.3, xend = 0, yend = 0.5), size = 1, color = "black") + 
     geom_segment(aes(x = 0, y = 0.5, xend = 0.708, yend = 0.3), size = 1, color = "black") + 
     geom_segment(aes(x = 0.708, y = 0.3, xend = 0.708, yend = 0.15), size = 1, color = "black") +
     geom_point(size = 3, na.rm = TRUE) +
     theme_bw() + theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
     theme(legend.position = "bottom", legend.text = element_text(size = 12), axis.title = element_blank())
   
 },width=375)
  
 output$ppa <- renderDataTable({
   thunderbats%>%
     filter(batter==input$player)%>%
     summarize(pitches=n(),pa = sum(korbb == "Walk" | korbb == "Strikeout" | playresult != "Undefined"),pitches_per_pa=round(pitches/pa,2))
 })
 
 output$nine <- renderDataTable({
   thunderbats%>%
     filter(!is.na(exitspeed))%>%
     group_by(batter)%>%
     summarize(ninetyth_per_ev = round(quantile(exitspeed,probs = 0.9),2))
     
 }) 
output$pwt <- renderDataTable({
  thunderbats%>%
    filter(batter==input$player)%>%
    summarize(pitches_with_two = sum(count == "0-2"|count == "1-2" | count == "2-2" | count == "3-2"), pa = sum(korbb == "Walk" | korbb == "Strikeout" | playresult != "Undefined"),ppa_withtwo = round(pitches_with_two/pa,2))
})
output$kbb <- renderDataTable({
  thunderbats%>%
    filter(korbb != "Undefined")%>%
    group_by(batter)%>%
    summarize(ks = sum(korbb=="Strikeout"), bbs = sum(korbb=="Walk"),k_bb_ratio = round(ks/bbs,2))
    
})
output$cp <- renderDataTable({
  thunderbats%>%
    filter(platelocheight > 3.5 | platelocheight < 1.5 | abs(platelocside) > .83)%>%
    group_by(batter)%>%
    summarize(pitches = n(),swings = sum(pitchcall == "StrikeSwinging" | pitchcall == "InPlay" | pitchcall == "FoulBall"),chase_percentage = round(swings/pitches*100,2),o_contact_percentage = round(sum(pitchcall == "FoulBall" | pitchcall == "InPlay")/pitches*100,2))
})
output$unlucky <- renderDataTable({
  thunderbats%>%
    filter(platelocheight > 3.5 | platelocheight < 1.5 | abs(platelocside) > .83)%>%
    group_by(batter)%>%
    summarize(out_of_zone_strikes_called = sum(pitchcall == "StrikeCalled"),missed_call_percentage = round(out_of_zone_strikes_called/n()*100,2))
})

output$bvv <- renderDataTable({
  topvelos <- thunderbats%>%
    filter(taggedpitchtype == "Fastball" | taggedpitchtype == "Sinker" | taggedpitchtype == "FourSeamFastBall" | taggedpitchtype == "TwoSeamFastBall")%>%
    filter(relspeed > mean(relspeed,na.rm=TRUE))
  
  topvelos%>%
    filter(pitchcall == "InPlay",!is.na(exitspeed))%>%
    group_by(batter)%>%
    summarize(hard_hit_velo = sum(exitspeed > 95), hard_hit_per = round(hard_hit_velo/n(),2))
}) 

 }

shinyApp(ui,server)


