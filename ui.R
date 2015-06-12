library(ggplot2)
library(reshape2)

if(!require("shinyIncubator")) devtools::install_github("rstudio/shiny-incubator")
library(shinyIncubator)

shinyUI(navbarPage(
  "Population Genetics",
  
  tabPanel("Make Your Population",
  
  fluidRow(
    column(4,
      numericInput("start", tags$div( 
        HTML(paste("Starting %")),
        HTML(paste("A", tags$sup(1), sep = ""))
         ), 50),
      numericInput("gen", "Number of Generations", 100),
      numericInput("pop", "Starting Population", 100),
      numericInput("AA", 
                   #withMathJax(helpText('$$A^1$$')),
                   tags$div(
                     HTML(paste("A", tags$sup(1), sep = "")), 
                     HTML(paste("A", tags$sup(1), sep = "")),
                     HTML(paste("Fitness %"))
                   ), 
                   100),
      numericInput("AB", 
                   tags$div(
                     HTML(paste("A", tags$sup(1), sep = "")), 
                     HTML(paste("A", tags$sup(2), sep = "")),
                     HTML(paste("Fitness %"))
                   )
                   , 100),
      numericInput("BB", 
                   tags$div(
                     HTML(paste("A", tags$sup(2), sep = "")), 
                     HTML(paste("A", tags$sup(2), sep = "")),
                     HTML(paste("Fitness %"))
                   ), 100),
      actionButton("begin","Click To Begin"),
      uiOutput("uiButton1"),
      uiOutput("uiButton2"),
      uiOutput("uiButton3")
    ),
    
    column(4,
           absolutePanel(
             top=125, bottom=0, left=320, right=0, fixed=TRUE,
             #              h3("Population 1"),
             imageOutput("Population1")
             
           )
    ),
    
    column(4,
           absolutePanel(
             top=425, bottom=0, left=320, right=0, fixed=TRUE,
             #              h3("Population 2"),
             imageOutput("Population2")
             
           )
    ),
    
    column(4,
           absolutePanel(  #Prints information for Population #1
             top=170, bottom=0, left=670, right=0, fixed=TRUE,
             tags$style(type="text/css",
                        ".shiny-output-error { visibility: hidden; }",
                        ".shiny-output-error:before { visibility: hidden; }"),
             uiOutput("start1"),
             uiOutput("gen1"),
             uiOutput("pop1"),
             uiOutput("AA1"),
             uiOutput("AB1"),
             uiOutput("BB1"),
             textOutput("s1"),
             textOutput("s2"),
             textOutput("s3"),
             textOutput("s4"),
             textOutput("s5"),
             textOutput("s6"),
             textOutput("s7")
           ), #absolutePanel
           absolutePanel(  #Prints information for Population #2
             top=470, bottom=0, left=670, right=0, fixed=TRUE,
              uiOutput("start2"),
              uiOutput("gen2"),
              uiOutput("pop2"),
              uiOutput("AA2"),
              uiOutput("AB2"),
              uiOutput("BB2")
           )
    ) #column
    
    
  ) #fluidRow
), #tabPanel

tabPanel("Simulation",
         sidebarLayout(
                  
           sidebarPanel(
             
             numericInput("drawmorepops", "Number of Simulations", 1),
             actionButton("drawpop", "Simulate")
             
           ),#sidebarPanel  
           mainPanel( 
             includeCSS('C:/Users/Helen/Dropbox/Hevin/SP_Render/styles.css'),
             #progressInit(),
             plotOutput("distPlot")
           )#mainPanel
         )#sidebarLayout
         
) #tabPanel
) #navbarPage
) #shinyUI