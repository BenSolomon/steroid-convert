library(shinythemes); library(shinyBS)
library(DT); library(dplyr)
# source("kdrisk.R")
# source("kdData.R")


ui <- navbarPage("Steroid Conversions", theme = shinytheme("flatly"), collapsible = T,
                 header = 
                   tags$head(
                     # includeHTML("google-analytics.js"),
                     tags$style(HTML("
                        #test {
                          padding: 100px;
                        }
                        .navbar {
                          margin: 0px;
                        }
                        .footer {
                            position: relative;
                            left: 0;
                            bottom: 0;
                            width: 100%;
                            background-color: #d7dfea;
                            # color: white;
                            text-align: center;
                        }
                        "))
                   ),
                 
                 tabPanel("Convert", id="test", 
                          div(h2("Kawasaki Disease Early Discharge Risk"),
                              h4("Adapted from Hester", em("et al."), "(2019)", a("PMID: 31501220", 
                                                                                  href="https://www.ncbi.nlm.nih.gov/pubmed/31501220"))),
                          sidebarLayout(
                            sidebarPanel(width = 3,
                                         bsCollapse(open = "panel",
                                                    bsCollapsePanel(p(icon("bars"),HTML('&nbsp;'), "Patient values"), value = "panel",
                                                                    div(fluidRow(
                                                                      column(6, numericInput("label_DOSE", label = h3("Dose"), value = 1)),
                                                                      column(6, radioButtons("label_UNIT", label = h3("Unit"), choices = list("mg/kg" = 1, "mg/m2" = 0), selected = 1))
                                                                    )),
                                                                    div(fluidRow(
                                                                      column(12, selectInput("label_DRUG", label = h3("Steroid"), 
                                                                                             choices = list("Hydrocortisone" = "hct",
                                                                                                            "Prednisone/Prednisolone" = "pred",
                                                                                                            "Methylprednisone" = "meth",
                                                                                                            "Dexamethasone" = "dex"), selected = "hct")),
                                                                    )),
                                                                    div(fluidRow(
                                                                      column(6, numericInput("label_HEIGHT", label = h3("Height (cm)"), value = 90)),
                                                                      column(6, numericInput("label_WEIGHT", label = h3("Weight (kg)"), value = 50))
                                                                    ))
                                                    )
                                         )
                            ),
                            mainPanel(
                              # uiOutput("conditionalDecision"),
                              # fluidRow(
                              #   column(4),
                              #   column(4, uiOutput("conditionalDecision"))
                              # ),
                              # div(DT::dataTableOutput("componentTable"), style = "overflow-x: auto;"),
                              # fluidRow(
                              #   tags$div(tags$ul(
                              #     tags$li(tags$span("Sensitivity = % of patients who DID require 2nd IVIG who would NOT be discharged early")),
                              #     tags$li(tags$span("Specificity = % of patients who DID NOT require 2nd IVIG who would HAVE been discharged early")),
                              #     tags$li(tags$span("PPV = % of patients above threshold that DID need a 2nd IVIG")),
                              #     tags$li(tags$span("NPV = % of patients below threshold that DID NOT need a 2nd IVIG"))))
                              # )
                            )
                          )
                 )
)

server <- function(input, output) {
  p <- reactive({
    round(
      kdRisk(
        WBC = input$labelWBC,
        Platelet = input$labelPLAT,
        Hgb = input$labelHGB,
        AST = input$labelAST,
        Na = input$labelNA,
        Alb = input$labelALB,
        Temp = input$labelTEMP,
        Classic = as.numeric(input$labelCLASSIC)),
      2
    )
    # kdRisk(
    #   WBC = 14,
    #   Platelet = 400,
    #   Hgb = 14,
    #   AST = 40,
    #   Na = 140,
    #   Alb = 4,
    #   Temp = 39,
    #   Classic = 1
    # )
  })
  
  output$conditionalDecision <- renderUI({
    if (p() <= cut[as.numeric(input$labelCUT)]){
      tags$div(
        style="border:solid; background-color:#66c2a550; border-radius: 5px; 
               overflow: hidden; text-align:center; vertical-align: middle;
               line-height: normal;",
        tags$h3("Probability of non-response: ", tags$b(as.character(p())), tags$br(), tags$h2("Safe to discharge"))
      )
    } else {
      tags$div(
        style="border:solid; background-color:#d53e4f50; border-radius: 5px; 
               overflow: hidden; text-align:center; vertical-align: middle;
               line-height: normal;",
        tags$h3("Probability of non-response: ", tags$b(as.character(p())), tags$br(), tags$h2("Not safe to discharge"))
      )
    }
  })
  
  #Data table generation
  # output$text <- renderText({ 
  #   paste0("||| WBC = ", input$labelWBC,
  #          "||| Platelets = ", input$labelPLAT,
  #          "||| Hgb = ", input$labelHGB,
  #          "||| AST = ", input$labelAST,
  #          "||| Na = ", input$labelNA,
  #          "||| ALB = ", input$labelALB,
  #          "||| Temp = ", input$labelTEMP,
  #          "||| Classic = ", input$labelCLASSIC)
  # })
  
  output$prob <- renderText({
    paste0(as.character(p()))
  })
  
  output$componentTable = DT::renderDataTable({
    datatable(
      data.frame(cut, sens, spec, PPV, NPV, "test" = as.integer(1:4 == input$labelCUT)),
      colnames = c("Cut-off", "Sensitivity", "Specificity", "PPV", "NPV", "test"),
      rownames = FALSE,
      caption = htmltools::tags$caption(
        style = 'caption-side: bottom; text-align: right;',
        "Bold row indicates selected probability cut-off"),
      class = "cell-border",
      options = list(dom = "t",
                     ordering = F,
                     columnDefs = list(
                       list(className = 'dt-center', targets = 0:5),
                       list(targets = 5, visible = F)))
    )  %>%
      formatStyle(
        columns = 'test',
        target = 'row',
        backgroundColor = styleEqual(c(0, 1), c('#white', '#00000010')),
        fontWeight = styleEqual(c(0,1), c("normal", "bold")))
  })
  #Plot generation
  # output$plotSBP <- renderPlot({
  #   S()$plot + ggtitle("Systolic BP percentiles")
  # })
  # 
  # output$plotDBP <- renderPlot({
  #   D()$plot +ggtitle("Diastolic BP percentiles")
  # })
}

shinyApp(ui, server)
