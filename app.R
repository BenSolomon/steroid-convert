library(shinythemes); library(shinyBS)
library(DT); library(dplyr)
source("utils.R")
# source("kdData.R")


ui <-
  navbarPage(
    "Steroid Conversions",
    theme = shinytheme("flatly"),
    collapsible = T,
    header =
      tags$head(# includeHTML("google-analytics.js"),
        tags$style(
          HTML(
            "
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
                        "
          )
        )),
    
    tabPanel("Convert", id = "test",
             sidebarLayout(
               sidebarPanel(width = 3,
                            bsCollapse(
                              open = "panel",
                              bsCollapsePanel(
                                p(icon("bars"), HTML('&nbsp;'), "Patient values"),
                                value = "panel",
                                div(fluidRow(
                                  column(6, numericInput(
                                    "label_DOSE", label = h3("Dose"), value = 1
                                  )),
                                  column(
                                    6,
                                    radioButtons(
                                      "label_UNIT",
                                      label = h3("Unit"),
                                      choices = list("mg/kg" = "mgkg", "mg/m2" = "mgm2", "mg" = "mg"),
                                      selected = "mgkg"
                                    )
                                  )
                                )),
                                div(fluidRow(column(
                                  12,
                                  selectInput(
                                    "label_DRUG",
                                    label = h3("Steroid"),
                                    choices = list(
                                      "Hydrocortisone" = "hct",
                                      "Prednisone/Prednisolone" = "pred",
                                      "Methylprednisone" = "meth",
                                      "Dexamethasone" = "dex"
                                    ),
                                    selected = "hct"
                                  )
                                ),)),
                                div(fluidRow(
                                  column(6, numericInput(
                                    "label_HEIGHT",
                                    label = h3("Height (cm)"),
                                    value = 89
                                  )),
                                  column(6, numericInput(
                                    "label_WEIGHT",
                                    label = h3("Weight (kg)"),
                                    value = 18.7
                                  ))
                                ))
                              )
                            )),
               mainPanel(# uiOutput("conditionalDecision"),
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
                 # div(DT::dataTableOutput("Table")),
                 fluidPage(
                   fluidRow(
                     column(6, 
                            DT::dataTableOutput("glucTable"),
                            br(),
                            DT::dataTableOutput("minTable")),
                     column(6,
                            plotOutput("ggsteroid"))
                   )
                 )
                 # ,
                 # div(DT::dataTableOutput("componentTable"))
             )))
  )

server <- function(input, output) {
  df_gluc <- reactive({
    steroid_conv_table(
        wt = input$label_WEIGHT,
        ht = input$label_HEIGHT,
        dose = input$label_DOSE,
        drug = input$label_DRUG,
        unit = input$label_UNIT,
        effect = "cort"
    ) 
  })
  
  df_min <- reactive({
    steroid_conv_table(
      wt = input$label_WEIGHT,
      ht = input$label_HEIGHT,
      dose = input$label_DOSE,
      drug = input$label_DRUG,
      unit = input$label_UNIT,
      effect = "mineral"
    ) 
  })
  
  # output$conditionalDecision <- renderUI({
  #   if (p() <= cut[as.numeric(input$labelCUT)]){
  #     tags$div(
  #       style="border:solid; background-color:#66c2a550; border-radius: 5px; 
  #              overflow: hidden; text-align:center; vertical-align: middle;
  #              line-height: normal;",
  #       tags$h3("Probability of non-response: ", tags$b(as.character(p())), tags$br(), tags$h2("Safe to discharge"))
  #     )
  #   } else {
  #     tags$div(
  #       style="border:solid; background-color:#d53e4f50; border-radius: 5px; 
  #              overflow: hidden; text-align:center; vertical-align: middle;
  #              line-height: normal;",
  #       tags$h3("Probability of non-response: ", tags$b(as.character(p())), tags$br(), tags$h2("Not safe to discharge"))
  #     )
  #   }
  # })
  
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
  
  # output$prob <- renderText({
  #   paste0(as.character(p()))
  # })
  
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
  
  output$glucTable = DT::renderDataTable({
    datatable(
      df_gluc(),
      colnames = c("Steroid", "Relative potency", "mg/kg", "mg/m2"),
      rownames = FALSE,
      caption = htmltools::tags$caption(
        style =
          'caption-side: top;
        text-align: left;
        color: black;
        font-size:150%',
        "Glucocorticoid conversion"
      ),
      class = "cell-border",
      options = list(
        dom = "t",
        ordering = F,
        columnDefs = list(list(
          className = 'dt-center', targets = 0:3
        ))
      )
    )
  })
  
  output$minTable = DT::renderDataTable({
    datatable(
      df_min(),
      colnames = c("Steroid", "Relative potency", "mg/kg", "mg/m2"),
      rownames = FALSE,
      caption = htmltools::tags$caption(
        style =
          'caption-side: top;
        text-align: left;
        color: black;
        font-size:150%',
        "Mineralocorticoid conversion"
      ),
      class = "cell-border",
      options = list(
        dom = "t",
        ordering = F,
        columnDefs = list(list(
          className = 'dt-center', targets = 0:3
        ))
      )
    )
  })
  
  output$ggsteroid <- renderPlot({
    df_gluc() %>% 
      filter(drug == "hct") %>% 
      pull(mgm2) %>% 
      gg_steroid_scale()
    })
  
  # output$Table = DT::renderDataTable({df()})
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
