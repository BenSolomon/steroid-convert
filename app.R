library(shinythemes); library(shinyBS)
library(DT); library(dplyr);
library(ggrepel)
source("utils.R")

ui <-
  navbarPage(
    "Steroid Conversions",
    theme = shinytheme("flatly"),
    collapsible = T,
    header =
      tags$head(includeHTML("google-analytics.js"),
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
            <!---
                        .datatables {
                            font-size: 1.5vw;
                        }
            --->
                        @media screen and (min-width: 1024px) {
                            .datatables {
                                font-size: 12px;
                            }
                        }
                        input[type=number] {
                          -moz-appearance:textfield;
                        }
                        input[type=number]::{
                              -moz-appearance:textfield;
                        }
                        input[type=number]::-webkit-outer-spin-button,
                        input[type=number]::-webkit-inner-spin-button {
                              -webkit-appearance: none;
                              margin: 0;
                        }
                        "
          )
        )),
    
    tabPanel("Convert", id = "test",
             fluidRow(
               # Sidebar
               column(3,
                      bsCollapse(
                        open = "panel",
                        bsCollapsePanel(
                          p(icon("bars"), HTML('&nbsp;'), "Patient values"),
                          value = "panel",
                          div(fluidRow(
                            column(6, numericInput(
                              "label_DOSE", label = "Dose", value = 1
                            )),
                            column(
                              6,
                              radioButtons(
                                "label_UNIT",
                                label = "Unit",
                                choices = list("mg/kg" = "mgkg", "mg/m2" = "mgm2", "mg" = "mg"),
                                selected = "mgkg"
                              )
                            )
                          )),
                          div(fluidRow(column(
                            12,
                            selectInput(
                              "label_DRUG",
                              label = "Steroid",
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
                              label = "Height (cm)",
                              value = 89
                            )),
                            column(6, numericInput(
                              "label_WEIGHT",
                              label = "Weight (kg)",
                              value = 18.7
                            ))
                          ))
                        )
                      ),
                      
               ),
               column(9,
                      fluidRow(
                        column(6, 
                               DT::dataTableOutput("glucTable"),
                               plotOutput("gg_gluc", height="200px")
                        ), 
                        column(6,
                               DT::dataTableOutput("minTable"),
                               plotOutput("gg_mineral", height="200px")
                        )
                      )
               ))
    ))

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
        HTML("Glucocorticoid<br/>conversion")
      ),
      class = "compact cell-border",
      options = list(
        dom = "t",
        ordering = F,
        # scrollX = TRUE,
        columnDefs = list(list(
          className = 'dt-center', targets = 0:3
        )),
        scrollX = TRUE
      )
    )
  })
  
  output$minTable = DT::renderDataTable({
    datatable(escape = F,
              df_min(),
              colnames = c("Steroid", "Relative potency", "mg/kg", "mg/m2"),
              rownames = FALSE,
              caption = htmltools::tags$caption(
                style =
                  'caption-side: top;
        text-align: left;
        color: black;
        font-size:150%',
        HTML("Mineralocorticoid<br/>conversion")
        
              ),
        class = "compact cell-border",
        options = list(
          dom = "t",
          ordering = F,
          # scrollX = TRUE,
          columnDefs = list(list(
            className = 'dt-center', targets = 0:3
          )),
          scrollX = TRUE
        )
    )
  })
  
  output$gg_gluc <- renderPlot({
    df_gluc() %>% 
      filter(drug == "hct") %>% 
      pull(mgm2) %>% 
      gg_steroid_scale()
    })

  output$gg_mineral <- renderPlot({
    df_min() %>% 
      filter(drug == "hct") %>% 
      pull(mgm2) %>% 
      gg_steroid_scale()
})
}

shinyApp(ui, server)
