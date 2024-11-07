library(rhandsontable)
library(shiny)
library(shinythemes)
library(DT)

shinyUI(fluidPage(theme=shinythemes::shinytheme('cosmo'),
                  tags$style(
                    type = 'text/css',
                    '.modal-dialog { width: fit-content !important; }'
                    ),
  titlePanel("The Triangulator/ Aggregator - Combining and Extrapolating Key Population Estimates"),
  p(),
  sidebarLayout(
    sidebarPanel(style='max-height: 100%',width=3,
      conditionalPanel(condition = "typeof input.upload !== null",
      tags$h4('Session Info'),
      htmlOutput('country_text'),
      htmlOutput('filename_text'),
      br(),
      #
      tags$h4('Current Key Pop'),
      selectInput('kp','Key Population',choices=c('')),
      #
      tags$h5('Progress'),
      wellPanel(DT::DTOutput('kp_progress_table')),
      #
      tags$h4('Overall Progress'),
      wellPanel(DT::DTOutput('overall_progress_table')),
      actionButton('download_button','Download Results',width='100%',style="color: #fff; background-color: #0c8509; border-color: #b2ffb0") # #fc5151; border-color: #ffb5b5")
    )),
    mainPanel(width=9,
  tabsetPanel(
    id = 'tabs',
    ########################
    # TAB 1: Enter KP Data #
    ########################
    tabPanel(
      "KP Data Input",
      br(),
      column(10,wellPanel(
             tags$h4('Instructions:'),
             htmlOutput("kp_instructions"),
             actionButton('show_full_kp_instructions',"Full Instructions")
      )),
      column(2,
             actionButton('progress_to_tri',label='Triangulator Inputs >',disabled=TRUE,style="color: #fff; background-color: #fc5151; border-color: #ffb5b5")
             ),
      br(),
      column(12,br(),
             wellPanel(rHandsontableOutput("kp_data_hot"))),
      br(),
    ),
    ##############################
    # TAB 2: TRIANGULATOR INPUTS #
    ##############################
    tabPanel(
      "Triangulator Inputs",
      br(),
      column(10,wellPanel(
             tags$h4('Instructions:'),
             htmlOutput("tri_instructions"),
             actionButton('show_full_tri_instructions',"Full Instructions"))),
      column(2,
             actionButton("run_tri","Run Triangulator",disabled=TRUE,style="color: #fff; background-color: #fc5151; border-color: #ffb5b5")
             ),
      column(5,
        tags$h3('KP Proportion Expected Value'),
        rHandsontableOutput("tri_priors_hot"),
      ),
      column(7,
        tags$h3('KP Proprotion Expected Value Plot'),
        selectInput('tri_prior_plot_select','Province:',choices=''), #,selectize=FALSE),
        plotOutput('tri_prior_plot'),
        p("Summaries"),
        tableOutput("tri_prior_summaries"),
        p("Quantiles:"),
        tableOutput("tri_prior_quant")
      )
    ),
    ###############################
    # TAB 3: TRIANGULATOR OUTPUTS #
    ###############################
    tabPanel(
      "Triangulator Outputs",
      br(),
      column(10,wellPanel(
             tags$h4('Instructions:'),
             htmlOutput("tri_out_instructions")
             )),
      column(2,
             actionButton('progress_to_agg',label='Aggregator Inputs >',disabled=FALSE,style="color: #fff; background-color: #fc5151; border-color: #ffb5b5")
             ),
      column(5,
             tags$h3('KP Proportion Consensus Estimates'),
             tableOutput("tri_out_table"),
      ),
      column(7,
             tags$h3('KP Estimates Forest Plot'),
             fluidRow(
             column(6,selectInput('kp_forest_plot_select','SNU:',c())),
             column(6,radioButtons('kp_forest_scale','Plot Scale: ',choices=c('Log','Linear'),selected='Log',inline=TRUE))),
             plotOutput('kp_forest_plot'),
             textOutput('kp_var_unexp')
      )
    ),
    ############################
    # TAB 4: AGGREGATOR INPUTS #
    ############################
    tabPanel(
      "Aggregator Inputs",
      br(),
      column(10,wellPanel(
             tags$h4('Instructions:'),
             htmlOutput("agg_instructions"),
             actionButton('show_full_agg_instructions',"Full Instructions")
             )),
      column(2,
             actionButton("run_agg","Run Aggregator",style="color: #fff; background-color: #fc5151; border-color: #ffb5b5")
             ),
      column(6,
             tags$h3('Demographic Data'),
             textOutput("demo_ref_pop"),
             br(),
             rHandsontableOutput("demo_data_hot",width=600),
             br()
             ),
      column(6,
             tags$h3('Rural:Urban Ratio'),
             fluidRow(
             column(6,numericInput("urb_prior_median","Median",.6)),
             column(6,numericInput("urb_prior_q95","95th Percentile", .7))
             ),
             plotOutput("urb_prior_plot"),
             p("Summaries"),
             tableOutput("urb_prior_summaries"),
             p("Quantiles:"),
             tableOutput("urb_prior_quant")
             )
    ),
    #############################
    # TAB 5: AGGREGATOR OUTPUTS #
    #############################
    tabPanel(
      "Aggregator Outputs",
      br(),
      fluidRow(column(10,wellPanel(
                      tags$h4('Instructions:'),
                      htmlOutput("agg_out_instructions")
                      )),
               column(2,
                      uiOutput('progress_kp_button')
                      )),
      br(),
      column(5,
             selectInput('agg_display_select','Display:',c('Total','Urban','Rural')),
             tableOutput("agg_nat_out_table"),
             br(),
             tableOutput("agg_SNU_out_table"),
      ),
      column(7,
             tags$h3('Aggregator Consensus Estimates Plot'),
             radioButtons('agg_out_scale','Plot Scale: ',choices=c('Log','Linear'),selected='Linear',inline=TRUE),
             plotOutput('agg_out_plot'),
      )
    )
  ))
  )
))
