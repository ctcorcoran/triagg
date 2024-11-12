library(rhandsontable)
library(dplyr)
library(ggplot2)
library(rstan)
library(openxlsx)
library(DT)
library(triagg)
library(countrycode)

options(shiny.maxRequestSize=30*1024^2)


function(input, output, session) {
  kp_ref_list <- list('MSM'='male','FSW'='female','PWID'='male','TGW'='female')
  kp_ref_display_list <- list('MSM'='Male','FSW'='Female','PWID'='Male','TGW'='Female')
  tab_names <- c('KP Data Input','Triangulator Inputs','Triangulator Outputs','Aggregator Inputs','Aggregator Outputs')

  ##########################
  # Define Reactive Values #
  ##########################
  values <- reactiveValues()

  year <- '2024'
  values[['country']] <- ''

  values[["kp_df"]] <- NULL
  kp_df <- NULL

  values[["demo_df"]] <- NULL
  demo_df <- NULL

  values[["tri_priors_df"]] <- NULL
  tri_priors_df <- NULL
  tri_priors_plot_options <- c()

  values[["tri_full_output"]] <- NULL
  values[["tri_consensus_output"]] <- NULL
  kp_forest_plot_options <- c()

  values[['t_value']] <- NULL
  values[["aggregator_output"]] <- NULL

  # Values to check validity and navigation
  values[['nav_list']] <- NULL
  values[['kp_prog_table']] <- NULL
  values[['overall_prog_table']] <- NULL

  #################################
  # KP WORKBOOK UPLOAD ON STARTUP #
  #################################

  values[['startup']] <- TRUE

  observeEvent(values[['startup']],{
    #Hide All Tabs to Begin, which will slowly be shown as the user progresses through the app the first time
    triagg:::showhide_tabs(c(0,0,0,0,0),tab_names)
    showModal(modalDialog(
      title='Upload KP Workbook',
      align='center',
      easyClose=FALSE,
      footer=NULL,
      size='l',
      selectInput('language_selector','Language:',choice=c('English','French'),selected='English'),
      fileInput("upload", "Upload KP Workbook",accept=c('.xlsx')),
      textOutput("uploaded")
    ))
  })

  # UPLOAD KP DATA
  uploaded_data <- reactive({
    req(input$upload)

    # Read in Data Validation Sheet
    df <- openxlsx::read.xlsx(input$upload$datapath,
                              sheet='Validation',
                              startRow=15,
                              colNames=TRUE,
                              cols=c(1,3:5,8:14,17:18,19,21,22))

    # Process Validation Sheet in to workable format
    df <- triagg:::process_kp_workbook(df,input$language_selector)
    df$confidence <- as.integer(NA)

    # Assign some session values and navigation lists
    values[['country']] <- unique(df$country)
    values[['iso3']] <- countrycode::countrycode(unique(df$country), "country.name", "iso3c")
    kp_list <- unique(df$kp)
    #
    updateSelectInput(session,inputId='kp',choices=kp_list)
    values[["nav_list"]] <- setNames(append(list(c(0,0,0,0,0)),rep(list(c(0,0,0,0,0)),length(kp_list)-1)),kp_list)

    # Make Demography Dataframes
    values[['full_demo_df']] <- ref_pops[(ref_pops$country_id==values[['iso3']])&(ref_pops$year==year),]

    # Make KP and Preallocated Output Dataframes
    dfs_to_save <- triagg:::generate_output_dataframes(df,isolate(values[['full_demo_df']]),input$upload$datapath)
    values[['triangulator_results']] <- dfs_to_save$triangulator_results
    values[['aggregator_results']] <- dfs_to_save$aggregator_results
    values[['urb_prior_df']] <- dfs_to_save$urb_prior_df
    values[['full_demo_df']] <- dfs_to_save$full_demo_df

    # Last Steps
    removeModal()
    showTab(inputId='tabs',target='KP Data Input')
    df
  })

  output$uploaded <- renderText({
    uploaded_data()
    " "
  })

  #############
  ## SIDEBAR ##
  #############

  output$country_text <- renderText({
    paste('<b>Country:</b>',values[['country']])
  })

  output$filename_text <- renderText({
    paste('<b>Filename:</b>',input$upload$name)
  })

  ## SELECT KP
  observeEvent(input$kp,{
    if(input$kp != ''){
      tri_res <- isolate(values[['triangulator_results']])
      agg_res <- isolate(values[['aggregator_results']])
      demo <- isolate(values[['full_demo_df']])
      urb <- isolate(values[['urb_prior_df']])
      #
      values[["kp_df"]] <- tri_res[tri_res$kp==input$kp&!(tri_res$method %in% c('Prior','Consensus')),]
      values[['demo_df']] <- demo[demo$sex==kp_ref_list[[input$kp]],c('province','year','pop','urban_proportion')]
      values[['tri_priors_df']] <- triagg:::tri_prior_results_to_HOT(tri_res,input$kp)
      #
      updateNumericInput(session,inputId='urb_prior_median',value=urb[urb$stat=='urb_prior_median',input$kp])
      updateNumericInput(session,inputId='urb_prior_q95',value=urb[urb$stat=='urb_prior_q95',input$kp])
      #
      updateTabsetPanel(inputId = 'tabs',selected='KP Data Input')
      if(values[['nav_list']][[input$kp]][1]==0){
        values[['nav_list']][[input$kp]][1] <- 1
      }
      triagg:::showhide_tabs(values[['nav_list']][[input$kp]],tab_names)
    }
  })

  ## DOWNLOAD RESULTS

  observeEvent(input$download_button,{
    showModal(modalDialog(
      title='Download Results',
      align='center',
      easyClose=TRUE,
      footer=NULL,
      downloadHandler(
        filename = function(){
          filename_split <- unlist(strsplit(input$upload$name,split='[.]'))
          paste0(filename_split[1],'_agg.',filename_split[2])
        },
        content = function(file){
          wb <- openxlsx::loadWorkbook(input$upload$datapath,na.convert=FALSE)
          for(name in c('triangulator_confidence','aggregator_output','aggregator_input')){
            if(!(name %in% openxlsx::getSheetNames(input$upload$datapath))){
              openxlsx::addWorksheet(wb,name)
            }
          }
          # Write any changes made to kp_df and tri_priors_df
          values[['triangulator_results']] <- rows_update(values[['triangulator_results']],triagg:::write_triangulator_inputs(isolate(values[['kp_df']]),isolate(values[['tri_priors_df']])),
                                                          by=c('country','kp','study_idx','observation_idx','method','year','area_name','province'))
          openxlsx::writeData(wb,sheet='triangulator_confidence',isolate(values[['triangulator_results']]))
          openxlsx::writeData(wb,sheet='aggregator_output',isolate(values[['aggregator_results']]))
          openxlsx::writeData(wb,sheet='aggregator_input',isolate(values[['urb_prior_df']]),startRow=1)
          openxlsx::writeData(wb,sheet='aggregator_input',isolate(values[['full_demo_df']]),startRow=4)
          #
          for(sheet in c('triangulator_confidence','aggregator_output','aggregator_input')){
            openxlsx::sheetVisibility(wb)[which(names(wb)==sheet)] <- 'hidden'
          }
          #
          openxlsx::saveWorkbook(wb,file,overwrite=TRUE)
          removeModal()
        }
      )
    ))
  })

  ## TAB VISIBILITY

  observeEvent(values[['nav_list']],{
    if(input$kp != ''){
      triagg:::showhide_tabs(values[['nav_list']][[input$kp]],tab_names)
    }
  })

  ## MODEL INPUT & OUTPUT VALIDITY

  observeEvent(values[['kp_df']],{
    if(!is.null(values[['nav_list']])){
      if(values[['nav_list']][[input$kp]][3] > 0){
        values[['nav_list']][[input$kp]][3] <- 1
      }
      if(values[['nav_list']][[input$kp]][5] > 0){
        values[['nav_list']][[input$kp]][5] <- 1
      }
      if(values[['nav_list']][[input$kp]][1] > 0){
        if(all(!is.na(values[['kp_df']]$confidence))){
          values[['nav_list']][[input$kp]][1] <- 2
        } else {
          values[['nav_list']][[input$kp]][1] <- 1
        }
      }
    }
  })

  observeEvent(values[['tri_priors_df']],{
    if(!is.null(values[['nav_list']])){
      if(values[['nav_list']][[input$kp]][3] > 0){
        values[['nav_list']][[input$kp]][3] <- 1
      }
      if(values[['nav_list']][[input$kp]][5] > 0){
        values[['nav_list']][[input$kp]][5] <- 1
      }
      if(values[['nav_list']][[input$kp]][2] > 0){
        if(all(!is.na(values[['tri_priors_df']]))){
          values[['nav_list']][[input$kp]][2] <- 2
        } else {
          values[['nav_list']][[input$kp]][2] <- 1
        }
      }
    }
  })

  observeEvent(c(values[['demo_df']],input$urb_prior_median,input$urb_prior_q95),{
    if(!is.null(values[['nav_list']])){
      if(values[['nav_list']][[input$kp]][5] > 0){
        values[['nav_list']][[input$kp]][5] <- 1
      }
      if(values[['nav_list']][[input$kp]][4] > 0){
        if(all(all(!is.na(values[['tri_priors_df']]),!is.na(input$urb_prior_median),!is.na(input$urb_prior_q95)))){
          values[['nav_list']][[input$kp]][4] <- 2
        } else {
          values[['nav_list']][[input$kp]][4] <- 1
        }
      }
    }
  })

  # Tweak the progress table to make two smaller ones
  observeEvent(values[['nav_list']],{
    temp <- triagg:::full_progress_table(values[['nav_list']],tab_names)
    #
    temp_kp_prog_table <- t(temp[input$kp,])
    colnames(temp_kp_prog_table)[1] <- 'Status'
    #
    temp_overall_prog_df <- t(temp[,'Aggregator Outputs'])
    colnames(temp_overall_prog_df) <- names(values[['nav_list']])
    rownames(temp_overall_prog_df)[1] <- 'Status'
    #
    values[['kp_prog_table']] <- temp_kp_prog_table
    values[['overall_prog_table']] <- temp_overall_prog_df
  })

  # Make progress tables (with proxies, which may help their reload speed)

  output$kp_progress_table <- DT::renderDT({
    if(!is.null(values[['kp_prog_table']])){
      DT::datatable(values[['kp_prog_table']],options = list(dom = 't',ordering=FALSE),selection='none',class='compact') %>%
        DT::formatStyle(1:ncol(values[['kp_prog_table']]),
                    color = DT::styleEqual(c('Incomplete','Invalid','In Progress','Valid','Complete'),
                                       c('red','red', 'goldenrod', 'green','green')
                                       )
                    )
    }
  })

  output$overall_progress_table <- DT::renderDT({
    if(!is.null(values[['overall_prog_table']])){
      DT::datatable(values[['overall_prog_table']],options = list(dom = 't',ordering=FALSE),selection='none',class='compact') %>%
        DT::formatStyle(1:ncol(values[['overall_prog_table']]),
                    color = DT::styleEqual(c('Incomplete','Invalid','In Progress','Valid','Complete'),
                                       c('red','red', 'goldenrod', 'green','green')
                    )
        )
    }
  })

  kp_progress_proxy <- DT::dataTableProxy('kp_progress_table')
  observe({
    DT::replaceData(kp_progress_proxy,values[['kp_prog_table']])
  })

  overall_progress_proxy <- DT::dataTableProxy('overall_progress_table')
  observe({
    DT::replaceData(overall_progress_proxy,values[['overall_prog_table']])
  })

  ##################
  # KP DATA INPUTS #
  ##################

  observe({
    if (!is.null(input$kp_data_hot)) {
        kp_df <- hot_to_r(input$kp_data_hot)
        colnames(kp_df)[c(3:9,12,14)] <- c('study_idx','observation_idx','method','year','area_name','province','proportion_estimate','display_CI','confidence')
        kp_df <- kp_df[,!(colnames(kp_df) %in% c('display_CI'))]
      } else {
        if (is.null(values[["kp_df"]]))
          kp_df <- kp_df
        else
          kp_df <- values[["kp_df"]]
      }
    updateActionButton(session,inputId='progress_to_tri',disabled=!all(!sapply(kp_df$confidence,is.na)))
    values[["kp_df"]] <- kp_df
  })

  output$kp_data_hot <- renderRHandsontable({
    kp_df <- values[["kp_df"]]
    values[['kp_change']][['kp_df']] <- FALSE
    if (!is.null(kp_df)){
      kp_df$display_CI <- ifelse(is.na(kp_df$proportion_lower)&is.na(kp_df$proportion_upper),'',paste0('(',round(100*kp_df$proportion_lower,1),'%, ',round(100*kp_df$proportion_upper,1),'%)'))
      kp_df <- kp_df[,c(1:11,14,12:13)]
      colnames(kp_df)[c(3:9,12,14)] <- c('Study ID','Observation ID','Method','Year','Area Name','Province','Proportion Estimate','Uncertainty Interval','Study Confidence')
      rownames(kp_df) <- 1:nrow(kp_df)
      rhandsontable(
        kp_df,
        useTypes = TRUE,
        readOnly = TRUE
      ) %>%
        hot_cols(columnSorting=TRUE) %>%
        hot_col('Study Confidence',readOnly = FALSE) %>%
        hot_validate_numeric(cols=c('Study Confidence'),min=0,max=100)%>%
        hot_col('Proportion Estimate',format='0.0%') %>%
        hot_col(c('country','kp','proportion_lower','proportion_upper','Province','SE_interpolated'),colWidths=0.1) #,'province'
    }
  })

  ############################
  # >>> ADVANCE TO TRI INPUTS

  observeEvent(input$progress_to_tri,{
    validity_check <- triagg:::validate_kp_df(isolate(values[['kp_df']]))
    if(validity_check$valid){
      values[['nav_list']][[input$kp]][2] <- ifelse(all(!is.na(values[['tri_priors_df']])),2,1)
      updateTabsetPanel(inputId='tabs',selected='Triangulator Inputs')
    } else {
      output$validity_table <- DT::renderDT({validity_check$invalid_rows[,1:11] %>% datatable(options = list(dom = 't',ordering=FALSE)) %>% formatRound(columns=c('proportion_lower','proportion_estimate','proportion_upper'),digits=3)})
      showModal(modalDialog(
        title='KP Data Validity Error',
        align='center',
        easyClose=FALSE,
        size='xl',
        tags$h4('Ensure proportion_lower < proportion_estimate < proportion_upper'),
        tags$h4('The following observations require your attention:'),
        shiny::dataTableOutput('validity_table')
      ))
    }
  })

  #######################
  # TRIANGULATOR INPUTS #
  #######################

  # Triangulator Prior
  observe({
    if (!is.null(input$tri_priors_hot)) {
          tri_priors_df <- hot_to_r(input$tri_priors_hot)
          colnames(tri_priors_df) <- c('province','prior_med','prior_q75')
          # Build in our validation here, since rHandsontable has issues with row validation
          tri_priors_df$prior_q75 <- ifelse(is.na(tri_priors_df$prior_med)|is.na(tri_priors_df$prior_q75),
                                            tri_priors_df$prior_q75,
                                            pmax(tri_priors_df$prior_med,tri_priors_df$prior_q75,na.rm=TRUE))
          tri_priors_df[,c('prior_med','prior_q75')] <- tri_priors_df[,c('prior_med','prior_q75')]/100
          tri_priors_plot_options <- sort(tri_priors_df$province[which(!is.na(tri_priors_df$prior_med)&!is.na(tri_priors_df$prior_q75))])
      } else {
        if (is.null(values[["tri_priors_df"]])){
          tri_priors_df <- tri_priors_df
          tri_priors_plot_options <- tri_priors_plot_options
      } else {
          tri_priors_df <- values[["tri_priors_df"]]
          tri_priors_plot_options <- sort(tri_priors_df$province[which(!is.na(tri_priors_df$prior_med)&!is.na(tri_priors_df$prior_q75))])
      }
    }
    values[["tri_priors_df"]] <- tri_priors_df
    #
    updateActionButton(session,inputId='run_tri',disabled=!all(!sapply(tri_priors_df,is.na)))
    #
    tri_prior_select_prev <- isolate(input$tri_prior_plot_select)
    if(length(tri_priors_plot_options)==0){
      tri_priors_select <- ''
    } else {
      if(tri_prior_select_prev!=''){
        tri_priors_select <- ifelse(tri_prior_select_prev %in% tri_priors_plot_options,tri_prior_select_prev,tri_priors_plot_options[1])
      } else{
        tri_priors_select <- tri_priors_plot_options[1]
      }
    }
    updateSelectInput(session,'tri_prior_plot_select','Province:',
                      choices=tri_priors_plot_options,
                      selected=tri_priors_select)
  })

  output$tri_priors_hot <- renderRHandsontable({
    tri_priors_df <- values[['tri_priors_df']]
    if (!is.null(tri_priors_df)){
      rownames(tri_priors_df) <- 1:nrow(tri_priors_df)
      colnames(tri_priors_df) <- c('Province','Expected Value - Median (%)','Expected Value - 75th Percentile (%)')
      tri_priors_df[,c('Expected Value - Median (%)','Expected Value - 75th Percentile (%)')] <- 100*tri_priors_df[,c('Expected Value - Median (%)','Expected Value - 75th Percentile (%)')]
      rhandsontable(
        tri_priors_df,
        useTypes = TRUE,
        readOnly = FALSE,
      ) %>%
        hot_col('Province',readOnly = TRUE)%>%
        hot_validate_numeric(cols=c('Expected Value - Median (%)','Expected Value - 75th Percentile (%)'),min=0.0,max=100.0)%>%
        hot_col('Expected Value - Median (%)',format='0.0')%>%
        hot_col('Expected Value - 75th Percentile (%)',format='0.0')
    } else {
      NULL
    }
  })

  tri_prior_samp <- reactiveVal()

  output$tri_prior_plot <- renderPlot({
    if(input$tri_prior_plot_select != ''){
      tri_priors_df <- values[['tri_priors_df']][values[['tri_priors_df']]$province==input$tri_prior_plot_select,] # isolate(values[['tri_priors_df']])
      x_max <- min(0.1,max(tri_priors_df$prior_med+qnorm(0.975)*(tri_priors_df$prior_q75-tri_priors_df$prior_med)/qnorm(0.75),na.rm = TRUE)) # Clip the plot at 10%, or smaller if the 95th pctle for each is less.
      #
      med <- triagg:::trans(tri_priors_df$prior_med)#[tri_priors_df$province==input$tri_prior_plot_select])
      q75 <- triagg:::trans(tri_priors_df$prior_q75)#[tri_priors_df$province==input$tri_prior_plot_select])
      prior_sd <- (q75 - med) / qnorm(0.75) #.674
      #
      if(prior_sd==0.0){
        samp <- rep(med,1000000)
      } else {
        samp <- rnorm(100000, med, prior_sd)
      }
      samp <- triagg:::inv_trans(samp[samp <= triagg:::trans(x_max)])
      tri_prior_samp(samp)
      plt <- ggplot() + geom_density(aes(x=samp)) +
        scale_x_continuous(labels = scales::percent,limits=c(0,x_max*1.1)) +
        labs(x="Population Proportion") +
        theme(panel.spacing = unit(0.5, "lines"),
              strip.background = element_blank(),
              axis.text=element_text(size=14,face='bold'),
              axis.title=element_text(size=16,face='bold'),
              strip.placement = "outside")
      suppressWarnings(plt)
    } else {
      NULL
    }
  })

  output$tri_prior_quant <- renderTable({
    if(is.null(tri_prior_samp()))
      return(NULL)
    q <- t(as.matrix(quantile(tri_prior_samp(), probs = c(.05,.1,.2,.3,.4,.5,.6,.7,.8,.9,.95),na.rm=TRUE)))
    as.data.frame(q)
  })

  output$tri_prior_summaries <- renderTable({
    if(is.null(tri_prior_samp()))
      return(NULL)
    s <- tri_prior_samp()
    data.frame(Median=median(s), Mean=mean(s), `Standard Deviation`=sd(s))
  })

  #######################
  # >>> RUN TRIANGULATOR

  # Run Triangulator
  observeEvent(input$run_tri,{
    if(is.null(values[["kp_df"]]) || !all(!is.na(values[['tri_priors_df']])))
      return(NULL)
    showModal(modalDialog(title=NULL,align='center',tags$h3('Running Triangulator'),footer=NULL,size='l',easyClose = FALSE))
    tri_out <- triagg:::triangulate(isolate(values[["kp_df"]]),isolate(values[['tri_priors_df']]))

    values[['tri_full_output']] <- tri_out$full_df
    values[['tri_consensus_output']] <- tri_out$consensus_df

    values[['triangulator_results']] <- rows_update(values[['triangulator_results']],tri_out$full_df,by=c('country','kp','study_idx','observation_idx','method','year','area_name','province'))

    output$tri_out_table <- renderTable({
      tab <- tri_out$consensus_df[,c('province','display_proportion_estimate','display_CI')]
      provinces_to_add <- isolate(values[['demo_df']]$province)[!(isolate(values[['demo_df']]$province) %in% tab$province)]
      tab <- bind_rows(tab,data.frame('province'=provinces_to_add,'display_proportion_estimate'=rep('No Data',length(provinces_to_add))))
      colnames(tab) <- c('Province','Population Percentage','95% CI')
      tab[order(tab$Province),]
    })

    kp_forest_plot_options <- sort(unique(tri_out$full_df$province))

    updateSelectInput(session,'kp_forest_plot_select','SNU:',
                      choices=kp_forest_plot_options)

    # Compute the value of T for aggregator:
    eb <- triagg:::empirical_bayes(triagg:::logit(tri_out$full_df$proportion_estimate),
                          ((triagg:::logit(tri_out$full_df$proportion_upper)-triagg:::logit(tri_out$full_df$proportion_lower))/(2*qnorm(0.975)))^2)
    values[['t_value']] <- eb$t

    removeModal()
    #
    values[['nav_list']][[input$kp]][3] <- 2
    updateTabsetPanel(inputId='tabs',selected='Triangulator Outputs')
  })

  ########################
  # TRIANGULATOR OUTPUTS #
  ########################

  # Triangulator Output Plots:
  output$kp_forest_plot <- renderPlot({
    if(!is.null(values[['tri_full_output']]))
      triagg:::plot_triangulator_forest(isolate(values[['tri_full_output']]),input$kp_forest_plot_select,input$kp_forest_scale)
  })

  output$kp_var_unexp <- renderText({
    if(!is.null(values[['tri_full_output']])){
      val <- values[['tri_consensus_output']]$var_unexp[values[['tri_consensus_output']]$province==input$kp_forest_plot_select]
      paste('Percent of Estimate Variability Attributable to Unaccounted-for Study Bias:',ifelse(!is.na(val),round(100*val,2),NA))
    }
  })

  ############################
  # >>> ADVANCE TO AGG INPUTS

  observeEvent(input$progress_to_agg,{
    values[['nav_list']][[input$kp]][4] <- ifelse(all(all(!is.na(values[['tri_priors_df']]),!is.na(input$urb_prior_median),!is.na(input$urb_prior_q95))),2,1)
    updateTabsetPanel(inputId='tabs',selected='Aggregator Inputs')
  })

  #####################
  # AGGREGATOR INPUTS #
  #####################

  # DEMOGRAPHIC DATA TABLE
  output$demo_ref_pop <- renderText({
    paste('Reference Population: ',kp_ref_display_list[[input$kp]],' (15-49)')
  })

  observe({
    if (!is.null(input$demo_data_hot)) {
      demo_df <- hot_to_r(input$demo_data_hot)
      demo_df <- demo_df[1:(nrow(demo_df)-1),]
      colnames(demo_df) <- c('province','year','pop','urban_proportion','prop_of_nat_pop')
    } else {
      if (is.null(values[["demo_df"]]))
        demo_df <- demo_df
      else
        demo_df <- values[["demo_df"]]
    }
    values[["demo_df"]] <- demo_df
  })

  output$demo_data_hot <- renderRHandsontable({
    demo_df <- values[['demo_df']]
    if (!is.null(demo_df)){
      demo_df <- demo_df[demo_df$province!='Total',]
      rownames(demo_df) <- 1:nrow(demo_df)
      demo_df <- demo_df %>%
        mutate(prop_of_nat_pop=pop/sum(pop)) %>%
        add_row(province='Total',
                year=unique(demo_df$year),
                pop=sum(demo_df$pop),
                urban_proportion=sum(demo_df$pop*demo_df$urban_proportion)/sum(demo_df$pop),
                prop_of_nat_pop=sum(demo_df$prop_of_nat_pop))
      colnames(demo_df) <- c('Province','Year','Population','Percent Urban','Percent of National Pop.')
      rhandsontable(
        demo_df,
        useTypes = TRUE,
        readOnly = TRUE, #FALSE,
      ) %>%
        hot_col('Province') %>%   #,readOnly = TRUE) %>%
        hot_col('Year') %>%   #,readOnly = TRUE) %>%
        hot_col('Population',format='0,0') %>%
        hot_col('Percent Urban',format='0.0%') %>%
        hot_col('Percent of National Pop.',format='0.0%') %>%   #,readOnly = TRUE) %>%
        hot_row(nrow(demo_df))  #,readOnly = TRUE)
    }
  })

  t_value <- reactiveVal(NA)
  global_t_value <- .3


  observeEvent(input$urb_prior_median,{
    if(input$urb_prior_median > input$urb_prior_q95){
      updateNumericInput(session,
                         inputId='urb_prior_q95',
                         value = input$urb_prior_median,
                         min = input$urb_prior_median)
    }
  })

  urb_prior_samp <- reactiveVal()
  #
  output$urb_prior_plot <- renderPlot({
      if(is.null(input$urb_prior_median)|is.null(input$urb_prior_q95)|(input$urb_prior_q95<input$urb_prior_median)){

      } else {
        urb_med <- log(input$urb_prior_median)
        urb_q95 <- log(input$urb_prior_q95)
        urb_prior_sd <- (urb_q95 - urb_med) / qnorm(0.975) #1.644854 #.674
        #
        samp <- rnorm(100000, urb_med, urb_prior_sd)
        samp <- exp(samp)
        urb_prior_samp(samp)
        ggplot() + geom_density(aes(x=samp)) +
          scale_x_continuous(labels = scales::percent,limits=c(0,1)) +
          labs(x="Rural/Urban KP Proportion Ratio",y='',) +
          theme(panel.spacing = unit(0.5, "lines"),
                strip.background = element_blank(),
                text=element_text(size=14,face='bold'),
                axis.text=element_text(size=14,face='bold'),
                axis.title=element_text(size=16,face='bold'),
                strip.placement = "outside")
      }
  })

  #####################
  # >>> RUN AGGREGATOR

  observeEvent(input$run_agg,{
    if(is.null(values[["tri_consensus_output"]]))
      return(NULL)
    showModal(modalDialog(title=NULL,align='center',tags$h3('Running Aggregator'),footer=NULL,size='l',easyClose = FALSE))
    #
    demo_df <- isolate(values[["demo_df"]])
    parameter_priors <- list(alpha=log(input$urb_prior_median),gamma=((log(input$urb_prior_q95)-log(input$urb_prior_median))/qnorm(0.975)),t=values[['t_value']])
    agg_out <- triagg:::run_aggregator(isolate(values[["tri_consensus_output"]]),demo_df,parameter_priors)
    values[['aggregator_output']] <- agg_out
    values[['aggregator_results']] <- rows_update(values[['aggregator_results']],agg_out,by=c('country','kp','level','urb','province'))
    values[['urb_prior_df']][,input$kp] <- c(input$urb_prior_median,input$urb_prior_q95)
    #
    demo_df$sex <- kp_ref_list[[input$kp]]
    demo_cols <- c('province','year','sex','pop','urban_proportion')
    values[['full_demo_df']][,demo_cols] <- values[['full_demo_df']][,demo_cols] %>%
      rows_update(demo_df[,demo_cols],by=c('province','year','sex'))
    #
    removeModal()
    #
    values[['nav_list']][[input$kp]][5] <- 2
    updateTabsetPanel(inputId='tabs',selected='Aggregator Outputs')
  })

  ######################
  # AGGREGATOR OUTPUTS #
  ######################

  # Aggregator Output Tables

  output$agg_nat_out_table <- renderTable({
    if(!is.null(values[["aggregator_output"]])){
      agg_output_df <- isolate(values[['aggregator_output']])
      # Make some display columns
      agg_output_df$display_proportion_estimate <- paste0(round(100*agg_output_df$proportion_estimate,2),'%')
      agg_output_df$display_CI <- paste0('(',round(100*agg_output_df$proportion_lower,2),'%, ',round(100*agg_output_df$proportion_upper,2),'%)')
      #
      tab <- agg_output_df[(agg_output_df$level=='National')&(agg_output_df$urb==input$agg_display_select),c('province','display_proportion_estimate','display_CI')]
      colnames(tab) <- c('','Population Percentage','95% CI')
      tab
    }
  })

  output$agg_SNU_out_table <- renderTable({
    if(!is.null(values[["aggregator_output"]])){
      agg_output_df <- isolate(values[['aggregator_output']])
      # Make some display columns
      agg_output_df$display_proportion_estimate <- paste0(round(100*agg_output_df$proportion_estimate,2),'%')
      agg_output_df$display_CI <- paste0('(',round(100*agg_output_df$proportion_lower,2),'%, ',round(100*agg_output_df$proportion_upper,2),'%)')
      #
      tab <- agg_output_df[(agg_output_df$level=='Province')&(agg_output_df$urb==input$agg_display_select),c('province','display_proportion_estimate','display_CI')]
      colnames(tab) <- c('Province','Population Percentage','95% CI')
      tab
    }
  })

  # Aggregator Output Plot
  output$agg_out_plot <- renderPlot({
    if(!is.null(values[['aggregator_output']]))
      triagg:::plot_aggregator_forest(isolate(values[['aggregator_output']]),input$agg_out_scale,input$agg_display_select)
  })

  ######################
  # >>> NEXT KP BUTTON

  output$progress_kp_button <- renderUI({
    if(all(values[['overall_prog_table']]=='Valid')){
      NULL
    } else {
      actionButton('progress_kp','Next KP >',style="color: #fff; background-color: #fc5151; border-color: #ffb5b5")
    }
  })

  observeEvent(input$progress_kp,{
    prog_table <- isolate(values[['overall_prog_table']])
    updateSelectInput(session,'kp',selected=colnames(prog_table)[which(prog_table=='Invalid')][1])
  })


  ############################
  # INSTRUCTIONS / HELP TEXT #
  ############################

  # KP Data Input

  output$kp_instructions <- renderText({"<ul>
    <li>For each population size estimate in the table below, assign a <b>Study Confidence</b> score between 0 and 100.</li>
    <li>The <b>Study Confidence</b> scales the standard error such that a value of 50 doubles the standard error and a value of 25 quadruples it.</li>
    <li><b> Study Confidence </b> should account for sources of uncertainty not captured by the listed uncertainity interval, such as issues with study implementation,
    violation of sampling assumptions, age of the estimate, etc...
    </li>
    </ul>
    "
  })

  output$full_kp_instructions <- renderText({'
    <div style="max-width:600px; word-wrap:break-word;">
    There are two sources of error that can be present in an estimate of a population quantity.
    The first is <i>sampling error</i>, which is the uncertainty arising from the fact that we only collect a sample from the population and not everyone in the population.
    Sampling error is what is accounted for by confidence intervals.
    <br>
    <br>
    <i>Non-sampling error</i> is the error that arises from violation of the sampling assumptions and/or flawed study implementation.
    The potential non-sampling error can assessed by critically looking at the designs, their assumptions and any implementation challenges that were present.
    Non-sampling error is introduced through the <b>Study Confidence</b> values.
    <br>
    <br>
    Teams should evaluate each study and its potential for non-sampling error, scoring it between 0 (no confidence) and 100 (assumptions met + great implementation).
    The <b>Study Confidence</b> scales the standard error such that a value of 50 doubles the standard error and a value of 25 quadruples it.
    </div>
    '})

  observeEvent(input$show_full_kp_instructions,{
    showModal(modalDialog(
      title='Study Confidence Instructions',
      easyClose=TRUE,
      htmlOutput('full_kp_instructions')
    ))
  })

  # Triangulator Inputs
  output$tri_instructions <- renderText({'
    For each province below, pose the following questions to your team to complete the table:
    <ul>
    <li>Ignoring the estimates from the previous tab, <b>what is your best guess for the true key population proportion?</b>
    Enter this value in the "Expected Value - Median" column.</li>
    <li>We are uncertain about the true key population proportion - it may be higher than our best guess, it may be lower.
    <b>Choose a value for which you think there is a 25% chance the true proportion is higher and a 75% chance the true proportion is lower.</b>
    Enter this value in the "Expected Value - 75th Percentile" column.</li>
    </ul>
    '
  })

  output$full_tri_instructions <- renderText({'
    <div style="max-width:600px; word-wrap:break-word;">
    The information about the "Expected Value estimate" entered here provides the model with information about <b>prior beliefs</b>, to use the language of Bayesian statistics.
    This prior information represents the current understanding of the quantity of interest and its uncertainty absent the empirical estimates included in the consensus process.
    As much as is possible, experts and stakeholders should ignore the information gained from the empirical estimates that are being used when constructing a prior.
    <br>
    <br>
    Prior elicitation is a art that requires some thought and care.
    The Triangulator requires a median value, such that the experts think that there is a 50% chance the true value is above and a 50% chance it is below.
    It also requires the 75% percentile of the distribution such that the experts think that there is a 25% chance the true value is above it and a 75% chance it is below.
    <br>
    <br>
    Begin with the elicitation of the median. If possible, pose the question <b> "What is your best guess for what the true key population proportion is?" </b> to each expert in the group.
    Remind them to ignore the estimates that are going to be used. Record each value and then open a discussion two synthesize these into a single value for the whole group.
    <br>
    <br>
    Next elicit the 75% percentile. Pose the question <b> "We are uncertain about what the key population proportion is. Maybe it is higher than our best guess and maybe lower.
    For some value larger than your best guess, you think that there is a 25% chance it is higher and a 75% chance it is lower.
    For what number do you think that the probability the key population proportion exceeds this number is 25%?"</b>
    <br>
    <br>
    Once that information is entered in the table, a histogram of the distribution of the population proportion will be shown.
    Ask each expert whether this is a reasonable representation of the uncertainty about the population proportion.
    If it is not, change the median and 75% percentile to refine the distribution. The summary tables can also be useful in evaluating the reasonableness of the prior distribution.
    Is it reasonable to think that there is a 20% chance the true value is above the 80th percentile? Is it reasonable to think that there is a 10% chance it is below the 10th percentile?
    <br>
    <br>
    Once the table has been completed for each province and the distributions are satisfactory, the "Run Triangulator" button can be pressed, at which point the Triangulator model will produce consensus estimates for each province listed.
    </div>
    '})

  observeEvent(input$show_full_tri_instructions,{
    showModal(modalDialog(
      title='Triangulator Expected Value (Prior Information) Instructions',
      easyClose=TRUE,
      htmlOutput('full_tri_instructions')
    ))
  })


  # Triangulator Outputs
  output$tri_out_instructions <- renderText({'
  For each province below, examine the table and forest plots of the Expected Value estimates (priors), the study estimates (and their confidence-scaled uncertainity), and the consensus estimate outputs.
  If the results for each province seem reasonable, press the "Aggregator Inputs" button to proceed to the Aggregator model to begin producing a national consensus estimate.
  '
  })

  # Aggregator Inputs

  output$agg_instructions <- renderText({'
    Examine and make any necessary adjustments to the following inputs:
    <ol>
    <li><b>Demographic Data:</b></li> Province-level estimates of the <b>proportion of the national population</b>, calculated from the reference population for the selected key population (e.g. men aged 15-49 for MSM), and percentage of the population that lives in an urban area (<b> percent urban</b>).
    <li><b>Rural:Urban Ratio:</b> the estimated (<b>median</b>) ratio of key population sizes in rural areas to key population sizes in urban areas. For example, if the urban PSE percentage is 1.5% and the rural:urban ratio is 0.6, then the estimated rural PSE percentage is 0.9%.
    As there is uncertainty around this value, the <b>95th percentile</b> must also be entered, such that there is a 95% chance that the rural:urban ratio is less than that value.
    </ol>
    '
  })

  output$full_agg_instructions <- renderText({'
    <div style="max-width:600px; word-wrap:break-word;">
    For the demographic data, population sizes for the key population\'s reference population (men aged 15-49 for MSM and TGW, women aged 15-49 for FSW, and men and women aged 15-49 for PWID) are listed for each province.
    These values are used to calculate the "proportion of national population column," which is used by the Aggregator.
    Verify that each estimate gives a reasonable "proportion of the national population," and make adjustments to the reference populations if necessary.
    As well, for each province, verify that the "urban percentage" (the percentage of the population in the province that lives in urban areas) is reasonable, and make adjustments if necessary.
    <br>
    <br>
    For the rural:urban ratio (the estimated ratio of key population size in rural areas to key population size in urban areas), elicitation of the median and 95th percentile can be done in a similar manner to the Triangulator Expected Value estimates.
    For the median value, pose the question <b> "What is your best guess for the ratio between rural key population sizes and urban key population sizes?" </b> to each expert in the group. Record each value and then open a discussion two synthesize these into a single value for the whole group.
    <br>
    <br>
    Next elicit the 95% percentile. Pose the question <b> "We are uncertain about what the rural:urban ratio is. Maybe it is higher than our best guess and maybe lower.
    For some value larger than your best guess, you think that there is a 5% chance it is higher and a 95% chance it is lower.
    For what number do you think that the probability the quantity of interest exceeds this number is 5%?"</b>
    <br>
    <br>
    Once that information is entered, a histogram of the distribution of the rural:urban ratio will be shown.
    Ask each expert whether this is a reasonable representation of the uncertainty about the rural:urban ratio.
    If it is not, change the median and 95% percentile to refine the distribution.
    <br>
    <br>
    Once all inputs have been verified or modified and the distributions are satisfactory, the "Run Aggregator" button can be pressed, at which point the Aggregator model will produce a national consensus estimate.
    </div>
    '})

  observeEvent(input$show_full_agg_instructions,{
    showModal(modalDialog(
      title='Aggregator Input Instructions',
      easyClose=TRUE,
      htmlOutput('full_agg_instructions')
    ))
  })

  # Aggregator Outputs
  output$agg_out_instructions <- renderText({'
    Examine the results of the consensus estimates for the national key population proportion in the table and forest plot below. Note that key population proportions estimates in provinces that are informed by study estimates will have lower uncertainty than provinces without estimates.
    If the results are resonable, press the "Next KP" button to proceed to the next key population and begin the Triangulator/Aggregator process again.
    '
  })


}
