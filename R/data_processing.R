
process_kp_workbook <- function(df){
  interp_stat <- 'q75'

  df <- df[(df$Indicator=='Population size estimate')&!(df$Method %in% c('PLACE/Mapping','Median / Delphi / Consensus')),]
  df <- df[,!(colnames(df) %in% c('Indicator'))]
  colnames(df) <- c('country','method','kp','area_name','province','year','count_estimate','proportion_lower','proportion_estimate','proportion_upper','study_idx','observation_idx')

  df <- df %>% mutate_at(vars('year','study_idx','observation_idx'),as.character) %>% mutate_at(vars('proportion_estimate','proportion_lower','proportion_upper'),function(x){suppressWarnings(as.numeric(x))})

  # Deal with SE interpolation
  df$SE_interpolated <- ifelse(is.na(df$proportion_lower)&is.na(df$proportion_upper),TRUE,
                               ifelse(df$proportion_lower==df$proportion_upper,TRUE,FALSE))
  df <- merge(df,logit_SE_interpolation[,c('method','kp',interp_stat)],by=c('method','kp'),all.x=TRUE)
  df$proportion_lower[df$SE_interpolated] <- expit(logit(df$proportion_estimate[df$SE_interpolated])-1.96*df[df$SE_interpolated,interp_stat])
  df$proportion_upper[df$SE_interpolated] <- expit(logit(df$proportion_estimate[df$SE_interpolated])+1.96*df[df$SE_interpolated,interp_stat])
  df <- df[,!(colnames(df) %in% c(interp_stat))]

  # Convert count_estimate to integer
  #df$count_estimate <- as.integer(df$count_estimate)

  # Reorder, drop count?
  df <- df[,c(3,2,11:12,1,6,4:5,9,8,10,13)] #c(11,12,1:10,13,14)]

  return(df)
}

generate_output_dataframes <- function(full_kp_df,full_demo_df,filename){
  country <- unique(full_kp_df$country)
  kps <- unique(full_kp_df$kp)

  tri_full_out <- full_kp_df

  agg_list <- list()

  for(kp in kps){
    tri_full_out <- bind_rows(tri_full_out,
                              data.frame('country'=country,'kp'=kp,'method'='Prior','province'=sort(unique(full_kp_df$province[full_kp_df$kp==kp]))),
                              data.frame('country'=country,'kp'=kp,'method'='Consensus','province'=sort(unique(full_kp_df$province[full_kp_df$kp==kp]))))
    agg_list[[length(agg_list)+1]] <- data.frame('country'=country,'kp'=kp,'level'='Province','urb'=rep(c('Urban','Rural','Total'),length(unique(full_demo_df$province))),'province'=rep(unique(full_demo_df$province),rep(3,length(unique(full_demo_df$province)))))
    agg_list[[length(agg_list)+1]] <- data.frame('country'=country,'kp'=kp,'level'='National','urb'=c('Urban','Rural','Total'),'province'='National')
  }

  agg_full_out <- do.call('rbind.data.frame',agg_list) %>%
    mutate(!!!setNames(rep(NA,6),c("proportion_estimate","proportion_lower","proportion_upper","has_ests","prop_of_nat_pop","urban_proportion"))) %>%
    mutate_at(vars(proportion_estimate,proportion_lower,proportion_upper,has_ests,prop_of_nat_pop,urban_proportion),as.numeric)

  tri_full_out <- tri_full_out %>% mutate_at(vars(proportion_estimate,proportion_lower,proportion_upper),as.numeric)

  # Now that the Triangulator and Aggregator Dataframes are created and properly sized to our input data, we will check if there are any saved inputs in the
  # backend sheets

  if('triangulator_confidence' %in% getSheetNames(filename)){
    prev_tri <- openxlsx::read.xlsx(filename,
                                    sheet='triangulator_confidence',
                                    colNames=TRUE)
    if(nrow(prev_tri)>0){
      tri_full_out <- tri_full_out %>%
        left_join(prev_tri[,c('observation_idx','confidence')], by=c('observation_idx'),na_matches='never') %>%
        mutate(confidence=as.integer(coalesce(confidence.y,confidence.x)),confidence.x=NULL,confidence.y=NULL)

      # Check for priors to load as well...
      tri_full_out[tri_full_out$method=='Prior',] <- tri_full_out %>%
        filter(method=='Prior') %>%
        rows_update(prev_tri[prev_tri$method=='Prior',],by=c('country','kp','method','province'))
    }
  }

  # Aggregator Inputs

  if('aggregator_input' %in% getSheetNames(filename)){
    prev_urb_prior <- openxlsx::read.xlsx(filename,
                                          sheet='aggregator_input',
                                          colNames=TRUE,
                                          startRow=1,
                                          rows=1:3)
    prev_demo_df <- openxlsx::read.xlsx(filename,
                                        sheet='aggregator_input',
                                        colNames=TRUE,
                                        startRow=4)
  } else {
    prev_urb_prior <- NULL
    prev_demo_df <- NULL
  }

  # URB PRIOR DF
  if(is.null(prev_urb_prior)||(nrow(prev_urb_prior) == 0)){
    prev_urb_prior <- data.frame(stat=c('urb_prior_median','urb_prior_q95'))
    for(kp in kps){
      if(!(kp %in% colnames(prev_urb_prior))){
        prev_urb_prior[kp] <- c(0.6,0.7)
      }
    }
  }
  # Demography DF
  if(is.null(prev_demo_df)||(nrow(prev_demo_df) == 0)){
    demo_out <- full_demo_df
  } else {
    demo_out <- full_demo_df %>% rows_update(prev_demo_df,by=c('area_id','year','sex'))
  }
  return(list('triangulator_results'=tri_full_out,'aggregator_results'=agg_full_out,'urb_prior_df'=prev_urb_prior,'full_demo_df'=demo_out))
}

# Translate between the Triangulator Prior HOT and the Dataframe Version

tri_prior_HOT_to_results <- function(tri_prior_HOT,triangulator_results,kp){
  tri_prior_HOT <- tri_prior_HOT %>%
    prov_order <- triangulator_results$province[(triangulator_results$kp==kp)&(triangulator_results$method=='Prior')]
    cols <- c('proportion_estimate','proportion_upper','proportion_lower')
    triangulator_results[(triangulator_results$kp==kp)&(triangulator_results$method=='Prior'),cols] <- tri_prior_HOT[match(tri_prior_HOT$province,prov_order),cols]
    return(triangulator_results)
}

tri_prior_results_to_HOT <- function(triangulator_results,kp,tri_prior_provinces=NA){
  tri_prior_HOT <- triangulator_results[(triangulator_results$kp==kp)&(triangulator_results$method=='Prior'),
                                        c('province','proportion_estimate','proportion_lower','proportion_upper')] %>%
    mutate('prior_q75'=expit(logit(proportion_estimate)+(qnorm(0.75)/(2*qnorm(0.975)))*(logit(proportion_upper)-logit(proportion_lower))),
           'proportion_lower'=NULL,
           'proportion_upper'=NULL) %>%
    rename(prior_med='proportion_estimate')
  if(!is.na(tri_prior_provinces)){
    if(all(tri_prior_provinces %in% tri_prior_HOT$province)){
      tri_prior_HOT <- tri_prior_HOT[tri_prior_HOT$province %in% tri_prior_provinces,]
    } else {
      tri_prior_HOT <- bind_rows(tri_prior_HOT,data.frame('province'=tri_prior_provinces[!(tri_prior_provinces %in% tri_prior_HOT$province)],
                                                          'kp'=kp,
                                                          'method'=prior))
    }
    tri_prior_HOT <- tri_prior_HOT %>% arrange(factor(province,tri_prior_provinces))
  }
  return(tri_prior_HOT)
}
