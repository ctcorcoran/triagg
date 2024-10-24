
# A. TRIANGULATOR FOREST

plot_triangulator_forest <- function(triangulator_full_output,province,plot_scale,display_name='method-year'){
  forest_df <- triangulator_full_output[triangulator_full_output$province==province,]
  #
  if(display_name=='method'){
    forest_df <- forest_df[rev(order(forest_df$year,forest_df$method)),]
    forest_df$display_name <- factor(forest_df$method,
                                     levels=c('Consensus',unique(forest_df$method[!(forest_df$method %in% c('Consensus','Prior'))]),'Prior'))
  } else if(display_name=='method-year'){
    forest_df$display_name <- ifelse(forest_df$method %in% c('Consensus','Prior'),
                                     forest_df$method,
                                     paste0(forest_df$method,' (',forest_df$year,')'))
    forest_df <- forest_df[rev(order(forest_df$year,forest_df$method)),]
    forest_df$display_name <- factor(forest_df$display_name,
                                     levels=c('Consensus',unique(forest_df$display_name[!(forest_df$display_name %in% c('Consensus','Prior'))]),'Prior'))
  }

  # Compute Confidence-Scaled Limits
  forest_df$CI_symmetric <- ifelse(abs((forest_df$proportion_upper-forest_df$proportion_estimate)/(forest_df$proportion_estimate-forest_df$proportion_lower)-1)<0.05,TRUE,FALSE)
  forest_df$conf_lower <- ifelse(forest_df$CI_symmetric,
                                 forest_df$proportion_estimate-(forest_df$proportion_upper-forest_df$proportion_lower)/(2*forest_df$confidence/100),
                                 expit(logit(forest_df$proportion_estimate)-(logit(forest_df$proportion_upper)-logit(forest_df$proportion_lower))/(2*forest_df$confidence/100)))
  forest_df$conf_upper <- ifelse(forest_df$CI_symmetric,
                                 forest_df$proportion_estimate+(forest_df$proportion_upper-forest_df$proportion_lower)/(2*forest_df$confidence/100),
                                 expit(logit(forest_df$proportion_estimate)+(logit(forest_df$proportion_upper)-logit(forest_df$proportion_lower))/(2*forest_df$confidence/100)))

  # Set some maximum/minimum values
  eff_min <- 1e-4
  extend_pct <- 0.05
  #
  conf_min <- max(eff_min,min(forest_df$conf_lower[!is.na(forest_df$conf_lower)],forest_df$proportion_lower))
  conf_max <- max(forest_df$conf_upper[!is.na(forest_df$conf_upper)],forest_df$proportion_upper)
  forest_df$conf_lower <- sapply(forest_df$conf_lower,function(x){max(conf_min,x)})
  forest_df$conf_upper <- sapply(forest_df$conf_upper,function(x){min(conf_max,x)})
  #
  x_min = conf_min*(1-extend_pct)
  x_max = conf_max*(1+extend_pct)

  # Set Aesthetic Columns
  forest_df$type <- factor(ifelse(forest_df$method=='Consensus','Consensus',
                                  ifelse(forest_df$method=='Prior','Prior','Estimate')),
                           levels=c('Prior','Estimate','Consensus'))
  forest_df$SE_interpolated[is.na(forest_df$SE_interpolated)] <- FALSE #Make Prior and Consensus "non-interpolated" for solid lines

  # Set Plot Scale
  if(plot_scale=='Log'){
    scale_kwrd <- 'log10'
    horiz_axis_label <- 'Population Percentage (Log Scale)'
  } else if(plot_scale=='Linear'){
    scale_kwrd <- 'identity'
    horiz_axis_label <- 'Population Percentage'
  }

  forest <- ggplot(data=forest_df,aes(y=display_name))+
    geom_point(aes(x=proportion_estimate,shape=type,color=type,size=type),position=position_dodge2(.75),show.legend = FALSE,na.rm=TRUE)+
    geom_linerange(aes(xmin=conf_lower,xmax=proportion_lower,color=type,linewidth=type,linetype='dotted'),
                   position=position_dodge2(.75),show.legend = TRUE,na.rm=TRUE)+
    geom_linerange(aes(xmin=proportion_upper,xmax=conf_upper,color=type,linewidth=type,linetype='dotted'),
                   position=position_dodge2(.75),show.legend = TRUE,na.rm=TRUE)+
    geom_linerange(aes(xmin=proportion_lower,xmax=proportion_upper,color=type,linewidth=type,linetype=SE_interpolated),
                   position=position_dodge2(.75),show.legend = TRUE,na.rm=TRUE)+
    scale_shape_manual(values=c('Prior'=15,'Estimate'=15,'Consensus'=16),guide="none") +
    scale_linetype_manual(values=c('FALSE'='solid','dotted'='11','TRUE'='31'),labels=c('FALSE'='Original 95% CI','dotted'='Confidence-scaled 95% CI','TRUE'='Original 95% CI (Imputed)'))+
    scale_size_manual(values=c('Prior'=5,'Estimate'=5,'Consensus'=7.5),guide="none")+
    scale_linewidth_manual(values=c('Prior'=1.5,'Estimate'=1.5,'Consensus'=2),guide="none")+
    scale_color_manual(values=c('Prior'="#F8766D",'Estimate'="gray35",'Consensus'="#619CFF"),guide="none") +
    labs(x=horiz_axis_label,y='')+
    scale_x_continuous(trans=scale_kwrd,labels=scales::percent,limits=c(x_min,x_max))+
    guides(linetype=guide_legend(override.aes=list(color="gray35",linewidth=1)))+
    theme(legend.position = 'bottom',
          legend.title=element_blank(),
          legend.text=element_text(size=12),
          legend.key.size = unit(2,"lines"),
          panel.spacing = unit(0.5, "lines"),
          strip.background = element_blank(),
          axis.text=element_text(size=14,face='bold'),
          axis.title=element_text(size=16,face='bold'),
          strip.placement = "outside")
  return(suppressWarnings(forest))
}

# B. AGGREGATOR FOREST

plot_aggregator_forest <- function(agg_output,plot_scale,urb='Total'){

  # Subset for Total/Urban/Rural
  plot_data = agg_output[agg_output$urb==urb,]

  # Adjust some datatypes for easier plotting
  plot_data$has_ests <- as.character(plot_data$has_ests)
  plot_data$province <- factor(plot_data$province,levels = rev(sort(plot_data$province))) #plot_data[order(plot_data$prop_of_nat_pop),'province'])
  plot_data$level <- factor(plot_data$level,levels=c('National','SNU'))
  plot_data$has_ests <- factor(plot_data$has_ests)

  # Set some fixed plot limits, so changing from total to urban to rural maintains a consistent scale
  extend_pct <- 0.05
  #
  x_max = max(agg_output$proportion_upper)*(1+extend_pct)
  x_min = min(agg_output$proportion_lower)*(1-extend_pct)

  # Set Plot Scale
  if(plot_scale=='Log'){
    scale_kwrd <- 'log10'
    horiz_axis_label <- 'Population Percentage (Log Scale)'
  } else if(plot_scale=='Linear'){
    scale_kwrd <- 'identity'
    horiz_axis_label <- 'Population Percentage'
  }

  p <- ggplot(data=plot_data,aes(x=proportion_estimate,y=province))+
    geom_pointrange(aes(xmin=proportion_lower,xmax=proportion_upper,color=has_ests,shape=has_ests),inherit.aes = TRUE,size=1.5,linewidth=1.5,show.legend = TRUE,position=position_dodge2(.25))+
    facet_grid(rows=vars(level),scale='free_y',space='free')+
    scale_color_manual(values=c('0'="#F8766D",'1'="#00BA38",'2'="#619CFF"),labels=c('0'='No Estimates','1'='Has Estimates','2'='National')) + #'1'="#7CAE00",
    scale_shape_manual(values=c('0'=16,'1'=15,'2'=18),labels=c('0'='No Estimates','1'='Has Estimates','2'='National')) + #'1'=18,
    scale_x_continuous(trans=scale_kwrd,labels=scales::percent,limits=c(x_min,x_max))+ #trans="log10",
    labs(x=horiz_axis_label,y='Province')+
    theme(legend.position = 'bottom',
          legend.title=element_blank(),
          legend.text=element_text(size=12),
          legend.key.size = unit(2,"lines"),
          panel.spacing = unit(0.5, "lines"),
          strip.background = element_blank(),
          strip.placement = "outside",
          axis.text=element_text(size=14,face='bold'),
          axis.title=element_text(size=16,face='bold'),
          strip.text.y=element_blank())
  return(suppressWarnings(p))
}
