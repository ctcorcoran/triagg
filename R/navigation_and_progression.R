showhide_tabs <- function(nav_list,tab_names){
  for(tab_to_show in which(nav_list > 0)){
    showTab(inputId='tabs',target=tab_names[tab_to_show])
  }
  for(tab_to_hide in which(nav_list == 0)){
    hideTab(inputId='tabs',target=tab_names[tab_to_hide])
  }
}

color_text <- function(text,color){
  text_out <- paste('<span style=\"color:', color,
                    '\">', text,
                    '</span>', sep = "")
  return(text_out)
}

progress_table <- function(nav_list,kp,tab_names){
  if(is.null(nav_list)){
    return(NULL)
  } else {
    input_text <- c('Incomplete','In Progress','Complete')
    result_text <- c('Invalid','Invalid','Valid')
    out <- as.data.frame(lapply(1:5,function(x){ifelse(x%in%c(3,5),
                                                       result_text[nav_list[[kp]][x]+1],
                                                       input_text[nav_list[[kp]][x]+1])}))
    colnames(out) <- tab_names
    rownames(out) <- kp
    return(out)
  }
}

full_progress_table <- function(nav_list,tab_names){
  if(is.null(nav_list)){
    return(NULL)
  } else {
    out <- list()
    for(kp in names(nav_list)){
      out[[length(out)+1]] <- progress_table(nav_list,kp,tab_names)
    }
    out_df <- do.call('rbind.data.frame',out)
    return(out_df)
  }
}

validate_kp_df <- function(kp_df){
  inds <- which(!(kp_df$proportion_upper >= kp_df$proportion_estimate)|!(kp_df$proportion_lower <= kp_df$proportion_estimate)|!(kp_df$proportion_upper >= kp_df$proportion_lower))
  return(list('valid'=ifelse(length(inds)==0,TRUE,FALSE),'invalid_rows'=kp_df[inds,]))
}
