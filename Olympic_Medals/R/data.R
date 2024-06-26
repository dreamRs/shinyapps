filter_discipline <- function(data, discipline_v=NULL){
  if (is.null(discipline_v)){
    return(data)
  }

  rtn_x <- data %>%
    filter(discipline_title %in% discipline_v)

  return(rtn_x)
}

filter_slug_game <- function(data, slug_game_v=NULL){
  if (is.null(slug_game_v)){
    return(data)
  }

  rtn_x <- data %>%
    filter(slug_game %in% slug_game_v)

  return(rtn_x)
}

filter_top <- function(data, top_n=10){
  rtn_x <- data %>% 
    filter(country_name %in% unique(country_name)[seq_len(top_n)])
  return(rtn_x)
}

filter_medal_type <- function(data, medal_type_v){
        #if (medal_type_v == "All"){
        #        medal_type_v <- c("BRONZE", "SILVER", "GOLD")
        #}
        rtn_x <- data %>%
                filter(medal_type %in% medal_type_v)
        return(rtn_x)
}

medal_calc <- function(data){
        rtn_x <- data %>%
          group_by(country_name, medal_type) %>%
          summarise(n_medal=n()) %>%
          mutate(n_tot=sum(n_medal)) %>%
          arrange(-n_tot) %>%
          ungroup()
        return(rtn_x)
}





