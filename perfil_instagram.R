
if(!require("install.load")) {
  install.packages("install.load")
  library(install.load)
}

install_load("plyr","jsonlite","tidyverse","rlist")

instascraper2 <- function(user, n) {
  
  pega_texto <- function(local){
    xz <- function(local){
      text <- local[["node"]][["text"]]
      return(text)}
    dd <- ldply(lapply(local, xz), rbind)
    dd <- as.character(dd$`1`)
    return(dd)
  }
  
  for (y in 1:n) {
    # paginar
    print(y)
    if (y == 1){
      final <- data.frame()
      url <- paste0("https://www.instagram.com/", user, "/?__a=1")
    } else{
      url <- paginar
    }
    
    
    # read url and convert to data.frame
    document <- fromJSON(txt=url)
    s <- document$graphql$user$edge_owner_to_timeline_media$edges$node
    k <- document$graphql$user$edge_felix_video_timeline$edges$node
    
    
    df1 <- data.frame(id_post = s$shortcode,
                      tipo = s$is_video,
                      data = s$taken_at_timestamp,
                      texto =     pega_texto(s[[3]][[1]]),
                      localizacao = s$location$name,
                      n_comentarios = s$edge_media_to_comment$count,
                      n_likes = s$edge_media_preview_like$count,
                      n_views = s$video_view_count,
                      url = paste0("https://www.instagram.com/p/",s$shortcode))  
    
    if(!is.null(k$shortcode)){
    df2 <- data.frame(id_post = k$shortcode,
                      tipo =  toupper(k$product_type),
                      data = k$taken_at_timestamp,
                      texto = pega_texto(k[[3]][[1]]),
                      localizacao = rep(NA, length(k$shortcode)),
                      n_comentarios = k$edge_media_to_comment$count,
                      n_likes = k$edge_media_preview_like$count,
                      n_views = k$video_view_count,
                      url = paste0("https://www.instagram.com/p/",k$shortcode))  
    } else { df2 <- data.frame()}
    
    df <- rbind(df1,df2)

    df$texto <- gsub("[\r\n]", "", df$texto)
    df$localizacao <- gsub("[,-].*", "", df$localizacao)
    df <- df %>% mutate(data = as.POSIXct(df$data, origin="1970-01-01 00:00:00"),
                        tipo = case_when(tipo == FALSE ~ "FOTO",
                                         tipo == TRUE ~ "VIDEO",
                                         tipo == "IGTV" ~ "IGTV"),
                        username = document$graphql$user$username,
                        name = document$graphql$user$full_name,
                        n_post = document$graphql$user$edge_owner_to_timeline_media$count,
                        n_seguindo = document$graphql$user$edge_follow$count,
                        n_seguidores = document$graphql$user$edge_followed_by$count) %>%  
                  arrange(desc(data)) %>%
                  select(username, name, n_post,n_seguindo, n_seguidores,id_post, data,tipo,texto,
                         localizacao,n_comentarios,n_likes,n_views,url)
                              
      
    
    final <- rbind(final,df)    
  
    
    
    end_cursor <- document$graphql$user$edge_owner_to_timeline_media$page_info$end_cursor
    paginar <- paste0("https://www.instagram.com/", user, "/?__a=1&max_id=", end_cursor)
    
  }
  return(final)
}

teste <- instascraper2("trifenol", 2)
