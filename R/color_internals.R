######################################################
# Hilfsobjekte für die App
######################################################

valid_seq_palettes<-((capture.output(colorspace::hcl_palettes(type = "Sequential (multi-hue)"))%>%
                        .[4:10])%>%str_sub(.,start = 8)%>%
                       map(.,function(x){str_split(.,",")})%>%unlist()%>%
                       .[.!=""])%>%
  str_trim(side = "both")%>%unique()
valid_seq_palettes <- valid_seq_palettes[is.na(valid_seq_palettes)==F]


#Datensatz mit möglichen Farbpaletten
palette_data<-tibble(
  palette=c("Tableau 10","Tableau 20",
            #"Greens","Blues","Reds",
            "Pastel 1", "Dark 2", "Dark 3", "Set 2", "Set 3", "Warm", "Cold", "Harmonic", "Dynamic",
            valid_seq_palettes),
  max_n=c(10,20,
          # 9,9,9,
          rep(20,9),rep(20,length(valid_seq_palettes))),
  fun=c(rep("tableau_color_pal(palette = '{palette}')({n})",2),
        # rep("brewer.pal(n = {n},name = '{palette}')",3),
        rep("qualitative_hcl({n},palette = '{palette}')",9),
        rep("sequential_hcl(n={n},palette = '{palette}')",length(valid_seq_palettes)))
)%>%
  arrange(palette)


######################################################
# Hilfsfunktionen für die App
######################################################



#Faktorlevel bzw. einzigartige Ausprägungen von Charakter Variablen extrahieren:
extract_codes<-function(variable){
  stopifnot(is.character(variable)||is.factor(variable))
  if(is.factor(variable)){
    as.character(levels(variable))
  } else {
    as.character(unique(variable))
  }
}
to_r_vector<-function(vector){
  map_chr(vector,function(x){glue("\'{x}\'")})%>%
    str_c(.,collapse = ",")%>%
    c("c(",.,")")%>%
    str_c(.,collapse = "")
}
#Shortcut zum evaluieren von Strings
process<-function(string){
  eval(parse_expr(string))
}
#Darstellen von Farben in einem Balkendiagramm:


accepted_palettes<-function(palette_data,n){
  result<-palette_data%>%
    filter(max_n>=n)
  
  result$fun<-map2_chr(result$palette,result$fun,function(palette,f){glue(f)})
  result$colors<-map(result$fun,~process(.x))
  return(result)
}

determine_table_length<-function(n_rows,n_cols=3){
  short_cols <- n_rows %/% n_cols
  c(n_rows-(n_cols-1)*short_cols,rep(short_cols,n_cols-1))
}

get_preview_table <- function(palette_data,n_cols){
  require(flextable)
  palette_colors <- accepted_palettes(palette_data,6)
  # Create preview plots
  preview_plots <- map(palette_colors$colors,function(current_colors){
    tibble(col=seq(1,6),
           x=seq(1,6),
           y=1)%>%
      mutate_all(as.factor)%>%
      ggplot(., aes(x = x, y = y, fill = col)) +
      geom_tile()+
      scale_fill_manual(values=current_colors,guide="none")+
      theme_void()
  })
  #Create preview table
  preview_table <- tibble(
    palette=palette_data$palette,
    preview = preview_plots
  )
  
  
  rows<-determine_table_length(dim(preview_table)[1],n_cols = n_cols)
  
  preview_table <- pmap(
    list(
      rows,
      c(1,cumsum(rows)[1:(length(rows)-1)]+1),
      seq_along(rows)
    ),
    function(n,start_row,id){
      result <- preview_table%>%
        filter(seq(from=1,to=sum(rows),by=1) %in% seq(from = start_row,by=1,length.out=n))
      if(dim(result)[1] < max(rows)){
        result2 <- map(1:(max(rows)-dim(result)[1]),function(i){
          tibble(palette="",preview=list(ggplot()+theme_void()))
        })%>%reduce(rbind)
        
        result <- rbind(result,result2)
      }
      return(result%>%rename_all(function(x){str_c(x,"_",id)}))
    })%>%reduce(cbind)%>%as_tibble()
  
  ft <- flextable(preview_table)
  c_env <- current_env()
  walk(names(preview_table)[str_detect(names(preview_table),"preview")],function(column){
    ft <- mk_par(ft, j = column,
                 value = as_paragraph(
                   gg_chunk(value = !!parse_expr(column), width = 1, height = 0.5)
                 ))
    env_poke(env = c_env,nm = "ft",value = ft)
  })
  
  return(ft)
}

extract_col_fill_var<-function(gg){
  #Raise error if not fill or color attribute
  stopifnot(any(names(gg$mapping) %in% c("colour","color","fill")))
  rel_mapping<-gg$mapping[names(gg$mapping) %in% c("colour","color","fill")]
  tibble(
    !!parse_expr(names(rel_mapping)[1]) :=   gg$data%>%pull(as_name(rel_mapping[[1]]))
  )
  
}
col_fill_var_to_scale<-function(tibble_data,hex_cols){
  parse_expr(glue("scale_{names(tibble_data)[1]}_manual(values={to_r_vector(hex_cols)})"))
}

