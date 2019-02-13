source('src/r/plots/ggplot_functions.R')

sh_plot <- function(plot_dt, add_title = TRUE, see_out = FALSE, 
                    y = "OCR", group = "Fibroblast_id", geom = "point", scale = "ptol"){
  
  # Check geoms
  if(! geom %in% c("point", "box", "boxplot") )
    stop("Geom unavailable. Available geoms: point, box or boxplot")
  if(geom == "boxplot") geom = "box"
  
  pt <- if.else("cell_culture" %in% names(plot_dt), plot_dt[!is.na(cell_culture)],
                plot_dt)
  
  # Create ggobject
  if(geom == "point"){
    g = ggplot(pt, aes(factor(time), get(y)))
    } else if(geom == "box"){
      g = ggplot(pt[is.out == F], aes(factor(time), get(y))) 
      } 
  
  # Add the geom
  if(geom == "point"){
    if(see_out == T){
      g = g + geom_point(aes(color = get(group), alpha = !is.out))
      } else
        g = g + geom_point(aes(color = get(group)))
  }else if(geom == "box"){
    g = g + geom_boxplot(aes(color = get(group)))
  } 
  
  g = g + labs(x = "Time", y = y)
  
  if(add_title == T)
    g = g + ggtitle(unique(pt$plate_id))
  
  g = g + theme_bw(base_size = 14)
  
  if(scale == 'ptol')
    g = g + scale_color_ptol(name = group)  # name attribute gives name to legend
  g
}

# sh_plot(dt, add_title = F)

bio_plot <- function(bio_dt, bio = "max_respiration", Method="LR_ao", group = "Fibroblast_id", geom="box", 
                     bw=TRUE, add_title=TRUE, fill="white"){
  # geom can be either box or violin
  # available methods: LR, RR, LR_ao
  
  g = ggplot(bio_dt[method == Method], aes(get(group), get(bio)))
  if(geom %in% c("box", "boxplot")){
    g = g + geom_boxplot(fill = fill) 
  }else if(geom == "violin"){
    g = g + geom_violin(scale = "count", adjust = 0.5, fill = fill)
  }
  
  g = g + geom_jitter(height = 0, width = 0.1)
  g = g + labs(x = group, y = bio)
  
  if(add_title == T)
    g = g + ggtitle(unique(bio_dt$plate_id))
  
  if(bw == T){
    g = add_theme_bw(g, legend_pos ="right")
  }else{
    g = increase_labs_ggplot(g)
  }
  g
}

# bio_plot(w_bio_dt[plate_id == pi], geom = "violin", fill = "cornflowerblue")

## Plot to display outlier status with alpha
outlier_plot = function(DT, cc, add_out = T, group = "cell_culture", y = 'OCR'){
  plot_dt = DT[get(group) == cc & Interval != "Int5"]
  
  p = ggplot(plot_dt, aes(x = time, y = get(y))) 
  
  # If there are outliers and add_out == T, add alpha fill
  if(sum(plot_dt$is.out) != 0 & add_out == T){
    p = p + geom_point(aes(col = row, shape = col, alpha = !is.out), size = 3)} else
      p = p + geom_point(aes(col = row, shape = col), size = 3) 
  
  p = p + ggtitle(cc) + theme_bw(base_size = 14) + labs(x = "Time", y = y)
  p = p + scale_color_ptol()
  p
}

