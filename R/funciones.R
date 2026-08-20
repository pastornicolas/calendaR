# funciones.R

# FECHAS ----
calendar_year <- 2026

# transforma el dia calendario en dia de año (1 a 365)
month_day_to_yday <- function(x) {
  as.integer(format(as.Date(paste(calendar_year, x, sep = "-"),
                            format = "%Y-%m-%d"),"%j"))
  }

# COMPLETAR DATA FRAMES RADIALES ----
fill_df <- function(df) {
  
  # Desfase para acomodar si un recurso cruza el 31 de diciembre
  df$ini <- ifelse(df$ini > df$fin, df$ini - 365, df$ini) 
  
  df$offI <- df$ini - 1
  df$offF <- df$fin
  df$iniR <- pi - df$offI * 2 * pi / 365
  df$finR <- pi - df$offF * 2 * pi / 365
  df$mean <- (df$iniR + df$finR) / 2
  df$r_lb <- (df$r + df$r0) / 2
  df$x <- df$r_lb * sin(df$mean)
  df$y <- df$r_lb * cos(df$mean)
  return(df)
}

# ESTACIONES ----
# DF fijo para hemisferio Sur
# (considerar llevar a modulo aparte con Meses)
seasons <- data.frame(label = c("VERANO", "OTOÑO", "INVIERNO", "PRIMAVERA"),
                      col = c("firebrick2","gold2","steelblue3","springgreen3"),
                      bor = c("firebrick2","gold2","steelblue3","springgreen3"),
                      ini = c(month_day_to_yday("12-21"),month_day_to_yday("03-21"),
                              month_day_to_yday("06-21"),month_day_to_yday("09-21")),
                      fin = c(month_day_to_yday("03-20"),month_day_to_yday("06-20"),
                              month_day_to_yday("09-20"),month_day_to_yday("12-20")),
                      r0 = 11, r = 12)
seasons <- fill_df(seasons)

# MESES ----
# DF fijo
# (considerar llevar a modulo aparte con Estaciones)
months <- data.frame(label = c("Enero","Febrero","Marzo","Abril",
                               "Mayo","Junio","Julio","Agosto",
                               "Septiembre","Octubre","Noviembre","Diciembre"),
                     col = NA, bor = NA,
                     ini = sapply(sprintf("%02d-01", 1:12),month_day_to_yday),
                     fin = c(month_day_to_yday("01-31"),month_day_to_yday("02-28"),
                             month_day_to_yday("03-31"),month_day_to_yday("04-30"),
                             month_day_to_yday("05-31"),month_day_to_yday("06-30"),
                             month_day_to_yday("07-31"),month_day_to_yday("08-31"),
                             month_day_to_yday("09-30"),month_day_to_yday("10-31"),
                             month_day_to_yday("11-30"),month_day_to_yday("12-31")),
                     r0 = 10, r = 11)
months <- fill_df(months)

# TRANSFORMAR CATEGORÍAS DEL USUARIO ----
categorias_to_df <- function(categorias) {
  n <- length(categorias)
  resultado <- list()
  if (n == 0) {
    return(data.frame())
  }
  for (i in seq_along(categorias)) {
    categoria <- categorias[[i]]
    # Categoria 1 es la mas lejana al centro
    # Por ej., con 3 cats:
    # Cat1 = 3-4 (siendo r0=3 y r=4)
    # Cat2 = 2-3
    # Cat3 = 1-2
    r0 <- n - i + 1
    r  <- n - i + 2
    for (j in seq_along(categoria$subcategories)) {
      recurso <- categoria$subcategories[[j]]
      resultado[[length(resultado) + 1]] <- data.frame(
        categoria = paste("Categoría", i),
        label = recurso$label,
        col = "black", # creo que esto se sobreescriube luego
        bor = NA,
        ini = month_day_to_yday(recurso$start_date),
        fin = month_day_to_yday(recurso$end_date),
        r0 = r0, r = r, stringsAsFactors = FALSE)
      }
    }
  
  df <- do.call(rbind, resultado)
  
  # Colores (REVISAAAAAAAAAAAAAAAAAAAR !!!!!!!!!) ----
  #cols <- hcl.colors(n, palette = "Dynamic")
  catcols <- c("purple", "green4", "orange", "blue", "red",
            "#11C638", "yellow3", "#4F53B7","coral2")
  
  df.c <- data.frame(cc = catcols, nn = 1:9, c1 = NA, c2 = NA, c3 = NA)
  for (i in seq_along(catcols)) {
    mycol <- catcols[i]
    ramp <- colorRampPalette(c("white", mycol, "black"))(21)
    df.c$c1[i] <- ramp[7]
    df.c$c2[i] <- ramp[4]
    df.c$c3[i] <- ramp[10]
  }
  
  category_number <- match(
    df$categoria,
    paste("Categoría", seq_len(n)))

  recurso_number <- ave(
    seq_len(nrow(df)),
    df$categoria,
    FUN = seq_along
  )
  
  shade_matrix <- df.c[, c("c1", "c2", "c3")]
  df$col <- shade_matrix[cbind(category_number, recurso_number)]
  
  df <- fill_df(df)
}


# PREPARAR CALENDARIO COMPLETO ----

preparar_calendario <- function(categorias) {
  n <- length(categorias)
  
    cat_df <- categorias_to_df(categorias)
    months2 <- months
    seasons2 <- seasons
    months2$r0 <- n + 1
    months2$r  <- n + 2
    seasons2$r0 <- n + 2
    seasons2$r  <- n + 3
    
    months2 <- fill_df(months2)
    seasons2 <- fill_df(seasons2)
    
    ## Divisores ----
    lins2 <- seasons2
    lins2$r0 <- lins2$r0 - 1.1
    lins2$r  <- lins2$r - 1.9
    lins2$bor <- lins2$col
    lins2$label <- NA
    # Re-calcular el radio
    lins2 <- fill_df(lins2)
    

# Agregar columna categoría
  
seasons2$categoria <- NA_character_
months2$categoria  <- NA_character_
lins2$categoria    <- NA_character_
  
  
# Arreglar el orden de columnas
  
common_cols <- c("categoria","label","col","bor",
                 "ini","fin","r0","r","offI","offF",
                 "iniR","finR","mean","r_lb","x","y")
  
seasons2 <- seasons2[, common_cols]
months2  <- months2[, common_cols]
lins2    <- lins2[, common_cols]
lins2$label    <- "lab"
  
if (nrow(cat_df) > 0) {
  cat_df <- cat_df[, common_cols]
  }
  
# Devolver todo el calendario completo
  rbind(seasons2, months2, cat_df, lins2)#, cat_df)
}


# PATHS para el texto ----

crear_text_path <- function(df) {
  
  df2 <- subset(df, !is.na(label) & !r0 %in% c(9.9))
  #print(df2)
  if (nrow(df2) == 0) {
    return(data.frame())
  }
  
  resultado <- lapply(
    seq_len(nrow(df2)),
    function(i) {
      
      d <- df2[i, ]
      angles <- seq(d$iniR, d$finR, length.out = 100)
      
      data.frame(label = d$label,
                 r0 = d$r0,
                 x = d$r_lb * sin(angles),
                 y = d$r_lb * cos(angles))
    }
  )

  do.call(rbind, resultado)
  
}


# LÍNEAS DE MESES Y ESTACIONES ----

crear_lineas <- function(df, n) {
  
  # Para Meses
  month_lines <- subset(df, r0 == n + 1, select = c("label", "iniR"))
  
  month_lines$x <- (n + 1) * sin(month_lines$iniR)
  month_lines$y <- (n + 1) * cos(month_lines$iniR)
  month_lines$xend <- (n + 2) * sin(month_lines$iniR)
  month_lines$yend <- (n + 2) * cos(month_lines$iniR)
  
  month_lines$col <- rep(c("firebrick2","gold2","steelblue3","springgreen3"),each = 3)
  
  
  # Para Estaciones
  seas_lines <- subset(df, r0 == n + 2, select = c("label", "iniR"))
  
  seas_lines$x <- (n + 2) * sin(seas_lines$iniR)
  seas_lines$y <- (n + 2) * cos(seas_lines$iniR)
  seas_lines$xend <- (n + 3) * sin(seas_lines$iniR)
  seas_lines$yend <- (n + 3) * cos(seas_lines$iniR)
  seas_lines$col <- "white"
  
  list(month_lines = month_lines,
       seas_lines = seas_lines)
}


# GRAFICO ----

crear_calendario_plot <- function(categorias) {
  
  n <- length(categorias)
  df <- preparar_calendario(categorias)
  text_path <- crear_text_path(df)
  lineas <- crear_lineas(df, n)
  month_lines <- lineas$month_lines
  seas_lines <- lineas$seas_lines
  
  ## Inicio ----
  p <- ggplot() +
    
  ## Arcos ----
  ggforce::geom_arc_bar(
    data = subset(df, !label == "lab"),
    aes(
      x0 = 0,
      y0 = 0,
      r0 = r0,
      r = r,
      start = iniR,
      end = finR,
      fill = col,
      color = bor),
    alpha = 1,
    linewidth = 1
  ) 
  ## Arco estetico para estación sobre mes ----
  p <- p + 
    ggforce::geom_arc_bar(
      data = subset(df, label == "lab"),
      aes(
        x0 = 0,
        y0 = 0,
        r0 = r0,
        r = r,
        start = iniR,
        end = finR,
        fill = col,
        color = bor),
      linewidth = 1
    )
  
  ## Etiquetas ----
  if (nrow(text_path) > 0) {
    
    ### Meses ----
    p <- p +
      geomtextpath::geom_textpath(
        data = subset(text_path, r0 == n + 1),
        aes(x = x,
            y = y,
            label = label,
            group = interaction(label, r0)),
        size = 4,
        colour = "black",
        upright = TRUE,
        linetype = 0,
        fontface = "plain",
        family = "sans")
    
    ## Estaciones ----
    p <- p +
      geomtextpath::geom_textpath(
        data = subset(text_path, r0 == n + 2),
        aes(x = x,
            y = y,
            label = label,
            group = interaction(label, r0)),
        size = 4,
        colour = "black",
        upright = TRUE,
        linetype = 0,
        fontface = "bold",
        family = "sans"
      )
    
    ## Categorías ----
    p <- p +
      geomtextpath::geom_textpath(
        data = subset(text_path, r0 <= n),
        aes(x = x,
            y = y,
            label = label,
            group = interaction(label, r0, label)),
        size = 3,
        colour = "black",
        upright = TRUE,
        linetype = 0,
        fontface = "plain",
        family = "sans")
  }
  
  # Divisiones en colores estéticas ----
  ### para Meses ----
  p <- p +
    geom_segment(
      data = month_lines,
      aes(
        x = x,
        y = y,
        xend = xend,
        yend = yend,
        colour = col), # ok
      linewidth = 2
    )
  
  ### para Estaciones ----
  p <- p +
    geom_segment(
      data = seas_lines,
      aes(
        x = x,
        y = y,
        xend = xend,
        yend = yend),
      colour = "white",
      lineend = "square",
      linewidth = 1.5
    )
  
  # Configuraciones finales ----
  p +
    scale_fill_identity(guide = "none") +
    scale_color_identity(guide = "none") +
    coord_flip() +
    theme_void() +
    theme(
      aspect.ratio = 1
    )
}
