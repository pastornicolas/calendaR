# server.R

server <- function(input, output, session) {
  

# CATEGORÍAS DEL USUARIO ----
  categorias <- mod_categorias_server("categorias")
  
  
# DATOS ----
  tabla_categorias <- reactive({
    cats <- categorias()
    if (length(cats) == 0) {
      return(data.frame())
    }
    filas <- list()
    
    for (i in seq_along(cats)) {
      cat <- cats[[i]]
      for (j in seq_along(cat$subcategories)) {
        
        recurso <- cat$subcategories[[j]]
        
        filas[[length(filas) + 1]] <- data.frame(
          Categoria = paste("Categoría", i),
          Recurso = recurso$label,
          Inicio = recurso$start_date,
          Fin = recurso$end_date,
          stringsAsFactors = FALSE)
        }
    }
    do.call(rbind, filas)
  })
  

# TABLA ----
  output$tabla_categorias <- DT::renderDT({
    
    DT::datatable(
      tabla_categorias(),
      rownames = FALSE,
      options = list(
        pageLength = 8,
        scrollX = TRUE
      )
    )
  })


# CALENDARIO ----
  output$calendario <- renderPlot({
    crear_calendario_plot(
      categorias()
      )
    })

# Bajar PNG ----
  output$guardar_png <- downloadHandler(
    
    filename = function() {"calendario.png"},
    content = function(file) {
      png(file, width = 2000, height = 2000, res = 300)
      print(crear_calendario_plot(categorias()))
      dev.off()
    })
  
# Bajar PDF ----
  output$guardar_pdf <- downloadHandler(
    
    filename = function() {"calendario.pdf"},
    content = function(file) {
      pdf(file, width = 8, height = 8)
      print(crear_calendario_plot(categorias()))
      dev.off()
    })
  
# Bajar CSV ----
  output$guardar_csv <- downloadHandler(
    
    filename = function() {"datos_calendario.csv"},
    
    content = function(file) {
      write.csv(tabla_categorias(), file, row.names = FALSE)
      }
  )
  
}