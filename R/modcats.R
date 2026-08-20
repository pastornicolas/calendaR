# modcats.R

# Generación de selector p/ categorías (UI) ----
mod_categorias_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(
      ns("n_categories"),
      "Número de categorías:",
      choices = 1:9,
      selected = 1,
      width = "100%"
    ),
    hr(),
    div(
      style = "
        max-height: 75vh;
        overflow-y: auto;
        padding-right: 5px;
      ",
      uiOutput(ns("categories_ui"))
      )
    )
  }

# Generación de selector p/ categorías (server) ----
mod_categorias_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
  # Recurso por defecto
  nuevo_recurso <- function(j) {
    list(label = paste("Recurso", j),
         start_date = "01-01",
         end_date = "12-31")
  }
  # Categoria por defecto
  nueva_categoria <- function(i, n_recursos) {
    list(name = paste("Categoría", i),
         n_subcats = n_recursos,
         subcategories = lapply(seq_len(n_recursos),nuevo_recurso))
  }
  # Estados conservados (si el usuario cambia seleccion)
  estado <- reactiveValues(n_categories = 1,
                           categories = list(nueva_categoria(1,1))
                           )
  # Selector de fechas
  fecha_anual <- function(id, label, selected = "01-01") {
    
    meses <- c("Ene", "Feb", "Mar", "Abr","May", "Jun",
               "Jul", "Ago","Sep", "Oct", "Nov", "Dic")
    
    partes <- strsplit(selected, "-", fixed = TRUE)[[1]]
    mes_inicial <- partes[1]
    dia_inicial <- partes[2]
    
    tagList(tags$label(label),
            fluidRow(
              column(6,
                     selectInput(paste0(id, "_month"),
                                 NULL,
                                 choices = stats::setNames(
                                   sprintf("%02d", 1:12),
                                   meses),
                                 selected = mes_inicial,
                                 width = "100%")),
              column(6,
                     selectInput(paste0(id, "_day"),
                                 NULL,
                                 choices = sprintf("%02d", 1:31),
                                 selected = dia_inicial,
                                 width = "100%"))
            ))
    }

  # Número de categorías
  observeEvent(input$n_categories,
               {
                 n <- as.integer(input$n_categories)
                 current <- estado$categories
                 
                 if (n > length(current)) {
                   for (i in (length(current) + 1):n) {
                     current[[i]] <- nueva_categoria(i, 1)}
                   }
                 
                 estado$categories <- current
                 estado$n_categories <- n
                 }
               )
    
  ## Output Categorías ----
  output$categories_ui <- renderUI({
      n <- estado$n_categories
      ns <- session$ns
      tagList(
        lapply(seq_len(n), function(i) {
          categoria <- estado$categories[[i]]
          div(
          style = "border: 1px solid #ddd; border-radius: 5px;
          padding: 8px; margin-bottom: 8px;",
          tags$strong(paste("Categoría", i),
                      style = "font-size: 13px;"),
          selectInput(
            ns(paste0("n_subcats_", i)),
            "Número de recursos:",
            choices = 1:3,
            selected = categoria$n_subcats,
            width = "100%"),
          uiOutput(
            ns(paste0("subcats_ui_", i))
            )
          )
          })
        )
      })
    
    # Selector de Recursos
    observe({
      n <- estado$n_categories
      lapply(seq_len(n), function(i) {
        local({
          ii <- i
          output[[paste0("subcats_ui_", ii)]] <- renderUI({
            categoria <- estado$categories[[ii]]
            n_subcats <- categoria$n_subcats
            ns <- session$ns
            tagList(
              lapply(seq_len(n_subcats),
                     function(j) {
                       recurso <- categoria$subcategories[[j]]
                       div(style = "background: #f7f7f7;
                       padding: 6px;
                       margin-top: 5px;
                       border-radius: 4px;",
                           tags$strong(
                             paste("Recurso", j),
                             style = "
                             font-size: 12px;"),
                           textInput(
                             ns(paste0("subcat_label_",ii,"_",j)),
                             NULL,
                             value = recurso$label,
                             width = "100%"),
                           fluidRow(
                             column(6,fecha_anual(
                               ns(paste0("subcat_start_",ii,"_",j)),
                               "Inicio",
                               recurso$start_date)),
                             column(6,fecha_anual(
                               ns(paste0("subcat_end_",ii,"_",j)),
                               "Fin",
                               recurso$end_date))))}
                     )
              )
            })
          })
        })
      })
    
    # Mantener la interfaz
    observe({
      n <- estado$n_categories
      current <- estado$categories
      changed <- FALSE
      
      for (i in seq_len(n)) {
        
        # Recursos
        id_n <- paste0("n_subcats_", i)
        
        if (!is.null(input[[id_n]])) {
          new_n <- as.integer(input[[id_n]])
          
          if (current[[i]]$n_subcats != new_n) {
            old_n <- length(current[[i]]$subcategories)
            
            if (new_n > old_n) {
              current[[i]]$subcategories <- c(current[[i]]$subcategories,
                                              lapply(
                                                (old_n + 1):new_n,
                                                nuevo_recurso))
              } else if (new_n < old_n) {
                current[[i]]$subcategories <-
                  current[[i]]$subcategories[seq_len(new_n)]
                }
            
            current[[i]]$n_subcats <- new_n
            changed <- TRUE
          }
        }
        
        # Valores de recursos
        n_sub <- current[[i]]$n_subcats
        
        for (j in seq_len(n_sub)) {
          id_label <- paste0("subcat_label_",i,"_",j)
          id_start_month <- paste0("subcat_start_",i,"_",j,"_month")
          id_start_day <- paste0("subcat_start_",i,"_",j,"_day")
          id_end_month <- paste0("subcat_end_",i,"_",j,"_month")
          id_end_day <- paste0("subcat_end_",i,"_",j,"_day")
          
          if (!is.null(input[[id_label]])) {
            current[[i]]$subcategories[[j]]$label <- input[[id_label]]
            changed <- TRUE
            }
          
          if (!is.null(input[[id_start_month]]) &&
              !is.null(input[[id_start_day]])) {
            
            current[[i]]$subcategories[[j]]$start_date <- paste(input[[id_start_month]],
                                                                input[[id_start_day]],
                                                                sep = "-")
            changed <- TRUE
            }
          
          if (!is.null(input[[id_end_month]]) &&
              !is.null(input[[id_end_day]])) {
            
            current[[i]]$subcategories[[j]]$end_date <- paste(input[[id_end_month]],
                                                              input[[id_end_day]],
                                                              sep = "-")
            changed <- TRUE
          }
        }
        }
      
      if (changed) {
        estado$categories <- current
        }
      })
    
    # Devolver configuración
    reactive({
      estado$categories[seq_len(estado$n_categories)]
      })
  })
  }