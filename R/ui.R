# interfaz de usuario ----
## ui.R
# version 0.7.5

version <- "0.7.5"

ui <- fluidPage(
  tags$head(
    tags$title("CalendaR"),
    tags$style(HTML("
      .form-group {
        margin-bottom: 6px;
      }
      .control-label {
        font-size: 12px;
        margin-bottom: 2px;
      }
      .form-control {
        height: 30px;
        padding: 4px 8px;
        font-size: 12px;
      }
      .titlePanel {
        font-size: 14px;
        font-weight: bold;
      }
      .sidebarPanel {
        font-size: 12px;
      }
      ")),
    ),
  
  #titlePanel("CalendaR - version 0.7.5"),
  titlePanel(HTML(paste(
    "CalendaR <span style='font-size: 0.65em; color: #777; font-style: italic;'>version", version, "</span>"
  ))),
  
  div(
    style = "margin: -5px 0 25px 0; color: #444;",
    p(style = "font-size: 1.05em;",
      "Herramienta interactiva para graficar ciclos forrajeros (u otros datos que varían a lo largo del año)."),
    
    p(style = "font-size: 1.0em; margin-top: 12px; padding-left: 10px; border-left: 3px solid #2C3E50;",
      HTML("<strong>Cita:</strong> Pastor, N. (2026). <em>CalendaR</em>."))),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons(inputId = "hemisf",
                   label = "Elija un su posición",
                   choices = c("Hemisferio Sur" = "hsur",
                               "Hemisferio Norte" = "hnorte"),
                   selected = "hsur", inline = TRUE),
      mod_categorias_ui("categorias"),
      width = 3
      ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Calendario",
          plotOutput(
            "calendario",
            height = "800px"),
          hr(),
          downloadButton(
            "guardar_png",
            "Guardar PNG",
            target = '_blank'),
          downloadButton(
            "guardar_pdf",
            "Guardar PDF")),
        tabPanel(
          "Datos",
          DT::DTOutput(
            "tabla_categorias"),
          hr(),
          downloadButton(
            "guardar_csv",
            "Guardar CSV"),
          downloadButton(
            "guardar_excel",
            "Guardar Excel")
          ),
        tabPanel(
          "Ayuda",
          div(style = "max-width: 900px; font-weight: normal; text-align: justify;",
              h4("¿Qué es CalendaR?"),
              p("CalendaR es una herramienta interactiva para graficar ciclos forrajeros ",
                "y otros datos que varían a lo largo del año. Actualmente permite graficar",
                "hasta 9 categorías diferentes cada una con hasta 3 subcategorías (recursos).",
                "Por el momento, el calendario esta fijado al hemisferio Sur, pero espero",
                "poder incluir la posibilidad de usar estaciones del hemisferio Norte."),
              
              h4("¿Cómo usar la aplicación?"),
              tags$ol(
                tags$li("Seleccionar el número de categorías (1 a 9)."),
              tags$li("Para cada categoría, elegir el número de recursos (1 a 3)."),
              tags$li("Completar los campos para cada recurso:",
                      tags$ul(
                        tags$li("Etiqueta del recurso"),
                        tags$li("Fecha de inicio"),
                        tags$li("Fecha de fin")
                        )),
              tags$li("Una vez completo, puede descargar el gráfico como un archivo PNG",
                      "y/o PDF. También puede desvar un CSV con los datos en la pestaña 'Datos'.")
              ),
              p("La aplicación es reactiva, es decir, se actualiza a medida que el usuario",
                "ingresa los datos. Por ello, se recomienda cargar los datos de a poco y",
                "esperar la actualización."),
              p(HTML("<u>Cuidado al elegir las fechas</u>!"),
                "Por el momento, la aplicación no detecta automáticamente las fechas inexistentes",
                "(por ejemplo, Feb-30). En estos casos puede aparecer el mensaje de error",
                HTML("<code>'from' must be a finite number</code>"),
                "y el gráfico puede desaparecer momentáneamente. Para solucionarlo, simplemente",
                "corrija la fecha ingresada."),
              
              h4("¿Cómo citar?"),
              p("Si utiliza esta aplicación, por favor incluya la cita:", tags$br(), "Pastor, N. (2026)",
                HTML("<i>CalendaR</i>. Disponible en: <a href='https://pastornicolas.github.io/calendaR'
                     target='_blank'>https://pastornicolas.github.io/CalendaR</a>")),
              hr(),
              h4("Contacto"),
              p("Si encuentra un error o tiene alguna sugerencia para mejorar la aplicación,",
                "puede comunicarse con el autor:", tags$a(href = "mailto:npastor@unc.edu.ar",
                                                           "npastor@unc.edu.ar")),
              ), width = 9
          )
        )
      )
    ),
  tags$script(HTML("
    $(document).ready(function() {$('#hemisf input[value=\"hnorte\"]')
    .prop('disabled', true)
    .parent()
    .css('color', '#999');
    });
                     "
                   )
              )
  )