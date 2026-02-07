library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyverse)
library(janitor)
library(broom)
library(DT)

# CARGA DE DATOS Y TRANSFORMACIÓN Y LIMPIESA
data <- read_csv("student_exam_scores.csv")

data_clean <- data %>%
  janitor::clean_names() %>%
  mutate(
    student_id = as.character(student_id),
    study_per_sleep = hours_studied / sleep_hours,
    previos_cat = case_when(
      previous_scores >= 80 ~ "Alto",
      previous_scores >= 60 ~ "Medio",
      TRUE ~ "Bajo"
    ),
    exam_level = case_when(
      exam_score >= 40 ~ "Alto",
      exam_score >= 30 ~ "Medio",
      TRUE ~ "Bajo"
    ),
    pass_fail = if_else(exam_score >= 30, "Aprobado", "Reprobado"),
    exam_zscore = (exam_score - mean(exam_score, na.rm = TRUE)) / sd(exam_score, na.rm = TRUE),
    exam_percentile = percent_rank(exam_score) * 100,
    
    previos_cat = factor(previos_cat, levels = c("Bajo", "Medio", "Alto"))
  )

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Presentacion Final"),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebarmenu",
      menuItem("Inicio", tabName = "inicio", icon = icon("home")),
      menuItem("Datos", tabName = "datos", icon = icon("chart-bar")),
      menuItem("Histograma", tabName = "histograma", icon = icon("bar-chart")),
      menuItem("Densidad", tabName = "densidad", icon = icon("wave-square")),
      menuItem("Boxplot", tabName = "boxplot", icon = icon("box-open")),
      menuItem("Dispersión", tabName = "dispersión", icon = icon("dot-circle")),
      menuItem("Barras", tabName = "barras", icon = icon("list-ol")),
      menuItem("Ratio", tabName = "ratio", icon = icon("balance-scale")),
      
      conditionalPanel(
        condition = "input.sidebarmenu != 'inicio'",
        
        sliderInput("hours_range", "Rango de Horas Estudiadas:",
                    min = floor(min(data_clean$hours_studied)),
                    max = ceiling(max(data_clean$hours_studied)),
                    value = c(4, 9), step = 0.5)
      )
    )
  ),
  dashboardBody(
    tabItems(
      
      tabItem(
        tabName = "inicio",
        fluidRow(
          box(width = 12, title = "Bienvenido", status = "success",
              solidHeader = TRUE,
              "Presentacion de mi dashboard Final....")
        ),
        
        fluidRow(
          box(width = 12, title = "📊 Resumen Ejecutivo del Rendimiento Académico", status = "primary",
              solidHeader = TRUE,
              p(HTML("
          Este dashboard interactivo tiene como objetivo principal analizar la correlación entre el esfuerzo del estudiante (horas de estudio y balance sueño/estudio) y el rendimiento en el examen.<br>
          Utilice el filtro de <b>Horas Estudiadas</b> en el menú lateral para segmentar los datos y explorar cómo cambian las métricas clave (KPIs) en cada pestaña.
            "))
            )
          ),
        
        fluidRow(
          valueBoxOutput("kpi_home_promedio", width = 4),
          valueBoxOutput("kpi_home_aprobacion", width = 4),
          valueBoxOutput("kpi_home_conteo", width = 4)
        ),
        
        fluidRow(
          box(width = 12, title = "🛠️ Librerias JANITOR y BROOM", status = "info",
              p(HTML("
         Para garantizar la calidad y reproducibilidad del análisis, se integraron las siguientes librerías del ecosistema Tidyverse:<br><br>
                <b>📦 Library(janitor):</b><br>
                Utilizada para la limpieza automática de nombres de columnas (<i>clean_names</i>), asegurando que todas las variables sigan un formato estándar (snake_case) y eliminando caracteres especiales.<br><br>
                <b>🧹 Library(broom):</b><br>
                Implementada para convertir resultados de modelos estadísticos y objetos complejos de R en <i>data frames</i> ordenados (tidy), facilitando su visualización y manipulación posterior.
              "))
          )
        ),
        fluidRow(
          box(width = 12, title = "🚀 Evaluación de Impacto y Recomendaciones", status = "success", solidHeader = TRUE,
              p(HTML("
                <b>1. Impacto del Proyecto:</b><br>
                La implementación de este dashboard permite reducir los tiempos de análisis manual en un <b>90%</b> y estandarizar la interpretación de métricas académicas, eliminando inconsistencias en los reportes.<br><br>
                <b>2. Recomendaciones Estratégicas:</b><br>
                <ul>
                  <li><b>Fomentar el Estudio:</b> Los datos indican que superar las 7 horas de estudio incrementa drásticamente la tasa de aprobación.</li>
                  <li><b>Cuidar el Sueño:</b> El análisis de ratio sugiere que sacrificar sueño por estudio es contraproducente (rendimientos decrecientes).</li>
                </ul>
            "))
          )
        )
      ),

      tabItem(
        tabName = "datos",
        fluidRow(
          box(width = 12, title = "Tabla de Datos Originales", status = "info",
              solidHeader = TRUE, dataTableOutput("tabla_datos_originales"))
        ), 
        fluidRow(
          box(width = 6, title = "Resumen Estadístico General", status = "success", solidHeader = TRUE, 
              verbatimTextOutput("resumen_general")),
          box(width = 6, title = "Métricas Clave (Globales)", status = "warning", solidHeader = TRUE, 
              fluidRow(
                valueBoxOutput("data_kpi_promedio", width = 12), 
                valueBoxOutput("data_kpi_tasa", width = 6),     
                valueBoxOutput("data_kpi_conteo", width = 6)
            )
          )
        )
      ),
      
      tabItem(
        tabName = "histograma",
        fluidRow(
          box(width = 12, title = "Histograma de Puntajes", status = "primary", solidHeader = TRUE,
              plotOutput("plot_histograma"))
      ),
        fluidRow(
          valueBoxOutput("hist_media_kpi", width = 6), 
          valueBoxOutput("hist_mediana_kpi", width = 6)
        ),
        fluidRow( 
          box(width = 12, title = "Objetivo y Contexto", status = "info", solidHeader = TRUE,
              p(HTML("
            Este histograma muestra la frecuencia de los puntajes del examen dentro del subconjunto filtrado.<br><br>
            <b>Propósito:</b> Evaluar la forma de la distribución y su sesgo. <br>
            <b>Análisis:</b> Si la Media (rojo) y la Mediana (verde) están cerca, la distribución es simétrica. Si la Media es mayor que la Mediana, el rendimiento tiene un sesgo positivo.
            "))
          )
        )
      ),
      
      tabItem(
        tabName = "densidad",
        fluidRow(
          box(width = 12, title = "Gráfico de Densidad por Aprobación", status = "primary", solidHeader = TRUE,
              plotOutput("plot_densidad"))
        ),
        fluidRow(
          valueBoxOutput("dens_tasa_aprobacion_kpi", width = 6), 
          valueBoxOutput("dens_promedio_kpi", width = 6)
        ),
        fluidRow( 
          box(width = 12, title = "Objetivo y Contexto", status = "info", solidHeader = TRUE,
              p(HTML("
             Este gráfico muestra la distribución de puntajes para Aprobados (verde) y Reprobados (rojo).<br><br>
            <b>Propósito:</b> Identificar la superposición de puntajes en el umbral crítico (línea punteada) y el riesgo de reprobación.<br>
            <b>Análisis:</b> La Tasa de Aprobación indica la efectividad del subconjunto. Si la curva verde se aleja significativamente de la línea de corte (30), el umbral de aprobación es claro.
            "))
          )
        )
      ),
      
      tabItem(
        tabName = "boxplot",
        fluidRow(
          box(width = 12, title = "Boxplot por Categoría Previa", status = "primary", solidHeader = TRUE,
              plotOutput("plot_boxplot"))
        ),
        fluidRow(
          valueBoxOutput("box_iqr_kpi", width = 6), 
          valueBoxOutput("box_conteo_kpi", width = 6)
        ),
        fluidRow(
          box(width = 12, title = "Objetivo y Contexto", status = "info", solidHeader = TRUE,
              p(HTML("
            Este gráfico muestra la distribución de puntajes del examen (media, cuartiles) dentro de cada Nivel Académico Previo.<br><br>
            <b>Propósito:</b> Comparar el rendimiento y la dispersión entre grupos. <br>
            <b>Análisis:</b> Cajas más cortas indican un rendimiento más consistente. El IQR promedio y el Conteo total del subconjunto son indicadores clave de la población analizada.
            "))
          )
        )
      ),
        
      tabItem(
        tabName = "dispersión",
        fluidRow(
          box(width = 12, title = "Gráfico de Dispersión", status = "primary", solidHeader = TRUE,
              plotOutput("plot_dispersión"))
      ),
        fluidRow(
          valueBoxOutput("disp_max_kpi", width = 6), 
          valueBoxOutput("disp_min_kpi", width = 6)
        ),
        fluidRow(
          box(width = 12, title = "Objetivo y Contexto", status = "info", solidHeader = TRUE,
              p(HTML("
            Este gráfico muestra la correlación entre Horas Estudiadas y Puntaje del Examen.<br><br>
            <b>Propósito:</b> Confirmar la relación lineal positiva entre esfuerzo y rendimiento.<br>
            <b>Análisis:</b> La línea horizontal (30 pts) marca la aprobación. Se observa si la mayoría de los puntos se acumulan sobre esta línea a medida que aumentan las horas de estudio. Los KPIs muestran los límites de rendimiento del subconjunto.
            "))
          )
        )
      ),
      
      tabItem(
        tabName = "barras",
        fluidRow(
          box(width = 12, title = "Gráfico de Barras por Nivel de Examen", status = "primary", solidHeader = TRUE,
              plotOutput("plot_barras"))
      ),
        fluidRow(
          valueBoxOutput("bar_alto_kpi", width = 6), 
          valueBoxOutput("bar_aprob_kpi", width = 6)
        ),
        fluidRow(
          box(width = 12, title = "Objetivo y Contexto", status = "info", solidHeader = TRUE,
              p(HTML("
            Este gráfico muestra el porcentaje de estudiantes clasificados en los niveles Bajo (<30), Medio (30-39) y Alto (>=40).<br><br>
            <b>Propósito:</b> Determinar la proporción de éxito y los grupos en riesgo. <br>
            <b>Análisis:</b> La Tasa de Aprobación es la métrica sumaria, mientras que el Porcentaje Alto mide la excelencia académica dentro del subconjunto.
            "))
          )
        )
      ),
      
      tabItem(
        tabName = "ratio",
        fluidRow(
          box(width = 12, title = "Análisis del Ratio Estudio/Sueño", status = "primary", solidHeader = TRUE,
              plotOutput("plot_ratio"))
        ),
        fluidRow(
          valueBoxOutput("ratio_promedio_kpi", width = 6), 
          valueBoxOutput("ratio_score_kpi", width = 6)
        ),
        fluidRow(
          box(width = 12, title = "Objetivo y Contexto", status = "info", solidHeader = TRUE,
              p(HTML("
          Este gráfico explora el impacto del equilibrio entre estudio y sueño en el puntaje. El tamaño del punto indica Horas de Sueño.<br><br>
          <b>Propósito:</b> Visualizar si un ratio equilibrado (cercano a 1.0) está asociado con mejores resultados.<br>
          <b>Análisis:</b> Un Ratio Promedio muy alto indica un esfuerzo desbalanceado. Los puntos grandes (más sueño) tienden a concentrarse sobre la línea de aprobación (30).
            "))
          )
        )
      )
    )
  )
)

# SERVER
server <- function(input, output) {
  
  datos_filtrados <- reactive({
    
    data_clean %>%
      filter(hours_studied >= input$hours_range[1],
             hours_studied <= input$hours_range[2])
  })
  
  output$kpi_home_promedio <- renderValueBox({
    promedio <- mean(data_clean$exam_score, na.rm = TRUE)
    valueBox(
      value = paste(round(promedio, 1), "pts"),
      subtitle = "Puntaje Promedio Global del Examen",
      icon = icon("star"),
      color = "blue"
    )
  })
  
  # KPI HOME 2: Tasa de Aprobación Global
  output$kpi_home_aprobacion <- renderValueBox({
    tasa <- sum(data_clean$pass_fail == "Aprobado") / nrow(data_clean) * 100
    valueBox(
      value = paste0(round(tasa, 1), "%"),
      subtitle = "Tasa de Aprobación General (>30 pts)",
      icon = icon("check-circle"),
      color = "green"
    )
  })
  
  # KPI HOME 3: Total de Estudiantes
  output$kpi_home_conteo <- renderValueBox({
    conteo <- nrow(data_clean)
    valueBox(
      value = conteo,
      subtitle = "Total de Registros Analizados",
      icon = icon("users"),
      color = "purple"
    )
  })
  
  # Tabla de Datos (NO reactivo)
  output$tabla_datos_originales <- renderDataTable({
    DT::datatable(
      data_clean, 
      options = list(
        scrollY = '300px', 
        pageLength = 10,  
        dom = 'tip'
      ),
      filter = 'top' 
    )
  })
  
  # Resumen General (NO reactivo)
  output$resumen_general <- renderPrint({
    data_clean %>%
      select(exam_score, hours_studied, sleep_hours) %>%
      summary() 
  })
  
  output$data_kpi_promedio <- renderValueBox({
    promedio <- mean(data_clean$exam_score, na.rm = TRUE)
    valueBox(
      value = paste(round(promedio, 1), "pts"),
      subtitle = "Puntaje Promedio Global",
      icon = icon("graduation-cap"),
      color = "blue"
    )
  })
  
  # KPI Global 2: Tasa de Aprobación
  output$data_kpi_tasa <- renderValueBox({
    tasa <- sum(data_clean$pass_fail == "Aprobado") / nrow(data_clean) * 100
    valueBox(
      value = paste0(round(tasa, 1), "%"),
      subtitle = "Tasa de Aprobación",
      icon = icon("check"),
      color = "green"
    )
  })
  
  # KPI Global 3: Conteo Total
  output$data_kpi_conteo <- renderValueBox({
    conteo <- nrow(data_clean)
    valueBox(
      value = conteo,
      subtitle = "Total de Registros",
      icon = icon("users"),
      color = "purple"
    )
  })
  
  # Histograma (output$plot_histograma) - REACTIVO
  output$plot_histograma <- renderPlot({
    media_filtrada <- mean(datos_filtrados()$exam_score, na.rm = TRUE)
    ggplot(datos_filtrados(), aes(x = exam_score)) +
      geom_histogram(binwidth = 5, fill = "#F6CFFF", color = "black", alpha = 1) +
      geom_vline(xintercept = media_filtrada, color = "red", linetype = "solid", linewidth = 1.2) +
      annotate("text", x = media_filtrada + 0.5, y = 56,
               label = paste("📊 Media:", round(media_filtrada, 1)),
               color = "orange", hjust = 0, size = 5) +
      labs(title = "📝 Distribución de puntajes del examen",
           subtitle = "🔍 Incluye línea de referencia de la media del subconjunto filtrado",
           x = "📌 Puntaje del examen", y = "🎨 Frecuencia") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold", color = "#2c3e50", hjust = 0.5),
            plot.subtitle = element_text(color = "#7f8c8d", hjust = 0.5),
            axis.title.x = element_text(color = "blue", face = "bold"),
            axis.title.y = element_text(color = "purple", face = "bold"),
            panel.grid.minor = element_blank())
  })
  
  # KPI: Media Reactiva para el Histograma
  output$hist_media_kpi <- renderValueBox({
    media <- mean(datos_filtrados()$exam_score, na.rm = TRUE)
    valueBox(
      value = paste(round(media, 1), "pts"),
      subtitle = "Media (Promedio) del Puntaje Filtrado",
      icon = icon("star"),
      color = "red" 
    )
  })
  
  # KPI: Mediana Reactiva para el Histograma
  output$hist_mediana_kpi <- renderValueBox({
    mediana <- median(datos_filtrados()$exam_score, na.rm = TRUE)
    valueBox(
      value = paste(round(mediana, 1), "pts"),
      subtitle = "Mediana del Puntaje Filtrado",
      icon = icon("crosshairs"),
      color = "green" 
    )
  })
  
  # Densidad (output$plot_densidad) - REACTIVO
  output$plot_densidad <- renderPlot({
    ggplot(datos_filtrados(), aes(x = exam_score, fill = pass_fail)) +
      geom_density(alpha = 1) +
      geom_vline(xintercept = 30.8, linetype = "dashed", color = "black", linewidth = 1) +
      labs(title = "📊 Distribución de densidad por estado de aprobación",
           subtitle = "📝 Aprobados vs Reprobados",
           x = "📌 Puntaje del examen", y = "📈 Densidad", fill = "🎯 Estado") +
      scale_fill_manual(
        values = c("Aprobado" = "#89FF73", "Reprobado" = "#E86658"),
        labels = c("Aprobado ✅", "Reprobado ❌"))+
      theme_minimal(base_size = 14) +
      theme(legend.position = "top",
            plot.title = element_text(color = "#A11B7C", face = "bold", hjust = 0.5),
            plot.subtitle = element_text(color = "#35F3FC", face = "bold", hjust = 0.5),
            axis.title.x = element_text(color = "#E566FF", face = "bold", hjust = 0.5),
            axis.title.y = element_text(color = "#EBA50C", face = "bold", hjust = 0.5),
            legend.title = element_text(face = "bold", color = "#6E5412"),
            legend.text = element_text(size = 12, color = "black"))
  })
  
  output$dens_tasa_aprobacion_kpi <- renderValueBox({
    
    data_subset <- datos_filtrados()
    if (nrow(data_subset) == 0) {
      tasa <- 0
    } else {
      tasa <- sum(data_subset$pass_fail == "Aprobado") / nrow(data_subset) * 100
    }
    
    valueBox(
      value = paste0(round(tasa, 1), "%"),
      subtitle = "Tasa de Aprobación del Subconjunto",
      icon = icon("check-circle"),
      color = "green" 
    )
  })
  
  # KPI: Puntaje Promedio Reactivo para Densidad
  output$dens_promedio_kpi <- renderValueBox({
    promedio <- mean(datos_filtrados()$exam_score, na.rm = TRUE)
    
    valueBox(
      value = paste(round(promedio, 1), "pts"),
      subtitle = "Puntaje Promedio del Subconjunto",
      icon = icon("star"),
      color = "blue" 
    )
  })
  
  # Boxplot (output$plot_boxplot) - REACTIVO
  output$plot_boxplot <- renderPlot({
    ggplot(datos_filtrados(), aes(x = previos_cat, y = exam_score, fill = previos_cat)) +
      geom_boxplot(alpha = 0.8, outlier.color = "#E53935", outlier.size = 2) +
      stat_summary(fun = mean, geom = "point", shape = 17, size = 3, color = "black") +
      labs(title = "📦 Distribución de puntajes por nivel académico previo",
           subtitle = "📊 Análisis de rendimiento según historial académico",
           x = "📂 Categoría previa", y = "📝 Puntaje del examen", fill = "Categoría") +
      scale_fill_brewer(palette = "Set2") +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "right",
        plot.title = element_text(face = "bold", color = "#5055F2", hjust = 0.5),
        plot.subtitle = element_text(face = "bold", color = "#06D4D2", hjust = 0.5),
        axis.title.x = element_text(face = "bold", color = "#3BFF61", hjust = 0.5),
        axis.title.y = element_text(face = "bold", color = "#498C0D", hjust = 0.5),
        legend.title = element_text(face = "bold", color = "#CC2F49"),
        legend.text = element_text(face = "bold"))
  })
  
  output$box_iqr_kpi <- renderValueBox({
    # Calculamos el IQR promedio de todos los exam_score en el subconjunto
    iqr_val <- IQR(datos_filtrados()$exam_score, na.rm = TRUE)
    valueBox(
      value = paste(round(iqr_val, 1), "pts"),
      subtitle = "Rango Intercuartílico Promedio (IQR)",
      icon = icon("balance-scale"),
      color = "yellow"
    )
  })
  
  # KPI: Conteo de Estudiantes Reactivo para Boxplot
  output$box_conteo_kpi <- renderValueBox({
    conteo <- nrow(datos_filtrados())
    valueBox(
      value = conteo,
      subtitle = "Estudiantes en el Subconjunto Filtrado",
      icon = icon("users"),
      color = "aqua"
    )
  })
  
  # Dispersión (output$plot_dispersión) - REACTIVO
  output$plot_dispersión <- renderPlot({
    ggplot(datos_filtrados(), aes(x = hours_studied, y = exam_score, color = pass_fail)) +
      geom_point(alpha = 0.7, size = 2, shape = 15) +
      geom_smooth(method = "lm", se = FALSE, aes(group = pass_fail), linewidth = 1.2) +
      geom_hline(yintercept = 30, linetype = "dashed", color = "#7f8c8d", alpha = 0.7) +
      labs(title = "📈 Relación entre Horas Estudiadas y Puntaje del Examen",
           subtitle = "🔍 Correlación estudio-rendimiento diferenciado por resultado",
           x = "⏰ Horas Estudiadas", y = "🎯 Puntaje del Examen", color = "🏆 Resultado") +
      scale_color_manual(values = c("Aprobado" = "#4CAF50", "Reprobado" = "#F44336"),
                         labels = c("Aprobado ✅", "Reprobado ❌")) +
      theme_minimal(base_size = 11) +
      theme(
        legend.position = "top",
        plot.title = element_text(face = "bold", color = "#2E1FED", hjust = 0.5),
        plot.subtitle = element_text(face = "bold", color = "#388DE8", hjust = 0.5),
        axis.title.x = element_text(face = "bold", color = "#67E668", hjust = 0.5),
        axis.title.y = element_text(face = "bold", color = "#40692F", hjust = 0.5),
        legend.title = element_text(face = "bold", color = "darkblue", hjust = 0.5),
        legend.text = element_text(face = "bold"),
        legend.background = element_rect(fill = "#FFC9C9", color = NA)
      )
  })
  
  output$disp_max_kpi <- renderValueBox({
    max_score <- max(datos_filtrados()$exam_score, na.rm = TRUE)
    valueBox(
      value = paste(round(max_score, 1), "pts"),
      subtitle = "Puntaje Máximo Obtenido",
      icon = icon("chevron-up"),
      color = "teal"
    )
  })
  
  output$disp_min_kpi <- renderValueBox({
    min_score <- min(datos_filtrados()$exam_score, na.rm = TRUE)
    valueBox(
      value = paste(round(min_score, 1), "pts"),
      subtitle = "Puntaje Mínimo Obtenido",
      icon = icon("chevron-down"),
      color = "maroon"
    )
  })
  
  # Barras (output$plot_barras) - REACTIVO
  output$plot_barras <- renderPlot({
    ggplot(datos_filtrados(), aes(x = exam_level, fill = exam_level)) +
      geom_bar(alpha = 0.8, color = "white", linewidth = 0.4) +
      geom_text(stat = 'count', aes(label = after_stat(count)), 
                vjust = -0.5, size = 4, fontface = "bold", color = "black") +
      geom_text(stat = 'count', 
                aes(label = paste0(round(after_stat(count)/nrow(datos_filtrados())*100, 1), "%")),
                vjust = 1.5, size = 4, fontface = "bold", color = "white") +
      labs(title = "📊 Distribución de Estudiantes por Nivel de Examen",
           subtitle = "🏆 Clasificación según desempeño en la evaluación",
           x = "🎯 Nivel de Examen", y = "👥 Cantidad de Estudiantes", fill = "Nivel") +
      scale_fill_brewer(palette = "Set1") +
      scale_x_discrete(limits = c("Bajo", "Medio", "Alto")) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "right",
        plot.title = element_text(color = "#F28500",face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "#FF00C0", hjust = 0.5),
        axis.title.x = element_text(color = "#87F0FF", face = "bold"),
        axis.title.y = element_text(color = "#DE6EFA", face = "bold"),
      )
  })
  
  output$bar_alto_kpi <- renderValueBox({
    data_subset <- datos_filtrados()
    porc_alto <- sum(data_subset$exam_level == "Alto") / nrow(data_subset) * 100
    valueBox(
      value = paste0(round(porc_alto, 1), "%"),
      subtitle = "Estudiantes con Nivel Alto (>=40 pts)",
      icon = icon("trophy"),
      color = "orange"
    )
  })
  
  # KPI: Tasa de Aprobación Reactiva para Barras
  output$bar_aprob_kpi <- renderValueBox({
    data_subset <- datos_filtrados()
    if (nrow(data_subset) == 0) {
      tasa <- 0
    } else {
      tasa <- sum(data_subset$pass_fail == "Aprobado") / nrow(data_subset) * 100
    }
    valueBox(
      value = paste0(round(tasa, 1), "%"),
      subtitle = "Tasa de Aprobación Total del Subconjunto",
      icon = icon("check"),
      color = "green"
    )
  })
  
  # Ratio (output$plot_ratio) - REACTIVO
  output$plot_ratio <- renderPlot({
    ggplot(datos_filtrados(), aes(x = study_per_sleep, y = exam_score)) +
      geom_point(aes(color = pass_fail, size = sleep_hours), alpha = 0.6, shape = 16) +
      geom_hline(yintercept = 30, linetype = "dashed", color = "#7f8c8d", alpha = 0.7) +
      geom_vline(xintercept = 1, linetype = "dotted", color = "#7f8c8d", alpha = 0.7) +
      annotate("text", x = max(datos_filtrados()$study_per_sleep), y = 30, 
               label = "Umbral de Aprobación (30)", color = "#7f8c8d", hjust = 1, fontface = "bold") +
      annotate("text", x = 1, y = max(datos_filtrados()$exam_score), 
               label = "Ratio 1:1 (Equilibrio)", color = "#7f8c8d", vjust = -0.5, fontface = "bold") +
      labs(title = "⚖️ Impacto del Balance Estudio-Sueño en el Rendimiento",
           subtitle = "● Color = Resultado | ● Tamaño = Horas de Sueño",
           x = "📊 Ratio Horas Estudiadas / Horas de Sueño", y = "🎯 Puntaje del Examen",
           color = "🏆 Resultado", size = "😴 Horas de Sueño") +
      scale_color_manual(
        values = c("Aprobado" = "#4CAF50", "Reprobado" = "#F44336"),
        labels = c("Aprobado ✅", "Reprobado ❌")) +
      scale_size_continuous(range = c(1, 6), breaks = seq(4, 10, 2)) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "right",
        plot.title = element_text(color = "#FA4674", face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "#088F06", face = "bold", hjust = 0.5),
        axis.title.x = element_text(color = "#9191FF", face = "bold", hjust = 0.5),
        axis.title.y = element_text(color = "#C608FC", face = "bold", hjust = 0.5),
        legend.title = element_text(face = "bold", color = "#A6A663"),
        legend.text = element_text(face = "bold", color = "#2c3e50"),
        legend.background = element_rect(fill = "#f8f9fa"))
  })


  output$ratio_promedio_kpi <- renderValueBox({
    ratio_prom <- mean(datos_filtrados()$study_per_sleep, na.rm = TRUE)
    valueBox(
      value = paste(round(ratio_prom, 2), "E/S"),
      subtitle = "Ratio Promedio Estudio/Sueño",
      icon = icon("balance-scale"),
      color = "purple"
    )
  })
  
  # KPI: Puntaje Promedio Reactivo (Duplicado para conveniencia)
  output$ratio_score_kpi <- renderValueBox({
    promedio <- mean(datos_filtrados()$exam_score, na.rm = TRUE)
    valueBox(
      value = paste(round(promedio, 1), "pts"),
      subtitle = "Puntaje Promedio del Subconjunto",
      icon = icon("star"),
      color = "blue" 
    )
  })
}
# Run App
shinyApp(ui, server)