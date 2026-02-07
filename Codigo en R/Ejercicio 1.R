#Instalacion de librerias para el trabajo final
install.packages("tidyverse")
install.packages("ggplot2")

#cargar librerias
library(tidyverse)
library(ggplot2)

#----1.- Importar datos descargados en formato csv

data <- read_csv("C:/Users/carlo/Desktop/4to CICLO SENATI/Lenguaje de programacion para Ciencia de Datos/Entregable/Codigo en R/student_exam_scores.csv")

#----2.- Limpieza basica y estandarizacion
install.packages("janitor")
install.packages("broom")
install.packages("DT")


library(janitor)
library(broom)

data <- data %>%
  janitor::clean_names() %>% 
  mutate(student_id = as.character(student_id))

#---3.- Exploracion inicial y estadistica descriptiva

glimpse(data)
summary(data)
head(data)

#---4.- Transformacion de los datos a traves de variables nuevas

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
    exam_percentile = percent_rank(exam_score) * 100
  )

write_csv(data_clean, "C:/Users/carlo/Desktop/4to CICLO SENATI/Lenguaje de programacion para Ciencia de Datos/Entregable/Codigo en R/data_clean.csv")

#--5.- Graficos de ggplot2

output_dir <- "C:/Users/carlo/Desktop/4to CICLO SENATI/Lenguaje de programacion para Ciencia de Datos/Entregable/Graficos del ggplot 2"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

#--5.1- Histograma

g1 <- ggplot(data_clean, aes(x = exam_score)) +
  geom_histogram(binwidth = 5, fill = "#56B4E9", color = "white") +
  labs(title = "Distribución de puntajes del examen",
       x = "Puntaje del examen",
       y = "Frecuencia") +
  theme_minimal(base_size = 14)

#--5.2- Densidad por categoría de aprobación

g2 <- ggplot(data_clean, aes(x = exam_score, fill = pass_fail)) +
  geom_density(alpha = 0.6) +
  labs(title = "Distribución de Densidad por Estado de Aprobación",
       subtitle = "Comparación entre estudiantes aprobados y reprobados",
       x = "Puntaje del Examen", 
       y = "Densidad",
       fill = "Estado") +
  scale_fill_manual(values = c("Aprobado" = "#4CAF50", "Reprobado" = "#F44336")) +
  theme_minimal()

#--5.3- Boxplot

g3 <- ggplot(data_clean, aes(x = previos_cat, y = exam_score, fill = previos_cat)) +
  geom_boxplot(alpha = 0.8) +
  labs(title = "Distribución de Puntajes por Nivel Académico Previo",
       subtitle = "Análisis de rendimiento según historial académico",
       x = "Categoría de Puntaje Previo", 
       y = "Puntaje del Examen",
       fill = "Categoría") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()

#--5.4- Gráfico de dispersión 

g4 <- ggplot(data_clean, aes(x = hours_studied, y = exam_score, color = pass_fail)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = FALSE, aes(group = pass_fail)) +
  labs(title = "Relación entre Horas Estudiadas y Puntaje del Examen",
       subtitle = "Correlación estudio-rendimiento diferenciado por resultado",
       x = "Horas Estudiadas", 
       y = "Puntaje del Examen",
       color = "Resultado") +
  scale_color_manual(values = c("Aprobado" = "#4CAF50", "Reprobado" = "#F44336")) +
  theme_minimal()

#--5.5- Gráfico de barras 

g5<- ggplot(data_clean, aes(x = exam_level, fill = exam_level)) +
  geom_bar(alpha = 0.8) +
  geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5, size = 4) +
  labs(title = "Distribución de Estudiantes por Nivel de Examen",
       subtitle = "Clasificación según desempeño en la evaluación",
       x = "Nivel de Examen", 
       y = "Cantidad de Estudiantes",
       fill = "Nivel de Examen") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal()

#--5.6- Análisis del ratio estudio/sueño

g6 <- ggplot(data_clean, aes(x = study_per_sleep, y = exam_score)) +
  geom_point(aes(color = pass_fail, size = sleep_hours), alpha = 0.6) +
  labs(title = "Impacto del Balance Estudio-Sueño en el Rendimiento",
       subtitle = "Tamaño del punto representa horas de sueño",
       x = "Ratio Horas Estudiadas / Horas de Sueño", 
       y = "Puntaje del Examen",
       color = "Resultado",
       size = "Horas de Sueño") +
  scale_color_manual(values = c("Aprobado" = "#4CAF50", "Reprobado" = "#F44336")) +
  theme_minimal()


ggsave(file.path(output_dir, "5.1_Histograma_Puntajes.png"), plot = g1, width = 8, height = 5, dpi = 300)
ggsave(file.path(output_dir, "5.2_Densidad_Aprobacion.png"), plot = g2, width = 8, height = 5, dpi = 300)
ggsave(file.path(output_dir, "5.3_Boxplot_NivelPrevio.png"), plot = g3, width = 8, height = 5, dpi = 300)
ggsave(file.path(output_dir, "5.4_Dispersion_Estudio_vs_Puntaje.png"), plot = g4, width = 8, height = 5, dpi = 300)
ggsave(file.path(output_dir, "5.5_Barras_NivelExamen.png"), plot = g5, width = 8, height = 5, dpi = 300)
ggsave(file.path(output_dir, "5.6_Ratio_Estudio_Sueno.png"), plot = g6, width = 8, height = 5, dpi = 300)

#--6.- Personalización de Visualizaciones

#--6.1- Histograma

ggplot(data_clean, aes(x = exam_score)) +
  geom_histogram(binwidth = 5, fill = "#F6CFFF", color = "black", alpha = 1) +
  geom_vline(aes(xintercept = mean(exam_score)), color = "red", linetype = "solid", linewidth = 1.2) +
  annotate("text", x = mean(data_clean$exam_score) + 0.5, y = 56,
           label = paste("📊 Media:", round(mean(data_clean$exam_score), 1)),
           color = "orange", hjust = 0, size = 5) +
  labs(title = "📝 Distribución de puntajes del examen",
       subtitle = "🔍 Incluye línea de referencia de la media general",
       x = "📌 Puntaje del examen", y = "🎨 Frecuencia") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", color = "#2c3e50", hjust = 0.4),
        plot.subtitle = element_text(color = "#7f8c8d", hjust = 0.4),
        axis.title.x = element_text(color = "blue", face = "bold"),
        axis.title.y = element_text(color = "purple", face = "bold"),
        panel.grid.minor = element_blank()
        )

#--6.2- Densidad por categoría de aprobación

ggplot(data_clean, aes(x = exam_score, fill = pass_fail)) +
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

#--6.3- Boxplot

ggplot(data_clean, aes(x = previos_cat, y = exam_score, fill = previos_cat)) +
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

#--6.4- Gráfico de dispersión 

ggplot(data_clean, aes(x = hours_studied, y = exam_score, color = pass_fail)) +
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

#--6.5- Gráfico de barras

ggplot(data_clean, aes(x = exam_level, fill = exam_level)) +
  geom_bar(alpha = 0.8, color = "white", linewidth = 0.4) +
  geom_text(stat = 'count', aes(label = after_stat(count)), 
            vjust = -0.5, size = 4, fontface = "bold", color = "black") +
  geom_text(stat = 'count', 
            aes(label = paste0(round(after_stat(count)/nrow(data_clean)*100, 1), "%")),
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

#--6.6- Análisis del ratio estudio/sueño

ggplot(data_clean, aes(x = study_per_sleep, y = exam_score)) +
  geom_point(aes(color = pass_fail, size = sleep_hours), alpha = 0.6, shape = 16) +
  geom_hline(yintercept = 30, linetype = "dashed", color = "#7f8c8d", alpha = 0.7) +
  geom_vline(xintercept = 1, linetype = "dotted", color = "#7f8c8d", alpha = 0.7) +
  annotate("text", x = max(data_clean$study_per_sleep), y = 30, 
           label = "Umbral de Aprobación (30)", color = "#7f8c8d", hjust = 1, fontface = "bold") +
  annotate("text", x = 1, y = max(data_clean$exam_score), 
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
