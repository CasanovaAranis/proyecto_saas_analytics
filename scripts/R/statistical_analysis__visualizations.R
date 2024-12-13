# Instalación de paquetes (ejecutar solo una vez)
install.packages(c("tidyverse", "plotly", "scales", "lubridate",
                   "corrplot", "gridExtra", "hrbrthemes", "stats"))

# Cargar librerías
library(tidyverse)     # Para manipulación de datos y visualización
library(plotly)        # Para gráficos interactivos
library(scales)        # Para formateo de escalas
library(lubridate)     # Para manejo de fechas
library(corrplot)      # Para matrices de correlación
library(gridExtra)     # Para combinar gráficos
library(hrbrthemes)    # Para temas de visualización
library(stats)         # Para análisis estadístico

# Cargar datos
datos <- read.csv("enhanced_saas_marketing_data.csv")

# Preparación inicial de datos
datos_prep <- datos %>%
  # Convertir fecha a formato Date
  mutate(
    date = as.Date(date),
    month = floor_date(date, "month"),
    # Crear métricas derivadas
    gross_margin = (revenue - costs) / revenue,
    marketing_efficiency = revenue / marketing_spend,
    clv_cac_ratio = customer_lifetime_value / customer_acquisition_cost
  )

# Verificar estructura de datos
str(datos_prep)
summary(datos_prep)

# Función para crear gráfico de tendencias
crear_tendencias <- function(datos, metrica, titulo) {
  grafico <- datos %>%
    group_by(date, company) %>%
    summarise(valor = mean(!!sym(metrica))) %>%
    ggplot(aes(x = date, y = valor, color = company)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    theme_ipsum() +
    labs(
      title = titulo,
      x = "Fecha",
      y = metrica
    ) +
    theme(legend.position = "bottom")
  
  return(ggplotly(grafico))
}

# Crear gráficos de tendencias
tendencias_revenue <- crear_tendencias(datos_prep, "revenue",
                                       "Tendencia de Ingresos por Empresa")
tendencias_cac <- crear_tendencias(datos_prep, "customer_acquisition_cost",
                                   "Evolución del CAC")
tendencias_revenue
tendencias_cac

# Función para análisis de distribución
analizar_distribucion <- function(datos, variable) {
  # Histograma
  hist <- ggplot(datos, aes(x = !!sym(variable), fill = company)) +
    geom_histogram(alpha = 0.6, bins = 30) +
    facet_wrap(~company, scales = "free") +
    theme_minimal() +
    labs(title = paste("Distribución de", variable))
  
  # Box plot
  box <- ggplot(datos, aes(x = company, y = !!sym(variable), fill = company)) +
    geom_boxplot() +
    theme_minimal() +
    labs(title = paste("Box Plot de", variable)) +
    theme(axis.text.x = element_text(angle = 45))
  
  # Combinar gráficos
  grid.arrange(hist, box, ncol = 1)
}

# Analizar distribuciones de métricas clave
analizar_distribucion(datos_prep, "revenue")
analizar_distribucion(datos_prep, "customer_lifetime_value")


# Función para análisis estadístico por empresa
analisis_estadistico <- function(datos) {
  resumen <- datos %>%
    group_by(company) %>%
    summarise(
      # Métricas financieras
      revenue_mean = mean(revenue),
      revenue_sd = sd(revenue),
      margin_mean = mean(gross_margin),
      
      # Métricas de cliente
      cac_mean = mean(customer_acquisition_cost),
      clv_mean = mean(customer_lifetime_value),
      clv_cac_ratio = mean(clv_cac_ratio),
      
      # Métricas de engagement
      satisfaction_mean = mean(customer_satisfaction),
      churn_rate_mean = mean(churn_rate),
      
      # Tests estadísticos
      revenue_normality = shapiro.test(revenue)$p.value
    )
  
  return(resumen)
}

# Ejecutar análisis estadístico
resumen_estadistico <- analisis_estadistico(datos_prep)
resumen_estadistico


# Función para análisis de correlaciones
analizar_correlaciones <- function(datos) {
  # Seleccionar métricas numéricas
  metricas_numericas <- datos %>%
    select_if(is.numeric) 
  
  # Calcular correlaciones
  correlaciones <- cor(metricas_numericas, use = "complete.obs")
  
  # Crear matriz con correlaciones fuertes
  correlaciones_fuertes <- ifelse(abs(correlaciones) > 0.5, correlaciones, NA)
  
  # Visualizar correlaciones fuertes
  corrplot(correlaciones_fuertes,
           method = "color",
           type = "upper",
           addCoef.col = "black",
           number.cex = 0.7,
           tl.cex = 0.7,
           tl.col = "black",
           cl.cex = 0.8,
           na.label = " ",               # Ocultar correlaciones bajas
           title = "Matriz de Correlaciones (Solo Fuertes)",
           mar = c(0, 0, 2, 0))
}

# Visualizar correlaciones
analizar_correlaciones(datos_prep)


# Función para análisis de ROI
analisis_roi <- function(datos) {
  roi_plot <- datos %>%
    group_by(company, month) %>%
    summarise(
      marketing_roi = mean(marketing_roi),
      marketing_spend = mean(marketing_spend)
    ) %>%
    ggplot(aes(x = marketing_spend, y = marketing_roi, color = company)) +
    geom_point(size = 3, alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE) +
    theme_minimal() +
    labs(
      title = "ROI vs Inversión en Marketing",
      x = "Inversión en Marketing",
      y = "ROI"
    )
  
  return(ggplotly(roi_plot))
}

# Visualizar análisis de ROI
grafico_roi <- analisis_roi(datos_prep)
grafico_roi
