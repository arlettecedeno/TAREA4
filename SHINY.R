#!/usr/bin/env Rscript

library(shiny)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(GGally)
library(reshape2)
library(arrow)

#--------------------------------------------------------------------------------------------
#Leyendo dataset el formato feather
boston_housing <- read_feather("boston_clean.feather")
#--------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------
#DATAFRAME DEL GLOSARIO
glosario <- data.frame(
  Variable = c("CRIM", "ZN", "INDUS", "CHAS", "NOX", "RM", "AGE",
               "DIS", "RAD", "TAX", "PTRATIO", "B", "LSTAT", "MEDV"),
  Descripción = c(
    "Tasa de criminalidad per cápita por ciudad",
    "Proporción de terreno residencial zonificado para lotes > 25,000 pies²",
    "Proporción de acres destinados a negocios no minoristas por ciudad",
    "Variable indicadora del Río Charles (1 si el tramo colinda con el río; 0 en otro caso)",
    "Concentración de óxidos nítricos (partes por cada 10 millones)",
    "Número promedio de habitaciones por vivienda",
    "Proporción de unidades ocupadas por sus dueños construidas antes de 1940",
    "Distancias ponderadas a cinco centros de empleo de Boston",
    "Índice de accesibilidad a autopistas radiales",
    "Tasa del impuesto predial por valor total por cada $10,000",
    "Relación alumno–profesor por ciudad",
    "1000·(Bk − 0.63)², donde Bk es la proporción de población negra por ciudad",
    "Porcentaje de la población con estatus socioeconómico bajo",
    "Valor mediano de viviendas ocupadas por sus dueños (en miles de dólares)"
  ),
  stringsAsFactors = FALSE
)
#--------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------
#EDA
#Calculando medidas unovariadas para cada variable
univariadas <- data.frame(variable = names(boston_housing),
                          minimum = sapply(boston_housing, min),
                          maximum = sapply(boston_housing, max),
                          mean = sapply(boston_housing, mean),
                          median = sapply(boston_housing, median),
                          sd = sapply(boston_housing, sd),
                          q1 = sapply(boston_housing, \(x) quantile(x, 0.25)),
                          q3 = sapply(boston_housing, \(x) quantile(x, 0.75)))

#Ordenando
univariadas <- univariadas |> 
  select(1,2,7,5,4,8,3,6)

#PORCENTAJE DE NULOS
pct_nulls <- data.frame(
  variable = names(boston_housing),
  nulls = sapply(boston_housing, function(x) mean(is.na(x)) * 100))

#===============================================

#Calculando correlaciones entre la variable independiente y el resto de variables
cor_matrix <- cor(boston_housing)

cor_medv <- data.frame(variable = names(cor_matrix["MEDV", ]),
                       correlacion = as.numeric(cor_matrix["MEDV", ]))

#Preparando para graficar todas las correlaciones
cors <- boston_housing |>
  summarise(across(-MEDV, ~ cor(.x, MEDV), .names = "{.col}")) %>%
  tidyr::pivot_longer(everything(), names_to = "variable", values_to = "r") %>%
  mutate(label = paste0(variable, " (r = ", sprintf("%.2f", r), ")"))

longer_boston <- boston_housing |> 
  pivot_longer(-MEDV, names_to = "variable", values_to = "value")

graficos_panel <- longer_boston |> 
  left_join(cors, by = "variable")

#===============================================

#PREPARANDO PARA LOESS
boston_loess <- boston_housing  |> 
  select(-CHAS)  |> 
  pivot_longer(-MEDV, names_to = "variable", values_to = "value")

#===============================================

#PREPARANDO PARA Heatmap
cor_matrix <- cor(boston_housing)
melted_cor <- melt(cor_matrix)

#===============================================

#PREPARANDO PARA Pair plot
vars_top <- c("LSTAT", "RM", "PTRATIO", "DIS", "MEDV")

#--------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------
# SHINY

#User Interface
ui <- navbarPage(
  title = "The Boston Housing Dataset",
  
  tabPanel(
    "Resumen",
    fluidPage(
      fluidRow(
        column(
          width = 12,
          wellPanel(
            h4(""),
            p("El Boston Housing Dataset se deriva de información recopilada por el Servicio del Censo de los Estados Unidos sobre la vivienda en el área de Boston, Massachusetts.
              El dataset está compuesto por 13 variables descriptivas (1 categórica y 12 numéricas, su detalle se muestra en la pestaña Glosario), y la variable objetivo MEDV.
              A continuación se presentan algunas observaciones iniciales de la data:"),
            tags$ul(
              tags$li("No se observan valores nulos en ninguna variable."),
              tags$li("MEDV (valor medio de la vivienda) tiene una mediana alrededor de 21 mil dólares, sin embargo en el dataset se encuentran registros que van desde los 5 a los 50 mil dólares."),
              tags$li("Algunas variables muestran mucha dispersión (como CRIM, ZN o TAX, pues sus mediana y medias son muy distintas entre sí), lo que puede traducirse en una alta heterogeneidad de los vecinarios."),
              tags$li("La tabla de correlación de las variables descriptivas con MEDV muestra qué variables parecen ser más informativas para explicar el precio de las viviendas.")
            )
          )
        )
      ),
      
      fluidRow(
        column(
          width = 12,
          h3("Medidas univariadas"),
          tableOutput("tabla_univariadas")
        )
      ),
      
      br(),
      
      fluidRow(
        column(
          width = 6,
          h3("Correlación con MEDV"),
          tableOutput("tabla_cor_medv")
        ),
        column(
          width = 6,
          h3("Porcentaje de valores nulos"),
          tableOutput("tabla_nulls")
        )
      )
    )
  ),
  
  
  tabPanel(
    "Univariado",
    fluidPage(
      fluidRow(
        column(
          width = 12,
          wellPanel(
            h4("Análisis univariado"),
            p("En esta sección, mediante boxplots, se muestra la distribución de cada una de las variables descrptivas,
              así como su histograma y su curva de densidad. A continuación se dan algunas conclusiones:"),
            tags$ul(
              tags$li("Variables como CRIM, TAX, B, ZN y RAD presentan fuerte asimetría hacia la derecha, además de presencia considerable de outliers."),
              tags$li("CHAS es una variable categórica binaria, por lo que su distribución parece concentrada en un solo valor."),
              tags$li("Variables como RM, NOX, PTRATIO y DIS presentan una dsipersión más moderada y distribuciones un poco más estables."),
              tags$li("El boxplot de la variable LSTAT muestra gran dispersión, además de la presencia de valores altos, lo que se traduce en una gran variedad socioeconómica entre vecinadrios (variable muy informativa)."),
              tags$li("En los histogramas se observa que la mayoría de las variables presentan distribuciones asimétricas sesgadas hacia la derecha, lo que podría indicar que se tiene valores extremos y alta heterogeneidad entre vecindarios.")
            )
          )
        )
      ),
      
      h3("Distribuciones univariadas"),
      plotOutput("plot_box_all", height = "500px"),
      hr(),
      h3("Histograma + densidad"),
      plotOutput("plot_hist_density", height = "500px")
    )
  ),
  

  tabPanel(
    "Bivariado",
    fluidPage(
      fluidRow(
        column(
          width = 12,
          wellPanel(
            h4("Correlación entre las variables informativas y el precio de la vivienda"),
            p("En esta sección se presenta un análisis bivariado entre cada variable independiente y la variable objetivo (MEDV)"),
            tags$ul(
              tags$li("Se observa una fuerte relación positiva entre el número promedio de habitaciones (RM) y MEDV: más habitaciones, viviendas más caras."),
              tags$li("LSTAT (% de población de nivel socioeconómico bajo) se relaciona fuertemente de forma negativa con MEDV: vecindarios con mayor proporción de población de nivel socieoconómico bajo tienden a tener viviendas más baratas."),
              tags$li("También, variables como NOX, PTRATIO, INDUS y TAX presentan relación negativa con MEDV."),
              tags$li("HAy variables que parecen no mostrar linealidad, como CRIM, RAD, B, DIS y ZN, por lo que un modelo lineal simple no capturaría su relación con la variable objetivo.")
            )
          )
        )
      ),
      
      h3("Relación lineal con MEDV (todas las variables)"),
      plotOutput("plot_scatter_medv_all", height = "600px"),
      hr(),
      h3("LOESS (relación no lineal con MEDV)"),
      plotOutput("plot_loess_all", height = "600px")
    )
  ),
  

  tabPanel(
    "Var. categórica",
    fluidPage(
      fluidRow(
        column(
          width = 12,
          wellPanel(
            h4("Colindancia con el Río Charles"),
            p("La variable CHAS nos indica si el vecindario colinda o no con el río Charles (1 = sí, 0 = no)."),
            tags$ul(
              tags$li("Se observa que las viviendas cercanas al río tienden a tener valores más altos, sin embargo se observa un grado importante de dispersión.")
            )
          )
        )
      ),
      
      h3("Valor medio de la vivienda (MEDV) según cercanía al río Charles"),
      plotOutput("plot_chas_box", height = "400px")
    )
  ),
  

  tabPanel(
    "Correlaciones",
    fluidPage(
      fluidRow(
        column(
          width = 12,
          wellPanel(
            h4("Grado de correlación entre variables descriptivas y la variable objetivo"),
            p("En esta sección, se presenta un heatmap para describir el grado y dirección de la relación lineal (si existe) entre todas las variables."),
            tags$ul(
              tags$li("Los colores indican si la relación es positiva (tonos verdes) o negativa (tonos rosas)."),
              tags$li("Es posible observar que las variables RM y LSTAT son las que presentan mayor correlación en con MEDV (positiva y negativa, respectivamente)."),
              tags$li("Algunas variables, como NOX Y DIS, están fuertemente correlacionadas entre sí.")
            )
          )
        )
      ),
      
      h3("Matriz de correlaciones"),
      plotOutput("plot_heatmap", height = "600px"),
      hr(),
      h3("Pair plot"),
      plotOutput("plot_pairplot", height = "700px")
    )
  ),
  
  tabPanel("Glosario",
           h3("Glosario de variables del dataset"),
           p("A continuación se enlista la descripción de cada una de las variables del dataset Boston Housing."),
           tableOutput("tabla_glosario")
  )
)

#===============================================

#SERVER
server <- function(input, output, session) {
  
  # Tablas
  output$tabla_univariadas <- renderTable({
    univariadas
  }, digits = 3)
  
  output$tabla_nulls <- renderTable({
    pct_nulls |> mutate(nulls = round(nulls, 2))
  }, digits = 2)
  
  output$tabla_cor_medv <- renderTable({
    cor_medv |> mutate(correlacion = round(correlacion, 3))
  }, digits = 3)
  
  # Boxplots univariados
  output$plot_box_all <- renderPlot({
    ggplot(longer_boston, aes(x = "", y = value, group = 1)) +
      geom_boxplot(fill = "darkslategray3", alpha = 0.8, outlier.alpha = 0.5) +
      facet_wrap(~ variable, scales = "free_y", ncol = 7) + 
      labs(title = "Distribuciones univariadas (boxplots)", x = NULL, y = NULL) +
      theme_minimal()
  })
  
  # Histograma + densidad
  output$plot_hist_density <- renderPlot({
    ggplot(longer_boston, aes(x = value)) +
      geom_histogram(aes(y = after_stat(density)), bins = 30,
                     fill = "darkslategray3", color = "white", alpha = 0.6) +
      geom_density(color = "deeppink2", linewidth = 1) +
      facet_wrap(~ variable, scales = "free", ncol = 7) +
      labs(title = "Distribuciones de cada variable con densidad KDE", x = NULL, y = NULL) +
      theme_minimal()
  })
  
  # Relación con MEDV (lm, todas las variables)
  output$plot_scatter_medv_all <- renderPlot({
    ggplot(graficos_panel, aes(value, MEDV)) +
      geom_point(alpha = 0.2, size = 1) +
      geom_smooth(method = "lm", se = FALSE, color = "deeppink2", linewidth = 0.8) +
      facet_wrap(~ label, scales = "free_x") +
      labs(title = "Relación de cada variable con MEDV (modelo lineal)", x = NULL, y = "MEDV") +
      theme_minimal()
  })
  
  # LOESS vs MEDV
  output$plot_loess_all <- renderPlot({
    ggplot(boston_loess, aes(value, MEDV)) +
      geom_point(color = "darkslategray3", alpha = 0.5, size = 1) +
      geom_smooth(method = "loess", se = TRUE, color = "deeppink2",
                  linewidth = 0.9, span = 0.9) +
      facet_wrap(~ variable, scales = "free_x") +
      labs(title = "Relación (LOESS) entre cada variable y MEDV", x = NULL, y = "MEDV") +
      theme_minimal()
  })
  
  # CHAS vs MEDV
  output$plot_chas_box <- renderPlot({
    ggplot(boston_housing, aes(x = factor(CHAS), y = MEDV, fill = factor(CHAS))) +
      geom_boxplot(alpha = 0.8, width = 0.6, outlier.alpha = 0.5) +
      scale_fill_manual(
        values = c("0" = "darkslategray3", "1" = "deeppink2"),
        labels = c("0" = "No junto al río", "1" = "Sí junto al río")
      ) +
      labs(
        title = "Valor medio de la vivienda (MEDV) según cercanía al río Charles",
        x = "Cercanía al río (CHAS)",
        y = "MEDV"
      ) +
      theme_minimal()
  })
  
  # Heatmap de correlaciones
  output$plot_heatmap <- renderPlot({
    ggplot(melted_cor, aes(Var1, Var2, fill = value)) +
      geom_tile() +
      scale_fill_gradient2(
        low = "deeppink2", mid = "white",
        high = "darkslategray3", midpoint = 0
      ) +
      labs(title = "Matriz de correlaciones", x = "", y = "", fill = "r") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  #Glosario
  output$tabla_glosario <- renderTable({
    glosario
  },
  striped = TRUE, bordered = TRUE, hover = TRUE, spacing = "s")
  
  # Pair plot 
  output$plot_pairplot <- renderPlot({
    ggpairs(
      boston_housing[, vars_top],
      upper = list(
        continuous = wrap("cor", size = 5, stars = FALSE)
      ),
      diag  = list(
        continuous = wrap(
          "densityDiag",
          colour = "deeppink2",
          fill   = "darkslategray3",
          alpha  = 0.4
        )
      ),
      lower = list(
        continuous = wrap("points", alpha = 0.4, size = 0.7)
      )
    )
  })
}

#===============================================

# Run the application 
shinyApp(ui = ui, server = server)
#--------------------------------------------------------------------------------------------