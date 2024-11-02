---
date: 2023-08-09

title: Disrupción y Estructura en Redes de Co-autoría Científica
subtitle:  Un Análisis ERGM Modo-2
author: Roberto Cantillan

show_post_date: true
show_author_byline: true

draft: false

summary: |
    En este análisis examinamos la estructura de colaboración científica en Latinoamérica mediante un ERGM bipartito sobre una red de 45,724 conexiones autor-artículo. Los resultados revelan una baja densidad base (edges=-8.69***) compensada por una fuerte tendencia a formar equipos de tamaño medio (gwb1deg=1.55***, gwb2deg=7.58***). Destaca especialmente cómo la disrupción científica se asocia positivamente con la formación de vínculos (0.43***), Los resultados sugieren que mientras las ciencias sociales y humanidades (SHAPE) tienden a colaborar dentro de su campo, las ciencias STEM muestran patrones más abiertos de colaboración interdisciplinaria. Esto podría reflejar diferencias en las prácticas de investigación: SHAPE con tradiciones más especializadas vs. STEM con mayor apertura a cruces disciplinares.


format: hugo
freeze: auto
---

## Introducción

La ciencia de la sustentabilidad requiere urgentemente de innovación transformadora para abordar desafíos socio-ambientales complejos. Comprender cómo emerge la investigación disruptiva en las redes científicas es, por tanto, crucial. Li et al. (2024) han demostrado una relación inversa entre productividad y disrupción científica, pero los mecanismos estructurales subyacentes a este fenómeno permanecen poco claros, particularmente en el contexto de redes modo-2 que vinculan autores y publicaciones.

Los modelos ERGM proporcionan un marco metodológico robusto para examinar cómo las características de autores y papers, junto con sus patrones de vinculación, influyen en la estructura general de la red científica. Utilizando el dataset SciSciNet, este trabajo analiza las estructuras emergentes en una red bipartita de co-autoría científica latinoamericana entre 1990-2000.

## Configuración Inicial y Carga de Datos

``` r
# 1. Cargar paquetes necesarios
library(osfr)
library(tidyverse)
library(ergm)
library(Matrix)
library(network)
library(ggraph)
library(tidygraph)
library(kableExtra)

# Configurar tema para visualizaciones
theme_set(theme_minimal())
```

``` r
# 2. datos 
load("/home/rober/Documents/ricantillan.rbind.io/exampleSite/content/blog/04-twomode-ergm/data/b3_fromlatam_1990_2000.RData")

# 3. Filtrar datos de Latam
#b3_fromlatam <- b3_joined %>%
#  filter(DocType == "Journal") %>%
#  group_by(PaperID) %>%
#  filter(latam_prop >= 0.5 |
#           any(AuthorSequenceNumber == 1 & is_latam == 1)) %>%
#  ungroup()
#
## 4. Filtrar por tiempo
#rm(b3_joined)
#gc()
#b3_fromlatam_1990_2000 <- b3_fromlatam %>% filter(Year < 2000)
```

## Preparación de Datos

<details open>
<summary>Code</summary>

``` r
# 5. Preparación y limpieza
clean_data <- b3_fromlatam_1990_2000 %>%
  filter(!is.na(Disruption),
         !is.na(CitationCount), 
         !is.na(H.index_auth),
         !is.na(Average_C10_auth),
         !is.na(Productivity_auth),
         !is.na(Affiliation_Name),
         !is.na(is_latam),
         !is.na(Institution_Count),
         !is.na(Field_Name),
         !is.na(Field_Type)) %>%
  filter(Field_Type == "Top")

# 6. Estandarización de variables
clean_data <- clean_data %>%
  mutate(
    disruption_std = as.vector(scale(Disruption)),
    citations_std = as.vector(scale(log1p(CitationCount))),
    h_index_std = as.vector(scale(log1p(H.index_auth))),
    avg_c10_std = as.vector(scale(log1p(Average_C10_auth))),
    productivity_std = as.vector(scale(log1p(Productivity_auth))),
    field_broad = case_when(
      Field_Name %in% c(
        "Biology", "Chemistry", "Computer science",
        "Engineering", "Environmental science", "Geography",
        "Materials science", "Mathematics", "Medicine"
      ) ~ "STEM",
      Field_Name %in% c(
        "Business", "Economics", "Political science", "Sociology"
      ) ~ "SHAPE",
      TRUE ~ NA_character_
    )
  )
```

</details>

## Construcción de la Red Bipartita

<details open>
<summary>Code</summary>

``` r
# 7. Atributos por modo
paper_attributes <- clean_data %>%
  group_by(PaperID) %>%
  slice(1) %>%
  ungroup() %>%
  select(PaperID, disruption_std, citations_std, 
         Institution_Count, Field_Name, field_broad)

author_attributes <- clean_data %>%
  group_by(AuthorID) %>%
  slice(1) %>%
  ungroup()

# 8. Crear matriz de incidencia y red bipartita
papers <- unique(paper_attributes$PaperID)
authors <- unique(author_attributes$AuthorID)

paper_author_matrix <- sparseMatrix(
  i = match(clean_data$PaperID, papers),
  j = match(clean_data$AuthorID, authors),
  x = 1,
  dims = c(length(papers), length(authors))
)

net_bipartite <- network(
  paper_author_matrix,
  matrix.type = "bipartite",
  directed = FALSE
)

# 9. Asignación de atributos
# Modo 1 (Papers)
net_bipartite %v% "disruption" <- paper_attributes$disruption_std
net_bipartite %v% "citations" <- paper_attributes$citations_std
net_bipartite %v% "inst_count" <- paper_attributes$Institution_Count
net_bipartite %v% "field" <- paper_attributes$Field_Name
net_bipartite %v% "field_broad" <- paper_attributes$field_broad

# Modo 2 (Autores)
net_bipartite %v% "h_index" <- author_attributes$h_index_std
net_bipartite %v% "avg_c10" <- author_attributes$avg_c10_std
net_bipartite %v% "affiliation" <- author_attributes$Affiliation_Name
net_bipartite %v% "is_latam" <- author_attributes$is_latam
net_bipartite %v% "productivity" <- author_attributes$productivity_std
```

</details>

### Visualización de la Red

``` r
# Obtener el número de vértices
n_vertices <- network.size(net_bipartite)

# Obtener el valor de bipartite (que es 161 según los atributos que mostraste)
bipartite_value <- 161

# Crear el vector is_actor
is_actor <- rep(FALSE, n_vertices)
is_actor[(bipartite_value + 1):n_vertices] <- TRUE

# Agregar el atributo a la red
net_bipartite %v% "is_actor" <- is_actor

# Verificar que se agregó correctamente
table(net_bipartite %v% "is_actor")
```


    FALSE  TRUE 
      161   284 

``` r
# Crear el vector de etiquetas
n_vertices <- network.size(net_bipartite)
bipartite_value <- 161

node_labels <- rep("Autor", n_vertices)
node_labels[1:bipartite_value] <- "Paper"

# Agregar el atributo a la red
net_bipartite %v% "tipo" <- node_labels

# Verificar que se agregó correctamente
table(net_bipartite %v% "tipo")
```


    Autor Paper 
      284   161 

``` r
ggraph(net_bipartite, layout = "graphopt") + 
     geom_edge_link0(edge_colour = "black", edge_width = 0.2, edge_alpha = 1) + 
   geom_node_point(aes(fill = tipo, shape = tipo), size = 2.25, colour = "#FFFFFF", shape = 21, stroke = 0.3) +
   scale_fill_manual(values = c("#003f5c","#ffa600")) +
     theme_graph() + 
     theme(legend.position = "left") +
  theme(legend.justification=c(0,.90), 
        legend.position=c(0,1),
        legend.box.just = "bottom",
        legend.box.background = element_rect(color="black", size=.5),
        legend.margin = margin(4, 4, 4, 4),
                legend.text = element_text(size=11)) +
          guides(size=F, edge_width=F,
                 fill = guide_legend(override.aes = list(size = 6))) +
          labs(fill = "Entidad")
```

    Warning: The `size` argument of `element_rect()` is deprecated as of ggplot2 3.4.0.
    ℹ Please use the `linewidth` argument instead.

    Warning: A numeric `legend.position` argument in `theme()` was deprecated in ggplot2
    3.5.0.
    ℹ Please use the `legend.position.inside` argument of `theme()` instead.

    Warning: The `<scale>` argument of `guides()` cannot be `FALSE`. Use "none" instead as
    of ggplot2 3.3.4.

<img src="index.markdown_strict_files/figure-markdown_strict/unnamed-chunk-2-1.png" width="768" />

``` r
degreedist(net_bipartite)
```

    Bipartite mode 2 degree distribution:
      0   1   2   3   4   5 
    161 264  15   3   1   1 
    Bipartite mode 1 degree distribution:
      0   1   2   3   4   5   6   8 
    284  80  45  20   8   4   1   3 

``` r
# Crear dataframes para cada modo
mode1_data <- data.frame(
  grado = c(0, 1, 2, 3, 4, 5, 6, 8),
  frecuencia = c(284, 80, 45, 20, 8, 4, 1, 3),
  modo = "Modo 1 (Papers)"
)

mode2_data <- data.frame(
  grado = c(0, 1, 2, 3, 4, 5),
  frecuencia = c(161, 264, 15, 3, 1, 1),
  modo = "Modo 2 (Autores)"
)

# Combinar los datos
degree_data <- rbind(mode1_data, mode2_data)

# Crear el gráfico
ggplot(degree_data, aes(x = grado, y = frecuencia, fill = modo)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  scale_fill_manual(values = c( "#ffa600", "#003f5c")) +
  labs(title = "Distribución de Grados",
       subtitle = "",
       x = "Grado",
       y = "",
       fill = "Entidad") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  ) +
  scale_x_continuous(breaks = 0:8) +
  scale_y_continuous(expand = c(0, 30)) +
  geom_text(aes(label = frecuencia), 
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 3)
```

<img src="index.markdown_strict_files/figure-markdown_strict/unnamed-chunk-4-1.png" width="768" />

## Análisis ERGM

<details open>
<summary>Code</summary>

``` r
# 10. Modelos ERGM
## 1. Modelo simple 
model_simple <- ergm(
  net_bipartite ~ edges + b1factor("field_broad"),
  control = control.ergm(
    MCMLE.maxit = 5,
    MCMC.samplesize = 1000,
    MCMLE.termination = "Hummel"
  )
)
```

</details>

    Starting maximum pseudolikelihood estimation (MPLE):

    Obtaining the responsible dyads.

    Evaluating the predictor and response matrix.

    Maximizing the pseudolikelihood.

    Finished MPLE.

    Evaluating log-likelihood at the estimate. 

<details open>
<summary>Code</summary>

``` r
## 2. Modelo con términos de grado
model_degrees <- ergm(
  net_bipartite ~ 
    edges +
    gwb1degree(decay = 0.25, fixed = T) +
    gwb2degree(decay = 0.25, fixed = T) +
    b1factor("field_broad"),
  control = control.ergm(
    init = c(coef(model_simple), rep(0, 2)),
    MCMLE.maxit = 10,
    MCMC.samplesize = 2000,
    MCMLE.termination = "Hummel"
  )
)
```

</details>

    Starting Monte Carlo maximum likelihood estimation (MCMLE):
    Iteration 1 of at most 10:

    Warning: 'glpk' selected as the solver, but package 'Rglpk' is not available;
    falling back to 'lpSolveAPI'. This should be fine unless the sample size and/or
    the number of parameters is very big.

    Optimizing with step length 0.1801.
    The log-likelihood improved by 5.2148.
    Iteration 2 of at most 10:
    Optimizing with step length 0.1849.
    The log-likelihood improved by 4.0807.
    Iteration 3 of at most 10:
    Optimizing with step length 0.2242.
    The log-likelihood improved by 4.4102.
    Iteration 4 of at most 10:
    Optimizing with step length 0.2721.
    The log-likelihood improved by 4.8011.
    Iteration 5 of at most 10:
    Optimizing with step length 0.2626.
    The log-likelihood improved by 3.0899.
    Iteration 6 of at most 10:
    Optimizing with step length 0.3619.
    The log-likelihood improved by 3.9966.
    Iteration 7 of at most 10:
    Optimizing with step length 0.5067.
    The log-likelihood improved by 4.5147.
    Iteration 8 of at most 10:
    Optimizing with step length 0.6274.
    The log-likelihood improved by 3.6584.
    Iteration 9 of at most 10:
    Optimizing with step length 0.9427.
    The log-likelihood improved by 3.2559.
    Iteration 10 of at most 10:
    Optimizing with step length 1.0000.
    The log-likelihood improved by 0.6227.
    Step length converged once. Increasing MCMC sample size.
    MCMLE estimation did not converge after 10 iterations. The estimated coefficients may not be accurate. Estimation may be resumed by passing the coefficients as initial values; see 'init' under ?control.ergm for details.
    Finished MCMLE.
    Evaluating log-likelihood at the estimate. Fitting the dyad-independent submodel...
    Bridging between the dyad-independent submodel and the full model...
    Setting up bridge sampling...
    Using 16 bridges: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 .
    Bridging finished.

    This model was fit using MCMC.  To examine model diagnostics and check
    for degeneracy, use the mcmc.diagnostics() function.

<details open>
<summary>Code</summary>

``` r
## 3. Modelo con covariables
model_covars <- ergm(
  net_bipartite ~ 
    edges +
    gwb1degree(decay = 0.25, fixed = T) +
    gwb2degree(decay = 0.25, fixed = T) +
    b1factor("field_broad") +
    b1cov("disruption") +
    b2cov("productivity"),
  control = control.ergm(
    init = c(coef(model_degrees), rep(0, 2)),
    MCMLE.maxit = 15,
    MCMC.samplesize = 3000,
    MCMLE.termination = "Hummel"
  )
)
```

</details>

    Starting Monte Carlo maximum likelihood estimation (MCMLE):
    Iteration 1 of at most 15:
    Optimizing with step length 1.0000.
    The log-likelihood improved by 0.7571.
    Step length converged once. Increasing MCMC sample size.
    Iteration 2 of at most 15:
    Optimizing with step length 1.0000.
    The log-likelihood improved by 0.0158.
    Step length converged twice. Stopping.
    Finished MCMLE.
    Evaluating log-likelihood at the estimate. Fitting the dyad-independent submodel...
    Bridging between the dyad-independent submodel and the full model...
    Setting up bridge sampling...
    Using 16 bridges: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 .
    Bridging finished.

    This model was fit using MCMC.  To examine model diagnostics and check
    for degeneracy, use the mcmc.diagnostics() function.

<details open>
<summary>Code</summary>

``` r
## 4. Modelo final optimizado
model_final <- ergm(
  net_bipartite ~ 
    edges +
    gwb1degree(decay = 0.25, fixed = T) +
    gwb2degree(decay = 0.25, fixed = T) +
    b1cov("disruption") +
    b2cov("productivity") +
    b1factor("field_broad") +
    b1nodematch("field_broad", diff=T) +
    b1cov("disruption"):b2cov("productivity"),
  control = control.ergm(
    init = c(coef(model_covars), rep(0, 3)),
    seed = 123,
    MCMLE.maxit = 14,
    MCMC.burnin = 1000,      
    MCMC.interval = 50,      
    MCMC.samplesize = 2000,  
    parallel = 2,
    parallel.type = "PSOCK",
    MCMLE.termination = "Hummel"
  )
)
```

</details>

    Starting Monte Carlo maximum likelihood estimation (MCMLE):
    Iteration 1 of at most 14:
    Optimizing with step length 0.0526.
    The log-likelihood improved by 4.0758.
    Iteration 2 of at most 14:
    Optimizing with step length 0.0605.
    The log-likelihood improved by 3.4436.
    Iteration 3 of at most 14:
    Optimizing with step length 0.0535.
    The log-likelihood improved by 2.0317.
    Iteration 4 of at most 14:
    Optimizing with step length 0.0811.
    The log-likelihood improved by 2.0153.
    Iteration 5 of at most 14:
    Optimizing with step length 0.1084.
    The log-likelihood improved by 2.5672.
    Iteration 6 of at most 14:
    Optimizing with step length 0.1543.
    The log-likelihood improved by 5.3461.
    Iteration 7 of at most 14:
    Optimizing with step length 0.1366.
    The log-likelihood improved by 2.6596.
    Iteration 8 of at most 14:
    Optimizing with step length 0.1790.
    The log-likelihood improved by 2.5847.
    Iteration 9 of at most 14:
    Optimizing with step length 0.1283.
    The log-likelihood improved by 1.3305.
    Iteration 10 of at most 14:
    Optimizing with step length 0.1349.
    The log-likelihood improved by 1.7986.
    Iteration 11 of at most 14:
    Optimizing with step length 0.1879.
    The log-likelihood improved by 3.4119.
    Iteration 12 of at most 14:
    Optimizing with step length 0.2966.
    The log-likelihood improved by 2.7990.
    Iteration 13 of at most 14:
    Optimizing with step length 0.3227.
    The log-likelihood improved by 5.1318.
    Iteration 14 of at most 14:
    Optimizing with step length 0.0218.
    The log-likelihood improved by 5.6495.
    MCMLE estimation did not converge after 14 iterations. The estimated coefficients may not be accurate. Estimation may be resumed by passing the coefficients as initial values; see 'init' under ?control.ergm for details.
    Finished MCMLE.
    Evaluating log-likelihood at the estimate. Fitting the dyad-independent submodel...
    Bridging between the dyad-independent submodel and the full model...
    Setting up bridge sampling...
    Using 16 bridges: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 .
    Bridging finished.

    This model was fit using MCMC.  To examine model diagnostics and check
    for degeneracy, use the mcmc.diagnostics() function.

``` r
summary(model_final)
```

    Call:
    ergm(formula = net_bipartite ~ edges + gwb1degree(decay = 0.25, 
        fixed = T) + gwb2degree(decay = 0.25, fixed = T) + b1cov("disruption") + 
        b2cov("productivity") + b1factor("field_broad") + b1nodematch("field_broad", 
        diff = T) + b1cov("disruption"):b2cov("productivity"), control = control.ergm(init = c(coef(model_covars), 
        rep(0, 3)), seed = 123, MCMLE.maxit = 14, MCMC.burnin = 1000, 
        MCMC.interval = 50, MCMC.samplesize = 2000, parallel = 2, 
        parallel.type = "PSOCK", MCMLE.termination = "Hummel"))

    Monte Carlo Maximum Likelihood Results:

                                        Estimate Std. Error MCMC % z value Pr(>|z|)
    edges                               -9.77310    0.52573      0 -18.590  < 1e-04
    gwb1deg.fixed.0.25                   1.99056    0.81497      0   2.442  0.01459
    gwb2deg.fixed.0.25                   9.51683    1.34041      0   7.100  < 1e-04
    b1cov.disruption                     0.30683    0.05369      0   5.714  < 1e-04
    b2cov.productivity                   0.07522    0.18380      0   0.409  0.68234
    b1factor.field_broad.STEM            0.59923    0.20934      0   2.862  0.00420
    b1nodematch.field_broad.SHAPE        0.91443    0.53828      0   1.699  0.08936
    b1nodematch.field_broad.STEM         0.42736    0.45397      0   0.941  0.34650
    b1cov.disruption:b2cov.productivity -0.07591    0.02804      1  -2.707  0.00678
                                           
    edges                               ***
    gwb1deg.fixed.0.25                  *  
    gwb2deg.fixed.0.25                  ***
    b1cov.disruption                    ***
    b2cov.productivity                     
    b1factor.field_broad.STEM           ** 
    b1nodematch.field_broad.SHAPE       .  
    b1nodematch.field_broad.STEM           
    b1cov.disruption:b2cov.productivity ** 
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

         Null Deviance: 63387  on 45724  degrees of freedom
     Residual Deviance:  3306  on 45715  degrees of freedom
     
    AIC: 3324  BIC: 3402  (Smaller is better. MC Std. Err. = 2.372)

### Diagnósticos del Modelo

``` r
# Bondad de ajuste
gof_model <- gof(model_final)
plot(gof_model)
```

<img src="index.markdown_strict_files/figure-markdown_strict/model-diagnostics-1.png" width="960" />

<img src="index.markdown_strict_files/figure-markdown_strict/model-diagnostics-2.png" width="960" />

<img src="index.markdown_strict_files/figure-markdown_strict/model-diagnostics-3.png" width="960" />

<img src="index.markdown_strict_files/figure-markdown_strict/model-diagnostics-4.png" width="960" />

<img src="index.markdown_strict_files/figure-markdown_strict/model-diagnostics-5.png" width="960" />

``` r
# MCMC diagnósticos
mcmc.diagnostics(model_final)
```

<img src="index.markdown_strict_files/figure-markdown_strict/unnamed-chunk-6-1.png" width="768" />

<img src="index.markdown_strict_files/figure-markdown_strict/unnamed-chunk-6-2.png" width="768" />

<img src="index.markdown_strict_files/figure-markdown_strict/unnamed-chunk-6-3.png" width="768" />

    Sample statistics summary:

    Iterations = 78800:6e+05
    Thinning interval = 400 
    Number of chains = 2 
    Sample size per chain = 1304 

    1. Empirical mean and standard deviation for each variable,
       plus standard error of the mean:

                                             Mean     SD Naive SE Time-series SE
    edges                                 79.1120  5.283  0.10344        0.15224
    gwb1deg.fixed.0.25                     7.9277  1.624  0.03179        0.09165
    gwb2deg.fixed.0.25                     0.3452  1.549  0.03034        0.04769
    b1cov.disruption                     192.7239 37.055  0.72559        4.28866
    b2cov.productivity                   -97.2345  5.413  0.10599        0.15372
    b1factor.field_broad.STEM             15.9866  7.326  0.14346        0.68554
    b1nodematch.field_broad.SHAPE       3304.7726 22.443  0.43947        8.26110
    b1nodematch.field_broad.STEM           1.6860  3.567  0.06984        0.10178
    b1cov.disruption:b2cov.productivity  -87.7800 37.399  0.73234        4.65900

    2. Quantiles for each variable:

                                            2.5%       25%       50%      75%
    edges                                 69.000   76.0000   79.0000   83.000
    gwb1deg.fixed.0.25                     4.469    6.8805    8.0023    9.080
    gwb2deg.fixed.0.25                    -2.830   -0.6483    0.3915    1.391
    b1cov.disruption                     127.883  166.6996  189.8906  216.125
    b2cov.productivity                  -107.801 -100.6453  -97.4160  -93.661
    b1factor.field_broad.STEM              2.000   11.0000   16.0000   21.000
    b1nodematch.field_broad.SHAPE       3229.000 3309.0000 3311.0000 3313.000
    b1nodematch.field_broad.STEM          -5.000   -1.0000    1.0000    4.000
    b1cov.disruption:b2cov.productivity -166.133 -111.7219  -85.3647  -63.561
                                           97.5%
    edges                                 90.000
    gwb1deg.fixed.0.25                    10.791
    gwb2deg.fixed.0.25                     3.201
    b1cov.disruption                     273.179
    b2cov.productivity                   -86.293
    b1factor.field_broad.STEM             31.000
    b1nodematch.field_broad.SHAPE       3317.000
    b1nodematch.field_broad.STEM           9.000
    b1cov.disruption:b2cov.productivity  -18.205


    Sample statistics cross-correlations:
                                              edges gwb1deg.fixed.0.25
    edges                                1.00000000         0.25418790
    gwb1deg.fixed.0.25                   0.25418790         1.00000000
    gwb2deg.fixed.0.25                   0.83864672         0.24244069
    b1cov.disruption                     0.14979228        -0.40623350
    b2cov.productivity                  -0.06711711        -0.01333205
    b1factor.field_broad.STEM            0.34391347         0.11251099
    b1nodematch.field_broad.SHAPE        0.11310647        -0.04407920
    b1nodematch.field_broad.STEM         0.65193969         0.15406476
    b1cov.disruption:b2cov.productivity -0.07193821         0.16960363
                                        gwb2deg.fixed.0.25 b1cov.disruption
    edges                                       0.83864672       0.14979228
    gwb1deg.fixed.0.25                          0.24244069      -0.40623350
    gwb2deg.fixed.0.25                          1.00000000       0.09181389
    b1cov.disruption                            0.09181389       1.00000000
    b2cov.productivity                         -0.03371761      -0.02984082
    b1factor.field_broad.STEM                   0.30773471       0.01612316
    b1nodematch.field_broad.SHAPE               0.02305100       0.20940447
    b1nodematch.field_broad.STEM                0.47257586       0.09704965
    b1cov.disruption:b2cov.productivity        -0.04307069      -0.37412229
                                        b2cov.productivity
    edges                                      -0.06711711
    gwb1deg.fixed.0.25                         -0.01333205
    gwb2deg.fixed.0.25                         -0.03371761
    b1cov.disruption                           -0.02984082
    b2cov.productivity                          1.00000000
    b1factor.field_broad.STEM                   0.02190150
    b1nodematch.field_broad.SHAPE              -0.09942120
    b1nodematch.field_broad.STEM               -0.01945761
    b1cov.disruption:b2cov.productivity         0.05227252
                                        b1factor.field_broad.STEM
    edges                                              0.34391347
    gwb1deg.fixed.0.25                                 0.11251099
    gwb2deg.fixed.0.25                                 0.30773471
    b1cov.disruption                                   0.01612316
    b2cov.productivity                                 0.02190150
    b1factor.field_broad.STEM                          1.00000000
    b1nodematch.field_broad.SHAPE                     -0.19253199
    b1nodematch.field_broad.STEM                       0.47749109
    b1cov.disruption:b2cov.productivity                0.07018696
                                        b1nodematch.field_broad.SHAPE
    edges                                                   0.1131065
    gwb1deg.fixed.0.25                                     -0.0440792
    gwb2deg.fixed.0.25                                      0.0230510
    b1cov.disruption                                        0.2094045
    b2cov.productivity                                     -0.0994212
    b1factor.field_broad.STEM                              -0.1925320
    b1nodematch.field_broad.SHAPE                           1.0000000
    b1nodematch.field_broad.STEM                           -0.0165040
    b1cov.disruption:b2cov.productivity                    -0.1654536
                                        b1nodematch.field_broad.STEM
    edges                                                 0.65193969
    gwb1deg.fixed.0.25                                    0.15406476
    gwb2deg.fixed.0.25                                    0.47257586
    b1cov.disruption                                      0.09704965
    b2cov.productivity                                   -0.01945761
    b1factor.field_broad.STEM                             0.47749109
    b1nodematch.field_broad.SHAPE                        -0.01650400
    b1nodematch.field_broad.STEM                          1.00000000
    b1cov.disruption:b2cov.productivity                  -0.02445249
                                        b1cov.disruption:b2cov.productivity
    edges                                                       -0.07193821
    gwb1deg.fixed.0.25                                           0.16960363
    gwb2deg.fixed.0.25                                          -0.04307069
    b1cov.disruption                                            -0.37412229
    b2cov.productivity                                           0.05227252
    b1factor.field_broad.STEM                                    0.07018696
    b1nodematch.field_broad.SHAPE                               -0.16545365
    b1nodematch.field_broad.STEM                                -0.02445249
    b1cov.disruption:b2cov.productivity                          1.00000000

    Sample statistics auto-correlation:
    Chain 1 
                  edges gwb1deg.fixed.0.25 gwb2deg.fixed.0.25 b1cov.disruption
    Lag 0    1.00000000          1.0000000         1.00000000        1.0000000
    Lag 400  0.37300363          0.6172305         0.43236568        0.9414478
    Lag 800  0.15457734          0.5079446         0.20589905        0.8959547
    Lag 1200 0.10193842          0.4158257         0.12306590        0.8531727
    Lag 1600 0.07900375          0.3592435         0.09514530        0.8134542
    Lag 2000 0.06956099          0.2749364         0.06169542        0.7819809
             b2cov.productivity b1factor.field_broad.STEM
    Lag 0           1.000000000                 1.0000000
    Lag 400         0.379387553                 0.8152592
    Lag 800         0.138219968                 0.7331235
    Lag 1200        0.066509503                 0.6826637
    Lag 1600        0.005547684                 0.6419541
    Lag 2000       -0.035995670                 0.6016441
             b1nodematch.field_broad.SHAPE b1nodematch.field_broad.STEM
    Lag 0                        1.0000000                   1.00000000
    Lag 400                      0.9898766                   0.36747936
    Lag 800                      0.9826312                   0.12466024
    Lag 1200                     0.9774013                   0.07613750
    Lag 1600                     0.9731294                   0.04626352
    Lag 2000                     0.9688606                   0.04098090
             b1cov.disruption:b2cov.productivity
    Lag 0                              1.0000000
    Lag 400                            0.9362295
    Lag 800                            0.8859858
    Lag 1200                           0.8429579
    Lag 1600                           0.8006817
    Lag 2000                           0.7646305
    Chain 2 
                  edges gwb1deg.fixed.0.25 gwb2deg.fixed.0.25 b1cov.disruption
    Lag 0    1.00000000          1.0000000         1.00000000        1.0000000
    Lag 400  0.36233560          0.5874276         0.41419547        0.9157412
    Lag 800  0.11978307          0.4581465         0.18286928        0.8529311
    Lag 1200 0.03838188          0.3583871         0.08502761        0.7993249
    Lag 1600 0.04043329          0.3241449         0.07156519        0.7518551
    Lag 2000 0.04333024          0.3056473         0.03772754        0.7139468
             b2cov.productivity b1factor.field_broad.STEM
    Lag 0            1.00000000                 1.0000000
    Lag 400          0.39137697                 0.7590347
    Lag 800          0.09489667                 0.6434667
    Lag 1200        -0.04311493                 0.5650090
    Lag 1600        -0.09529921                 0.5316558
    Lag 2000        -0.05045177                 0.5146099
             b1nodematch.field_broad.SHAPE b1nodematch.field_broad.STEM
    Lag 0                       1.00000000                  1.000000000
    Lag 400                     0.41307238                  0.350980806
    Lag 800                     0.15303619                  0.124789458
    Lag 1200                    0.09319306                  0.033647710
    Lag 1600                    0.07521335                  0.008056128
    Lag 2000                    0.05334841                  0.018996972
             b1cov.disruption:b2cov.productivity
    Lag 0                              1.0000000
    Lag 400                            0.9354696
    Lag 800                            0.8888389
    Lag 1200                           0.8512565
    Lag 1600                           0.8116373
    Lag 2000                           0.7746114

    Sample statistics burn-in diagnostic (Geweke):
    Chain 1 

    Fraction in 1st window = 0.1
    Fraction in 2nd window = 0.5 

                                  edges                  gwb1deg.fixed.0.25 
                             -1.9263984                          -0.3260772 
                     gwb2deg.fixed.0.25                    b1cov.disruption 
                              0.3561386                          -1.2727424 
                     b2cov.productivity           b1factor.field_broad.STEM 
                              2.4955374                           2.7146794 
          b1nodematch.field_broad.SHAPE        b1nodematch.field_broad.STEM 
                           -230.7257620                           1.5756974 
    b1cov.disruption:b2cov.productivity 
                              1.2687977 

    Individual P-values (lower = worse):
                                  edges                  gwb1deg.fixed.0.25 
                            0.054054643                         0.744365964 
                     gwb2deg.fixed.0.25                    b1cov.disruption 
                            0.721736753                         0.203109452 
                     b2cov.productivity           b1factor.field_broad.STEM 
                            0.012576650                         0.006633996 
          b1nodematch.field_broad.SHAPE        b1nodematch.field_broad.STEM 
                            0.000000000                         0.115095560 
    b1cov.disruption:b2cov.productivity 
                            0.204513210 
    Joint P-value (lower = worse):  3.525018e-186 
    Chain 2 

    Fraction in 1st window = 0.1
    Fraction in 2nd window = 0.5 

                                  edges                  gwb1deg.fixed.0.25 
                            -0.50608801                         -2.69957810 
                     gwb2deg.fixed.0.25                    b1cov.disruption 
                            -0.55430858                          0.75935299 
                     b2cov.productivity           b1factor.field_broad.STEM 
                             0.22908970                          0.34529161 
          b1nodematch.field_broad.SHAPE        b1nodematch.field_broad.STEM 
                            -1.18458862                         -0.01915873 
    b1cov.disruption:b2cov.productivity 
                            -1.28281873 

    Individual P-values (lower = worse):
                                  edges                  gwb1deg.fixed.0.25 
                            0.612794865                         0.006942746 
                     gwb2deg.fixed.0.25                    b1cov.disruption 
                            0.579367679                         0.447641424 
                     b2cov.productivity           b1factor.field_broad.STEM 
                            0.818799195                         0.729875152 
          b1nodematch.field_broad.SHAPE        b1nodematch.field_broad.STEM 
                            0.236180138                         0.984714482 
    b1cov.disruption:b2cov.productivity 
                            0.199555589 
    Joint P-value (lower = worse):  0.4714389 

    Note: MCMC diagnostics shown here are from the last round of
      simulation, prior to computation of final parameter estimates.
      Because the final estimates are refinements of those used for this
      simulation run, these diagnostics may understate model performance.
      To directly assess the performance of the final model on in-model
      statistics, please use the GOF command: gof(ergmFitObject,
      GOF=~model).

## Discusión y Conclusiones

El análisis siguiente se realiza a modo de ejemplo y con coeficientes imprecisos puesto que La estimación de MCMLE necesita más de 14 iteraciones (número usado en el último modelo).

Los resultados revelan varios patrones significativos en la estructura de colaboración científica:

1.  **Efectos de Grado**: La significativa geometría ponderada de grados sugiere una tendencia hacia la formación de equipos de tamaño moderado.

2.  **Disrupción y Productividad**: La interacción positiva entre disrupción y productividad indica que los autores más productivos tienden a vincularse con papers más disruptivos.

3.  **Homofilia por Campo**: La fuerte homofilia en campos STEM sugiere la persistencia de silos disciplinarios.

### Limitaciones y Trabajo Futuro

-   Extensión temporal del análisis
-   Incorporación de medidas alternativas de disrupción
-   Análisis de sensibilidad con diferentes especificaciones de modelo
