---
date: 2022-10-26

title: homophily
subtitle: Is everywhere!
author: Roberto Cantillan

show_post_date: true
show_author_byline: true

draft: false

summary: |
    In this post I focus on analyzing homophily in a multidimensional way using the `ergm.ego` library, from the `statnet` group. For this I use the data from the Longitudinal Social Study of Chile (ELSOC).

format: hugo

freeze: auto
---

-   [libraries](#libraries)
-   [data](#data)
-   [ELSOC 2017](#elsoc-2017)
    -   [Renombrar ID](#renombrar-id)
    -   [Crear data frame alteris para 2017=a](#crear-data-frame-alteris-para-2017a)
    -   [Crear vector alter id](#crear-vector-alter-id)
    -   [Recod alteris](#recod-alteris)
    -   [Borrar alteris con 5 parámetros con NA](#borrar-alteris-con-5-parámetros-con-na)
    -   [Data Frame Ego's](#data-frame-egos)
    -   [Recod data Ego's](#recod-data-egos)
    -   [Crear objeto Egor (requerido para trabajar con función `ergm.ego`)](#crear-objeto-egor-requerido-para-trabajar-con-función-ergm.ego)
    -   [Degree distribution](#degree-distribution)
-   [Modelos](#modelos)
    -   [Modelo 1](#modelo-1)
        -   [Summary](#summary)
        -   [Bondad de ajuste](#bondad-de-ajuste)
        -   [MCMC](#mcmc)
        -   [simulate](#simulate)
        -   [Tidy](#tidy)
        -   [Predict](#predict)

{{< figure src="img/Fig14_A_600.gif" caption="Homophily" >}}

## libraries

``` r
pacman::p_load(
  ergm,
  ergm.ego,
  car,
  egor,
  tidyverse,
  tibble,
  texreg,
  purrr,
  tidyr,
  prioritizr,
  questionr)
```

## data

``` r
load("/home/rober/Documents/ricantillan.rbind.io/dat/ELSOC/ELSOC_W02_v3.00_R.RData")
load("/home/rober/Documents/ricantillan.rbind.io/dat/ELSOC/ELSOC_W04_v2.01_R.RData")
load("/home/rober/Documents/ricantillan.rbind.io/dat/ELSOC/ELSOC_W01_v4.01_R.RData")
```

## ELSOC 2017

### Renombrar ID

``` r
a<-elsoc_2017 %>% dplyr::rename(.egoID = idencuesta)
```

### Crear data frame alteris para 2017=a

Creamos subset con data de cada uno de los alteris mencionados, manteniendo el ID de cada ego en el cual están anidados. Las columnas de cada uno de los subset deben tener los mismos nombres.

``` r
columnas <- c("sexo", "edad", "educ", "relig", "ideol", "barrio", "relacion")
num_alters <- 5

alter_list <- list()

for (i in 1:num_alters) {
  alter_cols <- paste0("r13_", columnas, "_", sprintf("%02d", i))
  alter <- a %>%
    dplyr::select(.egoID, all_of(alter_cols)) %>%
    rename_with(~ columnas, alter_cols) %>%
    mutate(n = i)
  
  alter_list[[i]] <- alter
}

alteris <- bind_rows(alter_list)
alteris<-arrange(alteris, .egoID)
```

### Crear vector alter id

En el siguiente chunk creamos un vector identificador para cada uno de los alteris presentes en la data "alteris". Lo identificamos como objeto `tibble` y eliminamos el vector "n".

``` r
alteris   <- rowid_to_column(alteris, var = ".alterID")
alteris   <- as_tibble(alteris)
alteris$n <- NULL
```

### Recod alteris

Recodificamos los valores de los atributos de los alteris.

``` r
# NA
alteris[alteris=="-999"]<-NA
alteris[alteris=="-888"]<-NA

# Educación 
#edulab<-c('ltsecondary', 'secondary', 'technicaled', 'collegeed')
alteris$educ <-factor(Recode(alteris$educ ,"1=1;2:3=2;4=3;5=4"))
table(alteris$educ)
```


       1    2    3    4 
    1362 3543 1042 1719 

``` r
# Religión 
#relilab<-c('catholic','evangelical','other','none')
alteris$relig<-factor(Recode(alteris$relig,"1=1;2=2;3:4=4;5=3"))
table(alteris$relig)
```


       1    2    3    4 
    4907 1407  373 1379 

``` r
# Ideología 
#ideolab<-c('rightwinger','centerright','center','centerleft','leftwinger','none')
alteris$ideol<-factor(Recode(alteris$ideol,"1=1;2=2;3=3;4=4;5=5;6=6"))
table(alteris$ideol)
```


       1    2    3    4    5    6 
     786  191  382  303  759 4644 

``` r
# Edad 
alteris$edad<-as.numeric(alteris$edad)
#alteris$edad <-factor(Recode(alteris$edad ,"0:18=1;19:29=2;30:40=3;41:51=4;52:62=5;63:100=6"))

# Sexo 
#sexolab<-c('male','female')
alteris$sexo <-factor(Recode(alteris$sexo ,"1=1;2=2"))
table(alteris$sexo)
```


       1    2 
    3388 4678 

``` r
# Relación
alteris<-alteris%>%
  dplyr::mutate(rel=case_when(relacion%in%1:3~"fam",
                              relacion%in%4:5~"nofam"))
table(alteris$relacion)
```


       1    2    3    4    5 
     951 1181 2519 2577  838 

``` r
# Barrio 
alteris$barrio<-factor(Recode(alteris$barrio,"1=1;2=2"))
table(alteris$barrio)
```


       1    2 
    4271 3788 

``` r
#alteris<-na.omit(alteris)
```

### Borrar alteris con 5 parámetros con NA

``` r
# Función para borrar casos con un número determinado de NA's. 
#delete.na <- function(DF, n=0) {
#  DF[rowSums(is.na(DF)) <= n,]
#}
#
#alteris<-delete.na(alteris, 4) #borro los casos que tienen más de 4 NA.  
```

### Data Frame Ego's

Creamos un subset con la data de ego equivalente a la data de los alteris. Las nombramos de la misma manera.

``` r
egos <-a %>%
       dplyr::select(.egoID, 
                     sexo=m0_sexo, 
                     edad=m0_edad, 
                     educ=m01, 
                     relig=m38, 
                     ideol=c15,
                     ponderador02,
                     estrato,
                     segmento)

egos <- as_tibble(egos)
```

### Recod data Ego's

Recodificamos las variables de la data de ego siguiendo el patrón de la data de alteris.

``` r
# NA
egos[egos=="-999"]<-NA
egos[egos=="-888"]<-NA

# Educación
egos$educ <-factor(Recode(egos$educ,"1:3=1;4:5=2;6:7=3;8:10=4"))
table(egos$educ)
```


       1    2    3    4 
     597 1045  402  429 

``` r
# Religión
egos$relig<-factor(Recode(egos$relig,"1=1;2=2;3:6=3;7:9=4"))
table(egos$relig)
```


       1    2    3    4 
    1383  499  276  308 

``` r
# Ideología
#ideolab2<-c('leftwinger','centerleft','center','centerright','rightwinger','none')
egos$ideol<-factor(Recode(egos$ideol,"0:2=5;3:4=4;5=3;6:7=2;8:10=1;11:12=6"))
table(egos$ideol)
```


       1    2    3    4    5    6 
     252  149  470  209  282 1080 

``` r
# Edad
egos$edad<-as.numeric(egos$edad)
#egos$edad <-factor(Recode(egos$edad,"18=1;19:29=2;30:40=3;41:51=4;52:62=5;63:100=6"))

# Sexo
egos$sexo <-factor(Recode(egos$sexo,"1=1;2=2"))
table(egos$sexo)
```


       1    2 
     951 1522 

``` r
# Barrio
egos$barrio <- matrix(rbinom(2473*5,1,0.6),2473,1) # Criterio minimalista
```

    Warning in matrix(rbinom(2473 * 5, 1, 0.6), 2473, 1): data length differs from
    size of matrix: [12365 != 2473 x 1]

``` r
egos$barrio<-factor(Recode(egos$barrio,"1=1;0=2"))
table(egos$barrio)
```


       1    2 
    1505  968 

### Crear objeto Egor (requerido para trabajar con función `ergm.ego`)

``` r
# definir diseño complejo
elsoc_ego <- egor(alters = alteris, 
                  egos = egos,
                  alter_design = list(max = 5),
                  ID.vars = list(
                    ego = ".egoID",
                    alter = ".alterID")) %>% as.egor()

ego_design(elsoc_ego) <- list(weight = "ponderador02",
                              strata = "estrato",
                              cluster="segmento") 

# eliminar atributos de diseño
elsoc_ego[["ego"]][["variables"]][["ponderador02"]]<-NULL
elsoc_ego[["ego"]][["variables"]][["estrato"]]<-NULL
elsoc_ego[["ego"]][["variables"]][["segmento"]]<-NULL

# drop NA
variables <- c("sexo", "educ", "relig", "ideol", "barrio")
for (variable in variables) {
  elsoc_ego[["ego"]] <- elsoc_ego[["ego"]] %>% drop_na({{ variable }})
  elsoc_ego[["alter"]] <- elsoc_ego[["alter"]] %>% drop_na({{ variable }})
}

as_tibble(elsoc_ego$alter)
```

    # A tibble: 6,849 × 10
       .altID .egoID  sexo   edad educ  relig ideol barrio relacion rel  
       <chr>  <chr>   <fct> <dbl> <fct> <fct> <fct> <fct>     <dbl> <chr>
     1 1      1101011 1        77 2     1     5     1             1 fam  
     2 6      1101012 1        60 2     1     6     1             1 fam  
     3 11     1101013 2        54 1     1     6     1             3 fam  
     4 12     1101013 2        33 2     1     6     1             3 fam  
     5 16     1101021 1        55 2     1     6     1             4 nofam
     6 21     1101022 2        44 4     4     6     1             2 fam  
     7 26     1101023 1        40 2     1     6     1             3 fam  
     8 31     1101032 2        31 4     1     6     1             2 fam  
     9 32     1101032 1        57 2     1     5     1             1 fam  
    10 36     1101033 1        37 2     1     2     1             1 fam  
    # ℹ 6,839 more rows

### Degree distribution

#### Educación

``` r
degreedist(elsoc_ego, by="educ", prob=T, plot = T, weight=TRUE)
```

<img src="index.markdown_strict_files/figure-markdown_strict/unnamed-chunk-11-1.png" width="768" />

#### Sexo

``` r
degreedist(elsoc_ego, by="sexo", prob=T, plot = T, weight=TRUE)
```

<img src="index.markdown_strict_files/figure-markdown_strict/unnamed-chunk-12-1.png" width="768" />

# Modelos

`nodefactor` = controla el grado de los diferentes grupos (ajustando las estimaciones de homofilia por el hecho de que algunos grupos, por ejemplo, los hombres, tienen más vínculos que otros grupos, como las mujeres).

El paquete "ergm" proporciona no sólo estadísticas resumidas sino también valores-p. Sin embargo, como indica Kolaczyk y Csárdi (2020), la justificación teórica para las distribuciones asintóticas chi-cuadrado y F utilizadas por `ergm` para calcular estos valores no se ha establecido hasta la fecha. Por lo tanto, puede ser pertinente interpretar estos valores de manera informal, como estadísticas resumidas adicionales.

``` r
## Set # replicates
reps = 1
## Set ppop size and construction
ppop = 15000
ppopwt = 'round' # Round gives consistent netsize/composition
constraint.formula <- (~ bd(maxout = 5)) # constraint por grado máximo para cada ego. 
```

## Modelo 1

``` r
modelo1<-ergm.ego(elsoc_ego~
                    #edges + 
                    #degree(1:5) +
                    nodefactor("sexo", levels = -2) +
                    nodefactor("educ") +
                    nodefactor("ideol") +
                    nodefactor("relig") +
                    nodematch("sexo", diff=TRUE) +
                    nodematch("educ", diff=TRUE) +
                    nodematch("ideol", diff=TRUE)+
                    nodematch("relig", diff=TRUE)+
                    absdiff("edad"),
                 constraints = constraint.formula,
                 control=control.ergm.ego(ppopsize=5000,
                                         ppop.wt=ppopwt,
                                         stats.wt="data",
                                         stats.est="survey"),
                  ergm = control.ergm(init.method = "MPLE",
                                      init.MPLE.samplesize = 5e7,
                                      MPLE.constraints.ignore = TRUE,
                                      MCMLE.effectiveSize = NULL,
                                      MCMC.burnin = 5e4,
                                      MCMC.interval = 5e4,
                                      MCMC.samplesize = 1000000,
                                      parallel = 16,
                                      SAN.nsteps = 5e7))
```

    Warning: 'glpk' selected as the solver, but package 'Rglpk' is not available;
    falling back to 'lpSolveAPI'. This should be fine unless the sample size and/or
    the number of parameters is very big.

    Warning: Argument(s) 'ergm' were not recognized or used. Did you mistype an
    argument name?

``` r
#?control.ergm.ego
#??ignore.max.alters
```

### Summary

``` r
summary(modelo1)
```

    Call:
    ergm.ego(formula = elsoc_ego ~ nodefactor("sexo", levels = -2) + 
        nodefactor("educ") + nodefactor("ideol") + nodefactor("relig") + 
        nodematch("sexo", diff = TRUE) + nodematch("educ", diff = TRUE) + 
        nodematch("ideol", diff = TRUE) + nodematch("relig", diff = TRUE) + 
        absdiff("edad"), constraints = constraint.formula, control = control.ergm.ego(ppopsize = 5000, 
        ppop.wt = ppopwt, stats.wt = "data", stats.est = "survey"), 
        ergm = control.ergm(init.method = "MPLE", init.MPLE.samplesize = 5e+07, 
            MPLE.constraints.ignore = TRUE, MCMLE.effectiveSize = NULL, 
            MCMC.burnin = 50000, MCMC.interval = 50000, MCMC.samplesize = 1e+06, 
            parallel = 16, SAN.nsteps = 5e+07))

    Monte Carlo Maximum Likelihood Results:

                          Estimate Std. Error MCMC % z value Pr(>|z|)    
    offset(netsize.adj) -8.499e+00  0.000e+00      0    -Inf  < 1e-04 ***
    nodefactor.sexo.1    2.632e-02  3.583e-01      0   0.073 0.941447    
    nodefactor.educ.2    5.089e-01  1.096e-01      0   4.643  < 1e-04 ***
    nodefactor.educ.3    4.535e-01  1.501e-01      0   3.021 0.002516 ** 
    nodefactor.educ.4    4.953e-01  1.780e-01      0   2.783 0.005378 ** 
    nodefactor.ideol.2  -3.233e-01  2.102e-01      0  -1.538 0.124051    
    nodefactor.ideol.3  -3.846e-01  1.840e-01      0  -2.091 0.036539 *  
    nodefactor.ideol.4  -7.388e-02  2.224e-01      0  -0.332 0.739708    
    nodefactor.ideol.5   1.535e-05  2.097e-01      0   0.000 0.999942    
    nodefactor.ideol.6   4.279e-01  1.903e-01      0   2.249 0.024533 *  
    nodefactor.relig.2  -6.594e-01  1.505e-01      0  -4.380  < 1e-04 ***
    nodefactor.relig.3  -2.219e-01  1.673e-01      0  -1.326 0.184746    
    nodefactor.relig.4   2.086e-01  1.581e-01      0   1.320 0.186938    
    nodematch.sexo.1     2.650e-01  3.605e-01      0   0.735 0.462222    
    nodematch.sexo.2     6.053e-01  3.648e-01      0   1.659 0.097033 .  
    nodematch.educ.1     8.236e-01  1.305e-01      0   6.312  < 1e-04 ***
    nodematch.educ.2     3.791e-01  1.118e-01      0   3.392 0.000695 ***
    nodematch.educ.3     6.918e-01  1.362e-01      0   5.078  < 1e-04 ***
    nodematch.educ.4     1.655e+00  1.382e-01      0  11.979  < 1e-04 ***
    nodematch.ideol.1    1.712e+00  2.002e-01      0   8.548  < 1e-04 ***
    nodematch.ideol.2    1.077e+00  3.358e-01      0   3.206 0.001345 ** 
    nodematch.ideol.3   -1.906e-02  2.347e-01      0  -0.081 0.935290    
    nodematch.ideol.4    2.891e-01  2.486e-01      0   1.163 0.244909    
    nodematch.ideol.5    1.554e+00  2.147e-01      0   7.238  < 1e-04 ***
    nodematch.ideol.6    5.515e-01  1.274e-01      0   4.328  < 1e-04 ***
    nodematch.relig.1    8.635e-01  1.365e-01      0   6.324  < 1e-04 ***
    nodematch.relig.2    2.410e+00  1.622e-01      0  14.861  < 1e-04 ***
    nodematch.relig.3    7.853e-01  2.587e-01      0   3.036 0.002399 ** 
    nodematch.relig.4    7.407e-01  1.722e-01      0   4.302  < 1e-04 ***
    absdiff.edad        -3.134e-02  2.153e-03      0 -14.556  < 1e-04 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


     The following terms are fixed by offset and are not estimated:
      offset(netsize.adj) 

Para interpretar los coeficientes, es útil pensar en términos de la probabilidad de que un par dado de nodos tenga un vínculo, condicionada al estado del nodo entre todos los demás pares. Para el término de homofilia de género (segundo orden) en el modelo anterio: El empate entre dos nodos mujeres, casi cuatriplica las probabilidades (odds) de tener un vínculo en la red observada (por su puesto, manteniendo todo lo demás igual).

Vale indicar también que para todas las variables el coeficiente difiere de cero en al menos un error estándar, lo que sugiere algún efecto no trivial de estas variables en la formación de vínculos en la red.

### Bondad de ajuste

La práctica actual para evaluar la bondad de ajuste en modelos ERGM es simular primero numerosos gráficos aleatorios del modelo ajustado y luego comparar varios resúmenes de estos gráficos con los del gráfico observado originalmente. Si las características de los grafos de red observados no coinciden con los valores típicos que surgen de las realizaciones del modelo de gráfico aleatorio ajustado, esto sugiere diferencias sistemáticas entre la clase especificada de modelos y los datos y, por lo tanto, una falta de bondad.

En general, al evaluar la bondad de ajuste en el modelado de redes, los resúmenes de uso común incluyen la distribución de cualquier número de los diversos resúmenes de la estructura de la red: como el grado, la centralidad y la distancia geodésica. Con los ERGMs, sin embargo, una elección natural de resumen son las propias estadísticas $g$ que definen el ERGM (es decir, las llamadas estadísticas suficientes). Para evaluar la bondad de ajuste de nuestro modelo anterior de homofilia, la función `ergm` ejecuta las simulaciones de Monte Carlo necesarias y calcula las comparaciones con la red original en términos de las distribuciones de cada uno de los estadísticos en el modelo.

``` r
plot(gof(modelo1))
```

<img src="index.markdown_strict_files/figure-markdown_strict/unnamed-chunk-16-1.png" width="768" />

Considerando las características particulares capturadas por las estadísticas, el ajuste del modelo es bastante bueno en general, toda vez que las estadísticas observadas están bastante cerca de la mediana de los valores simulados en la mayoría de los casos.

``` r
plot(gof(modelo1, GOF="degree"))
```

<img src="index.markdown_strict_files/figure-markdown_strict/unnamed-chunk-17-1.png" width="768" />

### MCMC

``` r
mcmc.diagnostics(modelo1, which = "plots")
```

<img src="index.markdown_strict_files/figure-markdown_strict/unnamed-chunk-18-1.png" width="768" />

<img src="index.markdown_strict_files/figure-markdown_strict/unnamed-chunk-18-2.png" width="768" />

<img src="index.markdown_strict_files/figure-markdown_strict/unnamed-chunk-18-3.png" width="768" />

<img src="index.markdown_strict_files/figure-markdown_strict/unnamed-chunk-18-4.png" width="768" />

<img src="index.markdown_strict_files/figure-markdown_strict/unnamed-chunk-18-5.png" width="768" />

<img src="index.markdown_strict_files/figure-markdown_strict/unnamed-chunk-18-6.png" width="768" />

<img src="index.markdown_strict_files/figure-markdown_strict/unnamed-chunk-18-7.png" width="768" />

<img src="index.markdown_strict_files/figure-markdown_strict/unnamed-chunk-18-8.png" width="768" />

<img src="index.markdown_strict_files/figure-markdown_strict/unnamed-chunk-18-9.png" width="768" />

<img src="index.markdown_strict_files/figure-markdown_strict/unnamed-chunk-18-10.png" width="768" />


    Note: To save space, only one in every 2 iterations of the MCMC sample
      used for estimation was stored for diagnostics. Sample size per chain
      was originally around 6634 with thinning interval 16384.

    Note: MCMC diagnostics shown here are from the last round of
      simulation, prior to computation of final parameter estimates.
      Because the final estimates are refinements of those used for this
      simulation run, these diagnostics may understate model performance.
      To directly assess the performance of the final model on in-model
      statistics, please use the GOF command: gof(ergmFitObject,
      GOF=~model).

### simulate

``` r
sim.modelo1 <- simulate(modelo1, popsize=500,
                       control=control.simulate.ergm.ego(
                       simulate=control.simulate.formula(MCMC.burnin=2e6)))
```

    Note: Constructed network has size 244 different from requested 500. Simulated statistics may need to be rescaled.

``` r
plot(sim.modelo1, vertex.col="educ")
legend('bottomleft',fill=1:4,legend=paste('Eduación',1:4),cex=0.75)
```

<img src="index.markdown_strict_files/figure-markdown_strict/unnamed-chunk-20-1.png" width="768" />

### Tidy

``` r
broom::tidy(modelo1, exponentiate = TRUE, conf.int = TRUE, conf.level = 0.99)
```

    Warning in tidy.ergm(modelo1, exponentiate = TRUE, conf.int = TRUE, conf.level
    = 0.99): Exponentiating but model didn't use log or logit link.

    # A tibble: 30 × 8
       term      estimate std.error mcmc.error  statistic p.value conf.low conf.high
       <chr>        <dbl>     <dbl>      <dbl>      <dbl>   <dbl>    <dbl>     <dbl>
     1 offset(n… 0.000204     0              0 -Inf       0       0.000204  0.000204
     2 nodefact… 1.03         0.358          0    7.35e-2 9.41e-1 0.408     2.58    
     3 nodefact… 1.66         0.110          0    4.64e+0 3.43e-6 1.25      2.21    
     4 nodefact… 1.57         0.150          0    3.02e+0 2.52e-3 1.07      2.32    
     5 nodefact… 1.64         0.178          0    2.78e+0 5.38e-3 1.04      2.60    
     6 nodefact… 0.724        0.210          0   -1.54e+0 1.24e-1 0.421     1.24    
     7 nodefact… 0.681        0.184          0   -2.09e+0 3.65e-2 0.424     1.09    
     8 nodefact… 0.929        0.222          0   -3.32e-1 7.40e-1 0.524     1.65    
     9 nodefact… 1.00         0.210          0    7.32e-5 1.00e+0 0.583     1.72    
    10 nodefact… 1.53         0.190          0    2.25e+0 2.45e-2 0.940     2.50    
    # ℹ 20 more rows

### Predict

``` r
#predict(modelo1, type = "response")
```
