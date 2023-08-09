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

{{< figure src="img/Fig14_A_600.gif" caption="Homophily" >}}

## libraries

``` r
library(ergm)
library(ergm.ego)
library(car)
library(egor)
library(tidyverse)
library(tibble)
library(texreg)
library(prioritizr)
library(questionr)
```

## data

``` r
load("/home/rober/Documents/ricantillan.rbind.io/dat/ELSOC/ELSOC_W02_v3.00_R.RData")
load("/home/rober/Documents/ricantillan.rbind.io/dat/ELSOC/ELSOC_W04_v2.01_R.RData")
load("/home/rober/Documents/ricantillan.rbind.io/dat/ELSOC/ELSOC_W01_v4.01_R.RData")
```

# ELSOC 2017

### Renombrar ID

``` r
a<-elsoc_2017 %>% dplyr::rename(.egoID = idencuesta)
```

# Análisis ELSOC 2017

## Crear data frame alteris para 2017=a

Creamos subset con data de cada uno de los alteris mencionados, manteniendo el ID de cada ego en el cual están anidados. Las columnas de cada uno de los subset deben tener los mismos nombres.

``` r
alter_1<-a %>%
        dplyr::select(.egoID, 
                      sexo=r13_sexo_01, 
                      edad=r13_edad_01, 
                      educ=r13_educ_01,
                      relig=r13_relig_01, 
                      ideol=r13_ideol_01,
                      barrio=r13_barrio_01,
                      rel=r13_relacion_01)
#View(alter_1)
alter_2<-a %>%
        dplyr::select(.egoID, 
                      sexo=r13_sexo_02, 
                      edad=r13_edad_02, 
                      educ=r13_educ_02, 
                      relig=r13_relig_02, 
                      ideol=r13_ideol_02,
                      barrio=r13_barrio_02,
                      rel=r13_relacion_02)

alter_3<-a %>%
        dplyr::select(.egoID, 
                      sexo=r13_sexo_03, 
                      edad=r13_edad_03, 
                      educ=r13_educ_03, 
                      relig=r13_relig_03, 
                      ideol=r13_ideol_03,
                      barrio=r13_barrio_03,
                      rel=r13_relacion_03)

alter_4<- a %>%
        dplyr::select(.egoID, 
                      sexo=r13_sexo_04, 
                      edad=r13_edad_04, 
                      educ=r13_educ_04, 
                      relig=r13_relig_04, 
                      ideol=r13_ideol_04,
                      barrio=r13_barrio_04,
                      rel=r13_relacion_04)

alter_5<-a %>%
        dplyr::select(.egoID, 
                      sexo=r13_sexo_05, 
                      edad=r13_edad_05, 
                      educ=r13_educ_05, 
                      relig=r13_relig_05, 
                      ideol=r13_ideol_05,
                      barrio=r13_barrio_05,
                      rel=r13_relacion_05)
```

## setear

Creamos un vector adicional en cada subset de alteris con un número constante que identifica a que alter representa la data.

``` r
alter_1$n<-1
alter_2$n<-2
alter_3$n<-3
alter_4$n<-4
alter_5$n<-5
```

## Crear base de alteris en formato *long*

Con la función `rbind` agregamos la data hacia abajo en relación al orden establecido por los vectores númericos creados anteriormente. Es necesario que todas las columnas (variables) tengan los mismos nombres. Posteriormente con la función `arrange`, ordenamos la data en orden descendente en función del vector identificador de los egos (respondentes).

``` r
alteris<-rbind(alter_1,alter_2,alter_3,alter_4,alter_5)
alteris<-arrange(alteris, .egoID)
```

## Crear vector alter id

En el siguiente chunk creamos un vector identificador para cada uno de los alteris presentes en la data "alteris". Lo identificamos como objeto `tibble` y eliminamos el vector "n".

``` r
alteris   <- rowid_to_column(alteris, var = ".alterID")
alteris   <- as_tibble(alteris)
alteris$n <- NULL
```

## Recod alteris

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
  dplyr::mutate(rel=case_when(rel%in%1:3~"fam",
                              rel%in%4:5~"nofam"))
table(alteris$rel)
```


      fam nofam 
     4651  3415 

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

## Data Frame Ego's

Creamos un subset con la data de ego equivalente a la data de los alteris. Las nombramos de la misma manera.

``` r
egos <-a %>%
       dplyr::select(.egoID, 
                     sexo=m0_sexo, 
                     edad=m0_edad, 
                     educ=m01, 
                     relig=m38, 
                     ideol=c15)

egos <- as_tibble(egos)
```

## Recod data Ego's

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
egos$barrio<-factor(Recode(egos$barrio,"1=1;0=2"))
table(egos$barrio)
```


       1    2 
    1482  991 

# Crear objeto Egor (requerido para trabajar con función `ergm.ego`)

## Todos los alteris

``` r
elsoc_ego <- egor(alters = alteris, 
                  egos = egos,
                  ID.vars = list(
                    ego = ".egoID",
                    alter = ".alterID"))

elsoc_ego<-as.egor(elsoc_ego)
#View(elsoc_ego$ego)
#View(elsoc_ego$alter)


elsoc_ego[["ego"]]  <-elsoc_ego[["ego"]]%>%drop_na(sexo)
elsoc_ego[["alter"]]<-elsoc_ego[["alter"]]%>%drop_na(sexo)
elsoc_ego[["ego"]]  <-elsoc_ego[["ego"]]%>%drop_na(educ)
elsoc_ego[["alter"]]<-elsoc_ego[["alter"]]%>%drop_na(educ)
elsoc_ego[["ego"]]  <-elsoc_ego[["ego"]]%>%drop_na(relig)
elsoc_ego[["alter"]]<-elsoc_ego[["alter"]]%>%drop_na(relig)
elsoc_ego[["ego"]]  <-elsoc_ego[["ego"]]%>%drop_na(ideol)
elsoc_ego[["alter"]]<-elsoc_ego[["alter"]]%>%drop_na(ideol)
elsoc_ego[["ego"]]<-elsoc_ego[["ego"]]%>%drop_na(barrio)
elsoc_ego[["alter"]]<-elsoc_ego[["alter"]]%>%drop_na(barrio)
```

# Modelos

## Modelo 6

``` r
modelo6<-ergm.ego(elsoc_ego~
                  nodematch("sexo", diff=TRUE),
                  control=control.ergm.ego(ppopsize="samp",
                                           ppop.wt="sample",
                                           stats.wt="data",
                                           stats.est="survey"),
                  ignore.max.alters=T,
                  boot.R = 1000000,
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

    Warning: Argument(s) 'ignore.max.alters', 'boot.R', and 'ergm' were not
    recognized or used. Did you mistype an argument name?

``` r
summary(modelo6)
```

    Call:
    ergm.ego(formula = elsoc_ego ~ nodematch("sexo", diff = TRUE), 
        control = control.ergm.ego(ppopsize = "samp", ppop.wt = "sample", 
            stats.wt = "data", stats.est = "survey"), ignore.max.alters = T, 
        boot.R = 1e+06, ergm = control.ergm(init.method = "MPLE", 
            init.MPLE.samplesize = 5e+07, MPLE.constraints.ignore = TRUE, 
            MCMLE.effectiveSize = NULL, MCMC.burnin = 50000, MCMC.interval = 50000, 
            MCMC.samplesize = 1e+06, parallel = 16, SAN.nsteps = 5e+07))

    Monte Carlo Maximum Likelihood Results:

                        Estimate Std. Error MCMC % z value Pr(>|z|)    
    offset(netsize.adj) -7.79811    0.00000      0    -Inf   <1e-04 ***
    nodematch.sexo.1     1.41764    0.03517      0   40.31   <1e-04 ***
    nodematch.sexo.2     1.08673    0.02140      0   50.77   <1e-04 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


     The following terms are fixed by offset and are not estimated:
      offset(netsize.adj) 

## Modelo 7

``` r
modelo7<-ergm.ego(elsoc_ego~
                    nodematch("sexo", diff=TRUE)
                  + nodematch("educ", diff=TRUE),
                  control=control.ergm.ego(ppopsize="samp",
                                           ppop.wt="sample",
                                           stats.wt="data",
                                           stats.est="survey"),
                  ignore.max.alters=T,
                  boot.R = 1000000,
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

    Warning: Argument(s) 'ignore.max.alters', 'boot.R', and 'ergm' were not
    recognized or used. Did you mistype an argument name?

``` r
summary(modelo7)
```

    Call:
    ergm.ego(formula = elsoc_ego ~ nodematch("sexo", diff = TRUE) + 
        nodematch("educ", diff = TRUE), control = control.ergm.ego(ppopsize = "samp", 
        ppop.wt = "sample", stats.wt = "data", stats.est = "survey"), 
        ignore.max.alters = T, boot.R = 1e+06, ergm = control.ergm(init.method = "MPLE", 
            init.MPLE.samplesize = 5e+07, MPLE.constraints.ignore = TRUE, 
            MCMLE.effectiveSize = NULL, MCMC.burnin = 50000, MCMC.interval = 50000, 
            MCMC.samplesize = 1e+06, parallel = 16, SAN.nsteps = 5e+07))

    Monte Carlo Maximum Likelihood Results:

                        Estimate Std. Error MCMC % z value Pr(>|z|)    
    offset(netsize.adj) -7.79811    0.00000      0    -Inf   <1e-04 ***
    nodematch.sexo.1     1.00413    0.04220      0   23.80   <1e-04 ***
    nodematch.sexo.2     0.62111    0.03159      0   19.66   <1e-04 ***
    nodematch.educ.1     1.02375    0.06650      0   15.40   <1e-04 ***
    nodematch.educ.2     0.83107    0.04366      0   19.03   <1e-04 ***
    nodematch.educ.3     1.09042    0.08167      0   13.35   <1e-04 ***
    nodematch.educ.4     1.88021    0.06496      0   28.94   <1e-04 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


     The following terms are fixed by offset and are not estimated:
      offset(netsize.adj) 

## Modelo 8

``` r
modelo8<-ergm.ego(elsoc_ego~
                    nodematch("sexo", diff=TRUE)
                  + nodematch("educ", diff=TRUE)
                  + nodematch("relig", diff=TRUE),
                  control=control.ergm.ego(ppopsize="samp",
                                           ppop.wt="sample",
                                           stats.wt="data",
                                           stats.est="survey"),
                  ignore.max.alters=T,
                  boot.R = 1000000,
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

    Warning: Argument(s) 'ignore.max.alters', 'boot.R', and 'ergm' were not
    recognized or used. Did you mistype an argument name?

``` r
summary(modelo8)
```

    Call:
    ergm.ego(formula = elsoc_ego ~ nodematch("sexo", diff = TRUE) + 
        nodematch("educ", diff = TRUE) + nodematch("relig", diff = TRUE), 
        control = control.ergm.ego(ppopsize = "samp", ppop.wt = "sample", 
            stats.wt = "data", stats.est = "survey"), ignore.max.alters = T, 
        boot.R = 1e+06, ergm = control.ergm(init.method = "MPLE", 
            init.MPLE.samplesize = 5e+07, MPLE.constraints.ignore = TRUE, 
            MCMLE.effectiveSize = NULL, MCMC.burnin = 50000, MCMC.interval = 50000, 
            MCMC.samplesize = 1e+06, parallel = 16, SAN.nsteps = 5e+07))

    Monte Carlo Maximum Likelihood Results:

                        Estimate Std. Error MCMC % z value Pr(>|z|)    
    offset(netsize.adj) -7.79811    0.00000      0    -Inf   <1e-04 ***
    nodematch.sexo.1     0.62041    0.04460      0  13.912   <1e-04 ***
    nodematch.sexo.2     0.20361    0.03182      0   6.400   <1e-04 ***
    nodematch.educ.1     0.45352    0.06408      0   7.077   <1e-04 ***
    nodematch.educ.2     0.57666    0.04036      0  14.289   <1e-04 ***
    nodematch.educ.3     0.87965    0.08176      0  10.759   <1e-04 ***
    nodematch.educ.4     1.74050    0.06633      0  26.239   <1e-04 ***
    nodematch.relig.1    0.96146    0.03835      0  25.070   <1e-04 ***
    nodematch.relig.2    1.53673    0.05922      0  25.949   <1e-04 ***
    nodematch.relig.3    0.88014    0.14990      0   5.872   <1e-04 ***
    nodematch.relig.4    1.73064    0.08100      0  21.367   <1e-04 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


     The following terms are fixed by offset and are not estimated:
      offset(netsize.adj) 

## Modelo 9

``` r
modelo9<-ergm.ego(elsoc_ego~
                    nodematch("sexo", diff=TRUE)
                  + nodematch("educ", diff=TRUE)
                  + nodematch("relig", diff=TRUE)
                  + nodematch("ideol", diff=TRUE),
                  control=control.ergm.ego(ppopsize="samp",
                                           ppop.wt="sample",
                                           stats.wt="data",
                                           stats.est="survey"),
                  ignore.max.alters=T,
                  boot.R = 1000000,
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

    Warning: Argument(s) 'ignore.max.alters', 'boot.R', and 'ergm' were not
    recognized or used. Did you mistype an argument name?

``` r
summary(modelo9)
```

    Call:
    ergm.ego(formula = elsoc_ego ~ nodematch("sexo", diff = TRUE) + 
        nodematch("educ", diff = TRUE) + nodematch("relig", diff = TRUE) + 
        nodematch("ideol", diff = TRUE), control = control.ergm.ego(ppopsize = "samp", 
        ppop.wt = "sample", stats.wt = "data", stats.est = "survey"), 
        ignore.max.alters = T, boot.R = 1e+06, ergm = control.ergm(init.method = "MPLE", 
            init.MPLE.samplesize = 5e+07, MPLE.constraints.ignore = TRUE, 
            MCMLE.effectiveSize = NULL, MCMC.burnin = 50000, MCMC.interval = 50000, 
            MCMC.samplesize = 1e+06, parallel = 16, SAN.nsteps = 5e+07))

    Monte Carlo Maximum Likelihood Results:

                        Estimate Std. Error MCMC % z value Pr(>|z|)    
    offset(netsize.adj) -7.79811    0.00000      0    -Inf  < 1e-04 ***
    nodematch.sexo.1     0.47684    0.04254      0  11.208  < 1e-04 ***
    nodematch.sexo.2     0.10682    0.03331      0   3.207 0.001340 ** 
    nodematch.educ.1     0.17976    0.06846      0   2.626 0.008646 ** 
    nodematch.educ.2     0.47467    0.04143      0  11.457  < 1e-04 ***
    nodematch.educ.3     1.00939    0.07554      0  13.363  < 1e-04 ***
    nodematch.educ.4     1.60532    0.06193      0  25.920  < 1e-04 ***
    nodematch.relig.1    0.75220    0.03476      0  21.637  < 1e-04 ***
    nodematch.relig.2    1.52857    0.06395      0  23.903  < 1e-04 ***
    nodematch.relig.3    0.80285    0.14385      0   5.581  < 1e-04 ***
    nodematch.relig.4    1.51603    0.08557      0  17.717  < 1e-04 ***
    nodematch.ideol.1    1.40607    0.10486      0  13.409  < 1e-04 ***
    nodematch.ideol.2    0.90588    0.22891      0   3.957  < 1e-04 ***
    nodematch.ideol.3   -0.31396    0.13316      0  -2.358 0.018385 *  
    nodematch.ideol.4    0.67036    0.17397      0   3.853 0.000116 ***
    nodematch.ideol.5    1.37281    0.09895      0  13.873  < 1e-04 ***
    nodematch.ideol.6    0.74416    0.04160      0  17.890  < 1e-04 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


     The following terms are fixed by offset and are not estimated:
      offset(netsize.adj) 

## Modelo 10

``` r
modelo10<-ergm.ego(elsoc_ego~
                    nodematch("sexo", diff=TRUE)
                  + nodematch("educ", diff=TRUE)
                  + nodematch("relig", diff=TRUE)
                  + nodematch("ideol", diff=TRUE)
                  + nodematch("barrio",diff=TRUE),
                  control=control.ergm.ego(ppopsize="samp",
                                           ppop.wt="sample",
                                           stats.wt="data",
                                           stats.est="survey"),
                  ignore.max.alters=T,
                  boot.R = 1000000,
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

    Warning: Argument(s) 'ignore.max.alters', 'boot.R', and 'ergm' were not
    recognized or used. Did you mistype an argument name?

``` r
summary(modelo10)
```

    Call:
    ergm.ego(formula = elsoc_ego ~ nodematch("sexo", diff = TRUE) + 
        nodematch("educ", diff = TRUE) + nodematch("relig", diff = TRUE) + 
        nodematch("ideol", diff = TRUE) + nodematch("barrio", diff = TRUE), 
        control = control.ergm.ego(ppopsize = "samp", ppop.wt = "sample", 
            stats.wt = "data", stats.est = "survey"), ignore.max.alters = T, 
        boot.R = 1e+06, ergm = control.ergm(init.method = "MPLE", 
            init.MPLE.samplesize = 5e+07, MPLE.constraints.ignore = TRUE, 
            MCMLE.effectiveSize = NULL, MCMC.burnin = 50000, MCMC.interval = 50000, 
            MCMC.samplesize = 1e+06, parallel = 16, SAN.nsteps = 5e+07))

    Monte Carlo Maximum Likelihood Results:

                        Estimate Std. Error MCMC % z value Pr(>|z|)    
    offset(netsize.adj) -7.79811    0.00000      0    -Inf  < 1e-04 ***
    nodematch.sexo.1     0.50069    0.04144      0  12.082  < 1e-04 ***
    nodematch.sexo.2     0.14210    0.03368      0   4.219  < 1e-04 ***
    nodematch.educ.1     0.21541    0.06564      0   3.282 0.001031 ** 
    nodematch.educ.2     0.46568    0.03901      0  11.938  < 1e-04 ***
    nodematch.educ.3     0.96468    0.07472      0  12.911  < 1e-04 ***
    nodematch.educ.4     1.67254    0.07020      0  23.827  < 1e-04 ***
    nodematch.relig.1    0.78634    0.03385      0  23.229  < 1e-04 ***
    nodematch.relig.2    1.60068    0.06023      0  26.576  < 1e-04 ***
    nodematch.relig.3    0.91222    0.13699      0   6.659  < 1e-04 ***
    nodematch.relig.4    1.37110    0.09179      0  14.937  < 1e-04 ***
    nodematch.ideol.1    1.61493    0.09927      0  16.268  < 1e-04 ***
    nodematch.ideol.2    0.95378    0.21732      0   4.389  < 1e-04 ***
    nodematch.ideol.3   -0.47730    0.14075      0  -3.391 0.000696 ***
    nodematch.ideol.4    0.64203    0.16228      0   3.956  < 1e-04 ***
    nodematch.ideol.5    1.41057    0.09267      0  15.222  < 1e-04 ***
    nodematch.ideol.6    0.81756    0.04182      0  19.549  < 1e-04 ***
    nodematch.barrio.1  -0.20368    0.03509      0  -5.805  < 1e-04 ***
    nodematch.barrio.2  -0.04030    0.04681      0  -0.861 0.389338    
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


     The following terms are fixed by offset and are not estimated:
      offset(netsize.adj) 

## Modelo 11

``` r
modelo11<-ergm.ego(elsoc_ego~
                    nodematch("sexo", diff=TRUE)
                  + nodematch("educ", diff=TRUE)
                  + nodematch("relig", diff=TRUE)
                  + nodematch("ideol", diff=TRUE)
                  + nodematch("barrio",diff=TRUE)
                  + absdiff("edad"),
                  control=control.ergm.ego(ppopsize="samp",
                                           ppop.wt="sample",
                                           stats.wt="data",
                                           stats.est="survey"),
                  ignore.max.alters=T,
                  boot.R = 1000000,
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

    Warning: Argument(s) 'ignore.max.alters', 'boot.R', and 'ergm' were not
    recognized or used. Did you mistype an argument name?

``` r
summary(modelo11)
```

    Call:
    ergm.ego(formula = elsoc_ego ~ nodematch("sexo", diff = TRUE) + 
        nodematch("educ", diff = TRUE) + nodematch("relig", diff = TRUE) + 
        nodematch("ideol", diff = TRUE) + nodematch("barrio", diff = TRUE) + 
        absdiff("edad"), control = control.ergm.ego(ppopsize = "samp", 
        ppop.wt = "sample", stats.wt = "data", stats.est = "survey"), 
        ignore.max.alters = T, boot.R = 1e+06, ergm = control.ergm(init.method = "MPLE", 
            init.MPLE.samplesize = 5e+07, MPLE.constraints.ignore = TRUE, 
            MCMLE.effectiveSize = NULL, MCMC.burnin = 50000, MCMC.interval = 50000, 
            MCMC.samplesize = 1e+06, parallel = 16, SAN.nsteps = 5e+07))

    Monte Carlo Maximum Likelihood Results:

                         Estimate Std. Error MCMC % z value Pr(>|z|)    
    offset(netsize.adj) -7.798113   0.000000      0    -Inf  < 1e-04 ***
    nodematch.sexo.1     0.604454   0.045362      0  13.325  < 1e-04 ***
    nodematch.sexo.2     0.326204   0.036893      0   8.842  < 1e-04 ***
    nodematch.educ.1     0.241201   0.070365      0   3.428 0.000608 ***
    nodematch.educ.2     0.553463   0.043400      0  12.752  < 1e-04 ***
    nodematch.educ.3     1.053887   0.077430      0  13.611  < 1e-04 ***
    nodematch.educ.4     1.706179   0.067742      0  25.187  < 1e-04 ***
    nodematch.relig.1    0.968363   0.038351      0  25.250  < 1e-04 ***
    nodematch.relig.2    1.633904   0.060964      0  26.801  < 1e-04 ***
    nodematch.relig.3    0.943021   0.139523      0   6.759  < 1e-04 ***
    nodematch.relig.4    1.769135   0.089358      0  19.798  < 1e-04 ***
    nodematch.ideol.1    1.522419   0.102108      0  14.910  < 1e-04 ***
    nodematch.ideol.2    1.031124   0.239581      0   4.304  < 1e-04 ***
    nodematch.ideol.3   -0.073857   0.142029      0  -0.520 0.603054    
    nodematch.ideol.4    0.584127   0.154353      0   3.784 0.000154 ***
    nodematch.ideol.5    1.456021   0.095836      0  15.193  < 1e-04 ***
    nodematch.ideol.6    0.843632   0.043200      0  19.529  < 1e-04 ***
    nodematch.barrio.1  -0.169993   0.036934      0  -4.603  < 1e-04 ***
    nodematch.barrio.2   0.132458   0.049392      0   2.682 0.007323 ** 
    absdiff.edad        -0.023545   0.001331      0 -17.690  < 1e-04 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


     The following terms are fixed by offset and are not estimated:
      offset(netsize.adj) 
