---
date: 2022-10-26

title: homophily
subtitle: Is everywhere!
author: Roberto Cantillan

show_post_date: true
show_author_byline: true

draft: false

summary: |
    Follow me on a journey to build this website. The idea is to have a 
    system that has the fewest steps as  possible to go from a blog post on 
    my computer to a website living online. Among other, I discuss about 
    Quarto, Hugo, Github, and Netlify.

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
    1489  984 

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
    nodematch.sexo.1     1.55069    0.03335      0   46.50   <1e-04 ***
    nodematch.sexo.2     1.00902    0.02726      0   37.01   <1e-04 ***
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
    nodematch.sexo.1     0.82783    0.04031      0   20.54   <1e-04 ***
    nodematch.sexo.2     0.74526    0.03141      0   23.73   <1e-04 ***
    nodematch.educ.1     0.73462    0.06671      0   11.01   <1e-04 ***
    nodematch.educ.2     0.89934    0.04072      0   22.08   <1e-04 ***
    nodematch.educ.3     1.22497    0.08336      0   14.70   <1e-04 ***
    nodematch.educ.4     1.86890    0.06688      0   27.95   <1e-04 ***
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
    nodematch.sexo.1     0.63415    0.04094      0  15.490   <1e-04 ***
    nodematch.sexo.2     0.22226    0.03093      0   7.185   <1e-04 ***
    nodematch.educ.1     0.47147    0.06440      0   7.321   <1e-04 ***
    nodematch.educ.2     0.51774    0.04370      0  11.849   <1e-04 ***
    nodematch.educ.3     1.00503    0.07726      0  13.008   <1e-04 ***
    nodematch.educ.4     1.69707    0.06201      0  27.367   <1e-04 ***
    nodematch.relig.1    0.88441    0.03428      0  25.798   <1e-04 ***
    nodematch.relig.2    1.72815    0.06308      0  27.398   <1e-04 ***
    nodematch.relig.3    0.94944    0.14308      0   6.636   <1e-04 ***
    nodematch.relig.4    1.63248    0.08149      0  20.033   <1e-04 ***
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
    nodematch.sexo.1     0.43075    0.04472      0   9.633  < 1e-04 ***
    nodematch.sexo.2     0.14857    0.03228      0   4.602  < 1e-04 ***
    nodematch.educ.1     0.25613    0.06661      0   3.845  0.00012 ***
    nodematch.educ.2     0.37488    0.04171      0   8.988  < 1e-04 ***
    nodematch.educ.3     0.98314    0.07498      0  13.112  < 1e-04 ***
    nodematch.educ.4     1.64597    0.06523      0  25.233  < 1e-04 ***
    nodematch.relig.1    0.76542    0.03548      0  21.574  < 1e-04 ***
    nodematch.relig.2    1.43122    0.06084      0  23.526  < 1e-04 ***
    nodematch.relig.3    0.88346    0.15039      0   5.874  < 1e-04 ***
    nodematch.relig.4    1.55015    0.08172      0  18.969  < 1e-04 ***
    nodematch.ideol.1    1.32334    0.10028      0  13.196  < 1e-04 ***
    nodematch.ideol.2    0.92788    0.22549      0   4.115  < 1e-04 ***
    nodematch.ideol.3   -0.13939    0.14182      0  -0.983  0.32569    
    nodematch.ideol.4    0.26421    0.16369      0   1.614  0.10652    
    nodematch.ideol.5    1.34040    0.09518      0  14.082  < 1e-04 ***
    nodematch.ideol.6    0.79616    0.04208      0  18.918  < 1e-04 ***
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
    nodematch.sexo.1     0.48639    0.04303      0  11.304  < 1e-04 ***
    nodematch.sexo.2     0.14117    0.02998      0   4.708  < 1e-04 ***
    nodematch.educ.1     0.22117    0.06527      0   3.388 0.000703 ***
    nodematch.educ.2     0.48385    0.04210      0  11.494  < 1e-04 ***
    nodematch.educ.3     0.99362    0.07746      0  12.828  < 1e-04 ***
    nodematch.educ.4     1.54289    0.06339      0  24.339  < 1e-04 ***
    nodematch.relig.1    0.73869    0.03537      0  20.882  < 1e-04 ***
    nodematch.relig.2    1.66566    0.06864      0  24.266  < 1e-04 ***
    nodematch.relig.3    0.66895    0.14853      0   4.504  < 1e-04 ***
    nodematch.relig.4    1.60469    0.08516      0  18.844  < 1e-04 ***
    nodematch.ideol.1    1.62981    0.09615      0  16.951  < 1e-04 ***
    nodematch.ideol.2    0.70166    0.22958      0   3.056 0.002241 ** 
    nodematch.ideol.3   -0.46476    0.13827      0  -3.361 0.000776 ***
    nodematch.ideol.4    0.41143    0.15408      0   2.670 0.007580 ** 
    nodematch.ideol.5    1.46219    0.10075      0  14.513  < 1e-04 ***
    nodematch.ideol.6    0.90205    0.04395      0  20.523  < 1e-04 ***
    nodematch.barrio.1  -0.21900    0.03515      0  -6.231  < 1e-04 ***
    nodematch.barrio.2   0.07977    0.04506      0   1.770 0.076674 .  
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
    nodematch.sexo.1     0.719082   0.044632      0  16.111  < 1e-04 ***
    nodematch.sexo.2     0.201656   0.035331      0   5.708  < 1e-04 ***
    nodematch.educ.1     0.455178   0.063237      0   7.198  < 1e-04 ***
    nodematch.educ.2     0.496877   0.042349      0  11.733  < 1e-04 ***
    nodematch.educ.3     0.955235   0.080841      0  11.816  < 1e-04 ***
    nodematch.educ.4     1.633114   0.068792      0  23.740  < 1e-04 ***
    nodematch.relig.1    0.936694   0.040772      0  22.974  < 1e-04 ***
    nodematch.relig.2    1.635200   0.066035      0  24.763  < 1e-04 ***
    nodematch.relig.3    0.982851   0.143586      0   6.845  < 1e-04 ***
    nodematch.relig.4    1.713260   0.086886      0  19.719  < 1e-04 ***
    nodematch.ideol.1    2.086729   0.108407      0  19.249  < 1e-04 ***
    nodematch.ideol.2    0.830460   0.222977      0   3.724 0.000196 ***
    nodematch.ideol.3   -0.381333   0.138407      0  -2.755 0.005867 ** 
    nodematch.ideol.4    0.716717   0.161950      0   4.426  < 1e-04 ***
    nodematch.ideol.5    1.380423   0.099085      0  13.932  < 1e-04 ***
    nodematch.ideol.6    0.855199   0.046575      0  18.362  < 1e-04 ***
    nodematch.barrio.1  -0.170762   0.036677      0  -4.656  < 1e-04 ***
    nodematch.barrio.2   0.281022   0.047696      0   5.892  < 1e-04 ***
    absdiff.edad        -0.021904   0.001297      0 -16.889  < 1e-04 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


     The following terms are fixed by offset and are not estimated:
      offset(netsize.adj) 
