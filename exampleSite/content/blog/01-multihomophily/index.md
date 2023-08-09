---
date: 2023-08-09

title: EI Index
subtitle: Homophily
author: Roberto Cantillan

show_post_date: true
show_author_byline: true

draft: false

summary: |
    En el presente documento se realiza un trabajo de datos para construir una base en formato long con la encuesta COES en su ola w2 y ola w4. Adicionalmente, se analizan datos panel con el modelo within-between (Bell & Jhones, 2014; Bell et al. 2019), ideales para el análisis de estructuras jerárquicas de datos, incluidos los datos datos de series de tiempo (de corte transversal), y de tipo panel. 

format: hugo

freeze: auto
---

En el presente documento se realiza un trabajo de datos para construir una base en formato long con la encuesta COES en su ola w2 y ola w4. Adicionalmente, se analizan datos panel con el modelo within-between (Bell & Jhones, 2014; Bell et al. 2019), ideales para el análisis de estructuras jerárquicas de datos, incluidos los datos datos de series de tiempo (de corte transversal), y de tipo panel.

El objetivo es explorar potenciales relaciones entre indicadores de homofilia y algunos indicadores de comportamiento cívico. El análisis de datos es realizado con el paquete panelr, diseñado para entorno R, el cual tiene como base el paquete lm4. Se prefiere el primero puesto que se reducen sustantivamente procedimientos y lineas de códigos y se obtienen los mismos resultados que si se realizará con el pquete lm4.

Para revisar la base teórica del E-I index revisar el siguiente [link](https://redeslab.gitlab.io/manuales/ei_long_elsoc.html)

Para atributos categóricos de los alter de la red ego, además de la proporción de alter similares a ego, una medida generalmente usada ha sido el índice EI (Krackhardt & Stern, 1988, Perry et al., 2018). Esta medida se define como el número de alter diferentes de ego (lazos externos E) menos el número de alter iguales a ego (lazos internos I), dividido por el número de alter. Esta es una medida "reversa" de homofilia toa vez que una medida alta de este índice índica mayor heterofilia. Además, debido a que es una transformación lineal de la medida de proporción de lazos homofilicos, su correlación es un perfecto -.1
