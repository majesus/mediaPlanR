# mediaPlanR

Herramientas en R para la planificación de medios publicitarios: modelos
clásicos de cobertura y distribución de contactos, indicadores de eficiencia
de un plan y aplicaciones Shiny para explorarlos de forma interactiva.

## Instalación

```r
# install.packages("devtools")
devtools::install_github("majesus/mediaPlanR")
```

## Qué incluye

**Modelos de cobertura y distribución de contactos**

- `calc_sainsbury()` — duplicación aleatoria, soportes heterogéneos
- `calc_binomial()` — duplicación aleatoria, soporte promedio homogéneo
- `calc_beta_binomial()` — heterogeneidad individual (un soporte, n inserciones)
- `calc_nbd()` — heterogeneidad vía mezcla Poisson-Gamma (Binomial Negativa), alternativa al Beta-Binomial
- `calc_metheringham()` — duplicación media observada entre soportes
- `calc_hofmans()` — curva empírica de audiencia acumulada
- `calc_agostini()` — duplicación aleatoria corregida con un coeficiente empírico k
- `calc_MBBD()` — Beta-Binomial calibrado contra una estimación de Morgensztern
- `calc_canex()` — expansión canónica multivariante (Danaher, 1991), con
  matriz de correlaciones entre vehículos

**Indicadores y optimización**

- `calc_grps()`, `calc_cpm()`, `calcular_metricas_medios()`
- `calcular_roas()` — ROAS/ROI a partir de un embudo de conversión
- `optimizar_d()`, `optimizar_dc()`, `optimize_media_sb()` — optimización de
  planes de medios sujetos a restricciones presupuestarias y de frecuencia
  efectiva

**Aplicaciones interactivas (Shiny)**

- `run_canex_explorer()`, `run_beta_binomial_explorer()`,
  `run_reach_converg_explorer()`, `run_aud_util_explorer()`

## Uso rápido

```r
library(mediaPlanR)

audiencias <- c(300000, 400000, 200000)
resultado <- calc_sainsbury(audiencias, pob_total = 1000000)
resultado
```

Para una introducción más completa, con ejemplos de todos los modelos:

```r
vignette("mediaPlanR-intro", package = "mediaPlanR")
```

## Referencias principales

- Aldás Manzano, J. (1998). *Modelos de determinación de la cobertura y la
  distribución de contactos en la planificación de medios publicitarios
  impresos*. Tesis doctoral, Universidad de Valencia.
- Agostini, J. M. (1961). How to estimate unduplicated audiences. *Journal
  of Advertising Research*, 1(3), 11-14.
- Danaher, P. J. (1991). A canonical expansion model for multivariate media
  exposure distributions. *Journal of Marketing Research*, 28(3), 361-367.
- Metheringham, R. A. (1964). Measuring the net cumulative coverage of a
  print campaign. *Journal of Advertising Research*, 4(4), 23-28.
- Leckenby, J. D., & Kishi, S. (1982). Performance of exposure distribution
  models. *Journal of Advertising Research*, 22(2), 35-44.

## Autoría

Manuel J. Sánchez-Franco, Universidad de Sevilla ([majesus@us.es](mailto:majesus@us.es))
— [ORCID 0000-0002-8042-3550](https://orcid.org/0000-0002-8042-3550)

## Licencia

MIT © Manuel J. Sánchez-Franco (ver [LICENSE](LICENSE.md))
