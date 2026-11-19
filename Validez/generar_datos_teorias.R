# =============================================================================
# generar_datos_teorias.R
#
# Simulación para el ejemplo "Comparar teorías explicativas de la
# informalidad con SEM: modelo de medición y modelo de explicación".
#
# Ejecutar ANTES de compilar manual_teorias_informalidad.Rmd:
#     source("generar_datos_teorias.R")
#
# Genera:
#   data/teorias_informalidad.csv  datos que analiza el manual
#   data/parametros_teorias.rds    valores verdaderos (para comparar)
# =============================================================================

dir.create("data", showWarnings = FALSE)

# -----------------------------------------------------------------------------
# La historia que simulamos
# -----------------------------------------------------------------------------
# INF es la informalidad laboral de cada trabajador (variable latente). Se mide
# con seis indicadores, como en el manual principal.
#
# Tres teorías compiten para explicar por qué un trabajador es informal:
#
#   DUALISTA (exclusión):     menor capital humano -> informalidad
#                             causas: escolaridad (-), edad
#   ESTRUCTURALISTA:          la estructura productiva -> informalidad
#                             causas: trabajar en microempresa (+),
#                                     sector de baja productividad (+)
#   LEGALISTA (escape):       altos costos de la formalidad -> informalidad
#                             causa: costo de trámites en el municipio (+)
#
# EN LA SIMULACIÓN, la verdad es una combinación:
#   - las causas dualistas y estructuralistas SÍ influyen en INF;
#   - el costo de trámites NO influye en INF (efecto verdadero = 0),
#     pero está correlacionado con la presencia de microempresas.
#     Por eso, analizado por separado, parecerá que importa.
#
# Simplificación: los indicadores se generan como respuestas latentes
# continuas (en una encuesta real serían binarios).
# -----------------------------------------------------------------------------

set.seed(515)
N <- 2000

# Efectos verdaderos sobre INF
p <- list(
  b_educ   = -0.06,  # por año de escolaridad
  b_edad   =  0.01,  # por año de edad
  b_micro  =  0.60,  # trabajar en microempresa (hasta 5 personas)
  b_sector =  0.40,  # sector de baja productividad (agro, comercio al por menor, construcción)
  b_costo  =  0.00,  # costo de trámites: SIN efecto verdadero
  sd_resid =  0.75,  # variación de INF no explicada por las causas
  cargas   = c(sin_ss = .80, sin_salud = .75, sin_prest = .70,
               sin_contrato = .70, pago_efec = .65, sin_registro = .60)
)

# Paso 1. Causas (variables exógenas)
educ   <- pmin(pmax(round(rnorm(N, 10, 3.5)), 0), 22)    # años de escolaridad
edad   <- round(runif(N, 18, 65))                        # años

# Costo de trámites en el municipio (días para abrir un negocio, estandarizado
# para que tenga media 0 y desviación 1)
costo  <- rnorm(N)

# Microempresa: más probable con menos escolaridad y donde los trámites cuestan más
prob_micro <- plogis(-0.3 - 0.12 * (educ - 10) + 1.2 * costo)
micro  <- rbinom(N, 1, prob_micro)

# Sector de baja productividad: más probable con menos escolaridad
prob_sector <- plogis(-0.4 - 0.10 * (educ - 10))
sector <- rbinom(N, 1, prob_sector)

# Paso 2. Informalidad latente
INF <- p$b_educ * educ + p$b_edad * edad + p$b_micro * micro +
       p$b_sector * sector + p$b_costo * costo +
       rnorm(N, 0, p$sd_resid)

# Paso 3. Indicadores (cada uno con varianza aproximada de 1)
INF_std <- as.vector(scale(INF))
indicador <- function(carga) carga * INF_std + rnorm(N, 0, sqrt(1 - carga^2))

d <- data.frame(
  sin_ss       = indicador(p$cargas["sin_ss"]),
  sin_salud    = indicador(p$cargas["sin_salud"]),
  sin_prest    = indicador(p$cargas["sin_prest"]),
  sin_contrato = indicador(p$cargas["sin_contrato"]),
  pago_efec    = indicador(p$cargas["pago_efec"]),
  sin_registro = indicador(p$cargas["sin_registro"]),
  educ = educ, edad = edad, micro = micro, sector = sector,
  costo_tramites = round(costo, 3)
)
d[, 1:6] <- round(d[, 1:6], 3)

# Guardamos también la escala de INF para poder comparar coeficientes
p$sd_INF <- sd(INF)

write.csv(d, "data/teorias_informalidad.csv", row.names = FALSE)
saveRDS(p, "data/parametros_teorias.rds")

message("Datos generados: data/teorias_informalidad.csv, data/parametros_teorias.rds")
