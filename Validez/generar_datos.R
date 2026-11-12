# =============================================================================
# generar_datos.R
# Simulación de datos para el manual "Validez de constructo con SEM:
# el caso de la informalidad laboral"
#
# Genera cinco bases en ./data/ y un archivo con los parámetros verdaderos.
# Ejecutar ANTES de compilar manual_validez_sem.Rmd:
#     source("generar_datos.R")
#
# Simplificación: los indicadores se generan como "respuestas latentes"
# continuas (y*) con varianza 1. En encuestas reales (p. ej. ENOE) suelen ser
# binarios; para el caso 1 también se guarda una versión dicotomizada que el
# manual analiza en un apéndice con WLSMV.
# =============================================================================

N <- 2000   # cada caso fija su propia semilla para ser reproducible por separado
dir.create("data", showWarnings = FALSE)

# -----------------------------------------------------------------------------
# Función auxiliar: genera un indicador con varianza total 1
#   x = sum_k lambda_k * F_k + e,  Var(e) = 1 - sum_k lambda_k^2
#   (los factores F_k deben ser ortogonales y de varianza 1)
# -----------------------------------------------------------------------------
indicador <- function(factores, cargas) {
  comun <- as.vector(as.matrix(factores) %*% cargas)
  var_e <- 1 - sum(cargas^2)
  stopifnot(var_e > 0)
  comun + rnorm(length(comun), 0, sqrt(var_e))
}

# =============================================================================
# CASO 1. Pérdida de independencia local
# -----------------------------------------------------------------------------
# INF  = informalidad laboral (constructo de interés)
# AUTO = "autoempleo / trabajo independiente": un segundo factor, ortogonal a
#        INF por diseño, que genera covarianza sistemática SOLO entre los
#        indicadores contractuales/de pago. Un trabajador por cuenta propia no
#        tiene contrato escrito, cobra en efectivo y no registra su unidad
#        económica aunque no sea "más informal" en el sentido teórico.
# =============================================================================
set.seed(101)
INF  <- rnorm(N)
AUTO <- rnorm(N)
F1   <- cbind(INF, AUTO)

cargas_c1 <- rbind(
  sin_ss       = c(0.80, 0.00),  # no cotiza a seguridad social
  sin_salud    = c(0.75, 0.00),  # sin acceso a servicios de salud por trabajo
  sin_prest    = c(0.70, 0.00),  # sin aguinaldo / vacaciones pagadas
  sin_contrato = c(0.55, 0.55),  # sin contrato escrito
  pago_efec    = c(0.50, 0.60),  # pago en efectivo
  sin_registro = c(0.50, 0.50)   # unidad económica sin registro fiscal
)
colnames(cargas_c1) <- c("INF", "AUTO")

d1 <- as.data.frame(sapply(rownames(cargas_c1),
                           function(v) indicador(F1, cargas_c1[v, ])))

# Versión binaria (prevalencias aproximadas a las de México, ~50-60 %)
prev <- c(sin_ss = .55, sin_salud = .57, sin_prest = .60,
          sin_contrato = .52, pago_efec = .48, sin_registro = .50)
d1_bin <- as.data.frame(lapply(names(prev), function(v)
  as.integer(d1[[v]] > qnorm(1 - prev[v]))))
names(d1_bin) <- names(prev)

write.csv(d1,     "data/c1_dependencia_local.csv",         row.names = FALSE)
write.csv(d1_bin, "data/c1_dependencia_local_binaria.csv", row.names = FALSE)

# Puntajes latentes verdaderos (sólo existen en simulación; permiten medir
# cuánto de cada puntaje estimado es informalidad y cuánto autoempleo)
write.csv(data.frame(INF = INF, AUTO = AUTO),
          "data/c1_latentes_verdaderas.csv", row.names = FALSE)

# =============================================================================
# CASO 2. Unidimensionalidad vs. multidimensionalidad
# -----------------------------------------------------------------------------
# Hipótesis del investigador: la informalidad tiene dos dimensiones
#   PROT  = protección social     (sin_ss, sin_salud, sin_prest)
#   LEGAL = legal-contractual     (sin_contrato, pago_efec, sin_registro)
# (a) Datos verdaderos: UN solo factor  -> rho(PROT, LEGAL) = 1
# (b) Contraste: dos factores reales con rho = 0.45
# =============================================================================
cargas_c2 <- c(sin_ss = .80, sin_salud = .75, sin_prest = .70,
               sin_contrato = .70, pago_efec = .65, sin_registro = .60)

set.seed(202)
INF2 <- rnorm(N)
d2_uni <- as.data.frame(sapply(names(cargas_c2),
                               function(v) indicador(cbind(INF2), cargas_c2[v])))

rho_contraste <- 0.45
PROT  <- rnorm(N)
LEGAL <- rho_contraste * PROT + sqrt(1 - rho_contraste^2) * rnorm(N)
d2_bi <- data.frame(
  sin_ss       = indicador(cbind(PROT),  cargas_c2["sin_ss"]),
  sin_salud    = indicador(cbind(PROT),  cargas_c2["sin_salud"]),
  sin_prest    = indicador(cbind(PROT),  cargas_c2["sin_prest"]),
  sin_contrato = indicador(cbind(LEGAL), cargas_c2["sin_contrato"]),
  pago_efec    = indicador(cbind(LEGAL), cargas_c2["pago_efec"]),
  sin_registro = indicador(cbind(LEGAL), cargas_c2["sin_registro"])
)

write.csv(d2_uni, "data/c2_unidimensional.csv",           row.names = FALSE)
write.csv(d2_bi,  "data/c2_contraste_bidimensional.csv", row.names = FALSE)

# =============================================================================
# CASO 3. Validez nomológica: ¿predice el constructo lo que la teoría dice,
#         CONDICIONAL en los controles?
# -----------------------------------------------------------------------------
# Ecuación salarial tipo Mincer con penalización por informalidad:
#   ln w = 3.0 + 0.08 educ + 0.03 exper - 0.05 (exper^2/100)
#          - 0.10 mujer + beta_INF * INF + e
# INF tiene varianza total 1 y depende de la escolaridad (selección).
#
# (a) Los indicadores miden INF (laborales).
# (b) Los indicadores miden MARG, un factor de "marginación del hogar"
#     fuertemente ligado a la escolaridad, pero SIN efecto propio sobre el
#     salario una vez que se condiciona en educ. El modelo de medida ajusta
#     igual de bien; la validez nomológica, no.
# =============================================================================
beta_INF <- -0.25        # penalización: -25 log-puntos por 1 DE de informalidad
gamma_educ_INF <- -0.45  # efecto de educ estandarizada sobre INF
gamma_educ_MARG <- -0.75 # efecto de educ estandarizada sobre MARG

gen_controles <- function(n) {
  educ  <- pmin(pmax(round(rnorm(n, 10, 3.5)), 0), 22)
  exper <- round(runif(n, 0, 40))
  mujer <- rbinom(n, 1, 0.45)
  data.frame(educ = educ, exper = exper, exper2 = exper^2 / 100, mujer = mujer)
}

gen_salario <- function(ctrl, INF) {
  3.0 + 0.08 * ctrl$educ + 0.03 * ctrl$exper - 0.05 * ctrl$exper2 -
    0.10 * ctrl$mujer + beta_INF * INF + rnorm(nrow(ctrl), 0, 0.45)
}

# Latente con varianza total 1, dependiente de la escolaridad estandarizada
latente_dep_educ <- function(educ, gamma, extra = 0) {
  z <- as.vector(scale(educ))
  sistematica <- gamma * z + extra
  sistematica + rnorm(length(z), 0, sqrt(1 - var(sistematica)))
}

# ---- (a) indicadores laborales de informalidad -------------------------------
set.seed(303)
ctrl_a <- gen_controles(N)
INF3a  <- latente_dep_educ(ctrl_a$educ, gamma_educ_INF,
                           extra = 0.15 * (ctrl_a$mujer - mean(ctrl_a$mujer)))
d3a <- cbind(
  ctrl_a,
  ln_w = gen_salario(ctrl_a, INF3a),
  as.data.frame(sapply(names(cargas_c2),
                       function(v) indicador(cbind(INF3a), cargas_c2[v])))
)

# ---- (b) indicadores de marginación del hogar ---------------------------------
ctrl_b <- gen_controles(N)
INF3b  <- latente_dep_educ(ctrl_b$educ, gamma_educ_INF,
                           extra = 0.15 * (ctrl_b$mujer - mean(ctrl_b$mujer)))
MARG   <- latente_dep_educ(ctrl_b$educ, gamma_educ_MARG)  # indep. de INF dado educ
cargas_marg <- c(sin_salud_hogar = .80, usa_efectivo = .75, sin_cuenta = .70,
                 sin_internet = .70, viv_precaria = .65, sin_credito = .60)
d3b <- cbind(
  ctrl_b,
  ln_w = gen_salario(ctrl_b, INF3b),   # el salario depende de INF, NO de MARG
  as.data.frame(sapply(names(cargas_marg),
                       function(v) indicador(cbind(MARG), cargas_marg[v])))
)

write.csv(d3a, "data/c3a_informalidad_salarios.csv", row.names = FALSE)
write.csv(d3b, "data/c3b_marginacion_salarios.csv",  row.names = FALSE)

# =============================================================================
# Parámetros verdaderos (para comparar estimaciones en el manual)
# =============================================================================
saveRDS(list(
  N               = N,
  cargas_c1       = cargas_c1,
  cargas_c2       = cargas_c2,
  rho_contraste   = rho_contraste,
  beta_INF        = beta_INF,
  gamma_educ_INF  = gamma_educ_INF,
  gamma_educ_MARG = gamma_educ_MARG,
  cargas_marg     = cargas_marg
), "data/parametros_verdaderos.rds")

message("Datos generados en ./data/: ",
        paste(list.files("data"), collapse = ", "))
