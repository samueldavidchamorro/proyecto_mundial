# -*- coding: utf-8 -*-
# ============================================================
# Shiny App: Simulador FIFA World Cup 2026
# Modelo Poisson . Fase de Grupos . Monte Carlo
# Metodologia: Dr. Lihki Rubio
# ============================================================

library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(scales)
library(stringr)
library(DT)
library(httr2)

# ============================================================
# CHATBOT GEMINI - Asistente del Mundial 2026
# ============================================================
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

GEMINI_MODEL <- "gemini-2.5-flash"   # modelo gratis; cambia aqui si quieres otro

CHAT_SYSTEM_BASE <- paste(
  "Eres 'Asistente Mundial 2026', un chatbot dentro de una app Shiny que simula",
  "la fase de grupos y eliminatorias del Mundial FIFA 2026.",
  "Respondes SIEMPRE en espanol, breve, claro y amigable (2-4 frases salvo que pidan mas detalle).",
  "Tu fuente de verdad son los DATOS incluidos abajo, que reflejan lo que el usuario ya calculo en la app.",
  "REGLAS:",
  "1) No inventes numeros: usa solo los de los DATOS.",
  "2) Si preguntan por clasificados, probabilidades de clasificar o de campeon y ese dato NO aparece abajo,",
  "   indica amablemente que primero corran esa simulacion en la pestana correspondiente",
  "   (Simulacion / Probabilidades / Eliminacion Directa).",
  "3) Metodologia: cada equipo tiene una fuerza segun su ranking FIFA; los anfitriones (USA, Canada, Mexico)",
  "   tienen una pequena ventaja; los goles se generan con azar realista; victoria=3, empate=1, derrota=0;",
  "   clasifican los 2 primeros de cada grupo mas los 8 mejores terceros = 32 equipos a dieciseisavos.",
  "4) Si la pregunta no tiene que ver con el Mundial o la app, responde corto y reconduce con amabilidad.",
  sep = "\n"
)

# ULTIMO RECURSO: si el .Renviron no carga, pega aqui tu clave entre comillas.
# Ej: GEMINI_API_KEY_FALLBACK <- "AQ.xxxxxxxx"
GEMINI_API_KEY_FALLBACK <- "AQ.Ab8RN6JC7QzTVPPasR6j4x0Y9TqV4JsKxCXf8deZWYgseYjtew"

# Busca la clave de forma robusta: entorno -> .Renviron (o .Renviron.txt) -> fallback
load_gemini_key <- function() {
  k <- Sys.getenv("GEMINI_API_KEY")
  if (!identical(k, "")) return(k)
  
  cand <- c(".Renviron", ".Renviron.txt",
            file.path(getwd(), ".Renviron"), file.path(getwd(), ".Renviron.txt"))
  for (f in cand) {
    if (file.exists(f)) {
      lines <- tryCatch(readLines(f, warn = FALSE), error = function(e) character(0))
      hit <- grep("GEMINI_API_KEY", lines, value = TRUE)
      if (length(hit) > 0) {
        val <- sub("^\\s*GEMINI_API_KEY\\s*=\\s*", "", hit[1])
        val <- trimws(gsub("[\"']", "", val))
        if (!identical(val, "")) { Sys.setenv(GEMINI_API_KEY = val); return(val) }
      }
    }
  }
  if (!identical(GEMINI_API_KEY_FALLBACK, "")) {
    Sys.setenv(GEMINI_API_KEY = GEMINI_API_KEY_FALLBACK)
    return(GEMINI_API_KEY_FALLBACK)
  }
  ""
}

# Llama a la API de Gemini: system_text (contexto) + history (lista role/text)
call_gemini <- function(system_text, history) {
  key <- load_gemini_key()
  if (identical(key, "")) {
    return(paste0(
      "No encuentro la clave de Gemini. Tres opciones: 1) revisa que el archivo se llame ",
      "exactamente .Renviron (sin .txt) y este junto a app.R; 2) en RStudio usa Session > Restart R; ",
      "o 3) abre app.R y pega tu clave en la linea GEMINI_API_KEY_FALLBACK <- \"...\" cerca del inicio."))
  }
  contents <- lapply(history, function(m) {
    list(role = m$role, parts = list(list(text = m$text)))
  })
  body <- list(
    system_instruction = list(parts = list(list(text = system_text))),
    contents = contents,
    generationConfig = list(temperature = 0.4, maxOutputTokens = 800)
  )
  url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/",
                GEMINI_MODEL, ":generateContent")
  req <- httr2::request(url)
  req <- httr2::req_headers(req, `x-goog-api-key` = key, `Content-Type` = "application/json")
  req <- httr2::req_body_json(req, body, auto_unbox = TRUE)
  req <- httr2::req_timeout(req, 30)
  req <- httr2::req_error(req, is_error = function(resp) FALSE)
  resp <- tryCatch(httr2::req_perform(req), error = function(e) e)
  if (inherits(resp, "error")) {
    return(paste0("No pude conectar con Gemini: ", conditionMessage(resp)))
  }
  parsed <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
  txt <- tryCatch(parsed$candidates[[1]]$content$parts[[1]]$text, error = function(e) NULL)
  if (is.null(txt) || identical(txt, "")) {
    errmsg <- tryCatch(parsed$error$message, error = function(e) NULL)
    if (!is.null(errmsg)) return(paste0("Gemini devolvio un error: ", errmsg))
    return("No recibi respuesta del modelo. Intenta de nuevo en unos segundos.")
  }
  txt
}


# ---- Paleta de colores (misma que app.R) ----
AZUL_MAIN   <- "#2E86AB"
AZUL_FUERTE <- "#1B4F72"
AZUL_CLARO  <- "#AED6F1"
GRIS_LINEA  <- "#4D4D4D"
ROJO_OUT    <- "#C0392B"
VERDE_OK    <- "#1E8449"
AMARILLO    <- "#F39C12"
CYAN_GLOW   <- "#64CFF6"

# ---- Colores por confederacion ----
CONF_COLORS <- c(
  "UEFA"     = "#3266ad",
  "CONMEBOL" = "#1a7f5a",
  "CONCACAF" = "#d4a017",
  "CAF"      = "#ba7517",
  "AFC"      = "#8e44ad",
  "OFC"      = "#c0392b"
)

# ---- Helper: layout plotly dark theme ----
pl_layout <- function(p, title = "", subtitle = "", xtitle = "", ytitle = "") {
  p %>% layout(
    title = list(
      text = if (nchar(subtitle) > 0) {
        paste0("<b>", title, "</b><br><sup>", subtitle, "</sup>")
      } else {
        paste0("<b>", title, "</b>")
      },
      font  = list(size = 14, color = CYAN_GLOW),
      x     = 0.02
    ),
    xaxis = list(
      title     = list(text = xtitle, font = list(color = "#8BAEC8")),
      gridcolor = "rgba(46,134,171,0.15)", zeroline = FALSE,
      showline  = TRUE, linecolor = "rgba(46,134,171,0.3)",
      tickfont  = list(color = "#8BAEC8")
    ),
    yaxis = list(
      title     = list(text = ytitle, font = list(color = "#8BAEC8")),
      gridcolor = "rgba(46,134,171,0.15)", zeroline = FALSE,
      showline  = TRUE, linecolor = "rgba(46,134,171,0.3)",
      tickfont  = list(color = "#8BAEC8")
    ),
    paper_bgcolor = "#112240",
    plot_bgcolor  = "#0D1F3C",
    font    = list(color = "#C5D8E8"),
    legend  = list(
      orientation = "h", x = 0, y = -0.22,
      font        = list(color = "#C5D8E8"),
      bgcolor     = "rgba(13,31,60,0.8)",
      bordercolor = "rgba(46,134,171,0.3)", borderwidth = 1
    ),
    margin     = list(t = 80, r = 30, b = 70, l = 65),
    hoverlabel = list(
      bgcolor    = "#0A1628",
      bordercolor= CYAN_GLOW,
      font       = list(size = 12, color = "#E8F4FD")
    )
  )
}

# ---- Helpers UI ----
interp_box <- function(...) {
  div(style = paste0(
    "background:linear-gradient(135deg,rgba(46,134,171,0.15) 0%,rgba(27,79,114,0.1) 100%);",
    "border-left:5px solid #2E86AB;border-radius:8px;",
    "padding:14px 18px;margin-top:12px;font-size:0.95em;line-height:1.75;color:#AED6F1;"
  ), icon("lightbulb", style = "color:#2E86AB;margin-right:6px;"), strong("Interpretacion: "), ...)
}

nota_box <- function(...) {
  div(style = paste0(
    "background:rgba(243,156,18,0.08);border:1px solid rgba(243,156,18,0.25);",
    "border-left:5px solid #F39C12;border-radius:6px;",
    "padding:12px 16px;margin-top:10px;font-size:0.9em;color:#F5CBA7;"
  ), icon("exclamation-triangle", style = "color:#F39C12;margin-right:6px;"), ...)
}

# ---- DATOS: Grupos y equipos oficiales WC2026 ----
GROUPS_DATA <- list(
  A = c("Mexico",        "South Korea",          "South Africa",    "Czech Republic"),
  B = c("Canada",        "Bosnia and Herzegovina","Qatar",           "Switzerland"),
  C = c("Brazil",        "Morocco",               "Haiti",           "Scotland"),
  D = c("United States", "Paraguay",              "Australia",       "Turkey"),
  E = c("Germany",       "Curazao",               "Ivory Coast",     "Ecuador"),
  F = c("Netherlands",   "Japan",                 "Sweden",          "Tunisia"),
  G = c("Belgium",       "Egypt",                 "Iran",            "New Zealand"),
  H = c("Spain",         "Cape Verde",            "Saudi Arabia",    "Uruguay"),
  I = c("France",        "Senegal",               "Iraq",            "Norway"),
  J = c("Argentina",     "Algeria",               "Austria",         "Jordan"),
  K = c("Portugal",      "DR Congo",              "Uzbekistan",      "Colombia"),
  L = c("England",       "Croatia",               "Ghana",           "Panama")
)

HOST_COUNTRIES <- c("United States", "Canada", "Mexico")

# Confederacion por equipo
CONF_MAP <- c(
  Mexico="CONCACAF", "South Korea"="AFC", "South Africa"="CAF", "Czech Republic"="UEFA",
  Canada="CONCACAF", "Bosnia and Herzegovina"="UEFA", Qatar="AFC", Switzerland="UEFA",
  Brazil="CONMEBOL", Morocco="CAF", Haiti="CONCACAF", Scotland="UEFA",
  "United States"="CONCACAF", Paraguay="CONMEBOL", Australia="AFC", Turkey="UEFA",
  Germany="UEFA", "Curazao"="CONCACAF", "Ivory Coast"="CAF", Ecuador="CONMEBOL",
  Netherlands="UEFA", Japan="AFC", Sweden="UEFA", Tunisia="CAF",
  Belgium="UEFA", Egypt="CAF", Iran="AFC", "New Zealand"="OFC",
  Spain="UEFA", "Cape Verde"="CAF", "Saudi Arabia"="AFC", Uruguay="CONMEBOL",
  France="UEFA", Senegal="CAF", Iraq="AFC", Norway="UEFA",
  Argentina="CONMEBOL", Algeria="CAF", Austria="UEFA", Jordan="AFC",
  Portugal="UEFA", "DR Congo"="CAF", Uzbekistan="AFC", Colombia="CONMEBOL",
  England="UEFA", Croatia="UEFA", Ghana="CAF", Panama="CONCACAF"
)

# ---- Banderas emoji por equipo (codigo de pais) ----
# ---- Codigos ISO de bandera (para flagcdn.com) ----
FLAG_MAP <- c(
  Mexico="mx", "South Korea"="kr", "South Africa"="za", "Czech Republic"="cz",
  Canada="ca", "Bosnia and Herzegovina"="ba", Qatar="qa", Switzerland="ch",
  Brazil="br", Morocco="ma", Haiti="ht", Scotland="gb-sct",
  "United States"="us", Paraguay="py", Australia="au", Turkey="tr",
  Germany="de", "Curazao"="cw", "Ivory Coast"="ci", Ecuador="ec",
  Netherlands="nl", Japan="jp", Sweden="se", Tunisia="tn",
  Belgium="be", Egypt="eg", Iran="ir", "New Zealand"="nz",
  Spain="es", "Cape Verde"="cv", "Saudi Arabia"="sa", Uruguay="uy",
  France="fr", Senegal="sn", Iraq="iq", Norway="no",
  Argentina="ar", Algeria="dz", Austria="at", Jordan="jo",
  Portugal="pt", "DR Congo"="cd", Uzbekistan="uz", Colombia="co",
  England="gb-eng", Croatia="hr", Ghana="gh", Panama="pa"
)

# Helper: bandera via flagcdn.com con fallback robusto
flag_img <- function(t) {
  idx  <- match(t, names(FLAG_MAP))
  code <- if (!is.na(idx)) FLAG_MAP[[idx]] else "un"
  tags$img(
    src    = paste0("https://flagcdn.com/20x15/", code, ".png"),
    width  = "22", height = "16",
    style  = "border-radius:2px;flex-shrink:0;object-fit:cover;vertical-align:middle;box-shadow:0 1px 3px rgba(0,0,0,0.5);display:inline-block;",
    onerror= "this.src='https://flagcdn.com/20x15/un.png'",
    alt    = t
  )
}

# Helper: copa del mundo SVG inline (no depende de URLs externas)
trophy_svg <- function(size = 34) {
  s <- size
  h <- round(s * 1.6)
  tags$svg(
    xmlns="http://www.w3.org/2000/svg",
    viewBox="0 0 100 160",
    width=as.character(s), height=as.character(h),
    style="flex-shrink:0;filter:drop-shadow(0 0 6px rgba(245,208,96,0.6));",
    # Gradientes
    tags$defs(
      tags$linearGradient(id="tgold", x1="0%", y1="0%", x2="100%", y2="100%",
                          tags$stop(offset="0%",   style="stop-color:#F5D060"),
                          tags$stop(offset="40%",  style="stop-color:#C8960C"),
                          tags$stop(offset="70%",  style="stop-color:#F5D060"),
                          tags$stop(offset="100%", style="stop-color:#8B6914")
      ),
      tags$linearGradient(id="tbase", x1="0%", y1="0%", x2="0%", y2="100%",
                          tags$stop(offset="0%",   style="stop-color:#2E5B3B"),
                          tags$stop(offset="100%", style="stop-color:#1A3525")
      )
    ),
    # Base esmeralda
    tags$rect(x="18", y="128", width="64", height="24", rx="4",
              fill="url(#tbase)", stroke="#1A3525", `stroke-width`="1"),
    # Pedestal
    tags$rect(x="33", y="112", width="34", height="19", rx="2", fill="url(#tgold)"),
    # Tallo
    tags$rect(x="42", y="92",  width="16", height="23", rx="3", fill="url(#tgold)"),
    # Cuerpo inferior copa
    tags$ellipse(cx="50", cy="91", rx="20", ry="9", fill="url(#tgold)"),
    # Cuenco de la copa
    tags$path(d="M30 91 Q25 52 50 40 Q75 52 70 91 Z", fill="url(#tgold)"),
    # Sombra interna
    tags$path(d="M37 88 Q34 60 50 50 Q66 60 63 88 Z", fill="#C8960C", opacity="0.35"),
    # Asas
    tags$path(d="M30 74 Q14 63 19 47 Q24 37 33 56",
              fill="none", stroke="url(#tgold)", `stroke-width`="6", `stroke-linecap`="round"),
    tags$path(d="M70 74 Q86 63 81 47 Q76 37 67 56",
              fill="none", stroke="url(#tgold)", `stroke-width`="6", `stroke-linecap`="round"),
    # Brillo
    tags$ellipse(cx="40", cy="60", rx="6", ry="10", fill="white", opacity="0.13",
                 transform="rotate(-25,40,60)")
  )
}
get_conf  <- function(t) { idx <- match(t, names(CONF_MAP));   if (!is.na(idx)) CONF_MAP[[idx]]   else "UEFA" }
get_color <- function(t) { conf <- get_conf(t); idx <- match(conf, names(CONF_COLORS)); if (!is.na(idx)) CONF_COLORS[[idx]] else "#888888" }

# Puntos FIFA aproximados (pre-torneo, valores ilustrativos calibrados)
FIFA_POINTS <- c(
  Argentina=1898, France=1876, Spain=1868, Brazil=1850,
  England=1824, Germany=1810, Portugal=1798, Netherlands=1780,
  Belgium=1767, Uruguay=1741, Japan=1724, Colombia=1715,
  Mexico=1690, "South Korea"=1680, "United States"=1674, Australia=1660,
  Switzerland=1655, Croatia=1648, Ecuador=1631, Senegal=1620,
  Morocco=1615, Turkey=1600, Norway=1590, "Czech Republic"=1580,
  Sweden=1575, Tunisia=1560, Austria=1555, Algeria=1545,
  "Saudi Arabia"=1530, Egypt=1520, Qatar=1510, Iraq=1505,
  "South Africa"=1495, Scotland=1490, Iran=1485, "Ivory Coast"=1480,
  Paraguay=1470, "Cape Verde"=1460, "Bosnia and Herzegovina"=1450, Ghana=1445,
  Jordan=1420, Panama=1415, Haiti=1400, "New Zealand"=1395,
  "DR Congo"=1390, Uzbekistan=1380, "Curazao"=1360
)

# Calcular strength_score y lambda_base
compute_team_stats <- function() {
  teams <- unique(unlist(GROUPS_DATA))
  # Lookup seguro por posicion (evita mismatch de encoding en nombres)
  get_pts <- function(t) {
    idx <- match(t, names(FIFA_POINTS))
    if (!is.na(idx)) FIFA_POINTS[[idx]] else 1400
  }
  fpts      <- sapply(teams, get_pts)
  fpts_norm <- (fpts - min(fpts)) / (max(fpts) - min(fpts))
  ss        <- 0.6 + fpts_norm * 1.6
  
  get_conf  <- function(t) { idx <- match(t, names(CONF_MAP));  if (!is.na(idx)) CONF_MAP[[idx]]  else "UEFA" }
  get_color <- function(t) { conf <- get_conf(t); idx <- match(conf, names(CONF_COLORS)); if (!is.na(idx)) CONF_COLORS[[idx]] else "#888888" }
  
  df <- data.frame(
    team           = teams,
    confederation  = sapply(teams, get_conf),
    fifa_points    = fpts,
    strength_score = fpts_norm * 14.0,
    lambda_base    = ss,
    is_host        = teams %in% HOST_COUNTRIES,
    color          = sapply(teams, get_color),
    stringsAsFactors = FALSE
  )
  rownames(df) <- df$team
  df$team_key  <- df$team  # alias explicito para joins
  df
}

TEAM_STATS <- compute_team_stats()

# Helper: lookup seguro en TEAM_STATS por nombre de equipo
ts_get <- function(team_name, col) {
  idx <- match(team_name, TEAM_STATS$team_key)
  if (is.na(idx)) return(NA)
  TEAM_STATS[[col]][idx]
}

# Helper: bandera por nombre de equipo
get_flag <- function(t) {
  idx <- match(t, names(FLAG_MAP))
  if (!is.na(idx)) FLAG_MAP[[idx]] else "\U0001F3F3"
}

# ============================================================
# RESULTADOS REALES DEL MUNDIAL - via API football-data.org
# ============================================================
# Las simulaciones usan el marcador REAL para partidos ya jugados
# y simulan con Poisson solo los partidos pendientes.

# --- Token de la API (gratis en football-data.org/client/register) ---
# Pega tu token aqui, o ponlo en .Renviron como FOOTBALL_DATA_TOKEN
FOOTBALL_DATA_TOKEN_FALLBACK <- "03469550b8904d2f84dc131728e85de8"  # <-- pega aqui tu token entre comillas

load_fd_token <- function() {
  k <- Sys.getenv("FOOTBALL_DATA_TOKEN")
  if (!identical(k, "")) return(k)
  FOOTBALL_DATA_TOKEN_FALLBACK
}

# --- Entorno global que guarda los resultados reales ---
# Estructura: lista de partidos con grupo, equipos, goles, jugado (TRUE/FALSE)
REAL_RESULTS <- new.env(parent = emptyenv())
REAL_RESULTS$matches  <- list()   # cada elemento: list(grp, a, b, ga, gb, played)
REAL_RESULTS$last_sync <- NULL     # timestamp de la ultima descarga
REAL_RESULTS$source    <- "ninguna"

# --- Mapa de nombres API -> nombres internos del simulador ---
# football-data.org usa nombres oficiales en ingles; mapeamos los que difieren
# Normaliza un nombre de la API al nombre interno
# Quita acentos y normaliza a minusculas sin espacios extra
strip_accents <- function(s) {
  if (is.null(s) || is.na(s)) return("")
  s <- as.character(s)

  # Reemplazo manual de caracteres acentuados comunes (robusto en Windows)
  from <- c("\u00e1","\u00e9","\u00ed","\u00f3","\u00fa","\u00fc","\u00f1",
            "\u00e0","\u00e8","\u00ec","\u00f2","\u00f9",
            "\u00e2","\u00ea","\u00ee","\u00f4","\u00fb",
            "\u00e7","\u00c7",
            "\u00c1","\u00c9","\u00cd","\u00d3","\u00da","\u00dc","\u00d1",
            "\u00e3","\u00f5","\u0131","\u015f","\u011f")
  to   <- c("a","e","i","o","u","u","n",
            "a","e","i","o","u",
            "a","e","i","o","u",
            "c","c",
            "a","e","i","o","u","u","n",
            "a","o","i","s","g")
  for (k in seq_along(from)) s <- gsub(from[k], to[k], s, fixed = TRUE)

  # iconv como segunda pasada por si quedan otros
  s2 <- tryCatch(iconv(s, to = "ASCII//TRANSLIT"), error = function(e) s)
  if (!is.na(s2)) s <- s2

  # Convertir guiones, comas, slashes y puntos a espacios (NO eliminarlos)
  s <- gsub("[-_,/.&]", " ", s)
  s <- gsub("[^a-zA-Z ]", "", s)   # quita apostrofes y demas
  s <- tolower(trimws(s))
  s <- gsub("\\s+", " ", s)
  s
}

# Diccionario de alias normalizado (clave = nombre API normalizado, valor = nombre interno)
ALIAS_NORM <- c(
  "korea republic"        = "South Korea",
  "south korea"           = "South Korea",
  "korea dpr"             = "South Korea",  # por si acaso (no aplica pero seguro)
  "czechia"               = "Czech Republic",
  "czech republic"        = "Czech Republic",
  "bosnia herzegovina"    = "Bosnia and Herzegovina",
  "bosnia and herzegovina"= "Bosnia and Herzegovina",
  "usa"                   = "United States",
  "united states"         = "United States",
  "cote divoire"          = "Ivory Coast",
  "cote d ivoire"         = "Ivory Coast",
  "ivory coast"           = "Ivory Coast",
  "curacao"               = "Curazao",
  "curazao"               = "Curazao",
  "cabo verde"            = "Cape Verde",
  "cabo verde islands"    = "Cape Verde",
  "cape verde"            = "Cape Verde",
  "cape verde islands"    = "Cape Verde",
  "dr congo"              = "DR Congo",
  "congo dr"              = "DR Congo",
  "democratic republic congo"= "DR Congo",
  "turkiye"               = "Turkey",
  "turkey"                = "Turkey"
)

normalize_team_name <- function(api_name) {
  if (is.null(api_name) || is.na(api_name)) return(NA_character_)

  norm <- strip_accents(api_name)
  if (norm == "") return(NA_character_)

  # 1) Buscar en el diccionario de alias normalizado
  idx <- match(norm, names(ALIAS_NORM))
  if (!is.na(idx)) return(unname(ALIAS_NORM[[idx]]))

  # 2) Comparar contra los nombres internos normalizados
  all_teams <- unlist(GROUPS_DATA)
  norm_internal <- sapply(all_teams, strip_accents)
  hit <- which(norm_internal == norm)
  if (length(hit) > 0) return(unname(all_teams[hit[1]]))

  # 3) Matching parcial: el nombre API contiene un nombre interno o viceversa
  for (i in seq_along(all_teams)) {
    ni <- norm_internal[i]
    if (nchar(ni) >= 4 && (grepl(ni, norm, fixed=TRUE) || grepl(norm, ni, fixed=TRUE))) {
      return(unname(all_teams[i]))
    }
  }

  NA_character_  # nombre desconocido
}

# Encuentra a que grupo pertenece un par de equipos
find_group_for_pair <- function(team_a, team_b) {
  for (g in names(GROUPS_DATA)) {
    if (team_a %in% GROUPS_DATA[[g]] && team_b %in% GROUPS_DATA[[g]]) return(g)
  }
  NA_character_
}

# --- Descarga resultados desde football-data.org ---
fetch_real_results <- function() {
  token <- load_fd_token()
  if (identical(token, "")) {
    return(list(ok = FALSE, msg = "No hay token configurado. Registrate en football-data.org y pega tu token."))
  }

  result <- tryCatch({
    req <- httr2::request("https://api.football-data.org/v4/competitions/WC/matches") |>
      httr2::req_url_query(season = 2026) |>
      httr2::req_headers("X-Auth-Token" = token) |>
      httr2::req_timeout(15)

    resp <- httr2::req_perform(req)
    data <- httr2::resp_body_json(resp)

    if (is.null(data$matches)) {
      return(list(ok = FALSE, msg = "La API no devolvio partidos. Verifica que el Mundial 2026 este disponible."))
    }

    parsed <- list()
    unmatched <- character(0)   # nombres de la API que no se reconocieron
    for (m in data$matches) {
      # Solo fase de grupos
      stage <- m$stage %||% ""
      if (!grepl("GROUP", toupper(stage))) next

      a_raw <- m$homeTeam$name %||% m$homeTeam$shortName %||% NA
      b_raw <- m$awayTeam$name %||% m$awayTeam$shortName %||% NA
      ta <- normalize_team_name(a_raw)
      tb <- normalize_team_name(b_raw)
      if (is.na(ta)) unmatched <- c(unmatched, as.character(a_raw))
      if (is.na(tb)) unmatched <- c(unmatched, as.character(b_raw))
      if (is.na(ta) || is.na(tb)) next  # equipo no reconocido

      grp <- find_group_for_pair(ta, tb)
      if (is.na(grp)) next

      status <- m$status %||% ""

      ga <- m$score$fullTime$home
      gb <- m$score$fullTime$away
      if (is.null(ga)) ga <- NA_integer_
      if (is.null(gb)) gb <- NA_integer_

      # Un partido cuenta como jugado si tiene status final O si ambos marcadores existen
      status_done <- status %in% c("FINISHED", "AWARDED", "IN_PLAY", "PAUSED")
      has_scores  <- !is.na(ga) && !is.na(gb)
      played <- (status_done || has_scores) && has_scores

      parsed[[length(parsed) + 1]] <- list(
        grp = grp, a = ta, b = tb,
        ga = ga, gb = gb,
        played = played
      )
    }

    REAL_RESULTS$matches   <- parsed
    REAL_RESULTS$last_sync <- Sys.time()
    REAL_RESULTS$source    <- "football-data.org"
    REAL_RESULTS$unmatched <- unique(unmatched)

    n_played <- sum(sapply(parsed, function(x) isTRUE(x$played)))
    msg <- paste0("Sincronizado: ", length(parsed), " partidos (", n_played, " ya jugados).")
    if (length(unique(unmatched)) > 0) {
      msg <- paste0(msg, " Equipos no reconocidos: ",
                    paste(unique(unmatched), collapse=", "))
    }
    list(ok = TRUE, msg = msg,
         total = length(parsed), played = n_played,
         unmatched = unique(unmatched))
  }, error = function(e) {
    list(ok = FALSE, msg = paste("Error al conectar con la API:", conditionMessage(e)))
  })

  result
}

# --- Lookup: devuelve el resultado real de un partido si existe ---
# Retorna NULL si el partido NO se ha jugado (o no hay datos)
get_real_match <- function(team_a, team_b) {
  ms <- REAL_RESULTS$matches
  if (length(ms) == 0) return(NULL)
  for (m in ms) {
    if (!isTRUE(m$played)) next
    # Coincide en cualquier orden
    if ((m$a == team_a && m$b == team_b)) {
      return(list(ga = as.integer(m$ga), gb = as.integer(m$gb), swapped = FALSE))
    }
    if ((m$a == team_b && m$b == team_a)) {
      return(list(ga = as.integer(m$gb), gb = as.integer(m$ga), swapped = TRUE))
    }
  }
  NULL
}

# --- Cuantos partidos reales hay cargados ---
real_results_summary <- function() {
  ms <- REAL_RESULTS$matches
  total  <- length(ms)
  played <- if (total > 0) sum(sapply(ms, function(x) isTRUE(x$played))) else 0
  list(total = total, played = played,
       last_sync = REAL_RESULTS$last_sync,
       source = REAL_RESULTS$source)
}

# ---- Motor de simulacion ----
HOME_BONUS <- 0.25

simulate_match <- function(team_a, team_b, ts = TEAM_STATS, home_team = NULL) {
  # --- Si el partido YA se jugo, usar el marcador REAL ---
  real <- get_real_match(team_a, team_b)
  if (!is.null(real)) {
    ga <- real$ga; gb <- real$gb
    if (ga > gb)       return(list(ga=ga, gb=gb, pts_a=3, pts_b=0, result="W", real=TRUE))
    else if (ga < gb)  return(list(ga=ga, gb=gb, pts_a=0, pts_b=3, result="L", real=TRUE))
    else               return(list(ga=ga, gb=gb, pts_a=1, pts_b=1, result="D", real=TRUE))
  }

  # --- Si NO se ha jugado, simular con Poisson ---
  lam_a <- ts_get(team_a, "lambda_base")
  lam_b <- ts_get(team_b, "lambda_base")
  if (is.na(lam_a)) lam_a <- 1.2
  if (is.na(lam_b)) lam_b <- 1.2
  if (!is.null(home_team)) {
    if (!is.na(home_team) && home_team == team_a) lam_a <- lam_a + HOME_BONUS
    if (!is.na(home_team) && home_team == team_b) lam_b <- lam_b + HOME_BONUS
  }
  ga <- rpois(1, lam_a)
  gb <- rpois(1, lam_b)
  if (ga > gb)       list(ga=ga, gb=gb, pts_a=3, pts_b=0, result="W", real=FALSE)
  else if (ga < gb)  list(ga=ga, gb=gb, pts_a=0, pts_b=3, result="L", real=FALSE)
  else               list(ga=ga, gb=gb, pts_a=1, pts_b=1, result="D", real=FALSE)
}

build_standings <- function(teams) {
  df <- data.frame(
    PJ=rep(0L,length(teams)), PTS=rep(0L,length(teams)),
    GF=rep(0L,length(teams)), GC=rep(0L,length(teams)),
    DG=rep(0L,length(teams)), PG=rep(0L,length(teams)),
    PE=rep(0L,length(teams)), PP=rep(0L,length(teams)),
    stringsAsFactors=FALSE, row.names=teams
  )
  df
}

update_standings <- function(st, team_a, team_b, ga, gb, pts_a, pts_b) {
  # Usar match para lookups robustos a encoding
  ia <- match(team_a, rownames(st))
  ib <- match(team_b, rownames(st))
  if (!is.na(ia)) {
    st[ia, "PJ"]  <- st[ia, "PJ"]  + 1L
    st[ia, "GF"]  <- st[ia, "GF"]  + ga
    st[ia, "GC"]  <- st[ia, "GC"]  + gb
    st[ia, "DG"]  <- st[ia, "DG"]  + (ga - gb)
    st[ia, "PTS"] <- st[ia, "PTS"] + pts_a
    if (pts_a == 3L)      st[ia, "PG"] <- st[ia, "PG"] + 1L
    else if (pts_a == 1L) st[ia, "PE"] <- st[ia, "PE"] + 1L
    else                  st[ia, "PP"] <- st[ia, "PP"] + 1L
  }
  if (!is.na(ib)) {
    st[ib, "PJ"]  <- st[ib, "PJ"]  + 1L
    st[ib, "GF"]  <- st[ib, "GF"]  + gb
    st[ib, "GC"]  <- st[ib, "GC"]  + ga
    st[ib, "DG"]  <- st[ib, "DG"]  + (gb - ga)
    st[ib, "PTS"] <- st[ib, "PTS"] + pts_b
    if (pts_b == 3L)      st[ib, "PG"] <- st[ib, "PG"] + 1L
    else if (pts_b == 1L) st[ib, "PE"] <- st[ib, "PE"] + 1L
    else                  st[ib, "PP"] <- st[ib, "PP"] + 1L
  }
  st
}

sort_standings <- function(st) {
  fpts <- sapply(rownames(st), function(t) { v <- ts_get(t, "fifa_points"); if (is.na(v)) 1400 else v })
  st$FIFA_PTS <- fpts
  ord <- order(-st$PTS, -st$DG, -st$GF, -st$FIFA_PTS)
  result <- st[ord, , drop=FALSE]
  # CRITICO: preservar rownames originales (nombres de equipos) despues del reorder
  rownames(result) <- rownames(st)[ord]
  result
}

# ---- Calendario REAL FIFA 2026 (por grupo, por jornada) ----
# Indices 1-4 corresponden al orden de los equipos en GROUPS_DATA.
# Cada grupo tiene su propio fixture; el Grupo A difiere del resto.
GROUP_FIXTURES <- list(
  A = list(list(c(1,3),c(2,4)), list(c(4,3),c(1,2)), list(c(4,1),c(3,2))),
  B = list(list(c(1,2),c(3,4)), list(c(4,2),c(1,3)), list(c(4,1),c(2,3))),
  C = list(list(c(1,2),c(3,4)), list(c(4,2),c(1,3)), list(c(4,1),c(2,3))),
  D = list(list(c(1,2),c(3,4)), list(c(1,3),c(4,2)), list(c(4,1),c(2,3))),
  E = list(list(c(1,2),c(3,4)), list(c(1,3),c(4,2)), list(c(4,1),c(2,3))),
  F = list(list(c(1,2),c(3,4)), list(c(1,3),c(4,2)), list(c(4,1),c(2,3))),
  G = list(list(c(1,2),c(3,4)), list(c(1,3),c(4,2)), list(c(4,1),c(2,3))),
  H = list(list(c(1,2),c(3,4)), list(c(1,3),c(4,2)), list(c(4,1),c(2,3))),
  I = list(list(c(1,2),c(3,4)), list(c(1,3),c(4,2)), list(c(4,1),c(2,3))),
  J = list(list(c(1,2),c(3,4)), list(c(1,3),c(4,2)), list(c(4,1),c(2,3))),
  K = list(list(c(1,2),c(3,4)), list(c(1,3),c(4,2)), list(c(4,1),c(2,3))),
  L = list(list(c(1,2),c(3,4)), list(c(1,3),c(4,2)), list(c(4,1),c(2,3)))
)

run_group_stage <- function(seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  all_st   <- lapply(GROUPS_DATA, build_standings)
  matches  <- list()
  
  for (jornada in 1:3) {
    for (grp in names(GROUPS_DATA)) {
      teams <- GROUPS_DATA[[grp]]
      pairs <- GROUP_FIXTURES[[grp]][[jornada]]
      for (pair in pairs) {
        ta <- teams[pair[1]]
        tb <- teams[pair[2]]
        # if/else en lugar de ifelse() - ifelse() no puede devolver NULL
        home_team <- if (ta %in% HOST_COUNTRIES) ta else if (tb %in% HOST_COUNTRIES) tb else NULL
        res <- simulate_match(ta, tb, home_team = home_team)
        all_st[[grp]] <- update_standings(all_st[[grp]], ta, tb,
                                          res$ga, res$gb, res$pts_a, res$pts_b)
        matches <- append(matches, list(data.frame(
          jornada=jornada, grupo=grp,
          team_a=ta, team_b=tb,
          goals_a=res$ga, goals_b=res$gb,
          pts_a=res$pts_a, pts_b=res$pts_b,
          result=res$result,
          stringsAsFactors=FALSE
        )))
      }
    }
  }
  
  # Sort standings
  all_st <- lapply(all_st, sort_standings)
  
  # Clasificados
  qualified <- list()
  thirds     <- list()
  for (grp in names(GROUPS_DATA)) {
    st <- all_st[[grp]]
    teams_sorted <- rownames(st)
    qualified <- append(qualified, list(list(group=grp, pos="1", team=teams_sorted[1],
                                             PTS=as.integer(st[1,"PTS"]), DG=as.integer(st[1,"DG"]), GF=as.integer(st[1,"GF"]))))
    qualified <- append(qualified, list(list(group=grp, pos="2", team=teams_sorted[2],
                                             PTS=as.integer(st[2,"PTS"]), DG=as.integer(st[2,"DG"]), GF=as.integer(st[2,"GF"]))))
    thirds <- append(thirds, list(list(group=grp, pos="3", team=teams_sorted[3],
                                       PTS=as.integer(st[3,"PTS"]), DG=as.integer(st[3,"DG"]), GF=as.integer(st[3,"GF"]),
                                       FIFA_PTS=as.numeric(ts_get(teams_sorted[3], "fifa_points")))))
  }
  
  thirds_df <- do.call(rbind, lapply(thirds, function(x) {
    data.frame(group=x$group, pos=as.character(x$pos), team=as.character(x$team),
               PTS=as.integer(x$PTS), DG=as.integer(x$DG), GF=as.integer(x$GF),
               FIFA_PTS=as.numeric(x$FIFA_PTS), stringsAsFactors=FALSE)
  }))
  thirds_df  <- thirds_df[order(-thirds_df$PTS, -thirds_df$DG, -thirds_df$GF, -thirds_df$FIFA_PTS), ]
  best8      <- thirds_df[seq_len(min(8, nrow(thirds_df))), ]
  for (i in seq_len(nrow(best8))) {
    r <- best8[i, ]
    qualified <- append(qualified, list(list(group=as.character(r$group), pos="3*",
                                             team=as.character(r$team),
                                             PTS=as.integer(r$PTS),
                                             DG=as.integer(r$DG),
                                             GF=as.integer(r$GF))))
  }
  
  qual_df <- do.call(rbind, lapply(qualified, function(x) {
    data.frame(group=as.character(x$group), pos=as.character(x$pos),
               team=as.character(x$team), PTS=as.integer(x$PTS),
               DG=as.integer(x$DG), GF=as.integer(x$GF),
               stringsAsFactors=FALSE)
  }))
  
  matches_df <- if (length(matches) > 0) do.call(rbind, matches) else data.frame()
  
  list(
    standings  = all_st,
    matches    = matches_df,
    qualified  = qual_df,
    thirds_df  = thirds_df
  )
}

# ---- Monte Carlo optimizado ----
# Version ligera: solo cuenta clasificados, no construye matches ni standings completos
run_group_stage_fast <- function() {
  # Precomputed lambda por equipo (vector nombrado)
  lams <- setNames(TEAM_STATS$lambda_base, TEAM_STATS$team_key)
  fpts <- setNames(TEAM_STATS$fifa_points, TEAM_STATS$team_key)
  
  qual_teams <- character(32)
  qi <- 0L
  thirds_list <- vector("list", 12)
  ti <- 0L
  
  for (grp in names(GROUPS_DATA)) {
    teams <- GROUPS_DATA[[grp]]
    pts <- setNames(integer(4), teams)
    dg  <- setNames(integer(4), teams)
    gf  <- setNames(integer(4), teams)
    
    # 6 partidos round-robin
    pairs <- list(c(1,2),c(1,3),c(1,4),c(2,3),c(2,4),c(3,4))
    for (p in pairs) {
      ta <- teams[p[1]]; tb <- teams[p[2]]

      # --- Si el partido YA se jugo, usar marcador REAL ---
      real <- get_real_match(ta, tb)
      if (!is.null(real)) {
        ga <- real$ga; gb <- real$gb
      } else {
        # Simular con Poisson
        la <- lams[ta]; lb <- lams[tb]
        if (is.na(la)) la <- 1.2
        if (is.na(lb)) lb <- 1.2
        if (ta %in% HOST_COUNTRIES) la <- la + HOME_BONUS
        if (tb %in% HOST_COUNTRIES) lb <- lb + HOME_BONUS
        ga <- rpois(1L, la); gb <- rpois(1L, lb)
      }

      gf[ta] <- gf[ta] + ga; gf[tb] <- gf[tb] + gb
      dg[ta] <- dg[ta] + (ga - gb); dg[tb] <- dg[tb] + (gb - ga)
      if (ga > gb)      { pts[ta] <- pts[ta] + 3L }
      else if (ga < gb) { pts[tb] <- pts[tb] + 3L }
      else              { pts[ta] <- pts[ta] + 1L; pts[tb] <- pts[tb] + 1L }
    }
    
    fp <- fpts[teams]; fp[is.na(fp)] <- 1400
    ord <- order(-pts, -dg, -gf, -fp)
    ranked <- teams[ord]
    
    qual_teams[qi+1L] <- ranked[1]; qual_teams[qi+2L] <- ranked[2]; qi <- qi + 2L
    ti <- ti + 1L
    thirds_list[[ti]] <- list(
      team=ranked[3],
      pts=pts[ranked[3]], dg=dg[ranked[3]], gf=gf[ranked[3]],
      fp=fp[ranked[3]]
    )
  }
  
  # Seleccionar 8 mejores terceros
  third_pts <- sapply(thirds_list, `[[`, "pts")
  third_dg  <- sapply(thirds_list, `[[`, "dg")
  third_gf  <- sapply(thirds_list, `[[`, "gf")
  third_fp  <- sapply(thirds_list, `[[`, "fp")
  third_ord <- order(-third_pts, -third_dg, -third_gf, -third_fp)
  best8_idx <- third_ord[1:8]
  
  for (i in best8_idx) {
    qi <- qi + 1L
    qual_teams[qi] <- thirds_list[[i]]$team
  }
  
  qual_teams[1:qi]
}

run_monte_carlo <- function(n_sims = 1000) {
  all_teams <- unlist(GROUPS_DATA)
  qual_count <- setNames(integer(length(all_teams)), all_teams)
  
  for (s in seq_len(n_sims)) {
    classified <- run_group_stage_fast()
    idx <- match(classified, names(qual_count))
    idx <- idx[!is.na(idx)]
    qual_count[idx] <- qual_count[idx] + 1L
  }
  
  teams_mc <- names(qual_count)
  data.frame(
    team        = teams_mc,
    prob        = as.numeric(qual_count) / n_sims,
    group       = sapply(teams_mc, function(t) { idx <- match(t, names(CONF_MAP)); if (!is.na(idx)) CONF_MAP[[idx]] else "UEFA" }),
    fifa_points = sapply(teams_mc, function(t) { v <- ts_get(t, "fifa_points"); if (is.na(v)) 1400 else v }),
    stringsAsFactors = FALSE
  ) |> dplyr::arrange(desc(prob))
}

# ============================================================
# BRACKET ELIMINATORIO - Motor de simulacion
# ============================================================

# Tabla de asignacion oficial FIFA 2026 para los 8 mejores terceros
# Clave: grupos ordenados de donde vienen los 8 terceros -> posiciones en bracket
# Simplificacion: asignamos terceros a slots libres respetando que
# no enfrenten equipos del mismo grupo en R32
assign_thirds_to_bracket <- function(third_groups) {
  # Slots disponibles para terceros segun posicion en bracket R32
  # Formato: (slot_id, enfrenta_a)
  # Usamos asignacion fija basada en los 8 mejores terceros ordenados
  third_groups[1:8]
}

# Construir bracket R32 desde qualified_df - robusto
build_bracket <- function(qualified_df) {
  q <- qualified_df
  
  # Normalizar pos para comparacion segura
  q$pos <- as.character(q$pos)
  q$group <- as.character(q$group)
  
  firsts  <- q[q$pos == "1",  ]
  seconds <- q[q$pos == "2",  ]
  thirds  <- q[q$pos == "3*", ]
  
  firsts  <- firsts[order(firsts$group),  ]
  seconds <- seconds[order(seconds$group), ]
  thirds  <- thirds[order(thirds$group),  ]
  
  # Lookup seguro: devuelve "TBD" si no encuentra
  get1 <- function(g) {
    v <- firsts$team[firsts$group == g]
    if (length(v) == 0) "TBD" else as.character(v[1])
  }
  get2 <- function(g) {
    v <- seconds$team[seconds$group == g]
    if (length(v) == 0) "TBD" else as.character(v[1])
  }
  get3 <- function(i) {
    if (i <= nrow(thirds)) as.character(thirds$team[i]) else "TBD"
  }
  
  list(
    list(a=get1("A"), b=get2("B")),
    list(a=get1("C"), b=get2("D")),
    list(a=get1("E"), b=get2("F")),
    list(a=get1("G"), b=get2("H")),
    list(a=get1("I"), b=get2("J")),
    list(a=get1("K"), b=get2("L")),
    list(a=get1("B"), b=get2("A")),
    list(a=get1("D"), b=get2("C")),
    list(a=get1("F"), b=get2("E")),
    list(a=get1("H"), b=get2("G")),
    list(a=get1("J"), b=get2("I")),
    list(a=get1("L"), b=get2("K")),
    list(a=get3(1),   b=get3(2)),
    list(a=get3(3),   b=get3(4)),
    list(a=get3(5),   b=get3(6)),
    list(a=get3(7),   b=get3(8))
  )
}

# Simular partido KO con Poisson + tiempo extra + penales Bradley-Terry
simulate_ko <- function(team_a, team_b, scores=FALSE) {
  la <- ts_get(team_a, "lambda_base"); if (is.na(la)) la <- 1.2
  lb <- ts_get(team_b, "lambda_base"); if (is.na(lb)) lb <- 1.2
  
  ga <- rpois(1L, la); gb <- rpois(1L, lb)
  
  # Tiempo extra si empate (30 min ~= 0.4 partido)
  if (ga == gb) {
    ga <- ga + rpois(1L, la*0.4)
    gb <- gb + rpois(1L, lb*0.4)
  }
  
  # Penales Bradley-Terry si sigue empate
  winner <- if (ga > gb) team_a else if (gb > ga) team_b else {
    if (runif(1) < la/(la+lb)) team_a else team_b
  }
  
  if (scores) list(winner=winner, ga=ga, gb=gb)
  else winner
}

# Simular bracket completo -> devuelve lista con resultados por ronda
simulate_bracket_full <- function(qualified_df) {
  bracket <- build_bracket(qualified_df)
  
  safe_team <- function(t) if (is.null(t) || length(t)==0 || is.na(t) || t=="") "TBD" else t
  
  run_round <- function(matches) {
    lapply(matches, function(m) {
      ta <- safe_team(m$a); tb <- safe_team(m$b)
      if (ta == "TBD" || tb == "TBD") {
        w <- if (ta != "TBD") ta else tb
        return(list(a=ta, b=tb, winner=w, ga=0, gb=0))
      }
      res <- simulate_ko(ta, tb, scores=TRUE)
      list(a=ta, b=tb, winner=safe_team(res$winner), ga=res$ga, gb=res$gb)
    })
  }
  
  r32 <- run_round(bracket)
  w32 <- sapply(r32, function(m) safe_team(m$winner))
  
  # Garantizar exactamente 16 ganadores
  while (length(w32) < 16) w32 <- c(w32, "TBD")
  w32 <- w32[1:16]
  
  r16_matches <- lapply(seq(1,16,by=2), function(i) list(a=w32[i], b=w32[i+1]))
  r16 <- run_round(r16_matches)
  w16 <- sapply(r16, function(m) safe_team(m$winner))
  while (length(w16) < 8) w16 <- c(w16, "TBD")
  w16 <- w16[1:8]
  
  qf_matches <- lapply(seq(1,8,by=2), function(i) list(a=w16[i], b=w16[i+1]))
  qf <- run_round(qf_matches)
  wqf <- sapply(qf, function(m) safe_team(m$winner))
  while (length(wqf) < 4) wqf <- c(wqf, "TBD")
  wqf <- wqf[1:4]
  
  sf_matches <- lapply(seq(1,4,by=2), function(i) list(a=wqf[i], b=wqf[i+1]))
  sf <- run_round(sf_matches)
  wsf <- sapply(sf, function(m) safe_team(m$winner))
  while (length(wsf) < 2) wsf <- c(wsf, "TBD")
  wsf <- wsf[1:2]
  
  fin_matches <- list(list(a=wsf[1], b=wsf[2]))
  fin <- run_round(fin_matches)
  champion <- safe_team(fin[[1]]$winner)
  
  list(r32=r32, r16=r16, qf=qf, sf=sf, final=fin, champion=champion)
}

# Monte Carlo de campeon (rapido)
run_champion_mc <- function(n_sims=500) {
  all_teams <- unlist(GROUPS_DATA)
  champ_count <- setNames(integer(length(all_teams)), all_teams)
  
  for (s in seq_len(n_sims)) {
    qual_teams_vec <- run_group_stage_fast()
    # Construir qualified_df minimo
    grp_of <- function(t) { for(g in names(GROUPS_DATA)) if(t %in% GROUPS_DATA[[g]]) return(g); NA }
    pos_idx <- setNames(integer(length(all_teams)), all_teams)
    for (g in names(GROUPS_DATA)) {
      ts <- GROUPS_DATA[[g]]
      for (k in 1:4) pos_idx[ts[k]] <- k
    }
    
    qual <- data.frame(
      team  = qual_teams_vec,
      group = sapply(qual_teams_vec, grp_of),
      pos   = ifelse(qual_teams_vec %in% sapply(names(GROUPS_DATA), function(g) {
        lams <- setNames(TEAM_STATS$lambda_base, TEAM_STATS$team_key)
        fpts <- setNames(TEAM_STATS$fifa_points, TEAM_STATS$team_key)
        teams <- GROUPS_DATA[[g]]
        teams[1]  # placeholder
      }), "1", "2"),  # simplificado
      stringsAsFactors = FALSE
    )
    # Asignar pos correctamente desde run_group_stage completo
    full_sim <- run_group_stage()
    q <- full_sim$qualified
    bracket_res <- tryCatch(simulate_bracket_full(q), error=function(e) NULL)
    if (is.null(bracket_res)) next
    ch <- bracket_res$champion
    idx <- match(ch, names(champ_count))
    if (!is.na(idx)) champ_count[idx] <- champ_count[idx] + 1L
  }
  
  data.frame(
    team  = names(champ_count),
    prob  = as.numeric(champ_count) / n_sims,
    stringsAsFactors = FALSE
  ) |> dplyr::arrange(desc(prob)) |> dplyr::filter(prob > 0)
}

# ============================================================
# UI
# ============================================================
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = tagList(
      tags$head(
        tags$title("WC 2026 Simulator"),
        tags$link(rel="icon", type="image/png", href="trophy.png")
      ),
      tags$span(
        style = "display:flex;align-items:center;gap:10px;",
        tags$img(
          src   = "trophy.png",
          height = "36",
          style  = "flex-shrink:0;filter:drop-shadow(0 0 6px rgba(245,208,96,0.5));"
        ),
        tags$span(
          style = "font-family:'Inter',sans-serif;font-weight:700;font-size:0.9em;letter-spacing:0.3px;",
          "WC 2026 . Simulador"
        )
      )
    ),
    titleWidth = 300
  ),
  
  dashboardSidebar(
    width = 265,
    
    tags$div(
      style = "padding:18px 18px 10px;",
      tags$div(
        style = paste0(
          "display:flex;align-items:center;gap:10px;",
          "background:linear-gradient(135deg,rgba(46,134,171,0.15) 0%,rgba(46,134,171,0.05) 100%);",
          "border:1px solid rgba(46,134,171,0.25);border-radius:10px;",
          "padding:10px 14px;"
        ),
        tags$div(style = paste0(
          "width:8px;height:8px;border-radius:50%;flex-shrink:0;",
          "background:#64CFF6;",
          "box-shadow:0 0 6px #64CFF6, 0 0 12px rgba(100,207,246,0.4);",
          "animation:dotBlink 2s ease-in-out infinite;"
        )),
        tags$style(HTML("
          @keyframes dotBlink {
            0%,100%{opacity:1;box-shadow:0 0 6px #64CFF6,0 0 12px rgba(100,207,246,0.4);}
            50%{opacity:0.5;box-shadow:0 0 3px #64CFF6;}
          }
        ")),
        tags$div(
          tags$div(style="color:#64CFF6;font-size:0.7em;text-transform:uppercase;letter-spacing:1.5px;font-weight:700;", "Modulos"),
          tags$div(style="color:#4A7A9B;font-size:0.65em;margin-top:1px;", "8 secciones disponibles")
        )
      )
    ),
    
    sidebarMenu(id = "sidebar_tabs",
                menuItem("Introduccion",     tabName = "intro",       icon = icon("info-circle")),
                menuItem("Grupos Oficiales", tabName = "grupos",      icon = icon("layer-group")),
                menuItem("Simulacion",       tabName = "simulacion",  icon = icon("play-circle")),
                menuItem("Clasificados",     tabName = "clasificados",icon = icon("trophy")),
                menuItem("Probabilidades",   tabName = "montecarlo",  icon = icon("dice")),
                menuItem(">>> Eliminacion Directa", tabName = "campeon", icon = icon("crown")),
                menuItem("Ranking equipos",  tabName = "variables",   icon = icon("chart-bar")),
                menuItem("Metodologia",      tabName = "metodologia", icon = icon("cogs"))
    ),
    
    tags$div(style = paste0(
      "margin:8px 14px;height:1px;",
      "background:linear-gradient(90deg,transparent,rgba(46,134,171,0.5),transparent);"
    )),
    
    # Modelo info
    tags$div(
      style = paste0(
        "margin:8px 14px 16px;padding:14px 16px;",
        "background:linear-gradient(135deg,#081424 0%,#0D1F3C 50%,#0A1628 100%);",
        "border-radius:12px;border:1px solid rgba(46,134,171,0.3);",
        "box-shadow:0 4px 20px rgba(0,0,0,0.4),inset 0 1px 0 rgba(100,207,246,0.07);",
        "position:relative;overflow:hidden;"
      ),
      tags$div(style=paste0(
        "position:absolute;top:0;left:0;right:0;height:2px;",
        "background:linear-gradient(90deg,#1B4F72,#64CFF6,#1B4F72);"
      )),
      tags$div(
        style="display:flex;align-items:center;gap:8px;margin-bottom:10px;",
        tags$div(style=paste0(
          "width:26px;height:26px;border-radius:50%;flex-shrink:0;",
          "background:linear-gradient(135deg,#1B4F72,#2E86AB);",
          "display:flex;align-items:center;justify-content:center;"
        ),
        icon("robot", style="color:#AED6F1;font-size:0.7em;")
        ),
        tags$div(style="color:#64CFF6;font-size:0.68em;text-transform:uppercase;letter-spacing:1.5px;font-weight:700;",
                 "Motor de simulacion")
      ),
      tags$div(style="display:flex;flex-direction:column;gap:5px;",
               tags$div(style="color:#E8F4FD;font-size:0.82em;font-weight:600;", "Basado en Ranking FIFA"),
               tags$div(style="color:#4A7A9B;font-size:0.72em;line-height:1.5;",
                        "Ventaja de jugar en casa",  tags$br(),
                        "Hasta 1,000 simulaciones")
      )
    ),
    
    tags$div(style="margin:8px 14px;height:1px;background:linear-gradient(90deg,transparent,rgba(46,134,171,0.5),transparent);"),
    
    # ---- AUTORES ----
    tags$div(
      style = "margin:0 14px 18px;",
      tags$div(style=paste0(
        "display:flex;align-items:center;gap:8px;margin-bottom:8px;",
        "padding:6px 10px;border-radius:8px;background:rgba(46,134,171,0.08);"
      ),
      tags$div(style=paste0(
        "width:24px;height:24px;border-radius:50%;flex-shrink:0;",
        "background:linear-gradient(135deg,#1B4F72,#2E86AB);",
        "display:flex;align-items:center;justify-content:center;"
      ), icon("users", style="color:#AED6F1;font-size:0.65em;")),
      tags$span(style="color:#64CFF6;font-size:0.68em;font-weight:800;text-transform:uppercase;letter-spacing:1.5px;",
                "Autores del Proyecto")
      ),
      do.call(tagList, lapply(list(
        list(ini="JL", name="Jacobo Londono",    url="https://www.linkedin.com/in/jacobo-londo%C3%B1o-baquero-a2b86733a/"),
        list(ini="SC", name="Samuel Chamorro",   url="https://www.linkedin.com/in/samuel-chamorro-sol%C3%B3rzano-b393b831a"),
        list(ini="JB", name="Jesus D. Barrios",  url="https://www.linkedin.com/in/jesus-barriosv/")
      ), function(a) {
        tags$div(style=paste0(
          "display:flex;align-items:center;gap:9px;padding:7px 10px;",
          "border-radius:8px;margin-bottom:5px;",
          "background:rgba(11,27,56,0.7);border:1px solid rgba(46,134,171,0.18);"
        ),
        tags$div(style=paste0(
          "width:28px;height:28px;border-radius:50%;flex-shrink:0;",
          "background:linear-gradient(135deg,#1B4F72,#2E86AB);",
          "display:flex;align-items:center;justify-content:center;",
          "font-size:0.65em;font-weight:800;color:#E8F4FD;"
        ), a$ini),
        tags$div(style="flex:1;min-width:0;",
                 tags$div(style="color:#C5D8E8;font-size:0.78em;font-weight:600;white-space:nowrap;overflow:hidden;text-overflow:ellipsis;",
                          a$name),
                 tags$a(href=a$url, target="_blank",
                        style=paste0(
                          "display:inline-flex;align-items:center;gap:4px;",
                          "color:#0A66C2;font-size:0.66em;font-weight:600;text-decoration:none;"
                        ),
                        tags$svg(xmlns="http://www.w3.org/2000/svg",viewBox="0 0 24 24",width="10",height="10",
                                 style="fill:#0A66C2;flex-shrink:0;",
                                 tags$path(d="M20.447 20.452h-3.554v-5.569c0-1.328-.027-3.037-1.852-3.037-1.853 0-2.136 1.445-2.136 2.939v5.667H9.351V9h3.414v1.561h.046c.477-.9 1.637-1.85 3.37-1.85 3.601 0 4.267 2.37 4.267 5.455v6.286zM5.337 7.433a2.062 2.062 0 01-2.063-2.065 2.064 2.064 0 112.063 2.065zm1.782 13.019H3.555V9h3.564v11.452zM22.225 0H1.771C.792 0 0 .774 0 1.729v20.542C0 23.227.792 24 1.771 24h20.451C23.2 24 24 23.227 24 22.271V1.729C24 .774 23.2 0 22.222 0h.003z")
                        ),
                        "LinkedIn"
                 )
        )
        )
      }))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$link(rel="preconnect", href="https://fonts.googleapis.com"),
      tags$link(rel="stylesheet", href="https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700;800&family=JetBrains+Mono:wght@400;500&display=swap"),
      tags$style(HTML("
      *, *::before, *::after { box-sizing: border-box; }
      body, .content-wrapper, .right-side {
        font-family: 'Inter', sans-serif !important;
        background: #0D1B2A !important;
      }
      code, pre { font-family: 'JetBrains Mono', monospace !important; }

      /* SIDEBAR */
      .main-sidebar, .left-side {
        background: linear-gradient(180deg, #060F1E 0%, #0A1628 35%, #0D1F3C 70%, #0A1A30 100%) !important;
        border-right: 1px solid rgba(46,134,171,0.25) !important;
        box-shadow: 4px 0 24px rgba(0,0,0,0.5) !important;
        overflow: hidden !important;
      }
      .main-sidebar::before {
        content: '' !important;
        position: absolute !important; top: 0 !important; left: 0 !important;
        width: 100% !important; height: 100% !important;
        background-image:
          radial-gradient(circle at 20% 20%, rgba(46,134,171,0.06) 1px, transparent 1px),
          radial-gradient(circle at 80% 60%, rgba(100,207,246,0.05) 1px, transparent 1px);
        background-size: 60px 60px, 80px 80px !important;
        animation: sidebarGrid 20s linear infinite !important;
        pointer-events: none !important; z-index: 0 !important;
      }
      @keyframes sidebarGrid {
        0%   { background-position: 0 0, 0 0; }
        100% { background-position: 60px 60px, -80px 80px; }
      }
      .main-sidebar::after {
        content: '' !important; position: absolute !important;
        top: 0 !important; left: 0 !important;
        width: 2px !important; height: 100% !important;
        background: linear-gradient(180deg,transparent 0%,#2E86AB 20%,#64CFF6 50%,#2E86AB 80%,transparent 100%) !important;
        animation: energyFlow 3s ease-in-out infinite !important; opacity: 0.7 !important;
      }
      @keyframes energyFlow {
        0%, 100% { opacity: 0.4; } 50% { opacity: 0.9; }
      }
      .sidebar-menu > li { position: relative !important; z-index: 1 !important;
        margin: 2px 10px !important; border-radius: 10px !important; overflow: hidden !important; }
      .sidebar-menu > li > a {
        color: #7FADD4 !important; font-size: 0.875em !important; font-weight: 500 !important;
        letter-spacing: 0.4px !important; padding: 12px 16px !important;
        border-left: none !important; border-radius: 10px !important;
        transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1) !important;
        position: relative !important; display: flex !important; align-items: center !important;
        overflow: hidden !important; background: transparent !important;
      }
      .sidebar-menu > li > a::before {
        content: '' !important; position: absolute !important;
        top: 0 !important; left: -100% !important;
        width: 100% !important; height: 100% !important;
        background: linear-gradient(90deg,transparent 0%,rgba(100,207,246,0.08) 50%,transparent 100%) !important;
        transition: left 0.5s ease !important;
      }
      .sidebar-menu > li > a:hover::before { left: 100% !important; }
      .sidebar-menu > li > a::after {
        content: '' !important; position: absolute !important;
        left: 6px !important; top: 50% !important;
        transform: translateY(-50%) scale(0) !important;
        width: 5px !important; height: 5px !important;
        border-radius: 50% !important; background: #64CFF6 !important;
        box-shadow: 0 0 8px #64CFF6 !important;
        transition: transform 0.25s cubic-bezier(0.34,1.56,0.64,1) !important;
      }
      .sidebar-menu > li > a:hover::after,
      .sidebar-menu > li.active > a::after { transform: translateY(-50%) scale(1) !important; }
      .sidebar-menu > li > a:hover {
        color: #E8F4FD !important;
        background: linear-gradient(135deg,rgba(46,134,171,0.18) 0%,rgba(46,134,171,0.08) 100%) !important;
        transform: translateX(4px) !important;
      }
      .sidebar-menu > li.active > a,
      .sidebar-menu > li.active > a:hover {
        color: #FFFFFF !important;
        background: linear-gradient(135deg,rgba(46,134,171,0.35) 0%,rgba(46,134,171,0.15) 100%) !important;
        font-weight: 700 !important; transform: translateX(4px) !important;
        box-shadow: inset 0 1px 0 rgba(100,207,246,0.2), inset 3px 0 0 #64CFF6,
                    0 4px 16px rgba(46,134,171,0.25) !important;
      }
      .sidebar-menu > li > a > .fa { margin-right: 12px !important; width: 20px !important;
        text-align: center !important; font-size: 1.05em !important;
        transition: transform 0.3s cubic-bezier(0.34,1.56,0.64,1), color 0.3s ease !important;
        flex-shrink: 0 !important;
      }
      .sidebar-menu > li > a:hover > .fa { transform: scale(1.25) rotate(-5deg) !important;
        color: #64CFF6 !important; text-shadow: 0 0 12px rgba(100,207,246,0.7) !important; }
      .sidebar-menu > li.active > a > .fa {
        color: #64CFF6 !important; animation: iconPulse 2.5s ease-in-out infinite !important; }
      @keyframes iconPulse {
        0%,100%{text-shadow:0 0 8px rgba(100,207,246,0.4);}
        50%{text-shadow:0 0 18px rgba(100,207,246,0.9),0 0 30px rgba(46,134,171,0.4);}
      }

      /* HEADER */
      .main-header .navbar, .main-header .logo {
        background: linear-gradient(90deg, #0A1628 0%, #112240 100%) !important;
        border-bottom: 1px solid rgba(46,134,171,0.3) !important;
      }
      .main-header .logo { font-family:'Inter',sans-serif !important; font-weight:700 !important;
        font-size:1em !important; color:#E8F4FD !important; }
      .main-header .navbar-nav > li > a,
      .main-header .navbar .sidebar-toggle { color: #8BAEC8 !important; }
      .main-header .navbar .sidebar-toggle:hover { color: #64CFF6 !important; }

      /* CONTENT */
      .content-wrapper, .right-side { background: #0D1B2A !important; }
      .content { padding: 20px 24px !important; }

      /* PAGE HEADER STRIP */
      .page-header-strip {
        background: linear-gradient(135deg, #0D1F3C 0%, #1B4F72 50%, #2E86AB 100%) !important;
        color: white !important; padding: 18px 24px !important;
        border-radius: 12px !important; margin-bottom: 20px !important;
        font-size: 1.15em !important; font-weight: 700 !important;
        letter-spacing: 0.5px !important;
        box-shadow: 0 4px 20px rgba(46,134,171,0.35) !important;
        border: 1px solid rgba(100,207,246,0.15) !important;
      }

      /* KPI BOXES */
      .kpi-box {
        background: linear-gradient(135deg, #112240 0%, #0D1F3C 100%) !important;
        border: 1px solid rgba(46,134,171,0.3) !important;
        border-radius: 14px !important; padding: 20px 22px !important;
        text-align: center !important;
        box-shadow: 0 4px 24px rgba(0,0,0,0.35), inset 0 1px 0 rgba(100,207,246,0.1) !important;
        margin-bottom: 18px !important; position: relative !important;
        overflow: hidden !important;
        transition: transform 0.2s ease, box-shadow 0.2s ease !important;
      }
      .kpi-box::before {
        content: '' !important; position: absolute !important;
        top: 0 !important; left: 0 !important; right: 0 !important; height: 3px !important;
        background: linear-gradient(90deg, #2E86AB, #64CFF6) !important;
        border-radius: 14px 14px 0 0 !important;
      }
      .kpi-box:hover { transform: translateY(-3px) !important;
        box-shadow: 0 8px 32px rgba(46,134,171,0.4) !important; }
      .kpi-val { font-size: 2.2em !important; font-weight: 800 !important;
        color: #64CFF6 !important; display: block !important;
        line-height: 1.1 !important; letter-spacing: -0.5px !important; }
      .kpi-lab { font-size: 0.75em !important; color: #8BAEC8 !important;
        text-transform: uppercase !important; letter-spacing: 1px !important;
        margin-top: 6px !important; display: block !important; }
      .kpi-icon { font-size: 1.6em !important; margin-bottom: 8px !important; opacity: 0.7 !important; }

      /* BOX */
      .box {
        background: #112240 !important; border: 1px solid rgba(46,134,171,0.2) !important;
        border-top: 3px solid #2E86AB !important; border-radius: 12px !important;
        box-shadow: 0 4px 20px rgba(0,0,0,0.3) !important; margin-bottom: 20px !important;
      }
      .box.box-warning { border-top-color: #F39C12 !important; }
      .box.box-success { border-top-color: #1E8449 !important; }
      .box.box-danger  { border-top-color: #C0392B !important; }
      .box.box-info    { border-top-color: #64CFF6 !important; }
      .box-header {
        background: transparent !important;
        border-bottom: 1px solid rgba(46,134,171,0.2) !important;
        padding: 14px 18px !important;
      }
      .box-header .box-title { color: #E8F4FD !important; font-size: 0.95em !important;
        font-weight: 600 !important; }
      .box-body { color: #C5D8E8 !important; padding: 18px !important;
        background: transparent !important; }

      /* TABS */
      .nav-tabs { border-bottom: 2px solid rgba(46,134,171,0.3) !important; margin-bottom: 0 !important; }
      .nav-tabs > li > a { color: #8BAEC8 !important; font-weight: 600 !important;
        font-size: 0.87em !important; border: none !important;
        border-bottom: 3px solid transparent !important; background: transparent !important;
        padding: 10px 18px !important; transition: all 0.2s ease !important; border-radius: 0 !important; }
      .nav-tabs > li > a:hover { color: #64CFF6 !important;
        background: rgba(46,134,171,0.1) !important; border-bottom-color: #2E86AB !important; }
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus {
        color: #FFFFFF !important; background: rgba(46,134,171,0.18) !important;
        border-bottom: 3px solid #64CFF6 !important; border-radius: 6px 6px 0 0 !important; }
      .tab-content {
        background: #112240 !important; border: 1px solid rgba(46,134,171,0.2) !important;
        border-top: none !important; padding: 22px !important; border-radius: 0 0 12px 12px !important; }

      /* INFO-TEXT */
      .info-text {
        background: rgba(46,134,171,0.12) !important; border-left: 4px solid #2E86AB !important;
        border-radius: 6px !important; padding: 12px 16px !important;
        margin-bottom: 14px !important; font-size: 0.91em !important; color: #AED6F1 !important; }

      /* DATATABLE */
      .dataTables_wrapper { color: #C5D8E8 !important; }
      .dataTables_wrapper label,
      .dataTables_wrapper .dataTables_info { color: #8BAEC8 !important; }
      .dataTables_wrapper .dataTables_filter input,
      .dataTables_wrapper .dataTables_length select {
        color: #C5D8E8 !important; background: #0D1F3C !important;
        border: 1px solid rgba(46,134,171,0.4) !important;
        border-radius: 6px !important; padding: 4px 8px !important; }
      table.dataTable { background: #112240 !important; color: #C5D8E8 !important; }
      table.dataTable thead th, table.dataTable thead td {
        background: #0A1628 !important; color: #64CFF6 !important;
        border-bottom: 2px solid rgba(46,134,171,0.5) !important;
        font-weight: 600 !important; font-size: 0.87em !important; padding: 10px 12px !important; }
      table.dataTable tbody tr { background: #112240 !important; color: #C5D8E8 !important; }
      table.dataTable tbody tr:nth-child(even) { background: #0D1F3C !important; }
      table.dataTable tbody tr:hover > td { background: rgba(46,134,171,0.2) !important; color: #E8F4FD !important; }
      table.dataTable tbody td { border-color: rgba(46,134,171,0.12) !important;
        color: #C5D8E8 !important; padding: 8px 12px !important; }
      .dataTables_wrapper .dataTables_paginate .paginate_button {
        color: #8BAEC8 !important; background: transparent !important;
        border: none !important; border-radius: 4px !important; }
      .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
        background: rgba(46,134,171,0.2) !important; color: #64CFF6 !important; border: none !important; }
      .dataTables_wrapper .dataTables_paginate .paginate_button.current,
      .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
        background: #2E86AB !important; color: white !important; border: none !important; }

      /* SELECT */
      .selectize-input { background: #0D1F3C !important;
        border: 1px solid rgba(46,134,171,0.5) !important;
        color: #E8F4FD !important; border-radius: 8px !important; font-size: 0.92em !important; }
      .selectize-dropdown { background: #112240 !important;
        border: 1px solid rgba(46,134,171,0.5) !important;
        border-radius: 8px !important; box-shadow: 0 6px 20px rgba(0,0,0,0.5) !important; }
      .selectize-dropdown-content .option { color: #C5D8E8 !important; padding: 7px 14px !important; }
      .selectize-dropdown-content .option:hover,
      .selectize-dropdown-content .option.active { background: rgba(46,134,171,0.30) !important;
        color: #FFFFFF !important; }

      /* SLIDER */
      .irs--shiny .irs-bar { background: #2E86AB !important; }
      .irs--shiny .irs-handle { background: #64CFF6 !important; border-color: #2E86AB !important; }
      .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single { background: #2E86AB !important; }
      .irs--shiny .irs-grid-text { color: #8BAEC8 !important; }

      /* SCROLLBAR */
      ::-webkit-scrollbar { width: 6px; height: 6px; }
      ::-webkit-scrollbar-track { background: #0A1628; }
      ::-webkit-scrollbar-thumb { background: #2E86AB; border-radius: 3px; }
      ::-webkit-scrollbar-thumb:hover { background: #64CFF6; }

      /* MISC */
      p, li, td, th, label { color: #C5D8E8; }
      h1, h2, h3, h4, h5, h6 { color: #E8F4FD !important; }
      strong, b { color: #E8F4FD !important; }
      a { color: #64CFF6 !important; }
      a:hover { color: #AED6F1 !important; text-decoration: underline !important; }
      code { background: rgba(46,134,171,0.2) !important; color: #64CFF6 !important;
        padding: 2px 6px !important; border-radius: 4px !important; font-size: 0.88em !important; }
      pre.shiny-text-output {
        background: #0A1628 !important; color: #A9DFBF !important;
        border: 1px solid rgba(46,134,171,0.25) !important;
        border-radius: 8px !important; font-size: 0.88em !important; }

      /* INTRO HERO */
      .intro-hero {
        background: linear-gradient(135deg, #0A1628 0%, #0D1F3C 40%, #112240 100%);
        border: 1px solid rgba(46,134,171,0.25); border-radius: 16px;
        padding: 32px 36px; margin-bottom: 20px; position: relative; overflow: hidden; }
      .intro-hero::after { content: ''; position: absolute; top: -60px; right: -60px;
        width: 200px; height: 200px;
        background: radial-gradient(circle, rgba(46,134,171,0.15) 0%, transparent 70%);
        border-radius: 50%; }
      .intro-hero h2 { color: #64CFF6 !important; font-size: 1.5em; font-weight: 800; margin: 0 0 8px; }
      .intro-hero p { color: #8BAEC8; font-size: 0.95em; margin: 0; }

      /* GROUP CARD */
      .group-card {
        background: linear-gradient(135deg, #112240 0%, #0D1F3C 100%);
        border: 1px solid rgba(46,134,171,0.3); border-radius: 12px;
        padding: 16px 18px; margin-bottom: 16px;
        box-shadow: 0 4px 16px rgba(0,0,0,0.25);
        transition: transform 0.2s ease, border-color 0.2s ease; }
      .group-card:hover { transform: translateY(-2px); border-color: rgba(100,207,246,0.4); }
      .group-card .group-title { color: #64CFF6 !important; font-size: 1.1em;
        font-weight: 800; margin: 0 0 12px;
        border-bottom: 1px solid rgba(46,134,171,0.3); padding-bottom: 8px; }
      .team-row { display: flex; align-items: center; gap: 10px;
        padding: 6px 8px; border-radius: 6px; margin-bottom: 4px;
        transition: background 0.2s; cursor: default; }
      .team-row:hover { background: rgba(46,134,171,0.12); }
      .conf-dot { width: 10px; height: 10px; border-radius: 50%; flex-shrink: 0; }
      .team-name { color: #C5D8E8; font-size: 0.88em; font-weight: 500; flex: 1; }
      .team-pts  { color: #64CFF6; font-size: 0.8em; font-weight: 700;
        background: rgba(46,134,171,0.15); padding: 2px 8px; border-radius: 10px; }
      .host-badge { background: rgba(212,160,23,0.2); color: #F39C12;
        font-size: 0.65em; font-weight: 700; padding: 1px 6px; border-radius: 8px;
        border: 1px solid rgba(212,160,23,0.3); }

      /* ACTION BUTTON */
      .btn-sim {
        background: linear-gradient(135deg, #1B4F72 0%, #2E86AB 100%) !important;
        color: white !important; font-weight: 700 !important;
        border: none !important; border-radius: 10px !important;
        padding: 12px 28px !important; font-size: 1em !important;
        letter-spacing: 0.4px !important;
        box-shadow: 0 4px 20px rgba(46,134,171,0.4) !important;
        transition: all 0.3s ease !important; }
      .btn-sim:hover {
        background: linear-gradient(135deg, #2E86AB 0%, #64CFF6 100%) !important;
        box-shadow: 0 6px 28px rgba(100,207,246,0.45) !important;
        transform: translateY(-2px) !important; }

      /* STANDINGS TABLE */
      .standings-table { width: 100%; border-collapse: collapse; font-size: 0.87em; }
      .standings-table th { background: #0A1628; color: #64CFF6; padding: 8px 10px;
        border-bottom: 2px solid rgba(46,134,171,0.4); font-weight: 600; text-align: center; }
      .standings-table td { padding: 7px 10px; border-bottom: 1px solid rgba(46,134,171,0.12);
        color: #C5D8E8; text-align: center; }
      .standings-table tr:hover td { background: rgba(46,134,171,0.12); color: #E8F4FD; }
      .standings-table .rank-1 td { color: #F39C12; font-weight: 700; }
      .standings-table .rank-2 td { color: #AED6F1; font-weight: 600; }
      .standings-table .rank-3 td { color: #7FADD4; }
      .pos-badge { display: inline-flex; align-items: center; justify-content: center;
        width: 22px; height: 22px; border-radius: 50%; font-size: 0.8em; font-weight: 800; }
      .pos-1 { background: rgba(212,160,23,0.2); color: #F39C12; }
      .pos-2 { background: rgba(174,214,241,0.2); color: #AED6F1; }
      .pos-3 { background: rgba(46,134,171,0.15); color: #7FADD4; }
      .pos-4 { background: rgba(192,57,43,0.12); color: #E74C3C; }

      /* QUALIFIED CARD */
      .qual-card {
        display: inline-flex; align-items: center; gap: 8px;
        background: rgba(46,134,171,0.1); border: 1px solid rgba(46,134,171,0.25);
        border-radius: 8px; padding: 6px 12px; margin: 4px;
        font-size: 0.83em; color: #C5D8E8; }
      .qual-card.first { border-color: rgba(212,160,23,0.4); background: rgba(212,160,23,0.08); color: #F5CBA7; }
      .qual-card.second { border-color: rgba(174,214,241,0.4); background: rgba(174,214,241,0.08); }
      .qual-card.third-best { border-color: rgba(142,68,173,0.4); background: rgba(142,68,173,0.08); color: #D7BDE2; }

      .shiny-notification { background: #112240 !important;
        border: 1px solid #2E86AB !important; color: #C5D8E8 !important;
        border-radius: 8px !important; }
      "))
    ),
    
    tabItems(
      
      # =========================================================
      # INTRODUCCION
      # =========================================================
      tabItem(tabName = "intro",
              fluidRow(column(12,
                              div(class = "intro-hero",
                                  # Animated background particles
                                  tags$style(HTML("
              .intro-hero { position: relative; overflow: hidden; }
              .hero-particle {
                position: absolute; border-radius: 50%;
                background: radial-gradient(circle, rgba(100,207,246,0.3), transparent);
                animation: floatParticle 6s ease-in-out infinite;
                pointer-events: none;
              }
              @keyframes floatParticle {
                0%,100% { transform: translateY(0) scale(1); opacity: 0.4; }
                50% { transform: translateY(-20px) scale(1.2); opacity: 0.8; }
              }
              .hero-sep {
                display: inline-block; cursor: pointer;
                color: #2E86AB; font-weight: 800; padding: 0 4px;
                transition: all 0.25s cubic-bezier(0.34,1.56,0.64,1);
                border-radius: 4px;
              }
              .hero-sep:hover {
                color: #64CFF6;
                text-shadow: 0 0 12px rgba(100,207,246,0.9);
                transform: scale(1.5) rotate(180deg);
                background: rgba(100,207,246,0.1);
              }
              .hero-tag {
                display: inline-block; cursor: default;
                transition: all 0.3s cubic-bezier(0.34,1.56,0.64,1);
                border-radius: 4px; padding: 1px 4px;
              }
              .hero-tag:hover {
                color: #E8F4FD !important;
                background: rgba(46,134,171,0.2);
                transform: translateY(-2px);
                text-shadow: 0 0 8px rgba(100,207,246,0.5);
              }
              .hero-ball { display: inline-block; animation: heroBallSpin 4s linear infinite; }
              @keyframes heroBallSpin {
                0%   { filter: drop-shadow(0 0 6px rgba(100,207,246,0.6)); }
                50%  { filter: drop-shadow(0 0 14px rgba(100,207,246,1)); }
                100% { filter: drop-shadow(0 0 6px rgba(100,207,246,0.6)); }
              }
            ")),
                                  # Floating particles
                                  tags$div(class="hero-particle", style="width:80px;height:80px;top:-20px;right:80px;animation-delay:0s;"),
                                  tags$div(class="hero-particle", style="width:40px;height:40px;top:10px;right:200px;animation-delay:2s;"),
                                  tags$div(class="hero-particle", style="width:60px;height:60px;bottom:-10px;right:120px;animation-delay:1s;"),
                                  tags$h2(
                                    tags$img(
                                      src   = "trophy.png",
                                      height = "52",
                                      style  = "margin-right:14px;vertical-align:middle;filter:drop-shadow(0 0 10px rgba(245,208,96,0.6));"
                                    ),
                                    "Simulador FIFA World Cup 2026"
                                  ),
                                  tags$p(style="margin:0;",
                                         tags$span(class="hero-tag", "Basado en Ranking FIFA"),
                                         tags$span(class="hero-sep", "."),
                                         tags$span(class="hero-tag", "Simula partidos con azar real"),
                                         tags$span(class="hero-sep", "."),
                                         tags$span(class="hero-tag", "Hasta 1,000 simulaciones"),
                                         tags$span(class="hero-sep", "."),
                                         tags$span(class="hero-tag", "48 equipos"),
                                         tags$span(class="hero-sep", "."),
                                         tags$span(class="hero-tag", "12 grupos")
                                  )
                              )
              )),
              
              # KPIs estaticos
              fluidRow(
                column(3, div(class="kpi-box",
                              div(class="kpi-icon", icon("futbol", style="color:#64CFF6;")),
                              span(class="kpi-val", "48"), span(class="kpi-lab", "Equipos clasificados"))),
                column(3, div(class="kpi-box",
                              div(class="kpi-icon", icon("layer-group", style="color:#F39C12;")),
                              span(class="kpi-val", "12"), span(class="kpi-lab", "Grupos . 6 confederaciones"))),
                column(3, div(class="kpi-box",
                              div(class="kpi-icon", icon("running", style="color:#1E8449;")),
                              span(class="kpi-val", "72"), span(class="kpi-lab", "Partidos fase de grupos"))),
                column(3, div(class="kpi-box",
                              div(class="kpi-icon", icon("trophy", style="color:#C0392B;")),
                              span(class="kpi-val", "32"), span(class="kpi-lab", "Clasificados a Ronda 32")))
              ),
              
              fluidRow(column(12,
                              tabsetPanel(type = "tabs",
                                          tabPanel("\u00bfComo funciona?", br(),
                                                   fluidRow(
                                                     column(7,
                                                            div(class="info-text",
                                                                p("Este simulador recrea la fase de grupos del",
                                                                  strong("FIFA World Cup 2026."),
                                                                  "Cada partido se juega con resultados al azar, pero los equipos mejor posicionados",
                                                                  "en el Ranking FIFA tienen mas probabilidades de ganar - igual que en la realidad.")
                                                            ),
                                                            div(style="background:rgba(46,134,171,0.08);border-left:4px solid #2E86AB;border-radius:8px;padding:16px 20px;margin-bottom:16px;",
                                                                tags$h5(icon("futbol", style="color:#64CFF6;margin-right:8px;"),
                                                                        strong("\u00bfComo se decide quien gana?"), style="color:#AED6F1;margin-top:0;"),
                                                                tags$ul(
                                                                  tags$li("Cada seleccion tiene una", strong("fuerza de ataque"), "segun su posicion en el Ranking FIFA."),
                                                                  tags$li(strong("USA, Canada y Mexico"), "tienen una pequena ventaja por jugar en casa."),
                                                                  tags$li("El simulador genera goles con azar realista: el mejor equipo anota mas,", strong("pero cualquier sorpresa es posible.")),
                                                                  tags$li("Puntos: victoria = 3, empate = 1, derrota = 0.")
                                                                )
                                                            ),
                                                            div(style="background:rgba(30,132,73,0.07);border-left:4px solid #1E8449;border-radius:8px;padding:16px 20px;",
                                                                tags$h5(icon("trophy", style="color:#1E8449;margin-right:8px;"),
                                                                        strong("\u00bfQuien clasifica?"), style="color:#AED6F1;margin-top:0;"),
                                                                p("De cada grupo avanzan los", strong("2 primeros."),
                                                                  "Ademas, los", strong("8 mejores terceros"), "de todos los grupos tambien pasan.",
                                                                  "En total:", strong("32 equipos"), "a la Ronda de 16avos.")
                                                            )
                                                     ),
                                                     column(5,
                                                            div(style="background:rgba(243,156,18,0.07);border-left:4px solid #F39C12;border-radius:8px;padding:16px 20px;margin-bottom:16px;",
                                                                tags$h5(icon("dice", style="color:#F39C12;margin-right:8px;"),
                                                                        strong("\u00bfQue son las simulaciones multiples?"), style="color:#AED6F1;margin-top:0;"),
                                                                p("El simulador puede jugar el torneo completo",
                                                                  strong("hasta 1,000 veces"), "con resultados distintos.",
                                                                  "Al final ves", strong("que tan seguido clasifica cada seleccion"),
                                                                  "-- entre mas alto el porcentaje, mas probable que avance.")
                                                            ),
                                                            div(style="background:rgba(46,134,171,0.08);border-left:4px solid #2E86AB;border-radius:8px;padding:16px 20px;",
                                                                tags$h5(icon("map-signs", style="color:#64CFF6;margin-right:8px;"),
                                                                        strong("Guia rapida"), style="color:#AED6F1;margin-top:0;"),
                                                                tags$ol(
                                                                  tags$li(strong("Grupos"), " - los 12 grupos con banderas y Ranking FIFA."),
                                                                  tags$li(strong("Simulacion"), " - juega una vez y ve todos los resultados."),
                                                                  tags$li(strong("Clasificados"), " - quienes pasaron a la Ronda de 16avos."),
                                                                  tags$li(strong("Probabilidades"), " - corre hasta 1,000 torneos y ve las chances."),
                                                                  tags$li(strong("Ranking"), " - fuerza de las 48 selecciones.")
                                                                )
                                                            )
                                                     )
                                                   )
                                          ),
                                          
                                          tabPanel("Datos del torneo", br(),
                                                   fluidRow(
                                                     column(6,
                                                            div(style="background:rgba(46,134,171,0.08);border-left:4px solid #2E86AB;border-radius:8px;padding:16px 20px;margin-bottom:16px;",
                                                                tags$h5(icon("globe", style="color:#64CFF6;margin-right:8px;"),
                                                                        strong("Paises sede"), style="color:#AED6F1;margin-top:0;"),
                                                                tags$div(style="display:flex;gap:12px;flex-wrap:wrap;margin-top:8px;",
                                                                         tags$span(style="background:rgba(212,160,23,0.15);border:1px solid rgba(212,160,23,0.3);color:#F5CBA7;padding:6px 14px;border-radius:8px;font-weight:600;font-size:0.9em;",
                                                                                   flag_img("United States"), " Estados Unidos"),
                                                                         tags$span(style="background:rgba(212,160,23,0.15);border:1px solid rgba(212,160,23,0.3);color:#F5CBA7;padding:6px 14px;border-radius:8px;font-weight:600;font-size:0.9em;",
                                                                                   flag_img("Canada"), " Canada"),
                                                                         tags$span(style="background:rgba(212,160,23,0.15);border:1px solid rgba(212,160,23,0.3);color:#F5CBA7;padding:6px 14px;border-radius:8px;font-weight:600;font-size:0.9em;",
                                                                                   flag_img("Mexico"), " Mexico")
                                                                )
                                                            ),
                                                            div(style="background:rgba(46,134,171,0.08);border-left:4px solid #2E86AB;border-radius:8px;padding:16px 20px;",
                                                                tags$h5(icon("calendar", style="color:#64CFF6;margin-right:8px;"),
                                                                        strong("Formato del torneo"), style="color:#AED6F1;margin-top:0;"),
                                                                tags$table(style="width:100%;font-size:0.9em;",
                                                                           tags$tr(tags$td(strong("Sorteo oficial:")), tags$td("5 de diciembre, 2025")),
                                                                           tags$tr(tags$td(strong("Formato:")),        tags$td("12 grupos de 4 equipos")),
                                                                           tags$tr(tags$td(strong("Clasificados:")),   tags$td("32 equipos a la Ronda de 16avos")),
                                                                           tags$tr(tags$td(strong("Criterio:")),       tags$td("Top 2 por grupo + 8 mejores terceros"))
                                                                )
                                                            )
                                                     ),
                                                     column(6,
                                                            div(style="background:rgba(46,134,171,0.08);border-left:4px solid #2E86AB;border-radius:8px;padding:16px 20px;",
                                                                tags$h5(icon("layer-group", style="color:#64CFF6;margin-right:8px;"),
                                                                        strong("Regiones representadas"), style="color:#AED6F1;margin-top:0;"),
                                                                lapply(list(
                                                                  list(n="UEFA (Europa)",          c=36, col="#3266ad"),
                                                                  list(n="CONMEBOL (Sudamerica)",  c=6,  col="#1a7f5a"),
                                                                  list(n="CONCACAF (N. y C. America)", c=6, col="#d4a017"),
                                                                  list(n="CAF (Africa)",           c=9,  col="#ba7517"),
                                                                  list(n="AFC (Asia)",             c=8,  col="#8e44ad"),
                                                                  list(n="OFC (Oceania)",          c=1,  col="#c0392b")
                                                                ), function(r) {
                                                                  tags$div(style="display:flex;align-items:center;gap:10px;padding:6px 0;border-bottom:1px solid rgba(46,134,171,0.1);",
                                                                           tags$div(style=paste0("width:12px;height:12px;border-radius:50%;flex-shrink:0;background:", r$col, ";")),
                                                                           tags$span(style="color:#C5D8E8;font-weight:600;font-size:0.88em;flex:1;", r$n),
                                                                           tags$span(style="color:#64CFF6;font-size:0.82em;font-weight:700;", paste0(r$c, " eq."))
                                                                  )
                                                                })
                                                            )
                                                     )
                                                   )
                                          )
                              )
              ))
      ), # fin intro
      
      
      tabItem(tabName = "grupos",
              fluidRow(column(12, div(class="page-header-strip",
                                      icon("layer-group", style="margin-right:10px;font-size:1.2em;"),
                                      "Grupos Oficiales - FIFA World Cup 2026"
              ))),
              
              # Cards de grupos: 4 por fila, 3 filas
              do.call(tagList, lapply(seq(1, 12, by=4), function(start_idx) {
                grp_names <- names(GROUPS_DATA)[start_idx:min(start_idx+3, 12)]
                fluidRow(lapply(grp_names, function(grp) {
                  teams <- GROUPS_DATA[[grp]]
                  column(3,
                         div(class="group-card",
                             # Encabezado del grupo con columna Pts. FIFA
                             div(style="display:flex;align-items:center;justify-content:space-between;border-bottom:1px solid rgba(46,134,171,0.3);padding-bottom:8px;margin-bottom:8px;",
                                 tags$span(class="group-title", style="margin:0;border:none;padding:0;font-size:1em;", paste("Grupo", grp)),
                                 tags$span(style="color:#64CFF6;font-size:0.68em;font-weight:700;text-transform:uppercase;letter-spacing:1px;", "Pts. FIFA")
                             ),
                             lapply(teams, function(t) {
                               fpts    <- { v <- ts_get(t, "fifa_points"); if (is.na(v)) 1400 else v }
                               is_host <- t %in% HOST_COUNTRIES
                               div(class="team-row",
                                   flag_img(t),
                                   div(class="team-name", t,
                                       if (is_host) tags$span(class="host-badge", style="margin-left:6px;", "SEDE")),
                                   tags$span(class="team-pts", formatC(fpts, format="d"))
                               )
                             })
                         )
                  )
                }))
              }))
      ), # fin grupos
      
      
      # =========================================================
      # SIMULACION
      # =========================================================
      tabItem(tabName = "simulacion",
              fluidRow(column(12, div(class="page-header-strip",
                                      icon("play-circle", style="margin-right:10px;font-size:1.2em;"),
                                      "Simulacion . Fase de Grupos"
              ))),
              
              # Panel de control redisenado
              fluidRow(column(12,
                              tags$style(HTML("
            .sim-control-panel {
              background: linear-gradient(135deg, #081424 0%, #0D1F3C 60%, #0A1A2E 100%);
              border: 1px solid rgba(100,207,246,0.25);
              border-radius: 16px; padding: 24px 28px; margin-bottom: 20px;
              box-shadow: 0 8px 32px rgba(0,0,0,0.5), inset 0 1px 0 rgba(100,207,246,0.08);
              position: relative; overflow: hidden;
            }
            .sim-control-panel::before {
              content: ''; position: absolute; top: 0; left: 0; right: 0; height: 3px;
              background: linear-gradient(90deg, #1B4F72, #64CFF6, #2E86AB, #1B4F72);
            }
            .seed-label {
              color: #64CFF6; font-size: 0.72em; font-weight: 800;
              text-transform: uppercase; letter-spacing: 2px; margin-bottom: 8px;
            }
            .seed-wrapper {
              display: flex; align-items: center; gap: 10px;
            }
            .seed-input-styled input[type=number] {
              background: rgba(13,31,60,0.9) !important;
              border: 1.5px solid rgba(100,207,246,0.4) !important;
              border-radius: 10px !important; color: #E8F4FD !important;
              font-size: 1.3em !important; font-weight: 700 !important;
              text-align: center !important; padding: 10px !important;
              width: 130px !important;
              box-shadow: 0 0 12px rgba(100,207,246,0.12), inset 0 2px 4px rgba(0,0,0,0.3) !important;
              transition: border-color 0.2s, box-shadow 0.2s !important;
            }
            .seed-input-styled input[type=number]:focus {
              border-color: #64CFF6 !important;
              box-shadow: 0 0 20px rgba(100,207,246,0.3), inset 0 2px 4px rgba(0,0,0,0.3) !important;
              outline: none !important;
            }
            .seed-input-styled .form-group { margin-bottom: 0 !important; }
            .seed-dice-btn {
              background: rgba(46,134,171,0.15) !important;
              border: 1px solid rgba(46,134,171,0.35) !important;
              color: #64CFF6 !important; border-radius: 10px !important;
              padding: 10px 14px !important; cursor: pointer !important;
              transition: all 0.2s !important; font-size: 1.1em !important;
            }
            .seed-dice-btn:hover {
              background: rgba(100,207,246,0.2) !important;
              border-color: #64CFF6 !important;
              transform: rotate(20deg) scale(1.1) !important;
            }
            .sim-hint { color: #4A7A9B; font-size: 0.8em; line-height: 1.5; margin-top: 4px; }
            .sim-hint strong { color: #8BAEC8; }
          ")),
                              div(class="sim-control-panel",
                                  fluidRow(
                                    # Seed picker
                                    column(4,
                                           div(class="seed-label", icon("dice", style="margin-right:6px;"), "Numero de torneo"),
                                           div(class="seed-wrapper",
                                               div(class="seed-input-styled",
                                                   numericInput("sim_seed", label=NULL, value=42, min=1, max=99999, step=1,
                                                                width="130px")
                                               ),
                                               actionButton("btn_seed_random", label=NULL, icon=icon("random"),
                                                            class="seed-dice-btn",
                                                            title="Generar numero al azar")
                                           ),
                                           p(class="sim-hint",
                                             "Cada numero genera un", strong("torneo diferente."),
                                             br(), "Cambia el numero y vuelve a simular.")
                                    ),
                                    # Boton simular
                                    column(4,
                                           div(style="display:flex;flex-direction:column;justify-content:center;height:100%;padding-top:6px;",
                                               actionButton("btn_simular",
                                                            tagList(icon("play"), tags$span(style="margin-left:8px;font-weight:700;font-size:1.05em;", "Simular torneo")),
                                                            class="btn-sim btn btn-primary",
                                                            style="width:100%;padding:16px !important;font-size:1em !important;"
                                               )
                                           )
                                    ),
                                    # Info rapida
                                    column(4,
                                           div(style="border-left:2px solid rgba(46,134,171,0.3);padding-left:18px;height:100%;display:flex;flex-direction:column;justify-content:center;",
                                               div(style="display:flex;align-items:center;gap:8px;margin-bottom:8px;",
                                                   tags$span(style="background:rgba(30,132,73,0.2);color:#1E8449;border-radius:50%;width:24px;height:24px;display:flex;align-items:center;justify-content:center;font-size:0.8em;font-weight:800;flex-shrink:0;", "1"),
                                                   tags$span(style="color:#8BAEC8;font-size:0.82em;", "Elige o cambia el numero de torneo")
                                               ),
                                               div(style="display:flex;align-items:center;gap:8px;margin-bottom:8px;",
                                                   tags$span(style="background:rgba(100,207,246,0.15);color:#64CFF6;border-radius:50%;width:24px;height:24px;display:flex;align-items:center;justify-content:center;font-size:0.8em;font-weight:800;flex-shrink:0;", "2"),
                                                   tags$span(style="color:#8BAEC8;font-size:0.82em;", "Presiona", strong(style="color:#64CFF6;", " Simular torneo"))
                                               ),
                                               div(style="display:flex;align-items:center;gap:8px;",
                                                   tags$span(style="background:rgba(243,156,18,0.15);color:#F39C12;border-radius:50%;width:24px;height:24px;display:flex;align-items:center;justify-content:center;font-size:0.8em;font-weight:800;flex-shrink:0;", "3"),
                                                   tags$span(style="color:#8BAEC8;font-size:0.82em;", "Ve los resultados y tablas")
                                               )
                                           )
                                    )
                                  )
                              )
              )),

              # ---- PANEL DE RESULTADOS REALES (API) ----
              fluidRow(column(12,
                div(style=paste0(
                  "background:linear-gradient(135deg,rgba(30,132,73,0.1),rgba(46,134,171,0.06));",
                  "border:1px solid rgba(30,132,73,0.3);border-radius:12px;",
                  "padding:14px 20px;margin-bottom:16px;",
                  "display:flex;align-items:center;gap:16px;flex-wrap:wrap;"
                ),
                  div(style="display:flex;align-items:center;gap:12px;flex:1;min-width:280px;",
                    tags$div(style=paste0(
                      "width:40px;height:40px;border-radius:50%;flex-shrink:0;",
                      "background:rgba(30,132,73,0.15);border:2px solid rgba(30,132,73,0.4);",
                      "display:flex;align-items:center;justify-content:center;"
                    ), icon("satellite-dish", style="color:#1E8449;font-size:1.1em;")),
                    div(
                      tags$div(style="color:#2ECC71;font-weight:800;font-size:0.9em;",
                        "Resultados reales del Mundial"),
                      tags$div(style="color:#8BAEC8;font-size:0.78em;",
                        "Sincroniza los partidos ya jugados. Las simulaciones usaran el marcador real cuando exista.")
                    )
                  ),
                  div(style="display:flex;align-items:center;gap:12px;",
                    uiOutput("real_results_status"),
                    actionButton("btn_diag_real",
                      tagList(icon("search")),
                      class="btn",
                      style=paste0(
                        "background:rgba(46,134,171,0.15) !important;",
                        "color:#64CFF6 !important;border:1px solid rgba(100,207,246,0.3) !important;",
                        "border-radius:8px !important;padding:10px 14px !important;"
                      ),
                      title="Ver diagnostico de resultados reales"
                    ),
                    actionButton("btn_sync_real",
                      tagList(icon("sync"), tags$span(style="margin-left:6px;font-weight:700;", "Sincronizar")),
                      class="btn btn-primary",
                      style=paste0(
                        "background:linear-gradient(135deg,#1E8449,#27AE60) !important;",
                        "border:none !important;border-radius:8px !important;",
                        "padding:10px 18px !important;font-weight:700 !important;"
                      )
                    )
                  )
                ),
                # Panel de diagnostico (oculto hasta presionar la lupa)
                uiOutput("real_results_diag")
              )),

              # Sub-pestanas de resultados
              fluidRow(column(12,
                              tags$style(HTML("
            /* Sub-tabs styling */
            .sim-subtabs .nav-tabs {
              border-bottom: 2px solid rgba(46,134,171,0.3) !important;
              margin-bottom: 16px !important;
            }
            .sim-subtabs .nav-tabs > li > a {
              background: rgba(13,31,60,0.6) !important;
              border: 1px solid rgba(46,134,171,0.2) !important;
              color: #8BAEC8 !important;
              border-radius: 8px 8px 0 0 !important;
              font-weight: 600 !important;
              font-size: 0.9em !important;
              padding: 10px 20px !important;
              margin-right: 4px !important;
              transition: all 0.2s !important;
            }
            .sim-subtabs .nav-tabs > li > a:hover {
              background: rgba(46,134,171,0.15) !important;
              color: #64CFF6 !important;
            }
            .sim-subtabs .nav-tabs > li.active > a {
              background: rgba(46,134,171,0.25) !important;
              border-bottom-color: transparent !important;
              color: #64CFF6 !important;
              border-top: 2px solid #64CFF6 !important;
            }
          ")),
                              div(class="sim-subtabs",
                                  tabsetPanel(type="tabs", id="sim_subtabs",
                                              tabPanel(
                                                tagList(icon("table", style="margin-right:6px;"), "Tablas por grupo"),
                                                br(),
                                                uiOutput("sim_standings_ui")
                                              ),
                                              tabPanel(
                                                tagList(icon("futbol", style="margin-right:6px;"), "Resultados por jornada"),
                                                br(),
                                                uiOutput("sim_jornadas_ui")
                                              )
                                  )
                              )
              ))
      ), # fin simulacion
      
      
      # =========================================================
      # CLASIFICADOS
      # =========================================================
      tabItem(tabName = "clasificados",
              fluidRow(column(12, div(class="page-header-strip",
                                      icon("trophy", style="margin-right:10px;font-size:1.2em;"),
                                      "Clasificados - Ronda de 16avos"
              ))),
              
              # Banner de advertencia (visible solo si no hay simulacion)
              uiOutput("clasificados_banner_ui"),
              fluidRow(
                column(3, div(class="kpi-box",
                              div(class="kpi-icon", icon("medal", style="color:#F39C12;")),
                              span(class="kpi-val", uiOutput("kpi_primeros")),
                              span(class="kpi-lab", "Primeros de grupo"))),
                column(3, div(class="kpi-box",
                              div(class="kpi-icon", icon("medal", style="color:#AED6F1;")),
                              span(class="kpi-val", uiOutput("kpi_segundos")),
                              span(class="kpi-lab", "Segundos de grupo"))),
                column(3, div(class="kpi-box",
                              div(class="kpi-icon", icon("medal", style="color:#8E44AD;")),
                              span(class="kpi-val", uiOutput("kpi_terceros")),
                              span(class="kpi-lab", "Mejores terceros"))),
                column(3, div(class="kpi-box",
                              div(class="kpi-icon", icon("futbol", style="color:#1E8449;")),
                              span(class="kpi-val", uiOutput("kpi_total_qual")),
                              span(class="kpi-lab", "Total clasificados")))
              ),
              
              # -- RESULTADOS PRIMERO: tabla de clasificados --
              fluidRow(column(12,
                              box(width=12, status="primary", solidHeader=TRUE,
                                  title=tags$span(icon("trophy", style="margin-right:8px;"), "Los 32 clasificados"),
                                  uiOutput("clasificados_tabla_ui")
                              )
              )),
              
              # -- TERCEROS --
              fluidRow(column(12,
                              box(width=12, status="warning", solidHeader=TRUE,
                                  title=tags$span(icon("sort-amount-down", style="margin-right:8px;"), "Batalla de los 12 terceros - solo 8 pasan"),
                                  div(style="color:#8BAEC8;font-size:0.85em;margin-bottom:12px;",
                                      icon("info-circle", style="color:#F39C12;margin-right:6px;"),
                                      "De cada grupo, el tercer lugar compite con los otros once terceros.",
                                      strong(" Solo los 8 mejor ubicados"), " avanzan a la Ronda de 16avos. Un solo punto o gol puede hacer la diferencia."
                                  ),
                                  DTOutput("thirds_tabla")
                              )
              )),
              
              # -- GRAFICO ABAJO --
              fluidRow(column(12,
                              box(width=12, status="info", solidHeader=TRUE,
                                  title=tags$span(icon("chart-pie", style="margin-right:8px;"), "Clasificados por region del mundo"),
                                  plotlyOutput("plot_conf_qual", height="320px")
                              )
              ))
      ), # fin clasificados
      
      
      # =========================================================
      # MONTE CARLO
      # =========================================================
      tabItem(tabName = "montecarlo",
              fluidRow(column(12, div(class="page-header-strip",
                                      icon("dice", style="margin-right:10px;font-size:1.2em;"),
                                      "Probabilidades de clasificar"
              ))),
              
              # Control panel
              fluidRow(column(12,
                              div(class="sim-control-panel",
                                  fluidRow(
                                    column(4,
                                           div(class="seed-label", icon("sliders-h", style="margin-right:6px;"), "Numero de torneos a simular"),
                                           sliderInput("mc_nsims", label=NULL, min=100, max=1000, value=500, step=100, width="100%"),
                                           p(class="sim-hint", "Mas torneos =", strong("mas precision."), "500 es un buen balance.")
                                    ),
                                    column(4,
                                           div(style="display:flex;flex-direction:column;justify-content:center;height:100%;padding-top:6px;",
                                               actionButton("btn_mc",
                                                            tagList(icon("dice"), tags$span(style="margin-left:8px;font-weight:700;font-size:1.05em;", "Calcular probabilidades")),
                                                            class="btn-sim btn btn-primary",
                                                            style="width:100%;padding:16px !important;"
                                               )
                                           )
                                    ),
                                    column(4,
                                           div(style="border-left:2px solid rgba(46,134,171,0.3);padding-left:18px;height:100%;display:flex;flex-direction:column;justify-content:center;",
                                               div(style="color:#8BAEC8;font-size:0.82em;line-height:1.7;",
                                                   icon("circle", style="color:#1E8449;font-size:0.6em;margin-right:6px;"),
                                                   strong(style="color:#1E8449;", "Verde"), " = clasifica mas del 70% de las veces", br(),
                                                   icon("circle", style="color:#2E86AB;font-size:0.6em;margin-right:6px;"),
                                                   strong(style="color:#2E86AB;", "Azul"), " = clasifica entre 40% y 70%", br(),
                                                   icon("circle", style="color:#1B4F72;font-size:0.6em;margin-right:6px;"),
                                                   strong(style="color:#4A7A9B;", "Oscuro"), " = clasifica menos del 40%"
                                               )
                                           )
                                    )
                                  )
                              )
              )),
              
              uiOutput("mc_progress_ui"),
              
              # -- RESULTADOS PRIMERO --
              fluidRow(
                box(width=6, status="warning", solidHeader=TRUE,
                    title=tags$span(icon("table", style="margin-right:8px;"), "Tabla de probabilidades - todos los equipos"),
                    DTOutput("mc_tabla")
                ),
                box(width=6, status="info", solidHeader=TRUE,
                    title=tags$span(icon("layer-group", style="margin-right:8px;"), "Probabilidad promedio por region"),
                    plotlyOutput("plot_mc_conf", height="340px")
                )
              ),
              
              # -- GRAFICO + TABLA LATERAL --
              fluidRow(
                box(width=8, status="info", solidHeader=TRUE,
                    title=tags$span(icon("chart-bar", style="margin-right:8px;"), "Probabilidad de clasificar - las 48 selecciones"),
                    plotlyOutput("plot_mc_probs", height="680px")
                ),
                box(width=4, status="primary", solidHeader=TRUE,
                    title=tags$span(icon("list-ol", style="margin-right:8px;"), "Ranking de probabilidades"),
                    uiOutput("mc_prob_list_ui")
                )
              )
      ), # fin montecarlo
      
      
      # =========================================================
      # ELIMINACION DIRECTA
      # =========================================================
      tabItem(tabName = "campeon",
              fluidRow(column(12, div(class="page-header-strip",
                                      icon("crown", style="margin-right:10px;font-size:1.2em;color:#F5D060;"),
                                      "Eliminacion Directa - Simulador de Campeon"
              ))),
              
              tags$style(HTML("
          .champ-panel {
            background:linear-gradient(135deg,#081424,#0D1F3C);
            border:1px solid rgba(245,208,96,0.25);
            border-radius:16px; padding:22px 28px; margin-bottom:18px;
            position:relative; overflow:hidden;
          }
          .champ-panel::before {
            content:''; position:absolute; top:0; left:0; right:0; height:3px;
            background:linear-gradient(90deg,#8B6914,#F5D060,#C8960C,#F5D060,#8B6914);
          }
          .champ-empty {
            text-align:center; padding:52px 20px;
            background:rgba(11,27,56,0.5); border-radius:14px;
            border:1px dashed rgba(245,208,96,0.2);
          }
          .champ-hero {
            background:linear-gradient(135deg,rgba(245,208,96,0.1),rgba(139,105,20,0.06));
            border:2px solid rgba(245,208,96,0.45); border-radius:16px;
            padding:22px; text-align:center;
            box-shadow:0 0 40px rgba(245,208,96,0.08);
          }
          @keyframes trophyPulse {
            0%,100%{filter:drop-shadow(0 0 10px rgba(245,208,96,0.5));}
            50%{filter:drop-shadow(0 0 28px rgba(245,208,96,0.9));}
          }
          .trophy-glow { animation:trophyPulse 2.5s ease-in-out infinite; }
          .champ-name {
            font-size:2em; font-weight:900; color:#F5D060;
            text-shadow:0 0 18px rgba(245,208,96,0.4); margin:10px 0 4px;
          }
          .round-label {
            text-align:center; font-size:0.6em; font-weight:800;
            text-transform:uppercase; letter-spacing:2px; color:#F5D060;
            padding:5px 0 8px; border-bottom:1px solid rgba(245,208,96,0.12);
            margin-bottom:8px;
          }
          .ko-match {
            background:rgba(11,27,56,0.85); border:1px solid rgba(46,134,171,0.2);
            border-radius:8px; overflow:hidden; margin-bottom:6px;
            transition:border-color 0.2s;
          }
          .ko-match:hover { border-color:rgba(245,208,96,0.35); }
          .ko-match.final-match {
            border:2px solid rgba(245,208,96,0.5);
            box-shadow:0 0 14px rgba(245,208,96,0.1);
          }
          .ko-team {
            display:flex; align-items:center; gap:7px;
            padding:6px 10px; font-size:0.78em;
            border-bottom:1px solid rgba(46,134,171,0.08);
          }
          .ko-team:last-child { border-bottom:none; }
          .ko-team.ko-w { background:rgba(39,174,96,0.1); }
          .ko-team.ko-l { opacity:0.5; }
          .ko-tname { flex:1; white-space:nowrap; overflow:hidden; text-overflow:ellipsis; }
          .ko-tname.ko-w { color:#E8F4FD; font-weight:700; }
          .ko-tname.ko-l { color:#4A7A9B; }
          .ko-score { font-weight:900; font-size:0.95em; flex-shrink:0; }
          .ko-score.ko-w { color:#64CFF6; }
          .ko-score.ko-l { color:#4A7A9B; }
          .prob-champ-row {
            display:flex; align-items:center; gap:8px;
            padding:5px 8px; border-radius:7px; margin-bottom:3px;
            border:1px solid rgba(46,134,171,0.1); transition:background 0.15s;
          }
          .prob-champ-row:hover { background:rgba(46,134,171,0.1); }
        ")),
              
              # -- CONTROL PANEL --
              fluidRow(column(12,
                              div(class="champ-panel",
                                  fluidRow(
                                    column(3,
                                           div(class="seed-label", style="color:#F5D060;",
                                               icon("sliders-h", style="margin-right:5px;"), "Simulaciones"),
                                           sliderInput("champ_nsims", label=NULL,
                                                       min=100, max=1000, value=300, step=100, width="100%"),
                                           p(class="sim-hint", "Mas simulaciones =", strong("mas precision."))
                                    ),
                                    column(3, div(style="height:100%;display:flex;align-items:center;padding-top:6px;",
                                                  actionButton("btn_campeon",
                                                               tagList(icon("crown"),
                                                                       tags$span(style="margin-left:8px;font-weight:800;font-size:1em;",
                                                                                 "Simular Campeon")),
                                                               class="btn-sim btn btn-primary",
                                                               style=paste0(
                                                                 "width:100%;padding:14px !important;",
                                                                 "background:linear-gradient(135deg,#8B6914,#C8960C,#F5D060) !important;",
                                                                 "color:#0D1F3C !important;font-weight:900 !important;"
                                                               )
                                                  )
                                    )),
                                    column(6,
                                           div(style=paste0(
                                             "background:rgba(46,134,171,0.06);border:1px solid rgba(46,134,171,0.18);",
                                             "border-left:4px solid #64CFF6;border-radius:10px;padding:16px 20px;"
                                           ),
                                           tags$div(style="color:#64CFF6;font-size:0.72em;font-weight:800;letter-spacing:1.5px;text-transform:uppercase;margin-bottom:12px;",
                                                    icon("question-circle", style="margin-right:5px;"), "Como funciona?"),
                                           lapply(list(
                                             list("1","Fase de grupos:","72 partidos con azar basado en Ranking FIFA. Los 32 mejores clasifican a eliminacion directa."),
                                             list("2","Bracket KO:","Si hay empate al 90 min, tiempo extra. Si sigue igual, penales. El mas fuerte tiene ventaja pero cualquiera puede ganar."),
                                             list("3","Repeticion N veces:","El torneo completo se juega N veces. El porcentaje = cuantas veces fue campeon esa seleccion.")
                                           ), function(step) {
                                             div(style="display:flex;align-items:flex-start;gap:12px;margin-bottom:10px;",
                                                 div(style=paste0(
                                                   "background:rgba(100,207,246,0.15);color:#64CFF6;",
                                                   "border-radius:50%;width:24px;height:24px;flex-shrink:0;",
                                                   "display:flex;align-items:center;justify-content:center;",
                                                   "font-size:0.72em;font-weight:800;"
                                                 ), step[[1]]),
                                                 div(style="color:#8BAEC8;font-size:0.84em;line-height:1.5;",
                                                     strong(style="color:#C5D8E8;", step[[2]]), " ", step[[3]])
                                             )
                                           })
                                           )
                                    )
                                  )
                              )
              )),
              
              uiOutput("campeon_ui")
      ), # fin campeon
      # =========================================================
      # VARIABLES / STRENGTH SCORE
      # =========================================================
      
      # =========================================================
      # VARIABLES / STRENGTH SCORE
      # =========================================================
      tabItem(tabName = "variables",
              fluidRow(column(12, div(class="page-header-strip",
                                      icon("chart-bar", style="margin-right:10px;font-size:1.2em;"),
                                      "Ranking de fuerza - 48 selecciones"
              ))),
              
              fluidRow(
                column(3, div(class="kpi-box",
                              div(class="kpi-icon", icon("star", style="color:#64CFF6;")),
                              span(class="kpi-val", uiOutput("kpi_max_score")),
                              span(class="kpi-lab", "Puntaje del equipo mas fuerte"))),
                column(3, div(class="kpi-box",
                              div(class="kpi-icon", icon("futbol", style="color:#F39C12;")),
                              span(class="kpi-val", uiOutput("kpi_mean_lambda")),
                              span(class="kpi-lab", "Goles esperados por partido"))),
                column(3, div(class="kpi-box",
                              div(class="kpi-icon", icon("trophy", style="color:#1E8449;")),
                              span(class="kpi-val", uiOutput("kpi_top_team")),
                              span(class="kpi-lab", "Seleccion mejor rankeada"))),
                column(3, div(class="kpi-box",
                              div(class="kpi-icon", icon("globe", style="color:#8E44AD;")),
                              span(class="kpi-val", "6"),
                              span(class="kpi-lab", "Regiones del mundo")))
              ),
              
              fluidRow(
                box(width=8, status="primary", solidHeader=TRUE,
                    title=tags$span(icon("sort-amount-down", style="margin-right:8px;"), "Poder ofensivo - 48 selecciones"),
                    plotlyOutput("plot_strength_score", height="560px")
                ),
                box(width=4, status="warning", solidHeader=TRUE,
                    title=tags$span(icon("futbol", style="margin-right:8px;"), "Goles esperados por partido"),
                    div(style="color:#8BAEC8;font-size:0.8em;margin-bottom:10px;",
                        icon("info-circle", style="color:#64CFF6;margin-right:5px;"),
                        "Goles que cada seleccion", strong("promedia por partido"), "segun su Ranking FIFA.",
                        "Mas goles = equipo mas peligroso."
                    ),
                    uiOutput("xg_list_ui")
                )
              ),
              
              fluidRow(column(12,
                              div(class="page-header-strip", style="margin-bottom:16px;",
                                  icon("layer-group", style="margin-right:10px;"),
                                  "Estadisticas por grupo - las 48 selecciones"
                              ),
                              tags$style(HTML("
            .stats-group-grid {
              display: grid;
              grid-template-columns: repeat(4, 1fr);
              gap: 14px;
              margin-bottom: 20px;
            }
            @media (max-width:1300px) { .stats-group-grid { grid-template-columns: repeat(2,1fr); } }
            .stats-group-card {
              background: #0D1F3C;
              border: 1px solid rgba(46,134,171,0.22);
              border-radius: 12px;
              overflow: hidden;
            }
            .stats-group-card-header {
              background: linear-gradient(90deg,rgba(46,134,171,0.25),rgba(46,134,171,0.08));
              padding: 8px 14px;
              display: flex; align-items: center; justify-content: space-between;
              border-bottom: 1px solid rgba(46,134,171,0.2);
            }
            .stats-grp-name {
              color:#64CFF6; font-weight:800; font-size:0.85em;
              letter-spacing:1.5px; text-transform:uppercase;
            }
            .stats-col-labels {
              display:flex; gap:0;
              font-size:0.6em; font-weight:700; color:#4A7A9B;
              text-transform:uppercase; letter-spacing:0.8px;
            }
            .stats-col-lbl { width:52px; text-align:right; }
            .stats-team-row {
              display:flex; align-items:center; gap:8px;
              padding:7px 12px; border-bottom:1px solid rgba(46,134,171,0.07);
              transition: background 0.15s;
            }
            .stats-team-row:last-child { border-bottom:none; }
            .stats-team-row:hover { background:rgba(46,134,171,0.1); }
            .stats-team-name {
              flex:1; font-size:0.82em; color:#C5D8E8;
              white-space:nowrap; overflow:hidden; text-overflow:ellipsis;
            }
            .stats-team-name.host { color:#F39C12; }
            .stats-cell {
              width:52px; text-align:right; font-size:0.8em;
              font-weight:700; flex-shrink:0;
            }
            .stats-bar-wrap {
              width:52px; flex-shrink:0;
              display:flex; align-items:center; gap:4px;
            }
            .stats-bar-bg {
              flex:1; height:5px; background:rgba(46,134,171,0.12);
              border-radius:3px; overflow:hidden;
            }
            .stats-bar-fill { height:5px; border-radius:3px; }
          ")),
                              {
                                # Precompute group + stats
                                ts <- TEAM_STATS
                                ts$grupo <- sapply(ts$team, function(t) {
                                  for (g in names(GROUPS_DATA)) if (t %in% GROUPS_DATA[[g]]) return(g)
                                  NA_character_
                                })
                                mx_xg <- max(ts$lambda_base, na.rm=TRUE)
                                mx_ss <- max(ts$strength_score, na.rm=TRUE)
                                
                                grp_names <- names(GROUPS_DATA)
                                # Split into 3 rows of 4
                                div(class="stats-group-grid",
                                    do.call(tagList, lapply(grp_names, function(grp) {
                                      rows <- ts[ts$grupo == grp, ]
                                      rows <- rows[order(-rows$lambda_base), ]
                                      div(class="stats-group-card",
                                          # Header con labels de columnas
                                          div(class="stats-group-card-header",
                                              tags$span(class="stats-grp-name", paste("Grupo", grp)),
                                              div(class="stats-col-labels",
                                                  tags$span(class="stats-col-lbl", "FIFA"),
                                                  tags$span(class="stats-col-lbl", "Gls/P"),
                                                  tags$span(class="stats-col-lbl", "Ataque")
                                              )
                                          ),
                                          # Filas de equipos
                                          do.call(tagList, lapply(seq_len(nrow(rows)), function(i) {
                                            r      <- rows[i, ]
                                            xg     <- round(r$lambda_base, 2)
                                            ss     <- round(r$strength_score, 2)
                                            xg_pct <- xg / mx_xg
                                            ss_pct <- ss / mx_ss
                                            xg_col <- if (xg_pct > 0.75) "#27AE60"
                                            else if (xg_pct > 0.5) "#2E86AB"
                                            else if (xg_pct > 0.25) "#F39C12"
                                            else "#8BAEC8"
                                            is_host <- r$team %in% HOST_COUNTRIES
                                            
                                            div(class="stats-team-row",
                                                flag_img(as.character(r$team)),
                                                tags$span(
                                                  class=paste0("stats-team-name", if(is_host) " host" else ""),
                                                  as.character(r$team),
                                                  if(is_host) tags$span(style="font-size:0.7em;margin-left:4px;color:#F39C12;", "*")
                                                ),
                                                # FIFA pts
                                                tags$span(class="stats-cell",
                                                          style="color:#4A7A9B;font-size:0.75em;font-weight:600;",
                                                          r$fifa_points),
                                                # Goles/partido
                                                tags$span(class="stats-cell",
                                                          style=paste0("color:", xg_col, ";"),
                                                          xg),
                                                # Barra poder ataque
                                                div(class="stats-bar-wrap",
                                                    div(class="stats-bar-bg",
                                                        div(class="stats-bar-fill",
                                                            style=paste0(
                                                              "width:", round(ss_pct * 100), "%;",
                                                              "background:", xg_col, ";"
                                                            )
                                                        )
                                                    )
                                                )
                                            )
                                          }))
                                      )
                                    }))
                                )
                              }
              ))
      ), # fin variables
      
      
      # =========================================================
      # METODOLOGIA
      # =========================================================
      tabItem(tabName = "metodologia",
              fluidRow(column(12, div(class="page-header-strip",
                                      icon("cogs", style="margin-right:10px;font-size:1.2em;"),
                                      "\u00bfComo funciona el simulador?"
              ))),
              
              fluidRow(column(12,
                              box(width=12, status="primary",
                                  fluidRow(
                                    column(6,
                                           div(style="background:rgba(46,134,171,0.08);border-left:4px solid #2E86AB;border-radius:8px;padding:16px 20px;margin-bottom:16px;",
                                               tags$h5(icon("ranking-star", style="color:#64CFF6;margin-right:8px;"),
                                                       strong("Paso 1 - Ranking FIFA"), style="color:#AED6F1;margin-top:0;"),
                                               p("A cada seleccion se le asigna una", strong("fuerza de ataque"), "proporcional a su posicion en el Ranking FIFA oficial.",
                                                 "Cuanto mas alto en el ranking, mas goles esperados por partido.",
                                                 "Argentina, Francia y Espana arrancan como los mas fuertes.")
                                           ),
                                           div(style="background:rgba(30,132,73,0.07);border-left:4px solid #1E8449;border-radius:8px;padding:16px 20px;margin-bottom:16px;",
                                               tags$h5(icon("dice", style="color:#1E8449;margin-right:8px;"),
                                                       strong("Paso 2 - Simulacion de partidos"), style="color:#AED6F1;margin-top:0;"),
                                               p("Los goles de cada partido se generan con", strong("azar estadistico:"),
                                                 "el mejor equipo tiene mas probabilidad de anotar, pero cualquier marcador es posible.",
                                                 "Esto refleja la naturaleza impredecible del futbol.",
                                                 "USA, Canada y Mexico tienen una pequena", strong("ventaja de local."))
                                           ),
                                           div(style="background:rgba(243,156,18,0.07);border-left:4px solid #F39C12;border-radius:8px;padding:16px 20px;",
                                               tags$h5(icon("sort-numeric-down", style="color:#F39C12;margin-right:8px;"),
                                                       strong("Paso 3 - Clasificacion"), style="color:#AED6F1;margin-top:0;"),
                                               p("Se aplican las reglas oficiales FIFA:"),
                                               tags$ol(
                                                 tags$li(strong("Puntos"), " acumulados en el grupo."),
                                                 tags$li(strong("Diferencia de goles"), " en caso de empate."),
                                                 tags$li(strong("Goles a favor"), " como segundo desempate."),
                                                 tags$li(strong("Ranking FIFA"), " como desempate final.")
                                               )
                                           )
                                    ),
                                    column(6,
                                           div(style="background:rgba(142,68,173,0.07);border-left:4px solid #8E44AD;border-radius:8px;padding:16px 20px;margin-bottom:16px;",
                                               tags$h5(icon("random", style="color:#8E44AD;margin-right:8px;"),
                                                       strong("Paso 4 - Simulaciones multiples"), style="color:#AED6F1;margin-top:0;"),
                                               p("El simulador puede jugar el torneo completo", strong("hasta 1,000 veces,"),
                                                 "cada vez con resultados distintos.",
                                                 "El porcentaje que ves en la pestana Probabilidades indica",
                                                 strong("cuantas veces clasifico ese equipo"), "en todas las simulaciones.",
                                                 "Un 80% significa que clasifico en 8 de cada 10 torneos simulados.")
                                           ),
                                           div(style="background:rgba(192,57,43,0.07);border-left:4px solid #C0392B;border-radius:8px;padding:16px 20px;",
                                               tags$h5(icon("exclamation-triangle", style="color:#C0392B;margin-right:8px;"),
                                                       strong("Limitaciones (lo que el simulador no sabe)"), style="color:#AED6F1;margin-top:0;"),
                                               tags$ul(
                                                 tags$li("No considera", strong("lesiones, sanciones ni rotaciones"), "de plantilla."),
                                                 tags$li("No incorpora la", strong("forma reciente"), "de los equipos."),
                                                 tags$li("La ventaja de local es igual para los 3 paises sede - en realidad varia."),
                                                 tags$li("El Ranking FIFA es una aproximacion del nivel real de cada seleccion.")
                                               )
                                           )
                                    )
                                  )
                              )
              ))
      ) # fin metodologia
      
    ) # fin tabItems
    ,
    # ============================================================
    # WIDGET FLOTANTE DE CHAT (Asistente Mundial 2026)
    # ============================================================
    tags$div(id = "wc-chat",
             tags$style(HTML("
        #wc-chat * { box-sizing: border-box; }
        #wc-fab {
          position: fixed; bottom: 24px; right: 24px; z-index: 9999;
          width: 60px; height: 60px; border-radius: 50%;
          background: linear-gradient(135deg,#1B4F72,#2E86AB);
          box-shadow: 0 6px 22px rgba(46,134,171,0.5);
          display: flex; align-items: center; justify-content: center;
          cursor: pointer; color: #fff; font-size: 1.5em;
          transition: transform .15s ease, opacity .15s ease;
        }
        #wc-fab:hover { transform: scale(1.08); }
        #wc-fab.hidden { opacity: 0; pointer-events: none; transform: scale(0.6); }
        #wc-panel {
          position: fixed; bottom: 24px; right: 24px; z-index: 9999;
          width: 370px; max-width: calc(100vw - 32px);
          height: 540px; max-height: calc(100vh - 48px);
          background: #0A1628; border: 1.5px solid rgba(100,207,246,0.35);
          border-radius: 16px; box-shadow: 0 18px 50px rgba(0,0,0,0.55);
          display: none; flex-direction: column; overflow: hidden;
        }
        #wc-panel.open { display: flex; }
        .wc-header {
          background: linear-gradient(135deg,#0A1628,#13314f);
          padding: 14px 16px; display: flex; align-items: center; justify-content: space-between;
          border-bottom: 1px solid rgba(100,207,246,0.2);
        }
        .wc-title { color: #64CFF6; font-weight: 800; font-size: 0.98em;
          display: flex; align-items: center; gap: 8px; }
        .wc-title .wc-dot { width: 8px; height: 8px; border-radius: 50%;
          background: #2ECC71; box-shadow: 0 0 8px #2ECC71; }
        .wc-close { color: #5C8AB0; cursor: pointer; font-size: 1.3em;
          line-height: 1; padding: 2px 9px; border-radius: 8px; }
        .wc-close:hover { color: #E8F4FD; background: rgba(255,255,255,0.06); }
        #wc-body { flex: 1; overflow-y: auto; padding: 16px;
          display: flex; flex-direction: column; gap: 10px; }
        #wc-body::-webkit-scrollbar { width: 7px; }
        #wc-body::-webkit-scrollbar-thumb { background: rgba(100,207,246,0.25); border-radius: 6px; }
        .chat-msg { display: flex; }
        .chat-user { justify-content: flex-end; }
        .chat-bot { justify-content: flex-start; }
        .chat-bubble { max-width: 82%; padding: 10px 13px; border-radius: 13px;
          font-size: 0.9em; line-height: 1.5; white-space: pre-wrap; word-wrap: break-word; }
        .chat-user .chat-bubble { background: linear-gradient(135deg,#1B4F72,#2E86AB);
          color: #fff; border-bottom-right-radius: 4px; }
        .chat-bot .chat-bubble { background: rgba(46,134,171,0.12); color: #E8F4FD;
          border: 1px solid rgba(46,134,171,0.25); border-bottom-left-radius: 4px; }
        .wc-input-row { padding: 12px; border-top: 1px solid rgba(100,207,246,0.18);
          display: flex; gap: 8px; align-items: flex-end; background: #0A1628; }
        .wc-input-row .form-group { margin: 0; flex: 1; }
        #chat_input { background: #13243a !important; color: #E8F4FD !important;
          border: 1px solid rgba(100,207,246,0.3) !important; border-radius: 10px !important;
          resize: none !important; font-size: 0.9em !important; min-height: 42px !important; max-height: 120px; }
        #chat_input::placeholder { color: #5C8AB0 !important; }
        #chat_send_btn { background: linear-gradient(135deg,#1B4F72,#2E86AB) !important;
          color: #fff !important; border: none !important; border-radius: 10px !important;
          height: 42px; width: 46px; display: flex; align-items: center;
          justify-content: center; padding: 0 !important; }
        #chat_send_btn:hover { filter: brightness(1.12); }
        @media (max-width: 480px) {
          #wc-panel { width: calc(100vw - 24px); height: calc(100vh - 90px); bottom: 12px; right: 12px; }
        }
      ")),
             tags$div(id = "wc-panel",
                      tags$div(class = "wc-header",
                               tags$div(class = "wc-title",
                                        tags$span(class = "wc-dot"), icon("robot"),
                                        tags$span("Asistente Mundial 2026")),
                               tags$div(id = "wc-close", class = "wc-close", HTML("&times;"))
                      ),
                      tags$div(id = "wc-body", uiOutput("chat_messages")),
                      tags$div(class = "wc-input-row",
                               textAreaInput("chat_input", label = NULL,
                                             placeholder = "Pregunta sobre el Mundial...", width = "100%", rows = 1),
                               actionButton("chat_send_btn", label = icon("paper-plane"))
                      )
             ),
             tags$div(id = "wc-fab", icon("comments")),
             tags$script(HTML("
        (function(){
          function scrollChat(){ var b=document.getElementById('wc-body');
            if(b){ setTimeout(function(){ b.scrollTop=b.scrollHeight; }, 60); } }
          function openPanel(){ var p=document.getElementById('wc-panel'),
              f=document.getElementById('wc-fab');
            if(p) p.classList.add('open'); if(f) f.classList.add('hidden');
            scrollChat(); var ta=document.getElementById('chat_input');
            if(ta) setTimeout(function(){ ta.focus(); }, 60); }
          function closePanel(){ var p=document.getElementById('wc-panel'),
              f=document.getElementById('wc-fab');
            if(p) p.classList.remove('open'); if(f) f.classList.remove('hidden'); }
          document.addEventListener('click', function(e){
            if(e.target.closest && e.target.closest('#wc-fab')) openPanel();
            if(e.target.closest && e.target.closest('#wc-close')) closePanel();
          });
          document.addEventListener('keydown', function(e){
            if(e.target && e.target.id==='chat_input' && e.key==='Enter' && !e.shiftKey){
              e.preventDefault();
              var btn=document.getElementById('chat_send_btn'); if(btn) btn.click();
            }
          });
          if(window.Shiny){ Shiny.addCustomMessageHandler('scrollChat', function(x){ scrollChat(); }); }
          var target=document.getElementById('wc-body');
          if(target && window.MutationObserver){
            new MutationObserver(scrollChat).observe(target, {childList:true, subtree:true});
          }
        })();
      "))
    )
  ) # fin dashboardBody
) # fin dashboardPage


# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {
  
  # ============================================================
  # CHATBOT GEMINI - logica de servidor
  # ============================================================
  chat_history <- reactiveVal(list(
    list(role = "model",
         text = paste0("Hola! Soy tu asistente del Mundial 2026. Preguntame por los grupos, ",
                       "el ranking, quien clasifica o quien es favorito. Corre las simulaciones ",
                       "para que pueda darte numeros exactos."))
  ))
  
  # Construye el contexto en vivo leyendo lo que el usuario ya calculo
  build_chat_context <- function() {
    L <- c("=== DATOS DEL TORNEO (FIFA World Cup 2026) ===", "", "GRUPOS:")
    for (g in names(GROUPS_DATA))
      L <- c(L, paste0("Grupo ", g, ": ", paste(GROUPS_DATA[[g]], collapse = ", ")))
    
    ts <- TEAM_STATS[order(-TEAM_STATS$fifa_points), ]
    L <- c(L, "", "RANKING Y FUERZA (equipo | confederacion | pts FIFA | fuerza/14 | sede):")
    for (i in seq_len(nrow(ts)))
      L <- c(L, sprintf("%s | %s | %d | %.1f | %s",
                        ts$team[i], ts$confederation[i], as.integer(ts$fifa_points[i]),
                        ts$strength_score[i], ifelse(isTRUE(ts$is_host[i]), "SEDE", "-")))
    
    sr <- sim_result()
    if (!is.null(sr) && !is.null(sr$qualified) && nrow(sr$qualified) > 0) {
      q <- sr$qualified
      L <- c(L, "", "=== CLASIFICADOS DE LA ULTIMA SIMULACION (fase de grupos) ===")
      for (i in seq_len(nrow(q)))
        L <- c(L, sprintf("%s - grupo %s, puesto %s, %d pts, DG %d",
                          q$team[i], q$group[i], q$pos[i], q$PTS[i], q$DG[i]))
    } else {
      L <- c(L, "", "NOTA: el usuario AUN NO ha corrido la simulacion de fase de grupos (pestana Simulacion).")
    }
    
    mc <- mc_result()
    if (!is.null(mc) && nrow(mc) > 0) {
      L <- c(L, "", "=== PROBABILIDAD DE CLASIFICAR A DIECISEISAVOS (Monte Carlo) ===")
      for (i in seq_len(nrow(mc)))
        L <- c(L, sprintf("%s: %.1f%%", mc$team[i], 100 * mc$prob[i]))
    } else {
      L <- c(L, "", "NOTA: el usuario AUN NO ha corrido las probabilidades Monte Carlo (pestana Probabilidades).")
    }
    
    cr <- champ_result()
    if (!is.null(cr) && nrow(cr) > 0) {
      L <- c(L, "", "=== PROBABILIDAD DE SER CAMPEON (top 20) ===")
      topn <- head(cr, 20)
      for (i in seq_len(nrow(topn)))
        L <- c(L, sprintf("%s: %.1f%%", topn$team[i], 100 * topn$prob[i]))
    }
    cb <- champ_bracket()
    if (!is.null(cb) && !is.null(cb$champion))
      L <- c(L, "", paste0("Campeon de la ultima simulacion de bracket: ", cb$champion))
    
    paste(L, collapse = "\n")
  }
  
  output$chat_messages <- renderUI({
    msgs <- chat_history()
    lapply(msgs, function(m) {
      cls <- if (identical(m$role, "user")) "chat-msg chat-user" else "chat-msg chat-bot"
      tags$div(class = cls, tags$div(class = "chat-bubble", m$text))
    })
  })
  # El panel arranca oculto (display:none); evitamos que Shiny suspenda el output
  outputOptions(output, "chat_messages", suspendWhenHidden = FALSE)
  
  observeEvent(input$chat_send_btn, {
    txt <- trimws(input$chat_input %||% "")
    if (identical(txt, "")) return(NULL)
    
    hist <- c(chat_history(), list(list(role = "user", text = txt)))
    chat_history(hist)
    updateTextAreaInput(session, "chat_input", value = "")
    session$sendCustomMessage("scrollChat", TRUE)
    
    system_text <- paste0(CHAT_SYSTEM_BASE, "\n\n", build_chat_context())
    reply <- call_gemini(system_text, hist)
    
    chat_history(c(chat_history(), list(list(role = "model", text = reply))))
    session$sendCustomMessage("scrollChat", TRUE)
  })
  
  
  # ---- Reactive: resultado de simulacion unica ----
  sim_result <- reactiveVal(NULL)
  
  # ---- Resultados reales: sincronizar con la API ----
  real_sync_trigger <- reactiveVal(0)

  observeEvent(input$btn_sync_real, {
    withProgress(message = "Conectando con football-data.org...", value = 0.3, {
      res <- fetch_real_results()
      setProgress(1.0)
      if (isTRUE(res$ok)) {
        showNotification(res$msg, type = "message", duration = 6)
        real_sync_trigger(real_sync_trigger() + 1)  # fuerza refresco de UI
      } else {
        showNotification(res$msg, type = "error", duration = 8)
      }
    })
  })

  # Diagnostico: mostrar/ocultar la lista de partidos cargados
  diag_visible <- reactiveVal(FALSE)
  observeEvent(input$btn_diag_real, {
    diag_visible(!diag_visible())
  })

  output$real_results_diag <- renderUI({
    real_sync_trigger()
    if (!diag_visible()) return(NULL)
    ms <- REAL_RESULTS$matches
    if (length(ms) == 0) {
      return(div(style="margin-top:12px;color:#8BAEC8;font-size:0.82em;",
        "No hay partidos cargados. Presiona Sincronizar primero."))
    }
    # Ordenar por grupo
    ord <- order(sapply(ms, function(m) m$grp))
    ms  <- ms[ord]

    div(style=paste0(
      "margin-top:14px;padding:14px;border-radius:10px;",
      "background:rgba(8,19,31,0.6);border:1px solid rgba(46,134,171,0.2);",
      "max-height:340px;overflow-y:auto;"
    ),
      tags$div(style="color:#64CFF6;font-size:0.78em;font-weight:800;text-transform:uppercase;letter-spacing:1px;margin-bottom:10px;",
        icon("clipboard-list", style="margin-right:6px;"),
        paste0("Diagnostico - ", length(ms), " partidos cargados desde la API")),
      tags$table(style="width:100%;border-collapse:collapse;font-size:0.8em;",
        tags$thead(tags$tr(style="border-bottom:1px solid rgba(46,134,171,0.3);",
          tags$th(style="text-align:left;padding:4px 8px;color:#4A7A9B;", "Grupo"),
          tags$th(style="text-align:left;padding:4px 8px;color:#4A7A9B;", "Local"),
          tags$th(style="text-align:center;padding:4px 8px;color:#4A7A9B;", "Marcador"),
          tags$th(style="text-align:left;padding:4px 8px;color:#4A7A9B;", "Visitante"),
          tags$th(style="text-align:center;padding:4px 8px;color:#4A7A9B;", "Estado")
        )),
        tags$tbody(
          lapply(ms, function(m) {
            played <- isTRUE(m$played)
            score  <- if (played) paste0(m$ga, " - ", m$gb) else "-"
            tags$tr(style="border-bottom:1px solid rgba(46,134,171,0.08);",
              tags$td(style="padding:4px 8px;color:#64CFF6;font-weight:700;", m$grp),
              tags$td(style="padding:4px 8px;color:#C5D8E8;", m$a),
              tags$td(style=paste0("text-align:center;padding:4px 8px;font-weight:800;color:",
                if(played) "#2ECC71" else "#4A7A9B", ";"), score),
              tags$td(style="padding:4px 8px;color:#C5D8E8;", m$b),
              tags$td(style="text-align:center;padding:4px 8px;",
                if (played)
                  tags$span(style="color:#2ECC71;font-size:0.85em;", icon("check"), " Real")
                else
                  tags$span(style="color:#4A7A9B;font-size:0.85em;", "Pendiente")
              )
            )
          })
        )
      )
    )
  })

  output$real_results_status <- renderUI({
    real_sync_trigger()  # dependencia reactiva
    s <- real_results_summary()
    if (s$total == 0) {
      return(tags$span(style="color:#8BAEC8;font-size:0.8em;",
        icon("circle", style="color:#4A7A9B;font-size:0.6em;margin-right:5px;"),
        "Sin sincronizar"))
    }
    tags$div(style="text-align:right;",
      tags$div(style="color:#2ECC71;font-size:0.82em;font-weight:700;",
        icon("check-circle", style="margin-right:4px;"),
        paste0(s$played, " partidos reales")),
      tags$div(style="color:#4A7A9B;font-size:0.7em;",
        paste0("de ", s$total, " cargados"))
    )
  })

  observeEvent(input$btn_seed_random, {
    updateNumericInput(session, "sim_seed", value = sample(1:99999, 1))
  })
  
  observeEvent(input$btn_simular, {
    withProgress(message = "Simulando fase de grupos...", value = 0, {
      setProgress(0.3, detail = "Ejecutando partidos jornada 1-3")
      res <- tryCatch(
        run_group_stage(seed = input$sim_seed),
        error = function(e) {
          showNotification(paste("Error en simulacion:", e$message), type="error", duration=5)
          NULL
        }
      )
      setProgress(1.0, detail = "Completado")
      sim_result(res)
    })
  })
  
  # ---- Banner de "simula primero" en clasificados ----
  output$clasificados_banner_ui <- renderUI({
    if (!is.null(sim_result())) return(NULL)  # ya hay simulacion, no mostrar nada
    
    tags$div(
      style = paste0(
        "margin:0 0 20px;padding:20px 24px;",
        "background:linear-gradient(135deg,rgba(243,156,18,0.12) 0%,rgba(243,156,18,0.06) 100%);",
        "border:1.5px solid rgba(243,156,18,0.45);border-radius:14px;",
        "display:flex;align-items:center;gap:18px;"
      ),
      # Icono grande
      tags$div(
        style = paste0(
          "width:52px;height:52px;border-radius:50%;flex-shrink:0;",
          "background:rgba(243,156,18,0.15);border:2px solid rgba(243,156,18,0.4);",
          "display:flex;align-items:center;justify-content:center;"
        ),
        icon("play-circle", style="color:#F39C12;font-size:1.4em;")
      ),
      # Texto
      tags$div(style="flex:1;",
               tags$div(style="color:#F39C12;font-weight:800;font-size:1.05em;margin-bottom:4px;",
                        "Primero debes simular el torneo"
               ),
               tags$div(style="color:#8BAEC8;font-size:0.88em;line-height:1.5;",
                        "Esta pestana muestra los resultados de la fase de grupos.",
                        tags$br(),
                        "Ve a la pestana ", tags$strong(style="color:#E8F4FD;", "Simulacion"),
                        ", presiona ", tags$strong(style="color:#64CFF6;", "Simular torneo"),
                        " y luego vuelve aqui para ver quienes clasificaron."
               )
      ),
      # Flecha indicadora
      tags$div(
        style = "flex-shrink:0;",
        actionButton("go_to_sim", 
                     tagList(icon("play-circle"), tags$span(style="margin-left:6px;", "Ir a Simulacion")),
                     style = paste0(
                       "background:linear-gradient(135deg,#1B4F72,#2E86AB)!important;",
                       "color:white!important;border:none!important;border-radius:10px!important;",
                       "padding:10px 18px!important;font-weight:700!important;",
                       "box-shadow:0 4px 16px rgba(46,134,171,0.4)!important;"
                     )
        )
      )
    )
  })
  
  # Navegar a simulacion al hacer click en el boton del banner
  observeEvent(input$go_to_sim, {
    updateTabItems(session, "sidebar_tabs", selected = "simulacion")
  })
  # ============================================================
  # ============================================================
  # CAMPEON SERVER - reescrito desde cero
  # ============================================================
  champ_result  <- reactiveVal(NULL)
  champ_bracket <- reactiveVal(NULL)
  
  # Helper interno: tarjeta de partido KO
  ko_card <- function(m, is_final=FALSE) {
    if (is.null(m)) return(NULL)
    ta <- if (!is.null(m$a) && !is.na(m$a)) as.character(m$a) else "TBD"
    tb <- if (!is.null(m$b) && !is.na(m$b)) as.character(m$b) else "TBD"
    ga <- if (!is.null(m$ga)) m$ga else 0
    gb <- if (!is.null(m$gb)) m$gb else 0
    wn <- if (!is.null(m$winner)) as.character(m$winner) else ta
    wa <- (wn == ta)
    cls <- if (is_final) "ko-match final-match" else "ko-match"
    div(class=cls,
        div(class=paste0("ko-team ", if(wa) "ko-w" else "ko-l"),
            flag_img(ta),
            tags$span(class=paste0("ko-tname ", if(wa) "ko-w" else "ko-l"), ta),
            tags$span(class=paste0("ko-score ", if(wa) "ko-w" else "ko-l"), ga)
        ),
        div(class=paste0("ko-team ", if(!wa) "ko-w" else "ko-l"),
            flag_img(tb),
            tags$span(class=paste0("ko-tname ", if(!wa) "ko-w" else "ko-l"), tb),
            tags$span(class=paste0("ko-score ", if(!wa) "ko-w" else "ko-l"), gb)
        )
    )
  }
  
  observeEvent(input$btn_campeon, {
    withProgress(message="Simulando el Mundial completo...", value=0, {
      n   <- input$champ_nsims
      all <- unlist(GROUPS_DATA)
      cnt <- setNames(integer(length(all)), all)
      last_brk <- NULL
      
      for (s in seq_len(n)) {
        full_sim <- tryCatch(run_group_stage(), error=function(e) NULL)
        if (is.null(full_sim)) next
        brk <- tryCatch(simulate_bracket_full(full_sim$qualified), error=function(e) {
          if (s == 1) showNotification(
            paste("Error bracket:", conditionMessage(e)), type="error", duration=6)
          NULL
        })
        if (is.null(brk)) next
        ch  <- brk$champion
        idx <- match(ch, names(cnt))
        if (!is.na(idx)) cnt[idx] <- cnt[idx] + 1L
        last_brk <- brk
        if (s %% 50 == 0) setProgress(0.1 + 0.85*(s/n),
                                      detail=paste0(s, "/", n, " simulaciones"))
      }
      
      setProgress(1.0, detail="Completado")
      
      if (is.null(last_brk)) {
        showNotification("No se pudo completar ninguna simulacion. Revisa los datos.", type="error")
        return()
      }
      
      prob_df <- data.frame(
        team = names(cnt),
        prob = as.numeric(cnt) / n,
        stringsAsFactors = FALSE
      )
      prob_df <- prob_df[order(-prob_df$prob), ]
      prob_df <- prob_df[prob_df$prob > 0, ]
      
      champ_result(prob_df)
      champ_bracket(last_brk)
    })
  })
  
  output$campeon_ui <- renderUI({
    # -- Estado vacio --
    if (is.null(champ_result())) {
      return(div(class="champ-empty",
                 tags$img(src="trophy.png", height="72",
                          style="display:block;margin:0 auto 18px;filter:drop-shadow(0 0 10px rgba(245,208,96,0.4));opacity:0.75;"),
                 tags$div(style="color:#F5D060;font-size:1.05em;font-weight:700;margin-bottom:8px;",
                          "Simula el Campeon del Mundial"),
                 tags$div(style="color:#4A7A9B;font-size:0.88em;",
                          "Elige el numero de simulaciones y presiona",
                          tags$strong(style="color:#64CFF6;", " Simular Campeon"), ".")
      ))
    }
    
    df  <- champ_result()
    brk <- champ_bracket()
    
    # Safe bracket accessors
    get_r <- function(lst, i) {
      if (is.null(lst) || i > length(lst)) {
        list(a="TBD", b="TBD", winner="TBD", ga=0, gb=0)
      } else lst[[i]]
    }
    get_sf <- function(i) get_r(brk$sf, i)
    get_fin <- function() get_r(brk$final, 1)
    
    champ     <- df$team[1]
    champ_pct <- round(df$prob[1]*100, 1)
    
    tagList(
      # -- CAMPEON HERO --
      fluidRow(column(12,
                      div(class="champ-hero", style="margin-bottom:18px;",
                          tags$div(style="font-size:0.68em;font-weight:800;letter-spacing:3px;color:#8B6914;text-transform:uppercase;margin-bottom:10px;",
                                   "FIFA World Cup 2026 - Campeon del Mundo"),
                          div(style="display:flex;align-items:center;justify-content:center;gap:20px;flex-wrap:wrap;",
                              tags$img(src="trophy.png", height="76",
                                       class="trophy-glow",
                                       style="flex-shrink:0;"),
                              div(style="text-align:left;",
                                  div(style="display:flex;align-items:center;gap:12px;",
                                      tags$div(style="transform:scale(2);", flag_img(champ)),
                                      tags$div(class="champ-name", champ)
                                  ),
                                  tags$div(style="color:#8BAEC8;font-size:0.88em;margin-top:6px;",
                                           paste0("Campeon en ", champ_pct, "% de ", input$champ_nsims, " simulaciones")),
                                  if (nrow(df) >= 2) tags$div(style="color:#4A7A9B;font-size:0.78em;margin-top:3px;",
                                                              paste0("2 lugar: ", df$team[2], " (", round(df$prob[2]*100,1), "%)  |  ",
                                                                     if(nrow(df)>=3) paste0("3 lugar: ", df$team[3], " (", round(df$prob[3]*100,1), "%)") else ""))
                              )
                          )
                      )
      )),
      
      # -- BRACKET + PROBABILIDADES --
      fluidRow(column(12,
                      div(style=paste0(
                        "background:rgba(243,156,18,0.07);border:1px solid rgba(243,156,18,0.25);",
                        "border-radius:10px;padding:12px 18px;margin-bottom:14px;",
                        "display:flex;align-items:center;gap:14px;"
                      ),
                      icon("info-circle", style="color:#F39C12;font-size:1.1em;flex-shrink:0;"),
                      div(
                        tags$span(style="color:#F39C12;font-weight:700;font-size:0.9em;",
                                  "Nota: "),
                        tags$span(style="color:#8BAEC8;font-size:0.88em;",
                                  "El bracket mostrado corresponde a la ",
                                  strong(style="color:#E8F4FD;", "ultima simulacion"),
                                  " de ", input$champ_nsims, " torneos jugados.",
                                  " El campeon puede variar en cada ejecucion.",
                                  " El ranking de la derecha muestra ",
                                  strong(style="color:#64CFF6;", "la probabilidad acumulada"),
                                  " de ganar el mundial en todas las simulaciones.")
                      )
                      )
      )),
      fluidRow(
        # Bracket mundialista
        column(9,
               div(style=paste0(
                 "background:linear-gradient(160deg,#061120,#0D1F3C,#061120);",
                 "border:1px solid rgba(245,208,96,0.2);border-radius:16px;",
                 "padding:20px;overflow-x:auto;"
               ),
               tags$style(HTML("
            .wc-bracket {
              display:flex;align-items:stretch;gap:0;min-width:860px;
            }
            .wc-col { display:flex;flex-direction:column;flex:1;min-width:0; }
            .wc-col-label {
              text-align:center;font-size:0.58em;font-weight:800;
              text-transform:uppercase;letter-spacing:1.5px;color:#F5D060;
              padding:5px 4px 10px;border-bottom:1px solid rgba(245,208,96,0.15);
              margin-bottom:8px;
            }
            .wc-matches { display:flex;flex-direction:column;justify-content:space-around;flex:1;gap:4px;padding:0 3px; }
            .wc-match {
              background:rgba(11,27,56,0.9);
              border:1px solid rgba(46,134,171,0.22);
              border-radius:7px;overflow:hidden;
              transition:border-color 0.2s;
            }
            .wc-match:hover { border-color:rgba(245,208,96,0.4); }
            .wc-match.wc-final {
              border:2px solid rgba(245,208,96,0.6);
              box-shadow:0 0 16px rgba(245,208,96,0.12);
            }
            .wc-team {
              display:flex;align-items:center;gap:5px;
              padding:5px 7px;font-size:0.72em;
              border-bottom:1px solid rgba(46,134,171,0.1);min-height:26px;
            }
            .wc-team:last-child { border-bottom:none; }
            .wc-team.wc-w { background:rgba(39,174,96,0.1); }
            .wc-team.wc-l { opacity:0.45; }
            .wc-tname { flex:1;white-space:nowrap;overflow:hidden;text-overflow:ellipsis; }
            .wc-tname.wc-w { color:#E8F4FD;font-weight:700; }
            .wc-tname.wc-l { color:#4A7A9B; }
            .wc-sc { font-weight:900;font-size:0.9em;flex-shrink:0;min-width:12px;text-align:right; }
            .wc-sc.wc-w { color:#64CFF6; }
            .wc-sc.wc-l { color:#4A7A9B; }
            .wc-conn { width:14px;flex-shrink:0;display:flex;flex-direction:column;justify-content:space-around;padding:0; }
          ")),
               
               tags$div(class="wc-bracket",
                        
                        # ---- LEFT SIDE: R32 = 16avos ----
                        tags$div(class="wc-col",
                                 tags$div(class="wc-col-label", "16avos"),
                                 tags$div(class="wc-matches",
                                          lapply(1:8, function(i) {
                                            m <- get_r(brk$r32, i)
                                            ta <- as.character(m$a); tb <- as.character(m$b)
                                            wa <- !is.null(m$winner) && as.character(m$winner)==ta
                                            div(class="wc-match",
                                                div(class=paste0("wc-team ",if(wa)"wc-w"else"wc-l"),
                                                    flag_img(ta), tags$span(class=paste0("wc-tname ",if(wa)"wc-w"else"wc-l"),ta),
                                                    tags$span(class=paste0("wc-sc ",if(wa)"wc-w"else"wc-l"),m$ga)),
                                                div(class=paste0("wc-team ",if(!wa)"wc-w"else"wc-l"),
                                                    flag_img(tb), tags$span(class=paste0("wc-tname ",if(!wa)"wc-w"else"wc-l"),tb),
                                                    tags$span(class=paste0("wc-sc ",if(!wa)"wc-w"else"wc-l"),m$gb))
                                            )
                                          })
                                 )
                        ),
                        
                        # ---- LEFT R16 = Octavos ----
                        tags$div(class="wc-col",
                                 tags$div(class="wc-col-label", "Octavos"),
                                 tags$div(class="wc-matches",
                                          lapply(1:4, function(i) {
                                            m  <- get_r(brk$r16, i)
                                            ta <- as.character(m$a); tb <- as.character(m$b)
                                            wa <- !is.null(m$winner) && as.character(m$winner)==ta
                                            div(class="wc-match",
                                                div(class=paste0("wc-team ",if(wa)"wc-w"else"wc-l"),
                                                    flag_img(ta), tags$span(class=paste0("wc-tname ",if(wa)"wc-w"else"wc-l"),ta),
                                                    tags$span(class=paste0("wc-sc ",if(wa)"wc-w"else"wc-l"),m$ga)),
                                                div(class=paste0("wc-team ",if(!wa)"wc-w"else"wc-l"),
                                                    flag_img(tb), tags$span(class=paste0("wc-tname ",if(!wa)"wc-w"else"wc-l"),tb),
                                                    tags$span(class=paste0("wc-sc ",if(!wa)"wc-w"else"wc-l"),m$gb))
                                            )
                                          })
                                 )
                        ),
                        
                        # ---- LEFT QF = Cuartos ----
                        tags$div(class="wc-col",
                                 tags$div(class="wc-col-label", "Cuartos"),
                                 tags$div(class="wc-matches",
                                          lapply(1:2, function(i) {
                                            m  <- get_r(brk$qf, i)
                                            ta <- as.character(m$a); tb <- as.character(m$b)
                                            wa <- !is.null(m$winner) && as.character(m$winner)==ta
                                            div(class="wc-match",
                                                div(class=paste0("wc-team ",if(wa)"wc-w"else"wc-l"),
                                                    flag_img(ta), tags$span(class=paste0("wc-tname ",if(wa)"wc-w"else"wc-l"),ta),
                                                    tags$span(class=paste0("wc-sc ",if(wa)"wc-w"else"wc-l"),m$ga)),
                                                div(class=paste0("wc-team ",if(!wa)"wc-w"else"wc-l"),
                                                    flag_img(tb), tags$span(class=paste0("wc-tname ",if(!wa)"wc-w"else"wc-l"),tb),
                                                    tags$span(class=paste0("wc-sc ",if(!wa)"wc-w"else"wc-l"),m$gb))
                                            )
                                          })
                                 )
                        ),
                        
                        # ---- LEFT SF = Semis ----
                        tags$div(class="wc-col",
                                 tags$div(class="wc-col-label", "Semifinal"),
                                 tags$div(class="wc-matches",
                                          {
                                            m  <- get_sf(1)
                                            ta <- as.character(m$a); tb <- as.character(m$b)
                                            wa <- !is.null(m$winner) && as.character(m$winner)==ta
                                            div(class="wc-match",
                                                div(class=paste0("wc-team ",if(wa)"wc-w"else"wc-l"),
                                                    flag_img(ta), tags$span(class=paste0("wc-tname ",if(wa)"wc-w"else"wc-l"),ta),
                                                    tags$span(class=paste0("wc-sc ",if(wa)"wc-w"else"wc-l"),m$ga)),
                                                div(class=paste0("wc-team ",if(!wa)"wc-w"else"wc-l"),
                                                    flag_img(tb), tags$span(class=paste0("wc-tname ",if(!wa)"wc-w"else"wc-l"),tb),
                                                    tags$span(class=paste0("wc-sc ",if(!wa)"wc-w"else"wc-l"),m$gb))
                                            )
                                          }
                                 )
                        ),
                        
                        # ---- CENTER: FINAL + TROPHY ----
                        tags$div(style="flex:0 0 140px;display:flex;flex-direction:column;align-items:center;justify-content:center;padding:0 6px;",
                                 tags$div(style="color:#F5D060;font-size:0.62em;font-weight:800;text-transform:uppercase;letter-spacing:2px;text-align:center;margin-bottom:12px;border-bottom:1px solid rgba(245,208,96,0.2);padding-bottom:6px;width:100%;",
                                          "GRAN FINAL"),
                                 {
                                   m  <- get_fin()
                                   ta <- as.character(m$a); tb <- as.character(m$b)
                                   wa <- !is.null(m$winner) && as.character(m$winner)==ta
                                   tagList(
                                     tags$img(src="trophy.png", height="50",
                                              class="trophy-glow",
                                              style="display:block;margin:0 auto 10px;"),
                                     div(class="wc-match wc-final", style="width:100%;",
                                         div(class=paste0("wc-team ",if(wa)"wc-w"else"wc-l"),
                                             flag_img(ta), tags$span(class=paste0("wc-tname ",if(wa)"wc-w"else"wc-l"),ta),
                                             tags$span(class=paste0("wc-sc ",if(wa)"wc-w"else"wc-l"),m$ga)),
                                         div(class=paste0("wc-team ",if(!wa)"wc-w"else"wc-l"),
                                             flag_img(tb), tags$span(class=paste0("wc-tname ",if(!wa)"wc-w"else"wc-l"),tb),
                                             tags$span(class=paste0("wc-sc ",if(!wa)"wc-w"else"wc-l"),m$gb))
                                     ),
                                     tags$div(style="text-align:center;margin-top:8px;font-size:0.7em;font-weight:800;color:#F5D060;",
                                              paste("Campeon:", as.character(m$winner)))
                                   )
                                 }
                        ),
                        
                        # ---- RIGHT SF = Semis ----
                        tags$div(class="wc-col",
                                 tags$div(class="wc-col-label", "Semifinal"),
                                 tags$div(class="wc-matches",
                                          {
                                            m  <- get_sf(2)
                                            ta <- as.character(m$a); tb <- as.character(m$b)
                                            wa <- !is.null(m$winner) && as.character(m$winner)==ta
                                            div(class="wc-match",
                                                div(class=paste0("wc-team ",if(wa)"wc-w"else"wc-l"),
                                                    flag_img(ta), tags$span(class=paste0("wc-tname ",if(wa)"wc-w"else"wc-l"),ta),
                                                    tags$span(class=paste0("wc-sc ",if(wa)"wc-w"else"wc-l"),m$ga)),
                                                div(class=paste0("wc-team ",if(!wa)"wc-w"else"wc-l"),
                                                    flag_img(tb), tags$span(class=paste0("wc-tname ",if(!wa)"wc-w"else"wc-l"),tb),
                                                    tags$span(class=paste0("wc-sc ",if(!wa)"wc-w"else"wc-l"),m$gb))
                                            )
                                          }
                                 )
                        ),
                        
                        # ---- RIGHT QF = Cuartos ----
                        tags$div(class="wc-col",
                                 tags$div(class="wc-col-label", "Cuartos"),
                                 tags$div(class="wc-matches",
                                          lapply(3:4, function(i) {
                                            m  <- get_r(brk$qf, i)
                                            ta <- as.character(m$a); tb <- as.character(m$b)
                                            wa <- !is.null(m$winner) && as.character(m$winner)==ta
                                            div(class="wc-match",
                                                div(class=paste0("wc-team ",if(wa)"wc-w"else"wc-l"),
                                                    flag_img(ta), tags$span(class=paste0("wc-tname ",if(wa)"wc-w"else"wc-l"),ta),
                                                    tags$span(class=paste0("wc-sc ",if(wa)"wc-w"else"wc-l"),m$ga)),
                                                div(class=paste0("wc-team ",if(!wa)"wc-w"else"wc-l"),
                                                    flag_img(tb), tags$span(class=paste0("wc-tname ",if(!wa)"wc-w"else"wc-l"),tb),
                                                    tags$span(class=paste0("wc-sc ",if(!wa)"wc-w"else"wc-l"),m$gb))
                                            )
                                          })
                                 )
                        ),
                        
                        # ---- RIGHT R16 = Octavos ----
                        tags$div(class="wc-col",
                                 tags$div(class="wc-col-label", "Octavos"),
                                 tags$div(class="wc-matches",
                                          lapply(5:8, function(i) {
                                            m  <- get_r(brk$r16, i)
                                            ta <- as.character(m$a); tb <- as.character(m$b)
                                            wa <- !is.null(m$winner) && as.character(m$winner)==ta
                                            div(class="wc-match",
                                                div(class=paste0("wc-team ",if(wa)"wc-w"else"wc-l"),
                                                    flag_img(ta), tags$span(class=paste0("wc-tname ",if(wa)"wc-w"else"wc-l"),ta),
                                                    tags$span(class=paste0("wc-sc ",if(wa)"wc-w"else"wc-l"),m$ga)),
                                                div(class=paste0("wc-team ",if(!wa)"wc-w"else"wc-l"),
                                                    flag_img(tb), tags$span(class=paste0("wc-tname ",if(!wa)"wc-w"else"wc-l"),tb),
                                                    tags$span(class=paste0("wc-sc ",if(!wa)"wc-w"else"wc-l"),m$gb))
                                            )
                                          })
                                 )
                        ),
                        
                        # ---- RIGHT R32 = 16avos ----
                        tags$div(class="wc-col",
                                 tags$div(class="wc-col-label", "16avos"),
                                 tags$div(class="wc-matches",
                                          lapply(9:16, function(i) {
                                            m  <- get_r(brk$r32, i)
                                            ta <- as.character(m$a); tb <- as.character(m$b)
                                            wa <- !is.null(m$winner) && as.character(m$winner)==ta
                                            div(class="wc-match",
                                                div(class=paste0("wc-team ",if(wa)"wc-w"else"wc-l"),
                                                    flag_img(ta), tags$span(class=paste0("wc-tname ",if(wa)"wc-w"else"wc-l"),ta),
                                                    tags$span(class=paste0("wc-sc ",if(wa)"wc-w"else"wc-l"),m$ga)),
                                                div(class=paste0("wc-team ",if(!wa)"wc-w"else"wc-l"),
                                                    flag_img(tb), tags$span(class=paste0("wc-tname ",if(!wa)"wc-w"else"wc-l"),tb),
                                                    tags$span(class=paste0("wc-sc ",if(!wa)"wc-w"else"wc-l"),m$gb))
                                            )
                                          })
                                 )
                        )
                        
               ) # fin wc-bracket
               ) # fin div background
        ), # fin column(9)
        
        # Probabilidades de campeonato
        column(3,
               div(style="background:#08131F;border:1px solid rgba(245,208,96,0.12);border-radius:14px;padding:16px;height:100%;",
                   tags$div(style="color:#F5D060;font-size:0.72em;font-weight:800;letter-spacing:2px;text-transform:uppercase;margin-bottom:4px;",
                            icon("crown", style="margin-right:6px;"),
                            "Frecuencia de Campeonato"),
                   tags$div(style="color:#4A7A9B;font-size:0.72em;margin-bottom:12px;",
                            paste0("Veces campeon / ", input$champ_nsims, " simulaciones")),
                   div(style="max-height:600px;overflow-y:auto;",
                       do.call(tagList, lapply(seq_len(nrow(df)), function(i) {
                         r    <- df[i,]
                         pct  <- round(r$prob*100, 1)
                         wins <- round(r$prob * input$champ_nsims)
                         bw   <- round(r$prob / df$prob[1] * 100)
                         col  <- if(i==1) "#F5D060" else if(i<=3) "#C8960C" else if(i<=8) "#2E86AB" else "#4A7A9B"
                         div(class="prob-champ-row",
                             tags$span(style=paste0("font-size:0.68em;font-weight:800;color:",col,";width:20px;flex-shrink:0;"),
                                       paste0("#",i)),
                             flag_img(as.character(r$team)),
                             tags$span(style=paste0("flex:1;font-size:0.78em;font-weight:",if(i==1)"800" else "500",";color:",if(i<=3)"#E8F4FD" else "#C5D8E8",";"),
                                       as.character(r$team)),
                             div(style="text-align:right;flex-shrink:0;",
                                 div(style=paste0("font-size:0.85em;font-weight:800;color:",col,";"), paste0(pct,"%")),
                                 div(style="font-size:0.65em;color:#4A7A9B;", paste0(wins,"x")),
                                 div(style=paste0("height:3px;width:",max(bw,3),"px;background:",col,";border-radius:2px;margin-top:2px;margin-left:auto;"))
                             )
                         )
                       }))
                   )
               )
        )
      ),

      # ============================================================
      # CAMINO DEL CAMPEON - ultima simulacion
      # ============================================================
      {
        champ_name <- df$team[1]
        brk_last   <- brk

        # Trazar el recorrido del campeon ronda por ronda
        trace_path <- function() {
          rounds <- list(
            list(label="16avos",     key="r32"),
            list(label="Octavos",    key="r16"),
            list(label="Cuartos",    key="qf"),
            list(label="Semifinal",  key="sf"),
            list(label="Final",      key="final")
          )
          path <- list()
          # Determinar quien fue el campeon real de ESTA simulacion (no el mas frecuente)
          actual_champ <- if (!is.null(brk_last$champion)) brk_last$champion else champ_name

          for (rd in rounds) {
            matches <- brk_last[[rd$key]]
            if (is.null(matches)) next
            for (m in matches) {
              ta <- as.character(m$a); tb <- as.character(m$b)
              if (actual_champ %in% c(ta, tb)) {
                rival <- if (actual_champ == ta) tb else ta
                gf    <- if (actual_champ == ta) m$ga else m$gb
                gc    <- if (actual_champ == ta) m$gb else m$ga
                won   <- as.character(m$winner) == actual_champ
                resultado <- if (gf > gc) "Victoria" else if (gf < gc) "Derrota" else "Empate (def. por penales)"
                path[[length(path)+1]] <- list(
                  ronda = rd$label, rival = rival,
                  gf = gf, gc = gc, marcador = paste0(gf, " - ", gc),
                  won = won, resultado = resultado
                )
                break
              }
            }
          }
          path
        }

        path <- trace_path()
        actual_champ <- if (!is.null(brk_last$champion)) brk_last$champion else champ_name

        fluidRow(column(12,
          div(style=paste0(
            "background:linear-gradient(135deg,#08131F,#0D1F3C);",
            "border:1px solid rgba(245,208,96,0.2);border-radius:14px;",
            "padding:20px;margin-top:18px;"
          ),
            # Encabezado
            div(style="display:flex;align-items:center;gap:12px;margin-bottom:8px;",
              tags$img(src="trophy.png", height="34",
                style="filter:drop-shadow(0 0 8px rgba(245,208,96,0.5));"),
              div(
                tags$div(style="color:#F5D060;font-size:0.95em;font-weight:800;letter-spacing:1px;text-transform:uppercase;",
                  "Camino del Campeon"),
                tags$div(style="color:#8BAEC8;font-size:0.8em;",
                  "Recorrido completo de ", tags$strong(style="color:#E8F4FD;", actual_champ),
                  " en la ultima simulacion (de la izquierda a la final).")
              )
            ),

            # Tabla interactiva del recorrido
            div(style="overflow-x:auto;margin-top:14px;",
              tags$table(style="width:100%;border-collapse:collapse;font-size:0.85em;",
                tags$thead(
                  tags$tr(style="border-bottom:2px solid rgba(245,208,96,0.3);",
                    tags$th(style="text-align:left;padding:8px 12px;color:#F5D060;font-size:0.78em;text-transform:uppercase;letter-spacing:1px;", "Ronda"),
                    tags$th(style="text-align:left;padding:8px 12px;color:#F5D060;font-size:0.78em;text-transform:uppercase;letter-spacing:1px;", "Rival"),
                    tags$th(style="text-align:center;padding:8px 12px;color:#F5D060;font-size:0.78em;text-transform:uppercase;letter-spacing:1px;", "Marcador"),
                    tags$th(style="text-align:center;padding:8px 12px;color:#F5D060;font-size:0.78em;text-transform:uppercase;letter-spacing:1px;", "Resultado")
                  )
                ),
                tags$tbody(
                  lapply(seq_along(path), function(i) {
                    p <- path[[i]]
                    is_final <- p$ronda == "Final"
                    row_bg <- if (is_final) "background:rgba(245,208,96,0.08);" else if (i %% 2 == 0) "background:rgba(46,134,171,0.04);" else ""
                    res_col <- if (grepl("Victoria", p$resultado)) "#27AE60" else if (grepl("Derrota", p$resultado)) "#C0392B" else "#F39C12"
                    tags$tr(style=paste0("border-bottom:1px solid rgba(46,134,171,0.12);transition:background 0.15s;", row_bg),
                      onmouseover="this.style.background='rgba(46,134,171,0.12)'",
                      onmouseout=paste0("this.style.background='", if (is_final) "rgba(245,208,96,0.08)" else if (i %% 2 == 0) "rgba(46,134,171,0.04)" else "transparent", "'"),
                      # Ronda
                      tags$td(style="padding:9px 12px;",
                        tags$span(style=paste0(
                          "font-weight:700;font-size:0.9em;",
                          if (is_final) "color:#F5D060;" else "color:#64CFF6;"
                        ),
                          if (is_final) tagList(icon("trophy", style="margin-right:5px;font-size:0.85em;"), p$ronda) else p$ronda)
                      ),
                      # Rival con bandera
                      tags$td(style="padding:9px 12px;",
                        div(style="display:flex;align-items:center;gap:8px;",
                          flag_img(p$rival),
                          tags$span(style="color:#C5D8E8;font-weight:600;", p$rival)
                        )
                      ),
                      # Marcador
                      tags$td(style="text-align:center;padding:9px 12px;",
                        tags$span(style=paste0(
                          "font-weight:900;font-size:1.05em;color:", res_col, ";",
                          "background:rgba(", if (grepl("Victoria", p$resultado)) "39,174,96" else if (grepl("Derrota", p$resultado)) "192,57,43" else "243,156,18", ",0.12);",
                          "padding:3px 12px;border-radius:6px;"
                        ), p$marcador)
                      ),
                      # Resultado
                      tags$td(style="text-align:center;padding:9px 12px;",
                        tags$span(style=paste0("color:", res_col, ";font-weight:700;font-size:0.85em;"),
                          p$resultado)
                      )
                    )
                  })
                )
              )
            ),

            # Resumen del recorrido
            div(style="display:flex;gap:24px;margin-top:16px;padding-top:14px;border-top:1px solid rgba(46,134,171,0.15);flex-wrap:wrap;",
              {
                total_gf <- sum(sapply(path, function(p) p$gf))
                total_gc <- sum(sapply(path, function(p) p$gc))
                n_partidos <- length(path)
                victorias <- sum(sapply(path, function(p) grepl("Victoria", p$resultado)))
                penales   <- sum(sapply(path, function(p) grepl("penales", p$resultado)))
                tagList(
                  div(style="display:flex;align-items:center;gap:8px;",
                    tags$span(style="color:#4A7A9B;font-size:0.75em;text-transform:uppercase;letter-spacing:1px;", "Partidos:"),
                    tags$span(style="color:#64CFF6;font-weight:800;font-size:1em;", n_partidos)),
                  div(style="display:flex;align-items:center;gap:8px;",
                    tags$span(style="color:#4A7A9B;font-size:0.75em;text-transform:uppercase;letter-spacing:1px;", "Goles a favor:"),
                    tags$span(style="color:#27AE60;font-weight:800;font-size:1em;", total_gf)),
                  div(style="display:flex;align-items:center;gap:8px;",
                    tags$span(style="color:#4A7A9B;font-size:0.75em;text-transform:uppercase;letter-spacing:1px;", "Goles en contra:"),
                    tags$span(style="color:#C0392B;font-weight:800;font-size:1em;", total_gc)),
                  div(style="display:flex;align-items:center;gap:8px;",
                    tags$span(style="color:#4A7A9B;font-size:0.75em;text-transform:uppercase;letter-spacing:1px;", "Diferencia:"),
                    tags$span(style=paste0("font-weight:800;font-size:1em;color:", if (total_gf-total_gc >= 0) "#27AE60" else "#C0392B", ";"),
                      if (total_gf-total_gc > 0) paste0("+", total_gf-total_gc) else total_gf-total_gc)),
                  if (penales > 0) div(style="display:flex;align-items:center;gap:8px;",
                    tags$span(style="color:#4A7A9B;font-size:0.75em;text-transform:uppercase;letter-spacing:1px;", "Definidos por penales:"),
                    tags$span(style="color:#F39C12;font-weight:800;font-size:1em;", penales))
                )
              }
            )
          )
        ))
      }
    )
  })

  output$kpi_primeros <- renderUI({
    req(sim_result())
    n <- sum(sim_result()$qualified$pos == "1", na.rm=TRUE)
    span(class="kpi-val", n)
  })
  output$kpi_segundos <- renderUI({
    req(sim_result())
    n <- sum(sim_result()$qualified$pos == "2", na.rm=TRUE)
    span(class="kpi-val", n)
  })
  output$kpi_terceros <- renderUI({
    req(sim_result())
    n <- sum(sim_result()$qualified$pos == "3*", na.rm=TRUE)
    span(class="kpi-val", n)
  })
  output$kpi_total_qual <- renderUI({
    req(sim_result())
    n <- nrow(sim_result()$qualified)
    span(class="kpi-val", style=if(!is.null(n) && n==32) "color:#1E8449;" else "color:#F39C12;", n)
  })
  
  # ---- KPIs variables ----
  output$kpi_max_score <- renderUI({
    top <- TEAM_STATS[which.max(TEAM_STATS$strength_score), ]
    span(class="kpi-val", round(max(TEAM_STATS$strength_score), 2))
  })
  output$kpi_mean_lambda <- renderUI({
    span(class="kpi-val", paste0(round(mean(TEAM_STATS$lambda_base), 2), " gls"))
  })
  output$kpi_top_team <- renderUI({
    top <- TEAM_STATS[which.max(TEAM_STATS$fifa_points), "team"]
    span(class="kpi-val", style="font-size:1.4em;", top)
  })
  
  # ---- Jornadas UI ----
  output$sim_jornadas_ui <- renderUI({
    req(sim_result())
    mh <- sim_result()$matches
    if (is.null(mh) || nrow(mh) == 0)
      return(div(class="info-text", "Ejecuta la simulacion primero."))
    
    tags$div(
      tags$style(HTML("
        .jornada-grid {
          display: grid;
          grid-template-columns: repeat(4, 1fr);
          gap: 10px;
          padding: 6px 0 16px;
        }
        @media (max-width: 1200px) { .jornada-grid { grid-template-columns: repeat(2, 1fr); } }
        .jornada-group-card {
          background: rgba(11,27,56,0.7);
          border: 1px solid rgba(46,134,171,0.22);
          border-radius: 12px;
          overflow: hidden;
        }
        .jornada-group-header {
          background: rgba(46,134,171,0.14);
          padding: 6px 12px;
          font-size: 0.7em; font-weight: 800;
          letter-spacing: 1.4px; text-transform: uppercase;
          color: #64CFF6; border-bottom: 1px solid rgba(46,134,171,0.18);
        }
        .jornada-match {
          display: flex; align-items: center;
          padding: 8px 12px; gap: 8px;
          border-bottom: 1px solid rgba(46,134,171,0.08);
        }
        .jornada-match:last-child { border-bottom: none; }
        .jornada-match:hover { background: rgba(46,134,171,0.1); }
        .jm-team { display: flex; align-items: center; gap: 5px; flex: 1; min-width: 0; }
        .jm-team.right { flex-direction: row-reverse; }
        .jm-name {
          font-size: 0.8em; color: #8BAEC8; white-space: nowrap;
          overflow: hidden; text-overflow: ellipsis;
        }
        .jm-name.winner { color: #E8F4FD; font-weight: 700; }
        .jm-score {
          display: flex; align-items: center; gap: 5px;
          flex-shrink: 0; min-width: 64px; justify-content: center;
        }
        .jm-goal { font-size: 1.25em; font-weight: 900; color: #C5D8E8; }
        .jm-goal.win { color: #64CFF6; }
        .jm-sep { color: #4A7A9B; font-size: 0.8em; }
        .jm-badge {
          font-size: 0.58em; font-weight: 800; padding: 1px 5px;
          border-radius: 5px; white-space: nowrap;
        }
        .jm-badge.draw { background: rgba(74,122,155,0.18); color: #4A7A9B; }
        .jm-badge.win  { background: rgba(100,207,246,0.12); color: #64CFF6; }
      ")),
      
      do.call(tabsetPanel, c(list(type = "tabs"),
                             lapply(1:3, function(j) {
                               mj   <- mh[mh$jornada == j, ]
                               grps <- sort(unique(mj$grupo))
                               
                               tabPanel(paste("Jornada", j),
                                        tags$div(class = "jornada-grid",
                                                 lapply(grps, function(grp) {
                                                   mg <- mj[mj$grupo == grp, ]
                                                   tags$div(class = "jornada-group-card",
                                                            tags$div(class = "jornada-group-header", paste("Grupo", grp)),
                                                            do.call(tagList, lapply(seq_len(nrow(mg)), function(i) {
                                                              r      <- mg[i, ]
                                                              winner <- if (r$goals_a > r$goals_b) "a"
                                                              else if (r$goals_b > r$goals_a) "b"
                                                              else "d"
                                                              tags$div(class = "jornada-match",
                                                                       # Equipo A izq
                                                                       tags$div(class = "jm-team",
                                                                                flag_img(r$team_a),
                                                                                tags$span(
                                                                                  class = paste0("jm-name", if (winner=="a") " winner" else ""),
                                                                                  r$team_a)
                                                                       ),
                                                                       # Marcador
                                                                       tags$div(class = "jm-score",
                                                                                tags$span(class = paste0("jm-goal", if (winner=="a") " win" else ""), r$goals_a),
                                                                                tags$span(class = "jm-sep", "-"),
                                                                                tags$span(class = paste0("jm-goal", if (winner=="b") " win" else ""), r$goals_b),
                                                                                tags$span(
                                                                                  class = paste0("jm-badge ", if (winner=="d") "draw" else "win"),
                                                                                  if (winner=="d") "EMP"
                                                                                  else paste0("^ ", if (winner=="a") r$team_a else r$team_b)
                                                                                )
                                                                       ),
                                                                       # Equipo B der
                                                                       tags$div(class = "jm-team right",
                                                                                flag_img(r$team_b),
                                                                                tags$span(
                                                                                  class = paste0("jm-name", if (winner=="b") " winner" else ""),
                                                                                  r$team_b)
                                                                       )
                                                              )
                                                            }))
                                                   )
                                                 })
                                        )
                               )
                             })
      ))
    )
  })
  
  # ---- Standings UI ----
  output$sim_standings_ui <- renderUI({
    req(sim_result())
    res <- sim_result()
    if (is.null(res)) return(div(class="info-text", "Ejecuta la simulacion primero."))
    all_st <- res$standings
    
    tagList(
      div(style="margin-bottom:10px;",
          tags$span(style="color:#64CFF6;font-size:0.75em;font-weight:800;text-transform:uppercase;letter-spacing:1.5px;",
                    icon("table", style="margin-right:6px;"), "Tablas de posiciones finales")
      ),
      do.call(tagList, lapply(seq(1, 12, by=4), function(start_idx) {
        grp_names <- names(GROUPS_DATA)[start_idx:min(start_idx+3, 12)]
        fluidRow(lapply(grp_names, function(grp) {
          st    <- all_st[[grp]]
          teams <- rownames(st)
          column(3,
                 div(class="group-card", style="margin-bottom:10px;",
                     # Header igual que grupos oficiales
                     div(style="display:flex;align-items:center;justify-content:space-between;border-bottom:1px solid rgba(46,134,171,0.3);padding-bottom:8px;margin-bottom:6px;",
                         tags$span(class="group-title", style="margin:0;border:none;padding:0;font-size:1em;", paste("Grupo", grp)),
                         div(style="display:flex;gap:6px;",
                             lapply(c("PTS","GF","DG"), function(col)
                               tags$span(style="color:#4A7A9B;font-size:0.6em;font-weight:700;width:22px;text-align:center;", col)
                             )
                         )
                     ),
                     # Filas de equipos
                     do.call(tagList, lapply(1:4, function(k) {
                       t   <- teams[k]
                       row <- st[k, ]
                       classified <- k <= 2
                       dg_val <- if (!is.na(row$DG) && row$DG > 0) paste0("+", row$DG) else as.character(row$DG)
                       dg_col <- if (!is.na(row$DG) && row$DG > 0) "#1E8449" else if (!is.na(row$DG) && row$DG < 0) "#C0392B" else "#8BAEC8"
                       div(class="team-row",
                           style=if(classified) "background:rgba(30,132,73,0.06);" else "",
                           flag_img(t),
                           tags$span(class=paste0("pos-badge pos-", k), style="font-size:0.7em;margin-right:2px;", k),
                           div(class="team-name", style="font-size:0.82em;",
                               t,
                               if (t %in% HOST_COUNTRIES) tags$span(class="host-badge", style="margin-left:4px;font-size:0.65em;", "SEDE"),
                               if (classified) tags$span(style="color:#1E8449;margin-left:4px;font-size:0.7em;font-weight:700;", "Q")
                           ),
                           # Stats compactos
                           div(style="display:flex;gap:0;margin-left:auto;",
                               tags$span(style="color:#64CFF6;font-weight:800;font-size:0.82em;width:22px;text-align:center;", row$PTS),
                               tags$span(style="color:#8BAEC8;font-size:0.78em;width:22px;text-align:center;", row$GF),
                               tags$span(style=paste0("color:", dg_col, ";font-size:0.78em;width:22px;text-align:center;font-weight:700;"), dg_val)
                           )
                       )
                     }))
                 )
          )
        }))
      }))
    )
  })
  output$clasificados_tabla_ui <- renderUI({
    req(sim_result())
    q <- sim_result()$qualified
    if (is.null(q) || nrow(q) == 0) return(div(class="info-text", "Corre la simulacion primero."))
    
    # Separar clasificados directos (pos 1 y 2) de mejores terceros
    directs  <- q[q$pos %in% c("1","2"), ]
    thirds   <- q[q$pos == "3*", ]
    
    # -- Tabla por grupos (2 columnas: izquierda grupos A-F, derecha G-L) --
    all_grps <- sort(unique(directs$group))
    left_grps  <- all_grps[1:min(6, length(all_grps))]
    right_grps <- if (length(all_grps) > 6) all_grps[7:length(all_grps)] else character(0)
    
    make_group_block <- function(grp) {
      rows <- directs[directs$group == grp, ]
      rows <- rows[order(rows$pos), ]
      div(style="margin-bottom:12px;",
          div(style="display:flex;align-items:center;gap:8px;margin-bottom:6px;",
              tags$span(style="background:rgba(100,207,246,0.15);color:#64CFF6;font-weight:800;
                           font-size:0.8em;padding:3px 10px;border-radius:8px;letter-spacing:1px;",
                        paste("GRUPO", grp)),
              tags$span(style="height:1px;flex:1;background:rgba(46,134,171,0.2);display:block;")
          ),
          do.call(tagList, lapply(1:nrow(rows), function(i) {
            r     <- rows[i, ]
            is1st <- r$pos == "1"
            div(style=paste0(
              "display:flex;align-items:center;gap:10px;padding:7px 12px;",
              "border-radius:8px;margin-bottom:4px;",
              if(is1st) "background:rgba(243,156,18,0.08);border:1px solid rgba(243,156,18,0.2);"
              else      "background:rgba(46,134,171,0.07);border:1px solid rgba(46,134,171,0.15);"
            ),
            tags$span(style=paste0(
              "font-size:0.75em;font-weight:800;width:22px;height:22px;",
              "display:inline-flex;align-items:center;justify-content:center;",
              "border-radius:50%;flex-shrink:0;",
              if(is1st) "background:#F39C12;color:#0D1F3C;"
              else      "background:rgba(174,214,241,0.2);color:#AED6F1;"
            ), if(is1st) "1" else "2"),
            flag_img(as.character(r$team)),
            tags$span(style=paste0(
              "font-size:0.88em;font-weight:", if(is1st) "700" else "500", ";",
              "color:", if(is1st) "#E8F4FD" else "#C5D8E8", ";"
            ), as.character(r$team)),
            tags$span(style="margin-left:auto;font-size:0.75em;font-weight:700;color:#64CFF6;
                           background:rgba(46,134,171,0.15);padding:2px 8px;border-radius:8px;",
                      paste0(r$PTS, " pts"))
            )
          }))
      )
    }
    
    tagList(
      # Grid de grupos
      fluidRow(
        column(6, do.call(tagList, lapply(left_grps,  make_group_block))),
        column(6, do.call(tagList, lapply(right_grps, make_group_block)))
      ),
      
      # Mejores terceros
      if (nrow(thirds) > 0) {
        div(style="margin-top:16px;",
            div(style="display:flex;align-items:center;gap:8px;margin-bottom:10px;",
                tags$span(style="background:rgba(142,68,173,0.15);color:#8E44AD;font-weight:800;
                             font-size:0.8em;padding:3px 10px;border-radius:8px;letter-spacing:1px;",
                          icon("medal", style="color:#8E44AD;margin-right:6px;"), "LOS 8 MEJORES TERCEROS"),
                tags$span(style="height:1px;flex:1;background:rgba(142,68,173,0.2);display:block;")
            ),
            div(style="display:flex;flex-wrap:wrap;gap:8px;",
                lapply(1:nrow(thirds), function(i) {
                  r <- thirds[i, ]
                  div(style="display:flex;align-items:center;gap:8px;padding:7px 12px;
                         background:rgba(142,68,173,0.07);border:1px solid rgba(142,68,173,0.2);
                         border-radius:8px;",
                      tags$span(style="font-size:0.72em;font-weight:700;color:#8E44AD;flex-shrink:0;",
                                paste0("G-", r$group)),
                      flag_img(as.character(r$team)),
                      tags$span(style="font-size:0.85em;font-weight:600;color:#C5D8E8;",
                                as.character(r$team)),
                      tags$span(style="font-size:0.72em;color:#64CFF6;font-weight:700;margin-left:4px;",
                                paste0(r$PTS, " pts"))
                  )
                })
            )
        )
      }
    )
  })
  
  # Grafico confederaciones clasificadas
  output$plot_conf_qual <- renderPlotly({
    req(sim_result())
    q <- sim_result()$qualified
    if (is.null(q)) return(NULL)
    
    conf_counts <- q |>
      mutate(conf = sapply(team, get_conf)) |>
      count(conf)
    
    colors <- sapply(conf_counts$conf, function(c) { idx <- match(c, names(CONF_COLORS)); if (!is.na(idx)) CONF_COLORS[[idx]] else "#888888" })
    
    plot_ly(conf_counts, labels=~conf, values=~n, type="pie",
            marker=list(colors=colors, line=list(color="#0D1F3C", width=2)),
            textinfo="label+value+percent",
            textfont=list(color="#E8F4FD", size=11),
            hovertemplate="%{label}: %{value} equipos (%{percent})<extra></extra>") |>
      layout(paper_bgcolor="#112240", plot_bgcolor="#112240",
             font=list(color="#C5D8E8"),
             legend=list(orientation="v", font=list(color="#C5D8E8", size=10),
                         bgcolor="rgba(13,31,60,0.8)"),
             margin=list(t=20, b=20, l=10, r=10),
             showlegend=TRUE)
  })
  
  # Terceros tabla
  output$thirds_tabla <- renderDT({
    req(sim_result())
    td <- sim_result()$thirds_df
    if (is.null(td)) return(NULL)
    
    td_show <- td
    n_rows  <- nrow(td_show)
    clasifica_vals <- ifelse(seq_len(n_rows) <= 8, "CLASIFICA", "Eliminado")
    
    td_out <- data.frame(
      Rk      = seq_len(n_rows),
      Grupo   = td_show$group,
      Equipo  = td_show$team,
      Region  = sapply(td_show$team, get_conf),
      PTS     = td_show$PTS,
      DG      = td_show$DG,
      GF      = td_show$GF,
      FIFA    = round(td_show$FIFA_PTS),
      Estado  = clasifica_vals,
      stringsAsFactors = FALSE
    )
    
    datatable(td_out,
              rownames = FALSE,
              colnames = c("#", "Grupo", "Equipo", "Region", "Pts", "DG", "GF", "FIFA pts", "Estado"),
              options  = list(
                pageLength = 12, dom = "t", ordering = FALSE,
                columnDefs = list(
                  list(className="dt-center", targets=c(0,1,4,5,6,7)),
                  list(width="30px",  targets=0),
                  list(width="50px",  targets=1),
                  list(width="80px",  targets=3),
                  list(width="100px", targets=8)
                )
              ),
              class = "compact"
    ) |>
      formatStyle("Rk",
                  color = styleInterval(c(4, 8), c("#F5D060", "#C8960C", "#4A7A9B")),
                  fontWeight = "800", fontSize = "1em"
      ) |>
      formatStyle("Estado",
                  color      = styleEqual(c("CLASIFICA","Eliminado"), c("#1E8449","#C0392B")),
                  fontWeight = "800",
                  background = styleEqual(
                    c("CLASIFICA","Eliminado"),
                    c("rgba(30,132,73,0.12)", "rgba(192,57,43,0.07)")
                  ),
                  borderRadius = "6px", padding = "3px 8px"
      ) |>
      formatStyle("Equipo", fontWeight="700", color="#E8F4FD") |>
      formatStyle("PTS",
                  fontWeight = "800", color = "#64CFF6",
                  background = styleColorBar(c(0, max(td_out$PTS, na.rm=TRUE)), "#1B4F72"),
                  backgroundSize = "100% 55%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "center"
      ) |>
      formatStyle(c("DG","GF"),
                  color = "#AED6F1", fontWeight = "600"
      ) |>
      formatStyle("FIFA",
                  color = "#4A7A9B", fontSize = "0.85em"
      ) |>
      formatStyle("Rk",
                  background = styleInterval(8, c("rgba(30,132,73,0.06)", "")),
                  borderLeft = styleInterval(8,
                                             c("3px solid rgba(30,132,73,0.4)", "3px solid rgba(192,57,43,0.25)"))
      )
  })
  
  # ============================================================
  # MONTE CARLO
  # ============================================================
  mc_result <- reactiveVal(NULL)
  
  output$mc_progress_ui <- renderUI({
    if (is.null(mc_result())) {
      div(class="info-text", style="margin-bottom:16px;",
          icon("info-circle", style="color:#2E86AB;margin-right:6px;"),
          "Configura el numero de simulaciones y presiona ", strong("Ejecutar Monte Carlo"), ".")
    }
  })
  
  observeEvent(input$btn_mc, {
    n_sims <- input$mc_nsims
    withProgress(message = paste("Monte Carlo:", n_sims, "simulaciones..."), value = 0, {
      res <- tryCatch(
        run_monte_carlo(n_sims = n_sims),
        error = function(e) {
          showNotification(paste("Error Monte Carlo:", e$message), type="error", duration=5)
          NULL
        }
      )
      setProgress(1.0, detail = "Completado")
      mc_result(res)
    })
  })
  
  output$mc_prob_list_ui <- renderUI({
    req(mc_result())
    df <- mc_result()
    df$prob_pct <- round(df$prob * 100, 1)
    df <- df[order(-df$prob_pct), ]
    df$rank <- seq_len(nrow(df))
    
    tags$div(
      style = "max-height:680px;overflow-y:auto;padding-right:4px;",
      tags$style(HTML("
        .prob-row {
          display:flex;align-items:center;gap:8px;
          padding:5px 8px;border-radius:8px;margin-bottom:3px;
          border:1px solid rgba(46,134,171,0.1);
          transition: background 0.15s;
        }
        .prob-row:hover { background: rgba(46,134,171,0.12); }
        .prob-rank {
          font-size:0.7em;font-weight:800;color:#4A7A9B;
          width:20px;text-align:right;flex-shrink:0;
        }
        .prob-name {
          font-size:0.82em;flex:1;white-space:nowrap;
          overflow:hidden;text-overflow:ellipsis;
        }
        .prob-pct {
          font-size:0.85em;font-weight:800;
          flex-shrink:0;min-width:38px;text-align:right;
        }
        .tier-high  { background:rgba(39,174,96,0.08);  border-color:rgba(39,174,96,0.2); }
        .tier-mid   { background:rgba(41,128,185,0.07); border-color:rgba(41,128,185,0.18); }
        .tier-low   { background:rgba(27,79,114,0.05);  border-color:rgba(27,79,114,0.15); }
      ")),
      do.call(tagList, lapply(seq_len(nrow(df)), function(i) {
        r     <- df[i, ]
        tier  <- if (r$prob_pct >= 70) "tier-high" else if (r$prob_pct >= 40) "tier-mid" else "tier-low"
        pct_c <- if (r$prob_pct >= 70) "#27AE60"   else if (r$prob_pct >= 40) "#2980B9"  else "#4A7A9B"
        tags$div(class = paste("prob-row", tier),
                 tags$span(class = "prob-rank",  paste0("#", r$rank)),
                 flag_img(as.character(r$team)),
                 tags$span(class = "prob-name",
                           style = paste0("color:", if (r$prob_pct >= 70) "#E8F4FD" else "#C5D8E8", ";"),
                           as.character(r$team)
                 ),
                 tags$span(class = "prob-pct",
                           style = paste0("color:", pct_c, ";"),
                           paste0(r$prob_pct, "%")
                 )
        )
      }))
    )
  })
  
  output$plot_mc_probs <- renderPlotly({
    req(mc_result())
    df <- mc_result() |>
      arrange(prob) |>
      mutate(
        conf     = sapply(team, get_conf),
        prob_pct = round(prob * 100, 1),
        team_ord = factor(team, levels = team),
        tier     = ifelse(prob >= 0.70, "Alta (>70%)",
                          ifelse(prob >= 0.40, "Media (40-70%)", "Baja (<40%)"))
      )
    
    bar_colors <- ifelse(df$prob >= 0.70, "#27AE60",
                         ifelse(df$prob >= 0.40, "#2980B9", "#1B4F72"))
    
    # Altura dinamica segun numero de equipos
    n <- nrow(df)
    bar_h <- max(18, min(28, 900 / n))  # px por barra
    
    plot_ly(df,
            x           = ~prob_pct,
            y           = ~team_ord,
            type        = "bar",
            orientation = "h",
            marker      = list(
              color = bar_colors,
              line  = list(color = "rgba(0,0,0,0)", width = 0)
            ),
            text          = ~paste0("<b>", prob_pct, "%</b>"),
            textposition  = "outside",
            textfont      = list(size = 12, color = "#E8F4FD"),
            insidetextanchor = "end",
            hovertemplate = paste0(
              "<b>%{y}</b><br>",
              "Clasifica: <b>%{x:.1f}%</b>",
              "<extra></extra>"
            ),
            name = ""
    ) |>
      layout(
        title = list(
          text = paste0(
            "<b>Probabilidad de clasificar a la Ronda de 16avos</b><br>",
            "<span style='font-size:12px;color:#8BAEC8;'>",
            input$mc_nsims, " simulaciones  .  ",
            "<span style='color:#27AE60'>?</span> >70%  ",
            "<span style='color:#2980B9'>?</span> 40-70%  ",
            "<span style='color:#1B4F72'>?</span> <40%</span>"
          ),
          font = list(size = 15, color = "#E8F4FD"),
          x = 0.02, xanchor = "left"
        ),
        xaxis = list(
          range       = c(0, 118),
          ticksuffix  = "%",
          tickfont    = list(size = 12, color = "#8BAEC8"),
          gridcolor   = "rgba(46,134,171,0.12)",
          zeroline    = FALSE,
          title       = list(text = "Probabilidad (%)", font = list(size = 13, color = "#8BAEC8"))
        ),
        yaxis = list(
          tickfont  = list(size = 13, color = "#C5D8E8"),
          gridcolor = "rgba(46,134,171,0.08)",
          title     = ""
        ),
        paper_bgcolor = "#112240",
        plot_bgcolor  = "#0D1F3C",
        margin        = list(t = 90, r = 90, b = 50, l = 175),
        bargap        = 0.35,
        showlegend    = FALSE,
        font          = list(family = "Inter, sans-serif")
      )
  })
  
  output$plot_mc_conf <- renderPlotly({
    req(mc_result())
    df <- mc_result() |>
      mutate(conf = sapply(team, get_conf)) |>
      group_by(conf) |>
      summarise(mean_prob = mean(prob), n_teams=n(), .groups="drop") |>
      arrange(desc(mean_prob))
    
    colors <- sapply(df$conf, function(c) { idx <- match(c, names(CONF_COLORS)); if (!is.na(idx)) CONF_COLORS[[idx]] else "#888888" })
    
    plot_ly(df, x=~reorder(conf, mean_prob), y=~round(mean_prob*100,1), type="bar",
            marker=list(color=colors, line=list(color="#0D1F3C", width=1)),
            text=~paste0(round(mean_prob*100,1), "%"), textposition="outside",
            hovertemplate="<b>%{x}</b><br>P media: %{y:.1f}%<extra></extra>") |>
      pl_layout("Probabilidad media de clasificacion por confederacion", "",
                "Confederacion", "P(clasifica) promedio (%)") |>
      layout(xaxis=list(tickangle=-20), yaxis=list(ticksuffix="%", range=c(0,115)), showlegend=FALSE)
  })
  
  output$mc_tabla <- renderDT({
    req(mc_result())
    df <- mc_result()
    df$conf     <- sapply(df$team, get_conf)
    df$prob_pct <- round(df$prob * 100, 1)
    df$grupo    <- sapply(df$team, function(t) {
      for (g in names(GROUPS_DATA)) if (t %in% GROUPS_DATA[[g]]) return(g)
      NA_character_
    })
    df2 <- data.frame(
      Grupo    = df$grupo,
      Equipo   = df$team,
      Region   = df$conf,
      Prob     = df$prob_pct,
      FIFA_pts = df$fifa_points,
      stringsAsFactors = FALSE
    )
    df2 <- df2[order(-df2$Prob), ]
    
    datatable(df2,
              rownames = FALSE,
              colnames = c("Grupo","Equipo","Region","Clasifica (%)","Pts. FIFA"),
              options  = list(
                pageLength = 16, dom = "frtip",
                scrollY = "460px", scrollCollapse = TRUE,
                order = list(list(3, "desc")),
                columnDefs = list(
                  list(className = "dt-center", targets = c(0, 3, 4)),
                  list(className = "dt-left",   targets = c(1, 2))
                )
              ),
              class = "compact stripe"
    ) |>
      formatStyle("Prob",
                  color      = styleInterval(c(40, 70), c("#8BAEC8", "#AED6F1", "#1E8449")),
                  fontWeight = "bold",
                  fontSize   = "1.05em"
      ) |>
      formatStyle("Equipo", fontWeight = "600", color = "#E8F4FD") |>
      formatStyle("Grupo",  fontWeight = "700", color = "#64CFF6") |>
      formatStyle("Region", color = "#8BAEC8") |>
      formatStyle("FIFA_pts", color = "#4A7A9B") |>
      formatStyle("Prob",
                  background = styleInterval(c(40, 70),
                                             c("rgba(74,122,155,0.08)", "rgba(46,134,171,0.1)", "rgba(30,132,73,0.12)")),
                  borderRadius = "6px"
      )
  })
  
  # ============================================================
  # VARIABLES
  # ============================================================
  # ---- Expected Goals list ----
  output$xg_list_ui <- renderUI({
    df <- TEAM_STATS[order(-TEAM_STATS$lambda_base), ]
    df$rank <- seq_len(nrow(df))
    
    # Rango para la barra visual
    mx <- max(df$lambda_base, na.rm = TRUE)
    mn <- min(df$lambda_base, na.rm = TRUE)
    
    tags$div(
      style = "max-height:560px;overflow-y:auto;",
      tags$style(HTML("
        .xg-row {
          display:flex;align-items:center;gap:8px;
          padding:5px 6px;border-radius:7px;margin-bottom:3px;
          border:1px solid transparent;
          transition:background 0.15s;
        }
        .xg-row:hover { background:rgba(46,134,171,0.1);border-color:rgba(46,134,171,0.2); }
        .xg-rank { font-size:0.68em;color:#4A7A9B;width:18px;text-align:right;flex-shrink:0;font-weight:700; }
        .xg-name { font-size:0.8em;flex:1;white-space:nowrap;overflow:hidden;text-overflow:ellipsis; }
        .xg-bar-wrap { width:60px;height:6px;background:rgba(46,134,171,0.12);border-radius:3px;flex-shrink:0; }
        .xg-bar-fill { height:6px;border-radius:3px; }
        .xg-val { font-size:0.82em;font-weight:800;width:30px;text-align:right;flex-shrink:0; }
      ")),
      do.call(tagList, lapply(seq_len(nrow(df)), function(i) {
        r    <- df[i, ]
        xg   <- round(r$lambda_base, 2)
        pct  <- (r$lambda_base - mn) / (mx - mn)
        bw   <- round(pct * 60)
        # Color degradado: rojo fuerte -> naranja -> verde segun nivel
        col  <- if (pct > 0.75) "#27AE60"
        else if (pct > 0.5) "#2E86AB"
        else if (pct > 0.25) "#F39C12"
        else "#C0392B"
        nm_col <- if (pct > 0.75) "#E8F4FD" else "#C5D8E8"
        
        tags$div(class="xg-row",
                 tags$span(class="xg-rank", paste0("#", r$rank)),
                 flag_img(as.character(r$team)),
                 tags$span(class="xg-name", style=paste0("color:", nm_col, ";"), as.character(r$team)),
                 tags$div(class="xg-bar-wrap",
                          tags$div(class="xg-bar-fill",
                                   style=paste0("width:", bw, "px;background:", col, ";"))
                 ),
                 tags$span(class="xg-val", style=paste0("color:", col, ";"), xg)
        )
      }))
    )
  })
  
  output$plot_strength_score <- renderPlotly({
    df <- TEAM_STATS |>
      arrange(strength_score) |>
      mutate(team_ord = factor(team, levels=team))
    
    plot_ly(df, x=~strength_score, y=~team_ord, type="bar", orientation="h",
            marker=list(color=~color, line=list(color="#0D1F3C", width=0.5)),
            text=~paste0("Goles/partido: ", round(lambda_base,2)),
            textposition="outside",
            hovertemplate=paste0(
              "<b>%{y}</b><br>",
              "Strength score: %{x:.2f}<br>",
              "Goles esperados/partido: %{customdata[0]:.2f}<br>",
              "FIFA pts: %{customdata[1]}<extra></extra>"
            ),
            customdata=matrix(c(df$lambda_base, df$fifa_points), ncol=2),
            name="") |>
      pl_layout("Poder de ataque - 48 selecciones", "A mayor barra = mas peligroso | Pasa el cursor para detalles",
                "Poder de ataque", "") |>
      layout(xaxis=list(range=c(0, max(TEAM_STATS$strength_score)*1.25)), showlegend=FALSE,
             margin=list(l=150, r=60, t=80, b=60))
  })
  
  output$plot_conf_scores <- renderPlotly({
    df <- TEAM_STATS
    
    confs <- unique(df$confederation)
    data_by_conf <- lapply(confs, function(c) df[df$confederation==c, "strength_score"])
    colors <- sapply(confs, function(c) { idx <- match(c, names(CONF_COLORS)); if (!is.na(idx)) CONF_COLORS[[idx]] else "#888888" })
    
    p <- plot_ly()
    for (i in seq_along(confs)) {
      p <- p |> add_trace(
        y = data_by_conf[[i]], name = confs[i], type = "box",
        marker = list(color = colors[i]),
        line   = list(color = colors[i]),
        fillcolor = paste0(colors[i], "55"),
        hovertemplate = paste0(confs[i], "<br>Strength: %{y:.2f}<extra></extra>")
      )
    }
    p |>
      pl_layout("Poder de ataque por region", "", "Confederacion", "Strength Score") |>
      layout(showlegend=FALSE, margin=list(t=70, b=60))
  })
  
  output$plot_lambda_hist <- renderPlotly({
    df <- TEAM_STATS
    plot_ly(x=~df$lambda_base, type="histogram",
            marker=list(color=AZUL_MAIN, line=list(color="#0D1F3C", width=0.8)),
            nbinsx=12,
            hovertemplate="lambda: %{x:.2f}<br>Freq: %{y}<extra></extra>") |>
      pl_layout("Distribucion de goles esperados", "", "Goles por partido (promedio)", "N? de equipos") |>
      layout(margin=list(t=60, b=50))
  })
  
  output$tabla_variables <- renderDT({
    df_show <- TEAM_STATS
    df_show$grupo <- sapply(df_show$team, function(t) {
      for (g in names(GROUPS_DATA)) if (t %in% GROUPS_DATA[[g]]) return(g)
      NA_character_
    })
    df_show$lambda_base    <- round(df_show$lambda_base, 2)
    df_show$strength_score <- round(df_show$strength_score, 2)
    df_show$Sede           <- ifelse(df_show$is_host, "Sede", "--")
    
    df_out <- data.frame(
      Grupo          = df_show$grupo,
      Equipo         = df_show$team,
      Region         = df_show$confederation,
      `FIFA pts`     = df_show$fifa_points,
      `Poder ataque` = df_show$strength_score,
      `Goles/partido`= df_show$lambda_base,
      Sede           = df_show$Sede,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    df_out <- df_out[order(df_out$Grupo, -df_out$`FIFA pts`), ]
    
    datatable(
      df_out,
      rownames = FALSE,
      filter   = "top",
      class    = "display compact",
      options  = list(
        pageLength = 16,
        scrollX    = TRUE,
        dom        = "frtip",
        autoWidth  = TRUE,
        columnDefs = list(
          list(className = "dt-center", targets = c(0, 3, 4, 5, 6)),
          list(width = "90px",  targets = 0),
          list(width = "170px", targets = 1),
          list(width = "90px",  targets = 2)
        ),
        initComplete = JS("
          function(settings, json) {
            var api = this.api();

            // Columnas con dropdown de seleccion unica: 0=Grupo, 1=Equipo, 2=Region, 6=Sede
            var selectCols = [0, 1, 2, 6];

            selectCols.forEach(function(col) {
              var colData = api.column(col).data().unique().sort().toArray();
              var filterInput = $('thead tr:eq(1) th:eq(' + col + ') input');
              if (filterInput.length === 0) return;

              var selectStyle = [
                'width:100%', 'padding:5px 8px', 'margin-top:2px',
                'background:#081424', 'color:#64CFF6',
                'border:1.5px solid rgba(100,207,246,0.3)',
                'border-radius:8px', 'font-size:0.78em', 'font-weight:700',
                'cursor:pointer', 'outline:none',
                'box-shadow:0 0 6px rgba(100,207,246,0.08)',
                'transition:border-color 0.2s'
              ].join(';');

              var sel = $('<select style=\"' + selectStyle + '\">');
              $('<option value=\"\">-- Todos --</option>').appendTo(sel);
              colData.forEach(function(v) {
                if (v !== null && v !== '') {
                  $('<option value=\"' + v + '\">' + v + '</option>').appendTo(sel);
                }
              });

              filterInput.replaceWith(sel);

              sel.on('mouseenter', function() {
                $(this).css('border-color', 'rgba(100,207,246,0.7)');
              }).on('mouseleave', function() {
                $(this).css('border-color', 'rgba(100,207,246,0.3)');
              }).on('change', function() {
                api.column(col).search(
                  this.value ? '^' + $.fn.dataTable.util.escapeRegex(this.value) + '$' : '',
                  true, false
                ).draw();
              });
            });

            // Columnas numericas con slider visual: 3=FIFA pts, 4=Poder ataque, 5=Goles/partido
            var numCols = [3, 4, 5];
            numCols.forEach(function(col) {
              var filterInput = $('thead tr:eq(1) th:eq(' + col + ') input');
              if (filterInput.length === 0) return;

              var inputStyle = [
                'width:100%', 'padding:5px 8px', 'margin-top:2px',
                'background:#081424', 'color:#AED6F1',
                'border:1.5px solid rgba(46,134,171,0.3)',
                'border-radius:8px', 'font-size:0.78em',
                'outline:none', 'box-shadow:none'
              ].join(';');

              filterInput.attr('placeholder', 'Filtrar...');
              filterInput.css({
                'background': '#081424',
                'color': '#AED6F1',
                'border': '1.5px solid rgba(46,134,171,0.3)',
                'border-radius': '8px',
                'padding': '5px 8px',
                'font-size': '0.78em',
                'width': '100%',
                'outline': 'none',
                'box-shadow': 'none',
                'margin-top': '2px'
              });
              filterInput.on('focus', function() {
                $(this).css('border-color', 'rgba(100,207,246,0.5)');
              }).on('blur', function() {
                $(this).css('border-color', 'rgba(46,134,171,0.3)');
              });
            });
          }
        ")
      )
    ) |>
      formatStyle("Poder ataque",
                  background         = styleColorBar(c(0, max(df_out[["Poder ataque"]])), "#2E86AB"),
                  backgroundSize     = "100% 65%",
                  backgroundRepeat   = "no-repeat",
                  backgroundPosition = "center",
                  color              = "#E8F4FD",
                  fontWeight         = "bold"
      ) |>
      formatStyle("Goles/partido",
                  color      = styleInterval(c(1.2, 1.7), c("#8BAEC8", "#64CFF6", "#F39C12")),
                  fontWeight = "700",
                  fontSize   = "1.0em"
      ) |>
      formatStyle("Sede",
                  color      = styleEqual(c("Sede", "--"), c("#F39C12", "#4A7A9B")),
                  fontWeight = "700"
      ) |>
      formatStyle("Grupo",
                  color      = "#64CFF6",
                  fontWeight = "800"
      ) |>
      formatStyle("Equipo",
                  color      = "#E8F4FD",
                  fontWeight = "600"
      )
  })
  
  
} # fin server

shinyApp(ui = ui, server = server)
