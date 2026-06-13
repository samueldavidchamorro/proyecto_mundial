# proyecto_mundial
# ⚽ Simulador Mundial FIFA 2026

Simulador interactivo de la **fase de grupos y eliminatorias del FIFA World Cup 2026**, construido con **R + Shiny**. Cada partido se juega con resultados al azar, pero los equipos mejor posicionados en el Ranking FIFA tienen más probabilidades de ganar, igual que en la realidad. Incluye un **asistente con IA (Gemini)** que responde preguntas sobre el torneo usando los datos que tú mismo calculas en la app.

---

## ✨ Características

- **Grupos** — los 12 grupos del Mundial con banderas y Ranking FIFA.
- **Simulación** — juega el torneo una vez y mira todos los resultados.
- **Clasificados** — quiénes avanzan a la Ronda de 16avos.
- **Probabilidades** — corre hasta 1.000 torneos (Monte Carlo) y observa las chances de cada selección.
- **Ranking** — fuerza de las 48 selecciones.
- **🤖 Asistente Mundial 2026** — chatbot flotante (esquina inferior derecha) que responde en español sobre grupos, ranking, clasificados y probabilidades, leyendo en vivo lo que ya calculaste.

---

## 🧠 ¿Cómo se decide quién gana?

- Cada selección tiene una **fuerza de ataque** según su posición en el Ranking FIFA.
- **USA, Canadá y México** tienen una pequeña ventaja por jugar en casa (anfitriones).
- El simulador genera goles con **azar realista**: el mejor equipo anota más, pero cualquier sorpresa es posible.
- **Puntos:** victoria = 3, empate = 1, derrota = 0.

**¿Quién clasifica?** De cada grupo avanzan los 2 primeros. Además, los **8 mejores terceros** de todos los grupos también pasan. En total: **32 equipos** a la Ronda de 16avos.

**¿Qué son las simulaciones múltiples?** El simulador puede jugar el torneo completo hasta **1.000 veces** con resultados distintos. Al final ves qué tan seguido clasifica cada selección: entre más alto el porcentaje, más probable que avance.

---

## 📋 Requisitos

- **R** 4.1 o superior
- **RStudio** (recomendado)
- Conexión a internet (necesaria para el asistente con IA)
- Una **API key gratuita de Google Gemini** (ver más abajo)

### Paquetes de R

```r
install.packages(c(
  "shiny", "shinydashboard", "dplyr", "tidyr",
  "ggplot2", "plotly", "scales", "stringr", "DT",
  "httr2"   # NUEVO: requerido por el asistente con IA (Gemini)
))
```

> `httr2` es el único paquete nuevo que añade el chatbot; el resto ya los usaba la app.

---

## 🔑 Configuración del asistente (Gemini)

El chatbot usa la API de **Google Gemini** (modelo `gemini-2.5-flash`), que tiene un tier **gratuito y sin tarjeta de crédito**.

1. Entra a [aistudio.google.com](https://aistudio.google.com) e inicia sesión con tu cuenta de Google.
2. Pulsa **"Get API key"** y copia la clave generada.
3. Abre `app.R`, busca cerca del inicio la línea `GEMINI_API_KEY_FALLBACK <- ""` y pega tu clave entre las comillas:

   ```r
   GEMINI_API_KEY_FALLBACK <- "tu_clave_aqui"
   ```

4. Guarda y corre la app.

> **⚠️ La clave queda dentro de `app.R`.** No compartas el archivo ni lo subas a un repositorio público; quien lo vea podrá usar tu cuota de Gemini.

---

## ▶️ Cómo ejecutar

1. Clona o descarga el proyecto.
2. Instala los paquetes de R (ver arriba).
3. Pega tu clave de Gemini en la línea `GEMINI_API_KEY_FALLBACK` de `app.R` (ver arriba).
4. Abre `app.R` en RStudio y pulsa **Run App**, o desde la consola:

   ```r
   shiny::runApp()
   ```

La app abre en el navegador. El botón del chat aparece abajo a la derecha en todas las pestañas.

---

## 📂 Estructura del proyecto

```
proyecto_mundial/
├── app.R          # Aplicación Shiny completa (UI + servidor + chatbot, incluye la clave)
└── README.md      # Este archivo
```

---

## ☁️ Despliegue (shinyapps.io u otro servidor)

Como la clave va dentro de `app.R`, la app funciona en **shinyapps.io** sin configuración extra: se despliega tal cual. Ten en cuenta que la clave viaja dentro del código, así que usa un proyecto/repositorio **privado** y no publiques el `app.R`. Si prefieres no incluir la clave en el código, shinyapps.io permite definir la variable de entorno `GEMINI_API_KEY` en el panel de la app (Settings); en ese caso bastaría con dejar `GEMINI_API_KEY_FALLBACK <- ""`.

> **Nota:** Ollama y modelos locales no funcionan en shinyapps.io porque ese servidor no los aloja; para despliegue en la nube usa un proveedor por API como Gemini.

---

## 🔒 Seguridad de la clave

- La clave da acceso a tu cuota gratuita de Gemini (sin costo, pero limitada). Mantenla privada.
- Si en algún momento la expusiste (la compartiste o subiste por error), **regenérala** en AI Studio: la anterior queda invalidada.

---

## 🛠️ Tecnologías

R · Shiny · shinydashboard · dplyr · tidyr · ggplot2 · plotly · DT · httr2 · Google Gemini API