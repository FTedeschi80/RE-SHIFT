library(shiny)
library(shinyjs)
library(DT)
library(plotly)
library(stringr)
library(htmltools)

embed_parent_origins_env <- Sys.getenv("CNMA_LMIC_EMBED_PARENT_ORIGINS", "")
embed_parent_origins_vec <- if (nzchar(embed_parent_origins_env)) {
  v <- trimws(strsplit(embed_parent_origins_env, ",", fixed = TRUE)[[1]])
  v[nzchar(v)]
} else {
  character()
}
embed_parent_origins_json <- if (length(embed_parent_origins_vec)) {
  pieces <- vapply(embed_parent_origins_vec, function(s) {
    s <- gsub("\\\\", "\\\\\\\\", s, fixed = TRUE)
    s <- gsub("\"", "\\\\\"", s, fixed = TRUE)
    paste0("\"", s, "\"")
  }, character(1))
  paste0("[", paste(pieces, collapse = ","), "]")
} else {
  "[]"
}

embed_allowlist_script <- tags$script(HTML(paste0(
  "window.CNMA_LMIC_EMBED_PARENT_ORIGINS = ",
  embed_parent_origins_json, ";"
)))

embed_first_paint_embed_theme <- tags$script(HTML(
  "(function(){try{var q=new URLSearchParams(window.location.search).get('embed_theme');if(q==='dark')document.documentElement.setAttribute('data-theme','dark');else if(q==='light')document.documentElement.removeAttribute('data-theme');}catch(e){}})();"
))

htmlTemplate(
  "template.html",
  embed_allowlist_script = embed_allowlist_script,
  embed_first_paint_embed_theme = embed_first_paint_embed_theme,
  effect.plot = plotOutput("effect.plot", height = "70px"),
  effect = textOutput("effect", inline = TRUE),
  lower.or.higher = textOutput("lower.or.higher", inline = TRUE),
  effect.ci = textOutput("effect.ci", inline = TRUE),
  significance = textOutput("significance", inline = TRUE)
)
