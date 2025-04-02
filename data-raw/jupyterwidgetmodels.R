library(jsonlite)
library(purrr)
library(tibble)
library(dplyr)
library(rlang)

url <- "https://raw.githubusercontent.com/jupyter-widgets/ipywidgets/refs/heads/main/packages/schema/jupyterwidgetmodels.latest.json"
models <- read_json(url, simplifyVector = FALSE)

view <- map_dfr(models, \(x) {
  tibble(
    `_view_module` = x$view$module,
    `_view_module_version` = x$view$version,
    `_view_name` = if (is.null(x$view$name)) "null" else x$view$name
  )
})

model <- map_dfr(models, \(x) {
  tibble(
    `_model_module`         = x$model$module,
    `_model_module_version` = x$model$version,
    `_model_name`           = if (is.null(x$model$name)) "null" else x$model$name
  )
})

attributes <- map(models, \(x) {
  out <- map_dfr(x$attributes, \(attr) {
    tibble(
      name       = attr$name,
      type       = list(attr$type),
      help       = attr$help,
      default    = list(attr$default),
      allow_none = attr$allow_none %||% TRUE,
      enum       = list(as.character(attr$enum))
    )
  })

  filter(out, !name %in% c(names(view), names(model)))

})

dom <- map_lgl(attributes, \(attr) {
  "_dom_classes" %in% attr$name
})

attributes <- map(attributes, \(attr) {
  is_dom <- "_dom_classes" %in% attr$name
  out <- filter(attr, !name %in% "_dom_classes")
  if (is_dom) {
    out <- filter(out, !name %in% c("layout", "style", "tabbable", "tooltip"))
  }
  out
})

jupyterwidgetmodels <- tibble(!!!view, !!!model, dom = dom, attributes = attributes)

usethis::use_data(jupyterwidgetmodels, overwrite = TRUE)
