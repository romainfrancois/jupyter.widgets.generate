#' Generate documentation for widgets that inherit from DOMWidget
#'
#' @param name name of the model
#' @param style if not NULL, enforce style
#' @inheritParams rlang::args_error_context
#'
#' @export
generate_dom_widget <- function(name = "Button", style = "ButtonStyle", error_call = current_env()) {
  template <- paste(readLines(system.file("template", "DOMWidget.txt", package = "jupyter.widgets.generate")), collapse = "\n")

  model_data <- extract_model_data(name = name, error_call = error_call)

  model_module <- model_data$`_model_module`
  model_name   <- model_data$`_model_name`
  view_module  <- model_data$`_view_module`
  view_name    <- model_data$`_view_name`

  init_style <- if (!is.null(style)) "style = style, " else ""

  initialize_params_roxygen      <- generate_initialize_params_roxygen(name = name, style = style, model_data = model_data, error_call = error_call)
  factory_params_roxygen         <- generate_factory_params_roxygen(name = name, style = style, model_data = model_data, error_call = error_call)

  initialize_params_defaults     <- generate_initialize_params_defaults(name = name, style = style, model_data = model_data, error_call = error_call)
  factory_params_defaults        <- generate_factory_params_defaults(name = name, style = style, model_data = model_data, error_call = error_call)

  forward_factory_to_constructor <- generate_forward_factory_to_constructor(name = name, style = style, model_data = model_data, error_call = error_call)

  init_params_state              <- generate_init_params_state(name = name, style = style, model_data = model_data, error_call = error_call)
  active_bindings                <- generate_active_bindings(name = name, style = style, model_data = model_data, error_call = error_call)

  glue(template, .trim = FALSE, .open = "{{", .close = "}}")
}

#' Generate documentation for style widgets
#'
#' @param name name of the model
#' @inheritParams rlang::args_error_context
#'
#' @export
generate_style_widget <- function(name = "ButtonStyle", error_call = current_env()) {
  template <- paste(readLines(system.file("template", "StyleWidget.txt", package = "jupyter.widgets.generate")), collapse = "\n")

  model_data <- extract_model_data(name = name, error_call = error_call)

  model_module <- model_data$`_model_module`
  model_name   <- model_data$`_model_name`
  view_module  <- model_data$`_view_module`
  view_name    <- model_data$`_view_name`

  initialize_params_roxygen      <- generate_initialize_params_roxygen(name = name, style = NULL, model_data = model_data, error_call = error_call)
  factory_params_roxygen         <- generate_factory_params_roxygen(name = name, style = NULL, model_data = model_data, error_call = error_call)

  initialize_params_defaults     <- generate_initialize_params_defaults(name = name, style = NULL, model_data = model_data, error_call = error_call)
  factory_params_defaults        <- generate_factory_params_defaults(name = name, style = NULL, model_data = model_data, error_call = error_call)

  forward_factory_to_constructor <- generate_forward_factory_to_constructor(name = name, style = NULL, model_data = model_data, error_call = error_call)

  init_params_state              <- generate_init_params_state(name = name, style = style, model_data = model_data, error_call = error_call)
  active_bindings                <- generate_active_bindings(name = name, style = style, model_data = model_data, error_call = error_call)

  glue(template, .trim = FALSE, .open = "{{", .close = "}}")
}



generate_initialize_params_roxygen <- function(name = "Button", style = NULL, model_data, error_call = caller_env()) {
  attrs <- model_data$attributes[[1]]

  help <- ifelse(attrs$help == "", "(undocumented)", attrs$help)

  lines <- c(
    glue("#' @param {attrs$name} {help}"),
    if (!is.null(style)) {
      glue("#' @param style Must inherit from [jupyter.widget.{style}].")
    }
  )
  glue_collapse(sep = "\n", paste0("    ", lines))
}

generate_factory_params_roxygen <- function(name = "Button", style = NULL, model_data, error_call = caller_env()) {
  attrs <- model_data$attributes[[1]]

  help <- ifelse(attrs$help == "", "(undocumented)", attrs$help)

  lines <- c(
    glue("#' @param {attrs$name} {help}"),
    "#' ",
    if (!is.null(style)) {
      glue("#' @param style Must inherit from [jupyter.widget.{style}].")
    },
    "#' "
  )
  glue_collapse(sep = "\n", lines)
}

extract_defaults <- function(attrs) {
  with(attrs, map2_chr(default, allow_none, \(default, allow_none) {
    # TODO: this will expand as we discover various default value possibilities
    constructive::construct(default)$code
  }))
}

generate_initialize_params_defaults <- function(name = "Button", style = NULL, model_data, error_call = caller_env()) {
  attrs <- model_data$attributes[[1]]
  defaults <- extract_defaults(attrs)

  lines <- c(
    glue("{attrs$name} = {defaults}"),
    if (!is.null(style)) {
      glue("style = {style}()")
    }
  )
  glue_collapse(sep = ",\n", paste0("      ", lines))
}

generate_factory_params_defaults <- function(name = "Button", style = NULL, model_data, error_call = caller_env()) {
  attrs <- model_data$attributes[[1]]
  defaults <- extract_defaults(attrs)

  lines <- c(
    glue("{attrs$name} = {defaults}"),
    if (!is.null(style)) {
      glue("style = {style}()")
    }
  )
  glue_collapse(sep = ",\n", paste0("  ", lines))
}


generate_init_params_state <- function(name = "Button", style = NULL, model_data, error_call = caller_env()) {
  attrs <- model_data$attributes[[1]]

  lines <- c(
    glue("{attrs$name} = self$check_state('{attrs$name}', {attrs$name})")
  )
  glue_collapse(sep = ",\n", paste0("        ", lines))
}

generate_active_bindings <- function(name = "Button", style = NULL, model_data, error_call = caller_env()) {
  attrs <- model_data$attributes[[1]]

  lines <- glue(.trim = FALSE, "
    #' @field {attrs$name}
    #' {attrs$help}
    {attrs$name} = function(x) if(missing(x)) private$state_[['{attrs$name}']] else self$update({attrs$name} = self$check_state('{attrs$name}', x))")
  glue_collapse(sep = ",\n", paste0("    ", lines))
}

generate_forward_factory_to_constructor <- function(name = "Button", style = NULL, model_data, error_call = caller_env()) {
  attrs <- model_data$attributes[[1]]

  lines <- c(
    glue("{attrs$name} = {attrs$name}"),
    if (!is.null(style)) {
      "style = style"
    },
    "...",
    "error_call = error_call"
  )
  glue_collapse(sep = ",\n", paste0("    ", lines))
}

extract_model_data <- function(name, error_call = caller_env()) {
  data <- filter(jupyter.widgets.generate::jupyterwidgetmodels, .data[["_model_name"]] == paste0(name, "Model"))
  if (nrow(data) != 1L) {
    cli::cli_abort(c("Wrong `model` {model}"), call = error_call)
  }
  data
}
