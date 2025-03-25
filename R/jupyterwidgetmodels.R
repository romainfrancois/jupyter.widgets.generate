#' Jupyter Widget Models
#'
#' @format ## `jupyterwidgetmodels`
#' A data frame with `r nrow(jupyterwidgetmodels)` and 8 columns
#' \describe{
#'   \item{_view_module}{Name of the view module}
#'   \item{_view_module_version}{version of the view module}
#'   \item{_view_name}{Name of the view}
#'   \item{_model_module}{Name of the model module}
#'   \item{_model_module_version}{version of the model module}
#'   \item{_model_name}{Name of the model}
#'   \item{dom}{TRUE if this is a DOM widget}
#'   \item{attributes}{List of attributes}
#'   ...
#' }
#'
#' @examples
#' # attributes for Button
#' jupyterwidgetmodels$attributes[[7]]
#'
#' @source <https://github.com/jupyter-widgets/ipywidgets/blob/main/packages/schema/jupyterwidgetmodels.latest.json>
"jupyterwidgetmodels"
