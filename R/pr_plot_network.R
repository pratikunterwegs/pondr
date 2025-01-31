#' @title Plot a social network.
#'
#' @description Function to plot a `tidygraph` network object made using
#' `pr_make_network`.
#'
#' @param network A `tidygraph` network object.
#' @param colour_by A variable in the node data (given in `block_ref_df`)
#' by which to colour the data, passed as an _unquoted_ name, such as _SL_.
#' @param wt_lim A numeric value of the upper limit of the weight. Can be used
#' to scale edges to a common line thickness, to compare across figures.
#' @param layout Pass an optional layout to `ggraph`. Useful if passing a single
#' layout for multiple graphs. This argument can be missing; the function will
#' generate a layout internally if so.
#' @return A `ggraph` plot.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data(block_df)
#' data(block_ref_df)
#'
#' network <- pr_make_network(block_df, block_ref_df, time_bin = "5 seconds")
#' 
#' # custom layout
#' layout = igraph::layout.fruchterman.reingold(network)
#'
#' # to colour by the categorical variable 'Plates'
#' pr_plot_network(network, Plates, layout = layout) # no quotes around Plates!
#'
#' # to colour by the numeric variable 'SL'
#' pr_plot_network(network, SL)
#'
#' # no colours, the default
#' pr_plot_network(network, NULL)
#' }
#'
pr_plot_network <- function(network, colour_by = NULL, wt_lim = 100, layout) {
  colour_by = rlang::enquo(colour_by)

  # check if manual layout provided
  layout_ = igraph::layout.kamada.kawai(network)
  if(!rlang::is_missing(layout)) {
    layout_ = layout
  }

  p <- ggraph::ggraph(
    network,
    layout = layout_
  ) +
    ggraph::geom_edge_fan(
      edge_colour = "grey70",
      ggplot2::aes(
        edge_width = weight
      ),
      edge_alpha = 0.5,
      show.legend = FALSE
    ) +
    ggraph::geom_node_point(
      ggplot2::aes(
        fill = !!colour_by
      ),
      size = 5,
      shape = 21
    ) +
    ggraph::scale_edge_width(
      range = c(0.1, 5),
      limits = c(1, wt_lim),
      oob = scales::squish
    )

  # pull the column by which to colour the data to identify
  # whether it's a numeric
  # first check whether the colour variable is NULL
  if (!rlang::quo_is_null(colour_by)) {

    # get colour variable as name
    colname = rlang::as_name(colour_by)
    # check whether it is a numeric column, if so, use a continuous scale
    if(is.numeric(dplyr::as_tibble(network)[[colname]])) {
      p <- p +
        ggplot2::scale_fill_viridis_c(
          direction = -1,
          na.value = "grey99"
        )
    } else {
      p <- p +
        ggplot2::scale_fill_viridis_d(
          option = "H",
          begin = 0.1, end = 0.9
        )
    }
  }
  p
}
