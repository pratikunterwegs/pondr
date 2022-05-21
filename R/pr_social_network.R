#' @title Construct social networks from reads data.
#'
#'
#' @description Function to return a `tidygraph` network object from fish
#' detected within a `time_bin` at one of multiple readers.
#'
#' @param block_df A data frame containing the reads from an experimental block.
#' This data frame must have at least the following columns:
#' `time`, `antenna`, `id`.
#' @param block_ref_df A data frame containing a reference list with all
#' individuals present in the experimental block.
#' @param time_bin A time bin that can be understood by `lubridate::round_date`,
#' such as "5 seconds", "1 minute", or similar.
#'
#' @return A `tidygraph` network object.
#'
#' @export
#'
#' @examples
#'
#' data(block_df)
#' data(block_ref_df)
#'
#' network <- pr_make_network(block_df, block_ref_df, time_bin = "5 seconds")
#'
pr_make_network <- function(block_df, block_ref_df, time_bin = "5 seconds") {

  # handle global variable errors
  time <- NULL
  time_binned <- NULL
  antenna <- NULL
  id <- NULL
  n <- NULL
  co_occur <- NULL
  edges <- NULL
  focal <- NULL
  subfocal <- NULL
  id1 <- NULL
  id2 <- NULL
  from <- NULL
  to <- NULL
  weight <- NULL
  g <- NULL

  assertthat::assert_that(
    "id" %in% colnames(block_df),
    msg = "make_network: reads data frame does not have column 'id'"
  )

  assertthat::assert_that(
    "id" %in% colnames(block_ref_df),
    msg = "make_network: individual attribute data frame does not have column 'id'"
  )

  block_df <-
    block_df %>%
    # round the time to the nearest value given in time_bin, default 5 seconds
    dplyr::mutate(
      time_binned = lubridate::round_date(
        time,
        unit = time_bin
      )
    ) %>%
    # keep distinct reads of individuals per antenna per time bin
    dplyr::distinct(
      time_binned, antenna, id
    ) %>%
    # remove antenna and time bin combinations with only 1 individual
    dplyr::add_count(
      antenna, time_binned
    ) %>%
    dplyr::filter(
      n > 1
    ) %>%
    dplyr::select(
      -n
    )

  # split into separate data frames per time bin
  block_df <-
    tidyr::nest(
      block_df,
      co_occur = -c(time_binned, antenna)
    ) %>%
    # make pairs of individuals
    dplyr::mutate(
      edges = lapply(
        co_occur, function(df) {
          tidyr::crossing(
            focal = df$id, subfocal = df$id
          )
        }
      )
    ) %>%
    # keep only unique, non-self pairs
    dplyr::mutate(
      edges = lapply(
        edges, function(df) {
          df %>%
            dplyr::transmute(
              id1 = pmin(focal, subfocal),
              id2 = pmax(focal, subfocal)
            ) %>%
            dplyr::distinct(
              id1, id2
            ) %>%
            dplyr::select(
              from = id1, to = id2
            ) %>%
            dplyr::filter(
              from != to
            )
        }
      )
    ) %>%
    # remove excess data
    dplyr::select(
      -co_occur
    )

  # unnest edges and count associations across antennas as weights
  block_df <-
    tidyr::unnest(
      block_df,
      cols = edges
    ) %>%
    dplyr::count(
      from, to,
      name = "weight"
    )

  # make network object
  # the from and to columns from the edges are matched to
  # the `id` column in `block_ref_df`, using the `node_key` argument
  g <- tidygraph::tbl_graph(
    nodes = block_ref_df,
    node_key = "id",
    edges = block_df,
    directed = FALSE
  )

  # return network object
  g
}
