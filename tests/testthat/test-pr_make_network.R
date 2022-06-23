context("check function pr_make_network\n")

testthat::test_that(
    "making networks works", {
        
        nt = pr_make_network(
            block_df = block_df,
            block_ref_df = block_ref_df,
            time_bin = "1 minute"
        )

        testthat::expect_s3_class(
            nt, "tbl_graph"
        )

    }
)

testthat::test_that(
    "plotting networks works", {
        
        nt = pr_make_network(
            block_df = block_df,
            block_ref_df = block_ref_df,
            time_bin = "1 minute"
        )

        custom_layout = igraph::layout.circle(nt)

        network_plot = pr_plot_network(
            network = nt,
            wt_lim = 1
        )

        # check that the basic plot works
        testthat::expect_s3_class(
            network_plot, "ggraph"
        )

        network_plot_2 = pr_plot_network(
            network = nt,
            wt_lim = 1,
            colour_by = NULL,
            layout = custom_layout
        )

        # check that the basic plot works
        testthat::expect_s3_class(
            network_plot, "ggraph"
        )        
    }
)
