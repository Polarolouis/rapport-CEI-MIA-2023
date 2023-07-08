require("ggplot2")
require("tictoc")
require("colSBM")

set.seed(1234)

# Generation of conditions
if (!exists("model_to_test")) {
    model_to_test <- "iid"
}

if (!exists("repetitions")) {
    repetitions <- seq.int(3)
}

nr <- 75
nc <- 75

pi <- matrix(c(0.2, 0.3, 0.5), nrow = 1, byrow = TRUE)
rho <- matrix(c(0.2, 0.3, 0.5), nrow = 1, byrow = TRUE)
epsilons <- seq(0.1, 0.4, by = 0.1)

if (!exists("arg")) {
    arg <- commandArgs(trailingOnly = TRUE)
}

if (identical(arg, character(0))) {
    model_to_test <- "iid"
} else {
    model_to_test <- arg
}

conditions <- tidyr::crossing(epsilons, pi, rho, repetitions)

results <- lapply(seq_len(nrow(conditions)), function(s) {
    eps <- conditions[s, ]$epsilons
    current_pi <- conditions[s, ]$pi
    current_rho <- conditions[s, ]$rho

    alpha_assortative <- matrix(0.3, nrow = 3, ncol = 3) +
        matrix(
            c(
                eps, -0.5 * eps, -0.5 * eps,
                -0.5 * eps, eps, -0.5 * eps,
                -0.5 * eps, -0.5 * eps, eps
            ),
            nrow = 3, byrow = TRUE
        )

    alpha_core_periphery <- matrix(0.3, nrow = 3, ncol = 3) +
        matrix(
            c(
                1.5 * eps, eps, 0.5 * eps,
                eps, 0.5 * eps, 0,
                0.5 * eps, 0, -0.5 * eps
            ),
            nrow = 3, byrow = TRUE
        )

    alpha_disassortative <- matrix(0.3, nrow = 3, ncol = 3) +
        matrix(
            c(
                -0.5 * eps, eps, eps,
                eps, -0.5 * eps, eps,
                eps, eps, -0.5 * eps
            ),
            nrow = 3, byrow = TRUE
        )

    assortative_collection <- generate_bipartite_collection(
        nr, nc,
        current_pi, current_rho,
        alpha_assortative, 3,
        model = model_to_test,
        return_memberships = TRUE
    )

    assortative_incidence <- lapply(
        seq_along(assortative_collection),
        function(m) {
            return(assortative_collection[[m]]$incidence_matrix)
        }
    )

    assortative_row_clustering <- lapply(
        seq_along(assortative_collection),
        function(m) {
            return(assortative_collection[[m]]$row_clustering)
        }
    )

    assortative_col_clustering <- lapply(
        seq_along(assortative_collection),
        function(m) {
            return(assortative_collection[[m]]$row_clustering)
        }
    )

    core_periphery_collection <- generate_bipartite_collection(
        nr, nc,
        current_pi, current_rho,
        alpha_core_periphery, 3,
        model = model_to_test,
        return_memberships = TRUE
    )

    core_periphery_incidence <- lapply(
        seq_along(core_periphery_collection),
        function(m) {
            return(core_periphery_collection[[m]]$incidence_matrix)
        }
    )

    core_periphery_row_clustering <- lapply(
        seq_along(core_periphery_collection),
        function(m) {
            return(core_periphery_collection[[m]]$row_clustering)
        }
    )

    core_periphery_col_clustering <- lapply(
        seq_along(core_periphery_collection),
        function(m) {
            return(core_periphery_collection[[m]]$row_clustering)
        }
    )

    disassortative_collection <- generate_bipartite_collection(
        nr, nc,
        current_pi, current_rho,
        alpha_disassortative, 3,
        model = model_to_test,
        return_memberships = TRUE
    )

    disassortative_incidence <- lapply(
        seq_along(disassortative_collection),
        function(m) {
            return(disassortative_collection[[m]]$incidence_matrix)
        }
    )

    disassortative_row_clustering <- lapply(
        seq_along(disassortative_collection),
        function(m) {
            return(disassortative_collection[[m]]$row_clustering)
        }
    )

    disassortative_col_clustering <- lapply(
        seq_along(disassortative_collection),
        function(m) {
            return(disassortative_collection[[m]]$row_clustering)
        }
    )

    real_row_clustering <- append(
        append(
            assortative_row_clustering,
            core_periphery_row_clustering
        ),
        disassortative_row_clustering
    )

    real_col_clustering <- append(
        append(
            assortative_col_clustering,
            core_periphery_col_clustering
        ),
        disassortative_col_clustering
    )

    incidence_matrices <- append(
        append(
            assortative_incidence,
            core_periphery_incidence
        ),
        disassortative_incidence
    )

    netids <- rep(c("as", "cp", "dis"), each = 3)

    tic()
    list_collection <- clusterize_bipartite_networks(
        netlist = incidence_matrices,
        net_id = netids,
        nb_run = 1,
        colsbm_model = model_to_test,
        global_opts = list(
            nb_cores = parallel::detectCores() - 1, verbosity = 2,
            plot_details = 0 # ,
            # parallelization_vector = c(FALSE, FALSE, FALSE)
        ),
        silent_parallelization = TRUE
    )

    best_partitions <- unlist(extract_best_bipartite_partition(list_collection))
    clustering <- unlist(lapply(seq_along(best_partitions), function(col_idx) {
        setNames(
            rep(col_idx, best_partitions[[col_idx]]$M),
            best_partitions[[col_idx]]$net_id
        )
    }))
    # ARI computation
    clustering <- clustering[order(names(clustering))]
    ari <- aricode::ARI(rep(c(1, 2, 3), each = 3), clustering)

    toc()
    cat(paste("Finished", s))
    return(
        data.frame(epsilon = eps, model = model_to_test, ARI = ari)
    )
}
# ,
# mc.cores = parallel::detectCores() - 1,
# mc.progress = TRUE,
# mc.retry = -1
)

data_frame_result <- do.call("rbind", results)

saveRDS(data_frame_result, file = paste0(
    "simulation/data/",
    "simulated_collection_data_clustering_",
    model_to_test, "_",
    format(Sys.time(), "%d-%m-%y-%X"),
    ".Rds"
))
