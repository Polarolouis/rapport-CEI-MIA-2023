# Sourcing all necessary files
require("sbm", quietly = T)
require("dplyr", quietly = T)
require("tictoc", quietly = T)
require("ggplot2", quietly = T)

devtools::load_all(path = "R/")

eps <- 0.05
M <- 2
nr <- 100
nc <- 250

pir1 <- c(0.2, 0, 0.8)
pir2 <- c(0.4, 0.6, 0)

pic1 <- c(0.6, 0, 0.4)
pic2 <- c(0.4, 0.6, 0)

Q <- c(length(pir1), length(pic1))

# Make a non common alpha structure

alpha <- matrix(
    c( # 12   2    1
        0.4, eps, eps, # 12
        eps, 0.5, 0, # 2
        eps, 0, 0.2 # 1
    ),
    nrow = Q[1], ncol = Q[2], byrow = TRUE
)

bipartite_collection <- list(
    generate_bipartite_network(nr, nc, pir1, pic1, alpha),
    generate_bipartite_network(nr, nc, pir2, pic2, alpha)
)

# This is a list of the M incidence matrices
bipartite_collection_incidence <- lapply(seq.int(M), function(m) {
    bipartite_collection[[m]]$incidence_matrix
})

Z <- lapply(seq.int(M), function(m) {
    list(
        bipartite_collection[[m]]$row_clustering,
        bipartite_collection[[m]]$col_clustering
    )
})

results <- data.frame(matrix(nrow = 0, ncol = 6))
names(results) <- c(
    "repetition",
    "user_func_parallel",
    "exploration_parallel",
    "subexploration_parallel",
    "ari_sum",
    "time"
)

repetition_number <- 3

conditions <- expand.grid(
    seq.int(3), c(TRUE),
    c(TRUE, FALSE), c(TRUE, FALSE)
)

for (i in seq_len(nrow(conditions))) {
    cat(
        "\nCondition ", i, "/", nrow(conditions), "\n",
        "repetition:", conditions[i, 1],
        " -- user_func_parallel:", conditions[i, 2],
        "-- exploration_parallel:", conditions[i, 3],
        "-- subexploration_parallel:", conditions[i, 4],
        "\n"
    )
    t0 <- Sys.time()
    mybisbmpop <- estimate_colBiSBM(
        netlist = bipartite_collection_incidence,
        colsbm_model = "pirho",
        global_opts = list(
            nb_cores = parallel::detectCores() - 1,
            verbosity = 0,
            parallelization_vector = unlist(conditions[i,c(2,3,4)])
        )
    )
    t1 <- Sys.time()
    ari_sums <- sum(sapply(
        seq_along(mybisbmpop$best_fit$Z),
        function(m) {
            sum(
                aricode::ARI(
                    Z[[m]][[1]],
                    mybisbmpop$best_fit$Z[[m]][[1]]
                ),
                aricode::ARI(
                    Z[[m]][[2]],
                    mybisbmpop$best_fit$Z[[m]][[2]]
                )
            )
        }
    ))

    results[nrow(results) + 1, ] <- c(
        conditions[i, 1],
        conditions[i, 2],
        conditions[i, 3],
        conditions[i, 4],
        ari_sums,
        t1 - t0
    )
}

saveRDS(results,
    file = paste0("./simulation/data/parallel_levels_", Sys.time(), ".Rds")
)
