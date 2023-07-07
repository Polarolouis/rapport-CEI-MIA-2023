require("ggplot2")
require("tictoc")
require("colSBM")

result_clustering <- readRDS("./Rcodes/simulation/data/simulated")

list_clustering <- lapply(
    seq_along(result_clustering), function(s) result_clustering[[s]]$list_of_clusterings
)

list_best_partition <- lapply(
    seq_along(list_clustering), function(s) {
        list(
            epsilon = result_clustering[[s]]$epsilon,
            best_partition = unlist(extract_best_bipartite_partition(list_clustering[[s]]))
        )
    }
)
