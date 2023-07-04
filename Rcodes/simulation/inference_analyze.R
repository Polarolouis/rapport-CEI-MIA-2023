require("ggplot2")
require("tidyr")
require("dplyr")
filenames <- list.files(
    path = "./Rcodes/simulation/data/",
    pattern = "inference_testing_2023-07*",
    full.names = TRUE
)
col_id_BICLS <- c(11, 16, 23, 30, 37)
data_list <- lapply(filenames, readRDS)
result_data_frame <- dplyr::bind_rows(data_list)
result_data_frame <- cbind(result_data_frame, preferred_model = sapply(seq_len(nrow(result_data_frame)), function(n) names(which.max(result_data_frame[n, col_id_BICLS]))))

ggplot(data = result_data_frame) +
    aes(x = epsilon_alpha, group = preferred_model, fill = preferred_model) +
    geom_bar()

# Build ARI row long table
ARI_long_table <- result_data_frame %>%
    # mutate(
    #     iid_mean_row_ARI = iid_mean_row_ARI - sep_mean_row_ARI,
    #     pi_mean_row_ARI = pi_mean_row_ARI - sep_mean_row_ARI,
    #     rho_mean_row_ARI = rho_mean_row_ARI - sep_mean_row_ARI,
    #     pirho_mean_row_ARI = pirho_mean_row_ARI - sep_mean_row_ARI,
    #     iid_mean_col_ARI = iid_mean_col_ARI - sep_mean_col_ARI,
    #     pi_mean_col_ARI = pi_mean_col_ARI - sep_mean_col_ARI,
    #     rho_mean_col_ARI = rho_mean_col_ARI - sep_mean_col_ARI,
    #     pirho_mean_col_ARI = pirho_mean_col_ARI - sep_mean_col_ARI,
    #     ) %>%
    dplyr::select(
        c(
            epsilon_alpha, pi1.1, pi1.2, pi1.3, pi1.4,
            rho2.1, rho2.2, rho2.3, rho2.4, repetition,
            # sep_mean_row_ARI, 
            iid_mean_row_ARI, pi_mean_row_ARI, rho_mean_row_ARI,
            pirho_mean_row_ARI, 
            # sep_mean_col_ARI,
            iid_mean_col_ARI,
            pi_mean_col_ARI,
            rho_mean_col_ARI,
            pirho_mean_col_ARI
        )
    ) %>%
    pivot_longer(
        cols = c(
            # sep_mean_row_ARI,
            iid_mean_row_ARI,
            pi_mean_row_ARI,
            rho_mean_row_ARI,
            pirho_mean_row_ARI,
            # sep_mean_col_ARI,
            iid_mean_col_ARI,
            pi_mean_col_ARI,
            rho_mean_col_ARI,
            pirho_mean_col_ARI
        ),
        names_to = c("model", "axis"),
        names_sep = "_mean_",
        names_transform = list(model = as.factor, axis = as.factor),
        values_to = "ARI"
    )

summarised_ARI <- ARI_long_table %>%
    group_by(
        epsilon_alpha, model, axis
    ) %>%
    summarise(mean_ARI = mean(ARI), sd_ARI = sd(ARI)) 

summarised_ARI %>%
    filter(axis == "row_ARI") %>%
    ggplot() +
    aes(x = epsilon_alpha, y = mean_ARI, color = model) +
    geom_ribbon(aes(x = epsilon_alpha, ymin = mean_ARI - sd_ARI, ymax = mean_ARI + sd_ARI, fill = model), alpha = 0.2) +
    geom_line() +
    geom_point()

summarised_ARI %>%
    filter(axis == "col_ARI") %>%
    ggplot() +
    aes(x = epsilon_alpha, y = mean_ARI, color = model) +
    geom_ribbon(aes(x = epsilon_alpha, ymin = mean_ARI - sd_ARI, ymax = mean_ARI + sd_ARI, fill = model), alpha = 0.2) +
    geom_line() +
    geom_point()

# Build Q long table
Q1_long_table <- result_data_frame %>%
    # mutate(
    #     iid_mean_row_ARI = iid_mean_row_ARI - sep_mean_row_ARI,
    #     pi_mean_row_ARI = pi_mean_row_ARI - sep_mean_row_ARI,
    #     rho_mean_row_ARI = rho_mean_row_ARI - sep_mean_row_ARI,
    #     pirho_mean_row_ARI = pirho_mean_row_ARI - sep_mean_row_ARI,
    #     iid_mean_col_ARI = iid_mean_col_ARI - sep_mean_col_ARI,
    #     pi_mean_col_ARI = pi_mean_col_ARI - sep_mean_col_ARI,
    #     rho_mean_col_ARI = rho_mean_col_ARI - sep_mean_col_ARI,
    #     pirho_mean_col_ARI = pirho_mean_col_ARI - sep_mean_col_ARI,
    #     ) %>%
    dplyr::select(
        c(
            epsilon_alpha, pi1.1, pi1.2, pi1.3, pi1.4,
            rho2.1, rho2.2, rho2.3, rho2.4, repetition,
            iid_Q1, 
            pi_Q1,
            rho_Q1,
            pirho_Q1
        )
    ) %>%
    pivot_longer(
        cols = c(
            iid_Q1,
            pi_Q1,
            rho_Q1,
            pirho_Q1
        ),
        names_to = c("model"),
        names_transform = list(model = as.factor),
        values_to = "row_blocks"
    )

Q1_summarised <- Q1_long_table %>%
    group_by(
        epsilon_alpha, model
    ) %>%
    summarise(mean_row_blocks = mean(row_blocks), sd_row_blocks = sd(row_blocks))

Q1_summarised %>%
    ggplot() +
    aes(x = epsilon_alpha, y = mean_row_blocks, color = model) +
    # geom_ribbon(aes(x = epsilon_alpha, ymin = mean_row_blocks - sd_row_blocks, ymax = mean_row_blocks + sd_row_blocks, fill = model), alpha = 0.2) +
    geom_hline(yintercept = 4)+
    geom_line() +
    geom_point() 

Q2_long_table <- result_data_frame %>%
    # mutate(
    #     iid_mean_row_ARI = iid_mean_row_ARI - sep_mean_row_ARI,
    #     pi_mean_row_ARI = pi_mean_row_ARI - sep_mean_row_ARI,
    #     rho_mean_row_ARI = rho_mean_row_ARI - sep_mean_row_ARI,
    #     pirho_mean_row_ARI = pirho_mean_row_ARI - sep_mean_row_ARI,
    #     iid_mean_col_ARI = iid_mean_col_ARI - sep_mean_col_ARI,
    #     pi_mean_col_ARI = pi_mean_col_ARI - sep_mean_col_ARI,
    #     rho_mean_col_ARI = rho_mean_col_ARI - sep_mean_col_ARI,
    #     pirho_mean_col_ARI = pirho_mean_col_ARI - sep_mean_col_ARI,
    #     ) %>%
    dplyr::select(
        c(
            epsilon_alpha, pi1.1, pi1.2, pi1.3, pi1.4,
            rho2.1, rho2.2, rho2.3, rho2.4, repetition,
            iid_Q2,
            pi_Q2,
            rho_Q2,
            pirho_Q2
        )
    ) %>%
    pivot_longer(
        cols = c(
            iid_Q2,
            pi_Q2,
            rho_Q2,
            pirho_Q2
        ),
        names_to = c("model"),
        names_transform = list(model = as.factor),
        values_to = "row_blocks"
    )

Q2_summarised <- Q2_long_table %>%
    group_by(
        epsilon_alpha, model
    ) %>%
    summarise(mean_row_blocks = mean(row_blocks), sd_row_blocks = sd(row_blocks))

Q2_summarised %>%
    ggplot() +
    aes(x = epsilon_alpha, y = mean_row_blocks, color = model) +
    # geom_ribbon(aes(x = epsilon_alpha, ymin = mean_row_blocks - sd_row_blocks, ymax = mean_row_blocks + sd_row_blocks, fill = model), alpha = 0.2) +
    geom_hline(yintercept = 4) +
    geom_line() +
    geom_point()
