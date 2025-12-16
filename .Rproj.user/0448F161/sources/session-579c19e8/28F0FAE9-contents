# Load packages
library(tidyverse)
library(tidygraph)
library(ggraph)
library(targets)

# Set output directory
output_dir <- here::here("resources")  # CHANGE THIS
# dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Load data
# tmp <- tar_read(path_results, store = here::here("../../_targets_network"))
main_g <- tar_read(main_g, store = here::here("../../_targets_network"))

tg <- main_g %>% 
  map(
    ~ .x %>% 
     as_tbl_graph()
  )


# Build network with centrality measures
tgs <- tg %>%
  map(
    ~ .x %>% 
      activate(nodes) %>%
      mutate(
        category = str_sub(name, end = 1),
        degree_total = centrality_degree(mode = "total"),
        degree_in = centrality_degree(mode = "in"),
        degree_out = centrality_degree(mode = "out"),
        betweenness = centrality_betweenness(directed = TRUE),
        burt_constraint = node_constraint(weights = weight),
        eigenvector = centrality_eigen(weights = weight, directed = TRUE),
        authority = centrality_authority(weights = weight),
        hub = centrality_hub(weights = weight)
      ) %>% 
      activate(edges) %>% 
      mutate(
        e_betweeness = centrality_edge_betweenness(directed = T)
      )
  )
  

# Extract centrality table
centrality_results <- tg %>%
  activate(nodes) %>%
  as_tibble()

# Save centrality data
write_csv(centrality_results, file.path(output_dir, "centrality_metrics.csv"))

# Create and save network plot
cat_colors <- c("A" = "#E64B35", "B" = "#4DBBD5", "C" = "#00A087", "D" = "#3C5488",
                "E" = "#F39B7F", "F" = "#8491B4", "G" = "#91D1C2", "H" = "#DC0000")

p <- tgs[[11]] %>% 
  ggraph(layout = "fr") +
  geom_edge_arc(
    aes(alpha = e_betweeness),
    color = "grey70",
    strength = 0.15,
    arrow = arrow(length = unit(1.5, "mm"), type = "closed"),
    end_cap = circle(3, "mm"),
    show.legend = FALSE,
    linewidth = 0.3
  ) +
  geom_node_point(
    aes(color = category, size = degree_total, alpha = eigenvector),
    shape = 16
  ) +
  scale_color_manual(values = cat_colors, name = "IPC Category") +
  scale_size_continuous(
    name = "Total Degree",
    range = c(1, 12),
    guide = guide_legend(nrow = 2, order = 2)
  ) +
  scale_alpha_continuous(
    name = "Eigenvector Centrality",
    range = c(0.5, 1),
    guide = guide_legend(nrow = 2, order = 3)
  ) +
  geom_node_text(aes(label = ifelse(eigenvector >= 0.115, name, "")),
                 repel = TRUE, size = 3
  ) +
  labs(
    caption = "Node size = degree, transparency = eigenvector centrality"
    # subtitle = "Node size = degree, transparency = eigenvector centrality"
  ) +
  theme_graph(base_size = 11) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    # plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    # plot.subtitle = element_text(color = "grey40", size = 10, hjust = 0.5, margin = margin(b = 15)),
    legend.position = "bottom",
    # plot.title.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 9, face = "bold", hjust = 0.5),
    legend.title.position = "top",
    legend.text = element_text(size = 8),
    legend.margin = margin(t = 15)
  ) +
  guides(
    color = guide_legend(nrow = 2, order = 1, override.aes = list(size = 4, alpha = 1))
  )

# Save plot
ggsave(file.path(output_dir, "technology_network.png"), p, width = 16, height = 11, dpi = 150)

cat("Files saved to:", output_dir, "\n")