library(socmod)
library(dplyr)
library(purrr)
library(igraph)
library(ggplot2)

# Helper function for Problem 3 on diffusion among Florentine oligarchs.
load_florentine_marriage_network <- function() {
  # Confirm that netrankr is installed since we get Florentine data from there.
  if (!requireNamespace("netrankr", quietly = TRUE)) {
    stop(
      "Please install the 'netrankr' package to access the Florentine families network."
    )
  }
  
  # Load Florentine marriage network.
  data("florentine_m", package = "netrankr", envir = environment())
  
  # One family in the dataset has no intermarriage links.
  graph <- igraph::delete_vertices(
    florentine_m,
    which(degree(florentine_m) == 0)
  )
  
  return (graph)
}


make_florentine_seed_model <- function(seed_families = c("Medici"),
                                       adaptive_fitness = 1.2,
                                       legacy_fitness = 1,
                                       graph = NULL) {
  if (is.null(graph)) {
    if (!requireNamespace("netrankr", quietly = TRUE)) {
      stop("Please install the 'netrankr' package to access the Florentine families network.")
    }
    data("florentine_m", package = "netrankr", envir = environment())
    graph <- igraph::delete_vertices(florentine_m, which(degree(florentine_m) == 0))
  }
  
  nodes <- igraph::V(graph)$name
  
  agents <- purrr::map(nodes, function(family) {
    behavior <- if (family %in% seed_families) "Adaptive" else "Legacy"
    fitness <- if (behavior == "Adaptive") adaptive_fitness else legacy_fitness
    Agent$new(id = family, name = family, behavior = behavior, fitness = fitness)
  })
  
  AgentBasedModel$new(agents = agents, graph = graph)
}


# Run the Florentine seed model for given seed_sets, adaptive_fitnesses, and
# optional legacy_fitness.
run_florentine_seed_model <- function(adaptive_fitnesses = c(1.0, 1.5),
                                      n_trials = 10, # number of trials per combination
                                      seed_sets = list(
                                        `Medici + Pazzi` = c("Medici", "Pazzi"),
                                        `Medici + Strozzi` = c("Medici", "Strozzi")
                                      ),
                                      legacy_fitness = 1) {

  # Generate all trials across seed sets and adaptive fitness levels
  trials <- purrr::map2(
    names(seed_sets),          # Get labels like "Medici + Pazzi"
    seed_sets,                 # Corresponding seed sets   (e.g., c("Medici", "Pazzi"))
    
    \(label, seeds) {          # For each seed set...

      # Iterate over all adaptive fitness values
      purrr::map(adaptive_fitnesses, \(f_A) {

        # Run n_trials trials for this seed set and fitness level
        run_trials(
          n_trials,  

          # Function to create a model with the specified seed set and fitness
          \() make_florentine_seed_model(
            seeds, adaptive_fitness = f_A
          ),

          label = label,  # used for grouping/plotting

          # Attach metadata so we can later group by it
          metadata = list(
            seed_set = label,
            adaptive_fitness = f_A
          ),

          # Stop each trial when everyone has the same behavior
          stop = fixated
        )

      }) |> flatten()  # Flatten the list of lists (1 per fitness level)

    }
  ) |> flatten()  # Flatten outer list (1 per seed set) into a flat list of Trial objects

  # Summarize outcomes by metadata fields
  summary_by_fitness <- summarise_by_metadata(
    trials, fields = c("adaptive_fitness", "seed_set")
  ) 

  return (summary_by_fitness)
}


# Plot success rate vs. adaptive fitness, grouped by seed set.
plot_florentine_seed_analysis <- function(adaptive_fitnesses, 
                                          summary_by_fitness,
                                          base_size = 14) {
  p <- ggplot(summary_by_fitness, aes(x = adaptive_fitness, y = success_rate,
                             color = seed_set, linetype = seed_set)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2) +
    labs(
      x = "Adaptive fitness",
      y = "Success rate",
      color = "Seed set",
      linetype = "Seed set"
    ) +
    scale_x_continuous(breaks = adaptive_fitnesses) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    geom_point(size = 3) +
    theme_classic(base_size = base_size)

    return (p)
}
