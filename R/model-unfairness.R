##
# Models and components for Ch 6 Achieving Fairness
# Author: Matt Turner
# Date: 2025-06-03
#

library(socmod)


#' Partner selection for partner dance game
#'
dance_partnering <- function(focal, model) {
  
}


#' Partner dance game interaction that yields payoffs.
#'
dance_interaction <- function(focal, partner, model) {
  
}


#' Social learning within gender for gender-based coordination models
#'
gender_coordination_social_learning <- function(model) {
  
  purrr::walk(
    model$agents,
    \(agent) {
      # Each agent has teachers attribute set at initialization, i.e., 
      # same-gender neighbors and self
      teachers <- agent$get_attribute("teachers")
      # Get fitnesses and do success-biased learning
      fitnesses <- purrr::map_vec(teachers, \(t) t$fitness_current)
      teacher <- sample(teachers, 1, prob = fitnesses)[[1]]
      agent$set_next_behavior(teacher$behavior_current)
      agent$set_next_fitness(teacher$fitness_current)
    }
  )
  
  # Call helper that sets all agents' current behavior and fitness to be the
  # next behavior/fitness
  learning_model_step(model)
}


initialize_dancers <- function(model, inversion_prevalence = 0.2) {
  
  # Make half the agents women, half men
  agents <- unlist(model$agents)
  N <- length(agents)
  
  
  women_idxs <- sample(1:N, N/2)
  men_idxs <- setdiff(1:N, women_idxs)
  
  women <- purrr::map(women_idxs, \(ii) model$get_agent(ii))
  men <- purrr::map(men_idxs, \(ii) model$get_agent(ii))
  
  purrr::walk(
    women, 
    \(w) {
      w$set_attribute("gender", "Woman")
    }
  )
  purrr::walk(
    men, 
    \(m) {
      m$set_attribute("gender", "Man")
    }
  )
 
  # Assign women inverters to lead, men inverters to follow
  n_inverters_per_gender <- round(inversion_prevalence * N * 0.5)
  
  women_leader_idxs <- sample(women_idxs, n_inverters_per_gender)
  women_follower_idxs <- setdiff(women_idxs, women_leader_idxs)
  
  women_leaders <- purrr::map(women_leader_idxs, \(ii) model$get_agent(ii))
  women_followers <- purrr::map(women_follower_idxs, \(ii) model$get_agent(ii))
  purrr::walk(
    women_leaders, 
    \(w) {
      w$set_behavior("Lead")
      w$set_next_behavior("Lead")
    }
  )
  purrr::walk(
    women_followers, 
    \(w) {
      w$set_behavior("Follow")
      w$set_next_behavior("Follow")
    }
  )
  
  men_follower_idxs <- sample(men_idxs, n_inverters_per_gender)
  men_leader_idxs <- setdiff(men_idxs, men_follower_idxs)
  
  men_followers <- purrr::map(men_follower_idxs, \(ii) model$get_agent(ii))
  men_leaders <- purrr::map(men_leader_idxs, \(ii) model$get_agent(ii))
  purrr::walk(
    men_followers, 
    \(m) {
      m$set_behavior("Follow")
      m$set_next_behavior("Follow")
    }
  )
  purrr::walk(
    men_leaders, 
    \(m) {
      m$set_behavior("Lead")
      m$set_next_behavior("Lead")
    }
  )
}



#--------- TESTING INITIALIZE_DANCERS -----------
abm <- make_abm(n_agents = 40); 

initialize_dancers(abm, inversion_prevalence = 0.5); 

cat("\n\nWomen's table of behaviors\n")
print(
  table(
    unlist(
      purrr::map(
        unlist(
          abm$agents %>% keep(\(a) a$get_attribute("gender") == "Woman")
        ), 
        \(a) a$get_behavior()
      )
    )
  )
)

cat("\n\nMen's table of behaviors\n")
print(
  table(
    unlist(
      purrr::map(
        unlist(
          abm$agents %>% keep(\(a) a$get_attribute("gender") == "Man")
        ), 
        \(a) a$get_behavior()
      )
    )
  )
)