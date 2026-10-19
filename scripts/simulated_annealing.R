items <- items
easiness <- easiness_1PL
infit <- infit_fit_sel
outfit <- outfit_fit_sel
discrimination <- disc_2PL

# Function measuring how far is x from 1 in absolute value
func <- function(x){
  abs(1-x)
}

# Function dividing x by 100 (for rescaling)

func2 <- function(x){
  x/100
}

# Score function: takes subset and scores it

# k = items number in the subset

# Objective funciton to determine score

score_fn <- function(subset) {
  easinesses <- sort(easiness[subset]) # takes easiness for the selected items
  nn_dists <- rep(0, sum(subset)-1) # a vector of 0s, distances between consecutive sorted values: there are k-1 such gaps.
  for(i in 1:sum(subset)-1) { 
    nn_dists[i] <- easinesses[i+1] - easinesses[i] # loop though the sorted eas. values; each time --> compute gap btw current and next item; Store in nn_dist
  }
  spacing <- -1*sd(nn_dists)/3 # sd of these gaps --> large if gaps very uneven --> we don't have a group of items that has equal spacing in easiness from one another
  # multiplied by -1 --> uneven spacing makes the score worse
  # dividing by 3 to scale it down
  
  var_disc_sample <- discrimination[subset]
  var_disc_2PL <- -1*var(var_disc_sample)*6
  
  infit_sample <- infit[subset] # take infit for selected items
  infit_dist <- unlist(lapply(infit_sample, func)) # apply function to calculate absolute value of distance from 1
  mean_infit <- -4*mean(infit_dist) # take average distance from 1, multp. by -4 --> larger deviations from 1 reduce the score more strongly
  # infit closer to one --> higher score
  
  outfit_sample <- outfit[subset]
  outfit_dist <- unlist(lapply(outfit_sample, func))
  mean_outfit <- -2*mean(outfit_dist)
  # same as above, but we penalize less if outfit is very far from 1
  
  # return(spacing + mean_infit + mean_outfit + var_disc_2PL)
  ret <- tibble(spacing = spacing,
                mean_infit = mean_infit,
                mean_outfit = mean_outfit,
                var_disc_2PL = var_disc_2PL,
                disc_penalty = disc_penalty,
                total_score = spacing + mean_infit + mean_outfit + var_disc_2PL)

  return(ret)
  # spacing: penalty if the easiness gaps vary --> we want a balanced subset
  # penalty if infit and outfit are far from 1
  # penalty for large mis
  # penalty for discrimination variance
  #return(spacing + mean_infit + mean_outfit)
}

# Function that takes the current subset and makes a new one 

proposal_fn <- function(subset) {
  # Randomly sample a number of swaps.
  # Prefer a small number of swaps for "fine tuning", but allow
  # occasional large numbers of swaps, including a complete
  # exchange of the subset
  
  subset_size = sum(as.integer(subset))
  max_swaps = min(subset_size, length(subset) - subset_size) # max possible swaps 
                                                             # is smaller than selected and unselected count
  swaps <- rbinom(1, max_swaps-1, 1/(max_swaps-1)) + 1 
  # randomly chooses how many swaps to do
  # takes random number from a binomial distribution
  # size : max_swaps-1
  # probability : 1/(max_swaps-1)
  # Takes usually small numbers, sometimes bigger
  
  
  # Choose the items to swap
  active_items <- seq(1:length(subset))[subset == TRUE] # indices from selected
  inactive_items <- seq(1:length(subset))[subset == FALSE] # indices from unselected
  actives_to_swap <- sample(active_items, swaps) # randomly removes swaps number of items from the subset
  inactives_to_swap <- sample(inactive_items, swaps) # randomly adds swaps number of unselected items
  
  # Do the swapping
  for(i in 1:swaps) {
    subset[actives_to_swap[i]] <- FALSE
    subset[inactives_to_swap[i]] <- TRUE
  }
  return(subset)
}

# The simulated annealing driver
# k = number of items we want to select
# cooling_ratio --> how quickly we "cool" (become stricter)
# reset_thresh and break_thresh control stopping/reset behaviour

simulated_annealing_rasch <- function(item_set, k, disc_penalty, cooling_ratio=0.999, reset_thresh=1000, break_thresh=10000) {
  
  N <- length(item_set) # total number of items
  
  current_subset <- sample(c(rep(TRUE, k), rep(FALSE, N-k))) # create logical vector with k true values and N-k Falses
                                                            # shuffle it randomly with sample()
  best_subset <- current_subset # start by current one
  best_scorings <- score_fn(best_subset, disc_penalty) # store all the different components of the score for the best subset
  best_score <- best_scorings$total_score # calc score

  
  temp <- 100 # how willing we are to accept “bad” moves early on (starting at 100 gives us probability 1)
  rejected <- 0 # how many bad proposals in a row we refused
  no_new_bests <- 0 # how long since we last found a new best
  for(i in 1:1e6) { 
    # 1 million tries
    # Score new subset, and toss a coin
    new_subset <- proposal_fn(current_subset) # propose new subset
    new_scorings <- score_fn(new_subset, disc_penalty)
    new_score <- new_scorings$total_score
    accept_decrease <- runif(1) < (temp / 100) # flips coin: 
                                                 # probability of TRUE is temp/100 (1)
                                                 # decreases later on
                                                 # changed from rbernoulli because it was deprecated in purrr 
    
    # Acceptance rule
    # Accept the new subset if it's an improvement, or if our
    # cooling coin came up heads.
    
    # accept the new score if it's the best so far or the random coin says accept anyway
    # initially accepts basically anything that comes, later on prob of coin = TRUE decreases, 
    # accepts only if better than what we already have.
    
    if(new_score > best_score | accept_decrease) {
      current_subset <- new_subset
      rejected <- 0
      if(new_score > best_score) {
        best_subset <- new_subset
        best_score <- new_score
        best_scorings <- new_scorings
        no_new_bests <- 0
      } else {
        no_new_bests <- no_new_bests + 1
      }
      # Quit if we've had too many rejections in a row and keep best solution found
    } else {
      rejected <- rejected + 1
      no_new_bests <- no_new_bests + 1 # count consecutive rejections and time since best
      if(rejected == break_thresh) {
        #print(best_score)
        ret <- tibble(
                      best_scorings = best_scorings,
                      spacing = best_scorings$spacing,
                      mean_infit = best_scorings$mean_infit,
                      mean_outfit = best_scorings$mean_outfit,
                      var_disc_2PL = best_scorings$var_disc_2PL,
                      disc_penalty = best_scorings$disc_penalty,
                      best_subset = list(best_subset),
                      best_score = best_score)
        
        return(ret)
      }
    }
    # Start random resets to the current best subset if we haven't
    # found anything better in quite a while (reset_thresh : default 1000)
    if(no_new_bests > reset_thresh & runif(1) < 1/100) {
      current_subset <- best_subset
    }
    
    # Cool it!
    temp <- temp*cooling_ratio # every iteration multiply temp by something less than 1 (default 0.999)
    # temperature gradually decreases toward 0
    # random acceptance becomes less likely
  }
  #print(best_score)
  ret <- tibble(
                spacing = best_scorings$spacing,
                mean_infit = best_scorings$mean_infit,
                mean_outfit = best_scorings$mean_outfit,
                var_disc_2PL = best_scorings$var_disc_2PL,
                disc_penalty = best_scorings$disc_penalty,
                best_subset = list(best_subset),
                best_score = best_score)
  
  return(ret)
}
