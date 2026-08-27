#' TODO
#' 
#' TODO
#' @param participants TODO
#' @param tournament_type TODO
#' @param tournament_phase TODO
#' @param options TODO
#' @param progress TODO
#' @param ... TODO
#' @returns TODO
#' @examples
#' participants <-
#'   dplyr::tibble(
#'     PARTICIPANT_ID = 5L:9L,
#'     QUALIFICATION_RATE = c(0.3, 0.5, 1, 1.3, 1.5)
#'   )
#' opts <- list(
#'   nsim = 100,
#'   revolutions = 2,
#'   weights = list(
#'     teamup         = 1,
#'     opposing       = 1,
#'     match_var      = 1,
#'     match_bal      = 1,
#'     match_bal_var  = 1,
#'     match_bal_extr = 1,
#'     part_bal       = 1,
#'     part_bal_var   = 1,
#'     part_bal_extr  = 1
#'   ),
#'   seed = 0
#' )
#' 
#' foosball3_generate_matches(
#'   participants, options = opts)
#' @export
foosball3_generate_matches <- function(
    participants, tournament_type = "individual", tournament_phase = "qualification", options, progress, ...) {
  switch(
    tournament_type,
    individual = {
      switch(
        tournament_phase,
        qualification = {
          matches <- .generateQualificationMatches(
            options$nsim, participants, options$revolutions, options$weights, progress, options$seed
          )
          data.frame(
            MATCH_ID       = seq_len(nrow(matches)),
            PARTICIPANT_ID = as.integer(participants$PARTICIPANT_ID[c(matches)]),
            POSITION_CODE  = c(rep("S1", nrow(matches)), rep("D1", nrow(matches)),
                               rep("D2", nrow(matches)), rep("S2", nrow(matches)))
          )
        },
        stop("Tournament phase not implemented")
      )
    },
    stop("Tournament type not implemented")
  )
  
}

.generateQualificationMatches <- function(nsim, participants, nfact, weights, progressbar, seed = NULL) {
  if (is.na(seed)) seed <- NULL
  set.seed(seed)
  p1 <- p2 <- p3 <- p4 <- NULL ## satisfy CHECK
  npart           <- nrow(participants)
  unique_quartets <- expand.grid(p1 = seq_len(npart),
                                 p2 = seq_len(npart),
                                 p3 = seq_len(npart),
                                 p4 = seq_len(npart))
  unique_quartets <- unique_quartets[unlist(apply(unique_quartets, 1, function(x) !any(duplicated(x)))),,drop=F]
  tournaments     <- array(NA, c(npart*nfact, 4, nsim))
  
  
  for (i in seq_len(npart*nfact)) {
    attempts <- 0
    ## pick random quartets:
    samples_to_set <- seq_len(nsim)
    proc_time <- proc.time()
    if (!missing(progressbar) && i %% 2) {
      progressbar(i/(npart*nfact))
    }
    repeat {
      attempts <- attempts + 1
      tournaments[i,,samples_to_set] <- t(unique_quartets[sample.int(nrow(unique_quartets), length(samples_to_set), T),,drop=F])
      ## test if selected match is ok
      reject <- unlist(lapply(samples_to_set, function(j) {
        check <- unlist(apply(tournaments[,,j], 2, table, simplify = F))
        any(check > nfact)
      }))
      samples_to_set <- samples_to_set[reject]
      if (length(samples_to_set) == 0 || attempts > 10) break
    }
    if (length(samples_to_set) > 0) {
      for (j in samples_to_set) {
        tour <- tournaments[-i,,j]
        saturated <- lapply(apply(tour, 2, table, simplify = F), function(x) {
          names(x)[x>= nfact]
        })
        valid <- subset(unique_quartets,
                        !((as.character(p1) %in% saturated[[1]]) |
                            (as.character(p2) %in% saturated[[2]]) |
                            (as.character(p3) %in% saturated[[3]]) |
                            (as.character(p4) %in% saturated[[4]])))
        if (nrow(valid) == 0) {
          ## nothing valid left:
          tournaments[i,,j] <- NA
        } else {
          tournaments[i,,j] <- unlist(valid[sample.int(nrow(valid), 1),,])
        }
      }
    }
  }
  invalid <- apply(tournaments,3,function(x) any(is.na(x)))
  tournaments <- tournaments[,,!invalid]
  ## Predicted likelihood that players 1 and 2 will win from 3 and 4.
  ## Note that the mean of the predictions per tournament is always 0.5, because all players play at all possible permutations with the same frequency
  prediction <- do.call(rbind, lapply(seq_len(dim(tournaments)[3]), function(i) {
    temp <- apply(tournaments[,,i], 2, function(j) participants$QUALIFICATION_RATE[j])
    temp[,3:4] <- 1 - temp[,3:4]
    rowMeans(temp)
  }))
  participant_prediction <- do.call(rbind, lapply(1:npart, function(i) {
    unlist(lapply(1:dim(tournaments)[3], function(j) {
      matches <- do.call(cbind, apply(tournaments[,,j] == i, 2, which, simplify = F))
      mean(c(prediction[j,c(matches[,1:2])], 1 - prediction[j,c(matches[,3:4])]))
    }))
  }))
  score <- do.call(rbind, lapply(seq_len(dim(tournaments)[3]), function(i) {
    data.frame(
      teamup_score = { ## High is unfavourable
        teamup_freq <- table(table(as.data.frame(tournaments[,1:2,i])) + table(as.data.frame(tournaments[,3:4,i])))
        sum(teamup_freq^(seq_along(teamup_freq) - 1))
      },
      oppose_score = { ## High is unfavourable
        oppose_freq <- table(table(as.data.frame(tournaments[,c(1,4),i])) + table(as.data.frame(tournaments[,2:3,i])))
        sum(oppose_freq^(seq_along(oppose_freq) - 1))
      },
      match_similarity_score = { ## High is unfavourable
        participants_ordered <- t(apply(tournaments[,,i], 1, sort))
        sum(unlist(lapply(2:4, function(j) { sum(duplicated(participants_ordered[,1:j]))}))^(1:3))
      }
    )
  }))
  score <- cbind(
    score,
    mean_balance           = abs(rowMeans(prediction) - 0.5),                       ## Mean success rate preferably close to 0.5
    match_balance_variance = apply(prediction, 1, stats::sd),                       ## Preferably as little variation as possible
    match_balance_extremes = apply(prediction, 1, function(x) {max(abs(x - 0.5))}), ## Get the most unbalances match in the tournament (preferably as small as possible)
    part_balance           = abs(colMeans(participant_prediction) - 0.5),
    part_balance_variance  = apply(participant_prediction, 2, stats::sd),
    part_balance_extremes = apply(participant_prediction, 2, function(x) {max(abs(x - 0.5))})
  )
  score_rank <- apply(score, 2, function(x) x/max(x))
  score_rank [is.nan(score_rank )] <- 0
  score_rank <- as.data.frame(apply(score_rank , 2, rank, ties.method = "average"))
  score_rank[c("teamup_score", "oppose_score", "match_similarity_score", "mean_balance", "match_balance_variance",
               "match_balance_extremes", "part_balance", "part_balance_variance", "part_balance_extremes")] <-
    score_rank[c("teamup_score", "oppose_score", "match_similarity_score", "mean_balance", "match_balance_variance",
                 "match_balance_extremes", "part_balance", "part_balance_variance", "part_balance_extremes")] *
    as.data.frame(weights[c("teamup", "opposing", "match_var", "match_bal", "match_bal_var",
                            "match_bal_extr", "part_bal", "part_bal_var", "part_bal_extr")])[rep(1, nrow(score_rank)),]
  score_rank$rank_mean <- apply(score_rank, 1, mean)
  score_rank$rank      <- rank(score_rank$rank_mean, ties.method = "min")
  return(tournaments[,,which(score_rank$rank == 1)[[1]]])
}