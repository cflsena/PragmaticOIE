getTransitiveInference <- function(pattern)
{
  tran_pattern <- getLemmas(pattern)
  status <- FALSE
  
  if (grepl(IS_A, tran_pattern))
    status <- TRUE
  else if (grepl(PART_OF, tran_pattern) & grepl(SIN, tran_pattern))
    status <- TRUE
  else if (grepl(LOC, tran_pattern) & grepl(PART_OF, tran_pattern))
    status <- TRUE
  else if (grepl(LOC, tran_pattern) & grepl(SIN, tran_pattern))
    status <- TRUE
  
  status
}

getSymmetricalInference <- function(pattern)
{
  sym_pattern <- getLemmas(pattern)
  status <- 0
  
  if (grepl(SYM_3, sym_pattern))
    status <- 2
  else if (grepl(SYM_1, sym_pattern))
    status <- 2
  else if (grepl(SYM_2, sym_pattern))
    status <- 2
  else if (grepl(SYM_4, sym_pattern))
    status <- 1
  
  status
}

getInferencialTriples <- function(triples, sentence_infos)
{
  output <- NULL
  
  sym_left <- c()
  sym_right <- c()
  sym_rel <- c()
  sym_left_con <- list()
  sym_right_con <- list()
  
  tran_left <- c()
  tran_right <- c()
  tran_rel <- c()
  tran_left_con <- list()
  tran_right_con <- list()
  
  sym_inf <- 0
  tran_inf <- 0
  
  left_arg <- triples$left_arg
  relations <- triples$relations
  indexes <- triples$indexes
  right_arg <- triples$right_arg
  left_con <- triples$index_context_arg1
  right_con <- triples$index_context_arg2
  
  #symmetrical inference
  sym_pos <- 1
  for (iCount in 1:length(relations)) {
    for (jCount in 1:length(left_arg[[iCount]])) {
      for (kCount in 1:length(right_arg[[iCount]])) {
        pattern <-
          getLemmas(paste0(left_arg[[iCount]][jCount], relations[iCount], right_arg[[iCount]][kCount]))
        status <- getSymmetricalInference(pattern)
        if (status == 1) {
          sym_inf <- 1
          sym_left[sym_pos] <- right_arg[[iCount]][kCount]
          sym_rel[sym_pos] <- relations[iCount]
          sym_right[sym_pos] <- left_arg[[iCount]][jCount]
          sym_left_con[[sym_pos]] <- left_con[[iCount]][[1]]
          sym_right_con[[sym_pos]] <- right_con[[iCount]][[1]]
          sym_pos <- sym_pos + 1
        } else if (status == 2) {
          sym_inf <- 1
          splited_rel <- str_split(relations[iCount], " ")[[1]]
          if (length(splited_rel) > 2) {
            aux <-
              paste0(sentence_infos[[1]][(min(indexes[[iCount]]) + 1):(max(indexes[[iCount]]))], collapse = "")
            sym_left[sym_pos] <-
              paste0(aux, " ", right_arg[[iCount]][kCount])
            sym_rel[sym_pos] <-
              sentence_infos[[1]][min(indexes[[iCount]])]
            sym_right[sym_pos] <- left_arg[[iCount]][jCount]
            sym_left_con[[sym_pos]] <- left_con[[iCount]][[1]]
            sym_right_con[[sym_pos]] <- right_con[[iCount]][[1]]
          } else {
            sym_left[sym_pos] <- right_arg[[iCount]][kCount]
            sym_rel[sym_pos] <- relations[iCount]
            sym_right[sym_pos] <- left_arg[[iCount]][jCount]
            sym_left_con[[sym_pos]] <- left_con[[iCount]][[1]]
            sym_right_con[[sym_pos]] <- right_con[[iCount]][[1]]
          }
          sym_pos <- sym_pos + 1
        }
      }
    }
  }
  
  #transitive inference
  if (length(relations) >= 2)
  {
    tran_pos <- 1
    for (iCount in 1:(length(relations) - 1)) {
      for (jCount in iCount:length(relations)) {
        for (kCount in 1:length(left_arg[[iCount]])) {
          for (wCount in 1:length(right_arg[[iCount]])) {
            if (iCount != jCount) {
              rep_arg <-
                str_split(right_arg[[iCount]][wCount], " ")[[1]] %in% str_split(left_arg[[jCount]][kCount], " ")[[1]]
              if (length(rep_arg[rep_arg != T]) == 0) {
                pattern <-
                  paste0(left_arg[[iCount]][kCount],
                         relations[iCount],
                         left_arg[[jCount]][kCount],
                         relations[jCount],
                         right_arg[[jCount]][wCount])
                status <- getTransitiveInference(pattern)
                if (status) {
                  tran_inf <- 1
                  for (hCount in 1:length(left_arg[[iCount]])) {
                    for (lCount in 1:length(right_arg[[jCount]])) {
                      tran_left[tran_pos] <- left_arg[[iCount]][hCount]
                      tran_rel[tran_pos] <- relations[jCount]
                      tran_right[tran_pos] <-
                        right_arg[[jCount]][lCount]
                      tran_left_con[[tran_pos]] <-
                        left_con[[jCount]][[1]]
                      tran_right_con[[tran_pos]] <-
                        right_con[[jCount]][[1]]
                      tran_pos <- tran_pos + 1
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  
  if (sym_inf) {
    aux_sym_output <-
      clearInfTriples(sym_left, sym_rel, sym_right, sym_left_con, sym_right_con)
    output$left_sym_arg <- aux_sym_output$left_arg
    output$sym_rel <- aux_sym_output$relations
    output$right_sym_arg <- aux_sym_output$right_arg
    output$left_sym_con <- aux_sym_output$left_context
    output$right_sym_con <- aux_sym_output$right_context
    output$sym_status <- aux_sym_output$status
  } else
    output$sym_status <- sym_inf
  
  if (tran_inf) {
    aux_tran_output <-
      clearInfTriples(tran_left,
                      tran_rel,
                      tran_right,
                      tran_left_con,
                      tran_right_con)
    output$left_tran_arg <- aux_tran_output$left_arg
    output$tran_rel <- aux_tran_output$relations
    output$right_tran_arg <- aux_tran_output$right_arg
    output$left_tran_con <- aux_tran_output$left_context
    output$right_tran_con <- aux_tran_output$right_context
    output$tran_status <- aux_tran_output$status
  } else
    output$tran_status <- tran_inf
  
  output
}