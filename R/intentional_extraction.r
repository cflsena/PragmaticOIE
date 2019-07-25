getIntentionalTriples <- function(normal_triples, inference_triples)
{
  output <- NULL
  
  aux <-
    intentionExtraction(
      normal_triples$left_arg,
      normal_triples$relations,
      normal_triples$right_arg,
      normal_triples$left_context,
      normal_triples$right_context
    )
  
  output$normal_left_arg <- aux$left
  output$normal_relations <- aux$relations
  output$normal_right_arg <- aux$right
  output$normal_left_con <- aux$left_con
  output$normal_right_con <- aux$right_con
  
  if (length(inference_triples$left_sym_arg) > 0) {
    aux_sym <-
      intentionExtraction(
        inference_triples$left_sym_arg,
        inference_triples$rel_sym,
        inference_triples$right_sym_arg,
        inference_triples$left_sym_context,
        inference_triples$right_sym_context
      )
    output$left_sym_arg <- aux_sym$left
    output$rel_sym <- aux_sym$relations
    output$right_sym_arg <- aux_sym$right
    output$left_sym_con <- aux_sym$left_con
    output$right_sym_con <- aux_sym$right_con
  }
  
  if (length(inference_triples$left_tran_arg) > 0) {
    aux_tran <-
      intentionExtraction(
        inference_triples$left_tran_arg,
        inference_triples$rel_tran,
        inference_triples$right_tran_arg,
        inference_triples$left_tran_context,
        inference_triples$right_tran_context
      )
    output$left_tran_arg <- aux_tran$left
    output$rel_tran <- aux_tran$relations
    output$right_tran_arg <- aux_tran$right
    output$left_tran_con <- aux_tran$left_con
    output$right_tran_con <- aux_tran$right_con
  }
  
  output
}

intentionExtraction <-
  function(left_arg,
           relations,
           right_arg,
           left_context,
           right_context)
  {
    output <- NULL
    left <- list()
    left_con <- list()
    int_rel <- list()
    right <- list()
    right_con <- list()
    
    if (!is.null(left_arg) &
        !is.null(relations) & !is.null(right_arg)) {
      for (iCount in 1:length(relations)) {
        flag <- 0
        
        split_rel <- strsplit(relations[iCount], " ")[[1]]
        split_rel <- split_rel[split_rel != ""]
        tk_rel <- split_rel[1]
        
        postag_rel <- getPostags(relations[iCount])
        split_postag_rel <- strsplit(postag_rel, " ")[[1]]
        tk_postag_rel <- split_postag_rel[1]
        
        if (!grepl("v-", tk_postag_rel)) {
          split_postag_rel <- split_postag_rel[-1]
          tk_postag_rel <- split_postag_rel[1]
          split_rel <- split_rel[-1]
          tk_rel <- split_rel[1]
          flag <- 1
        }
        
        int_verb <- getFeatures(tk_rel)
        
        if (grepl("COND", int_verb)) {
          int_pattern <- ""
          for (jCount in 1:length(split_postag_rel)) {
            if (grepl("v-", split_postag_rel[jCount]))
              int_pattern <-
                paste0(int_pattern, split_postag_rel[jCount], " ")
            else
              break
          }
          
          if (grepl(INT_1, int_pattern)) {
            if (flag) {
              if (grepl("COND=3S", int_verb) || grepl("COND=1S", int_verb)) {
                if (length(split_rel) > 3)
                  int_rel[iCount] <-
                    paste0(
                      wordStem(split_rel[3], language = "portuguese"),
                      "ou ",
                      paste0(split_rel[4:length(split_rel)], collapse = " ")
                    )
                else
                  int_rel[iCount] <-
                    paste0(wordStem(split_rel[3], language = "portuguese"),
                           "ou ")
              } else if (grepl("COND=3P", int_verb)) {
                if (length(split_rel) > 3)
                  int_rel[iCount] <-
                    paste0(
                      wordStem(split_rel[3], language = "portuguese"),
                      "aram ",
                      paste0(split_rel[4:length(split_rel)], collapse = " ")
                    )
                else
                  int_rel[iCount] <-
                    paste0(wordStem(split_rel[3], language = "portuguese"),
                           "aram ")
              }
            } else{
              if (grepl("COND=3S", int_verb) || grepl("COND=1S", int_verb)) {
                if (length(split_rel) > 3) {
                  int_rel[iCount] <-
                    paste0(
                      "não ",
                      wordStem(split_rel[3], language = "portuguese"),
                      "ou ",
                      paste0(split_rel[4:length(split_rel)], collapse = " ")
                    )
                  int_rel[[iCount]][2] <-
                    paste0(
                      "ainda não ",
                      wordStem(split_rel[3], language = "portuguese"),
                      "ou ",
                      paste0(split_rel[4:length(split_rel)], collapse = " ")
                    )
                } else{
                  int_rel[iCount] <-
                    paste0("não ",
                           wordStem(split_rel[3], language = "portuguese"),
                           "ou ")
                  int_rel[[iCount]][2] <-
                    paste0("ainda não ",
                           wordStem(split_rel[3], language = "portuguese"),
                           "ou ")
                }
              } else if (grepl("COND=3P", int_verb)) {
                if (length(split_rel) > 3) {
                  int_rel[iCount] <-
                    paste0(
                      "não ",
                      wordStem(split_rel[3], language = "portuguese"),
                      "aram ",
                      paste0(split_rel[4:length(split_rel)], collapse = " ")
                    )
                  int_rel[[iCount]] <-
                    paste0(
                      "ainda não ",
                      wordStem(split_rel[3], language = "portuguese"),
                      "aram ",
                      paste0(split_rel[4:length(split_rel)], collapse = " ")
                    )
                } else{
                  int_rel[iCount] <-
                    paste0("não ",
                           wordStem(split_rel[3], language = "portuguese"),
                           "aram ")
                  int_rel[[iCount]][2] <-
                    paste0("ainda não ",
                           wordStem(split_rel[3], language = "portuguese"),
                           "aram ")
                }
              }
            }
          } else if (grepl(INT_2, int_pattern)) {
            if (flag) {
              if (grepl("COND=3S", int_verb) || grepl("COND=1S", int_verb)) {
                if (length(split_rel) > 2)
                  int_rel[iCount] <-
                    paste0(
                      wordStem(split_rel[2], language = "portuguese"),
                      "ou ",
                      paste0(split_rel[3:length(split_rel)], collapse = " ")
                    )
                else
                  int_rel[iCount] <-
                    paste0(wordStem(split_rel[2], language = "portuguese"),
                           "ou ")
              } else if (grepl("COND=3P", int_verb)) {
                if (length(split_rel) > 2)
                  int_rel[iCount] <-
                    paste0(
                      wordStem(split_rel[2], language = "portuguese"),
                      "aram ",
                      paste0(split_rel[3:length(split_rel)], collapse = " ")
                    )
                else
                  int_rel[iCount] <-
                    paste0(wordStem(split_rel[2], language = "portuguese"),
                           "aram ")
              }
            } else{
              if (grepl("COND=3S", int_verb) || grepl("COND=1S", int_verb)) {
                if (length(split_rel) > 2) {
                  int_rel[iCount] <-
                    paste0(
                      "não ",
                      wordStem(split_rel[2], language = "portuguese"),
                      "ou ",
                      paste0(split_rel[3:length(split_rel)], collapse = " ")
                    )
                  int_rel[[iCount]][2] <-
                    paste0(
                      "ainda não ",
                      wordStem(split_rel[2], language = "portuguese"),
                      "ou ",
                      paste0(split_rel[3:length(split_rel)], collapse = " ")
                    )
                } else{
                  int_rel[iCount] <-
                    paste0("não ",
                           wordStem(split_rel[2], language = "portuguese"),
                           "ou ")
                  int_rel[[iCount]][2] <-
                    paste0("ainda não ",
                           wordStem(split_rel[2], language = "portuguese"),
                           "ou ")
                }
              } else if (grepl("COND=3P", int_verb)) {
                if (length(split_rel) > 2) {
                  int_rel[iCount] <-
                    paste0(
                      "não ",
                      wordStem(split_rel[2], language = "portuguese"),
                      "aram ",
                      paste0(split_rel[3:length(split_rel)], collapse = " ")
                    )
                  int_rel[[iCount]][2] <-
                    paste0(
                      "ainda não ",
                      wordStem(split_rel[2], language = "portuguese"),
                      "aram ",
                      paste0(split_rel[3:length(split_rel)], collapse = " ")
                    )
                } else{
                  int_rel[iCount] <-
                    paste0("não ",
                           wordStem(split_rel[2], language = "portuguese"),
                           "aram ")
                  int_rel[[iCount]][2] <-
                    paste0("ainda não ",
                           wordStem(split_rel[2], language = "portuguese"),
                           "aram ")
                }
              }
            }
          } else{
            if (flag) {
              if (grepl("COND=3S", int_verb) || grepl("COND=1S", int_verb)) {
                if (length(split_rel) > 1)
                  int_rel[iCount] <-
                    paste0(
                      wordStem(split_rel[1], language = "portuguese"),
                      "ou ",
                      paste0(split_rel[2:length(split_rel)], collapse = " ")
                    )
                else
                  int_rel[iCount] <-
                    paste0(wordStem(split_rel[1], language = "portuguese"),
                           "ou ")
              } else if (grepl("COND=3P", int_verb)) {
                if (length(split_rel) > 1)
                  int_rel[iCount] <-
                    paste0(
                      wordStem(split_rel[1], language = "portuguese"),
                      "aram ",
                      paste0(split_rel[2:length(split_rel)], collapse = " ")
                    )
                else
                  int_rel[iCount] <-
                    paste0(wordStem(split_rel[1], language = "portuguese"),
                           "aram ")
              }
            } else{
              if (grepl("COND=3S", int_verb) || grepl("COND=1S", int_verb)) {
                if (length(split_rel) > 1) {
                  int_rel[iCount] <-
                    paste0(
                      "não ",
                      wordStem(split_rel[1], language = "portuguese"),
                      "ou ",
                      paste0(split_rel[2:length(split_rel)], collapse = " ")
                    )
                  int_rel[[iCount]][2] <-
                    paste0(
                      "ainda não ",
                      wordStem(split_rel[1], language = "portuguese"),
                      "ou ",
                      paste0(split_rel[2:length(split_rel)], collapse = " ")
                    )
                } else{
                  int_rel[iCount] <-
                    paste0("não ",
                           wordStem(split_rel[1], language = "portuguese"),
                           "ou ")
                  int_rel[[iCount]][2] <-
                    paste0("ainda não ",
                           wordStem(split_rel[1], language = "portuguese"),
                           "ou ")
                }
              } else if (grepl("COND=3P", int_verb)) {
                if (length(split_rel) > 1) {
                  int_rel[iCount] <-
                    paste0(
                      "não ",
                      wordStem(split_rel[1], language = "portuguese"),
                      "aram ",
                      paste0(split_rel[2:length(split_rel)], collapse = " ")
                    )
                  int_rel[[iCount]][2] <-
                    paste0(
                      "ainda não ",
                      wordStem(split_rel[1], language = "portuguese"),
                      "aram ",
                      paste0(split_rel[2:length(split_rel)], collapse = " ")
                    )
                } else{
                  int_rel[iCount] <-
                    paste0("não ",
                           wordStem(split_rel[1], language = "portuguese"),
                           "aram ")
                  int_rel[[iCount]][2] <-
                    paste0("ainda não ",
                           wordStem(split_rel[1], language = "portuguese"),
                           "aram ")
                }
              }
            }
          }
          
          left[iCount] <- left_arg[iCount]
          right[iCount] <- right_arg[iCount]
          
          if (!is.null(left_context[[iCount]]))
            left_con <- intentionalContext(left_context[[iCount]])
          else
            left_con[iCount] <- NULL
          
          if (!is.null(right_context[[iCount]]))
            right_con <- intentionalContext(right_context[[iCount]])
          else
            right_con[iCount] <- NULL
          
        }
        
      }
    }
    
    if (length(int_rel) > 0) {
      aux_left <- NULL
      aux_int_rel <- NULL
      aux_right <- NULL
      aux_con_left <- NULL
      aux_con_right <- NULL
      pos <- 1
      for (iCount in 1:length(int_rel)) {
        for (jCount in 1:length(int_rel[[iCount]])) {
          if (!is.na(int_rel[iCount]) &
              !is.null(left[[iCount]]) & !is.null(right[[iCount]])) {
            aux_int_rel[pos] <- int_rel[[iCount]][jCount]
            aux_left[pos] <- left[iCount]
            aux_right[pos] <- right[iCount]
            aux_con_left[pos] <- left_con[iCount]
            aux_con_right[pos] <- right_con[iCount]
            pos <- pos + 1
          }
        }
      }
      if (!is.null(aux_left) &
          !is.null(aux_int_rel) & !is.null(aux_right)) {
        left <- aux_left
        int_rel <- aux_int_rel
        right <- aux_right
        left_con <- aux_con_left
        right_con <- aux_con_right
      }
    } else{
      left <- NULL
      int_rel <- NULL
      right <- NULL
      left_con <- NULL
      right_con <- NULL
    }
    
    output$left <- left
    output$relations <- int_rel
    output$right <- right
    output$left_con <- left_con
    output$right_con <- right_con
    output
  }

intentionalContext <- function(context) {
  output <- c()
  
  context <- str_trim(context, "left")
  splited_tokens <- str_split(context, " ")[[1]]
  splited_posTags <- str_split(getPostags(context), " ")[[1]]
  
  if (length(splited_posTags) != length(splited_tokens))
    splited_posTags <- clearPostags(splited_posTags, splited_tokens)
  
  index_conj <- grep("conj-c", splited_posTags)
  indexes_verbs <- grep("v-", splited_posTags)
  
  if (grepl(
    paste0(
      "(",
      SUBORD_CONJ_COMPARATIVE,
      CONTEXT_INTENT,
      SUBORD_CONJ_CONCESSIVE,
      SUBORD_CONJ_PROPORTIONAL,
      SUBORD_CONJ_TEMPORAL,
      ")"
    ),
    paste0(" ", tolower(str_split(context, " ")[[1]][1]), " ")
  ))
  output <- context
  else if (grepl(SUBORD_CONJ_CAUSAL, paste0(" ", tolower(str_split(context, " ")[[1]][1]), " "))) {
    if (length(index_conj))
      output <-
        intentionalInferenceConjc(index_conj,
                                  indexes_verbs,
                                  splited_tokens,
                                  splited_posTags)
    else
      output <-
        intentionalInferenceGeneral(indexes_verbs[1], splited_tokens)
  } else if (grepl(SUBORD_CONJ_CONDITIONAL, paste0(" ", tolower(str_split(context, " ")[[1]][1]), " "))) {
    if (length(index_conj)) {
      output <-
        intentionalInferenceCond(indexes_verbs, splited_tokens, index_conj)
      
      splited_tokens <- str_split(output, " ")[[1]]
      splited_posTags <- str_split(getPostags(output), " ")[[1]]
      
      if (length(splited_posTags) != length(splited_tokens))
        splited_posTags <-
        clearPostags(splited_posTags, splited_tokens)
      
      index_conj <- grep("conj-c", splited_posTags)
      indexes_verbs <- grep("v-", splited_posTags)
      
      output <-
        intentionalInferenceConjc(index_conj,
                                  indexes_verbs,
                                  splited_tokens,
                                  splited_posTags)
    } else
      output <-
        intentionalInferenceGeneral(indexes_verbs[1], splited_tokens)
  }
  
  output
}

intentionalInferenceConjc <-
  function(index_conj,
           indexes_verbs,
           splited_tokens,
           splited_posTags) {
    part1 <- c()
    part2 <- c()
    part3 <- c()
    output <- c()
    
    for (iPos in 1:length(index_conj)) {
      if (iPos == 1)
      {
        index_neg <- indexes_verbs[indexes_verbs < index_conj[iPos]][1] - 1
        if (grepl("não", splited_tokens[index_neg])) {
          part1 <- paste0(splited_tokens[1:(index_neg - 1)], collapse = " ")
          part2 <-
            paste0(splited_tokens[(index_neg + 1):(index_conj[iPos] - 1)], collapse = " ")
        } else {
          part1 <- paste0(splited_tokens[1:(index_neg)], collapse = " ")
          part2 <-
            paste0("não ", paste0(splited_tokens[(index_neg + 1):(index_conj[iPos] -
                                                                    1)], collapse = " "))
        }
        
        if (grepl("não", splited_tokens[index_conj[iPos] + 1])) {
          if (!is.na(index_conj[iPos + 1]))
            part3 <-
              paste0("ou ", paste0(splited_tokens[(index_conj[iPos] + 2):(index_conj[iPos +
                                                                                       1] - 1)], collapse = " "))
          else
            part3 <-
              paste0("ou ", paste0(splited_tokens[(index_conj[iPos] + 2):(length(splited_tokens))], collapse = " "))
        } else {
          if (!is.na(index_conj[iPos + 1])) {
            if (grepl("v-", splited_posTags[index_conj[iPos] + 1]))
              part3 <-
                paste0("ou não ", paste0(splited_tokens[(index_conj[iPos] + 1):(index_conj[iPos +
                                                                                             1] - 1)], collapse = " "))
            else
              part3 <-
                paste0("ou não ",
                       splited_tokens[index_neg + 1],
                       " ",
                       paste0(splited_tokens[(index_conj[iPos] + 1):(index_conj[iPos + 1] - 1)], collapse = " "))
          } else{
            if (grepl("v-", splited_posTags[index_conj[iPos] + 1]))
              part3 <-
                paste0("ou não ", paste0(splited_tokens[(index_conj[iPos] + 1):(length(splited_tokens))], collapse = " "))
            else
              part3 <-
                paste0("ou não ",
                       splited_tokens[index_neg + 1],
                       " ",
                       paste0(splited_tokens[(index_conj[iPos] + 1):(length(splited_tokens))], collapse = " "))
          }
        }
        output[iPos] <- paste0(part1, " ", part2, " ", part3)
      } else {
        index_neg <- indexes_verbs[indexes_verbs > index_conj[iPos]][1] - 1
        if (!is.na(index_neg)) {
          if (grepl("não", splited_tokens[index_neg])) {
            if (!is.na(index_conj[iPos + 1]))
              output[iPos] <-
                paste0("ou ", paste0(splited_tokens[(index_neg + 1):(index_conj[iPos + 1] -
                                                                       1)], collapse = " "))
            else
              output[iPos] <-
                paste0("ou ", paste0(splited_tokens[(index_neg + 1):length(splited_tokens)], collapse = " "))
          }
        } else {
          index_neg <- indexes_verbs[indexes_verbs > index_conj[iPos - 1]][1] - 1
          
          for (iCount in length(index_conj):2) {
            index_neg <- indexes_verbs[indexes_verbs > index_conj[iCount - 1]][1] -
              1
            if (!is.na(index_neg))
              break
          }
          
          if (!is.na(index_conj[iPos + 1]))
            output[iPos] <-
              paste0("ou não ",
                     splited_tokens[index_neg + 1],
                     " ",
                     paste0(splited_tokens[(index_conj[iPos] + 1):(index_conj[iPos + 1] - 1)], collapse = " "))
          else
            output[iPos] <-
              paste0("ou não ",
                     splited_tokens[index_neg + 1],
                     " ",
                     paste0(splited_tokens[(index_conj[iPos] + 1):length(splited_tokens)], collapse = " "))
        }
      }
    }
    
    output <- paste0(output, collapse = " ")
    output
  }

intentionalInferenceGeneral <- function(index_verb, splited_tokens) {
  output <- c()
  
  if (grepl("não", splited_tokens[index_verb - 1]))
    output <- paste0(splited_tokens[-(index_verb - 1)], collapse = " ")
  else
    output <-
      paste0(append(splited_tokens, "não", index_verb - 1), collapse = " ")
  
  output
}

intentionalInferenceCond <-
  function(indexes_verbs,
           splited_tokens,
           index_conj) {
    output <- c()
    
    splited_tokens[1] <- "pois"
    
    for (iCount in 1:length(indexes_verbs))
      splited_tokens[indexes_verbs[iCount]] <-
      paste0(wordStem(splited_tokens[indexes_verbs[iCount]], language = "portuguese"),
             "ou",
             collapse = " ")
    
    if (length(index_conj) == 0) {
      if (grepl("não", splited_tokens[index_verb - 1]))
        output <-
          paste0(splited_tokens[-(index_verb - 1)], collapse = " ")
      else
        output <-
          paste0(append(splited_tokens, "não", index_verb - 1), collapse = " ")
    } else
      output <- paste0(splited_tokens, collapse = " ")
    
    output
  }

clearPostags <- function(splited_posTags, splited_tokens) {
  num_tags <- grep("prp", splited_posTags)
  
  for (iCount in 1:length(num_tags))
  {
    if (length(grepl("art", splited_posTags[num_tags[iCount] + 1])))
      splited_posTags <- splited_posTags[-(num_tags[iCount] + 1)]
  }
  splited_posTags
}