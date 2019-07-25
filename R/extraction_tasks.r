sepInfo <- function(info)
{
  result <- strsplit(info, ", ")
  
  for (iCount in 2:length(result[[1]])) {
    result[[1]][iCount] <- paste("", result[[1]][iCount])
    result[[2]][iCount] <- paste("", result[[2]][iCount])
    result[[3]][iCount] <- paste("", result[[3]][iCount])
  }
  result
}

getIndex <- function(sentence_infos, pattern_info, pos_info)
{
  index <- grep(pattern_info, sentence_infos[[pos_info]], fixed = T)
  index
}

getPattern <-
  function(sentence_infos,
           pos_info,
           left_limit,
           right_limit)
  {
    result <-
      as.String(sentence_infos[[pos_info]][left_limit:right_limit])
    result <- gsub("\n", "", result, fixed = F)
    result <- paste0(result, " ")
    result <- str_trim(result, "left")
    result
  }

getPatternArg <- function(sentence_infos, pos_info, interval)
{
  if (length(interval) == 2)
    if (interval[1] == interval[2])
      interval <- interval[1]
    
    result <- as.String(sentence_infos[[pos_info]][interval])
    result <- gsub("\n", "", result, fixed = F)
    result <- paste0(result, " ")
    result <- str_trim(result, "left")
    result
}

getLeftInterval <- function (sentence_infos, relations, pos_info)
{
  index_arg1 <- list()
  index_context_arg1 <- list()
  output <- NULL
  pos_iCount <- 1
  
  for (iCount in 1:length(relations$relations))
  {
    index_token <- min(relations$indexes[[iCount]]) - 1
    token <- sentence_infos[[1]][index_token]
    token_tag <- sentence_infos[[2]][index_token]
    
    if (index_token == 0) {
      index_arg1[[pos_iCount]] <- list(0)
      pos_iCount <- pos_iCount + 1
      next
    } else if (grepl(" ,", token, fixed = T)) {
      aux_index_token <-
        grep(" ,", sentence_infos[[pos_info]][1:(index_token - 1)], fixed = T)
      if (length(aux_index_token) == 0) {
        index_token_initial_parenthesis <-
          grep(" (", sentence_infos[[pos_info]][1:(index_token - 1)], fixed = T)[1]
        if (is.na(index_token_initial_parenthesis)) {
          index_arg1[[pos_iCount]] <- list(c(1:(index_token - 1)))
          index_context_arg1[[pos_iCount]] <-
            list(c(1:(index_token - 1)))
        } else {
          index_token_final_parenthesis <-
            grep(" )", sentence_infos[[pos_info]][1:(index_token - 1)], fixed = T)
          index_token_final_parenthesis <-
            index_token_final_parenthesis[length(index_token_final_parenthesis)]
          if (length(index_token_final_parenthesis) == 0) {
            index_arg1[[pos_iCount]] <-
              list(c(1:(
                index_token_initial_parenthesis - 1
              )))
            index_context_arg1[[pos_iCount]] <-
              list(c(1:(
                index_token_initial_parenthesis - 1
              )))
          } else {
            index_arg1[[pos_iCount]] <-
              list(c(1:(
                index_token_initial_parenthesis - 1
              )))
            index_context_arg1[[pos_iCount]] <-
              list(c(1:(
                index_token_initial_parenthesis - 1
              )))
            if (!grepl("( ,| ;| :| .)", sentence_infos[[pos_info]][index_token_final_parenthesis +
                                                                   1], fixed = F)) {
              index_arg1[[pos_iCount]] <-
                list(c(
                  1:(index_token_initial_parenthesis - 1),
                  (index_token_final_parenthesis + 1):(index_token - 1)
                ))
              index_context_arg1[[pos_iCount]] <-
                list(c(
                  1:(index_token_initial_parenthesis - 1),
                  (index_token_final_parenthesis + 1):(index_token - 1)
                ))
              index_context_arg1[[pos_iCount]][2] <-
                list(c((index_token_initial_parenthesis + 1):(index_token_final_parenthesis -
                                                                1)
                ))
            }
          }
        }
      } else {
        index_token_initial_parenthesis <-
          grep(" (", sentence_infos[[pos_info]][1:(index_token - 1)], fixed = T)
        if (length(index_token_initial_parenthesis) == 0) {
          aux_index_token <- aux_index_token[1]
          index_arg1[[pos_iCount]] <-
            list(c(1:(aux_index_token - 1)))
          index_context_arg1[[pos_iCount]] <-
            list(c(1:(aux_index_token - 1)))
        } else {
          index_token_initial_parenthesis <-
            index_token_initial_parenthesis[1]
          aux_index_token <-
            grep(" ,", sentence_infos[[pos_info]][1:(index_token_initial_parenthesis -
                                                       1)], fixed = T)
          if (length(aux_index_token) == 0) {
            index_token_final_parenthesis <-
              grep(" )", sentence_infos[[pos_info]][1:(index_token - 1)], fixed = T)
            if (length(index_token_final_parenthesis) == 0) {
              index_arg1[[pos_iCount]] <-
                list(c(1:(
                  index_token_initial_parenthesis - 1
                )))
              index_context_arg1[[pos_iCount]] <-
                list(c(1:(
                  index_token_initial_parenthesis - 1
                )))
            } else {
              index_arg1[[pos_iCount]] <-
                list(c(1:(
                  index_token_initial_parenthesis - 1
                )))
              index_context_arg1[[pos_iCount]] <-
                list(c(1:(
                  index_token_initial_parenthesis - 1
                )))
              index_token_final_parenthesis <-
                index_token_final_parenthesis[length(index_token_final_parenthesis)]
              aux_index_token <-
                grep(" ,", sentence_infos[[pos_info]][(index_token_final_parenthesis + 1):(index_token -
                                                                                             1)], fixed = T)
              if (length(aux_index_token) == 0) {
                index_arg1[[pos_iCount]] <-
                  list(c(
                    1:(index_token_initial_parenthesis - 1),
                    (index_token_final_parenthesis + 1):(index_token - 1)
                  ))
                index_context_arg1[[pos_iCount]] <-
                  list(c(
                    1:(index_token_initial_parenthesis - 1),
                    (index_token_final_parenthesis + 1):(index_token - 1)
                  ))
              } else {
                if (!grepl("( \\,| \\;| \\:| \\.)",
                           sentence_infos[[pos_info]][index_token_final_parenthesis + 1],
                           fixed = F)) {
                  index_arg1[[pos_iCount]] <-
                    list(c(
                      1:(index_token_initial_parenthesis - 1),
                      (index_token_final_parenthesis + 1):((
                        index_token_final_parenthesis + aux_index_token[1]
                      ) - 1
                      )
                    ))
                  if (length(aux_index_token) > 1) {
                    index_context_arg1[[pos_iCount]] <-
                      list(c(
                        1:(index_token_initial_parenthesis - 1),
                        (index_token_final_parenthesis + 1):((
                          index_token_final_parenthesis + aux_index_token[1]
                        ) - 1
                        )
                      ))
                    index_context_arg1[[pos_iCount]][2] <-
                      list(c((index_token_initial_parenthesis + 1):(index_token_final_parenthesis -
                                                                      1)
                      ))
                    index_context_arg1[[pos_iCount]][3] <-
                      list(c((aux_index_token[1] + 1):(aux_index_token[2] - 1)
                      ))
                  } else {
                    index_context_arg1[[pos_iCount]] <-
                      list(c(
                        1:(index_token_initial_parenthesis - 1),
                        (index_token_final_parenthesis + 1):((
                          index_token_final_parenthesis + aux_index_token[1]
                        ) - 1
                        )
                      ))
                    index_context_arg1[[pos_iCount]][2] <-
                      list(c((index_token_initial_parenthesis + 1):(index_token_final_parenthesis -
                                                                      1)
                      ))
                  }
                }
              }
            }
          } else {
            index_arg1[[pos_iCount]] <- list(c(1:(aux_index_token[1] - 1)))
            if (length(aux_index_token) > 1) {
              index_context_arg1[[pos_iCount]] <-
                list(c(1:(aux_index_token[1] - 1)))
              index_context_arg1[[pos_iCount]][2] <-
                list(c((aux_index_token[1] + 1):(aux_index_token[2] - 1)))
            } else
              index_context_arg1[[pos_iCount]] <-
                list(c(1:(aux_index_token[1] - 1)))
          }
        }
      }
    } else if (grepl(" )", token, fixed = T)) {
      index_token_initial_parenthesis <-
        grep(" (", sentence_infos[[pos_info]][1:(index_token - 1)], fixed = T)
      if (length(index_token_initial_parenthesis) == 0) {
        index_token_comma <-
          grep(" ,", sentence_infos[[pos_info]][1:(index_token - 1)], fixed = T)
        if (length(index_token_comma) == 0) {
          index_arg1[[pos_iCount]] <- list(c(1:(index_token - 1)))
          index_context_arg1[[pos_iCount]] <-
            list(c(1:(index_token - 1)))
        } else {
          index_arg1[[pos_iCount]] <-
            list(c(1:(index_token_comma[length(index_token_comma)] - 1)))
          if (length(index_token_comma) > 1) {
            index_context_arg1[[pos_iCount]] <-
              list(c(1:(index_token_comma[length(index_token_comma)] - 1)))
            index_context_arg1[[pos_iCount]][2] <-
              list(c((index_token_comma[length(index_token_comma) - 1] + 1):(index_token_comma[length(index_token_comma)] -
                                                                               1)))
          } else
            index_context_arg1[[pos_iCount]] <-
              list(c(1:(index_token_comma[length(index_token_comma)] - 1)))
        }
      } else {
        index_token_initial_parenthesis <-
          index_token_initial_parenthesis[1]
        index_token_comma <-
          grep(" ,", sentence_infos[[pos_info]][1:(index_token_initial_parenthesis -
                                                     1)], fixed = T)
        if (length(index_token_comma) == 0) {
          index_arg1[[pos_iCount]] <-
            list(c(1:(
              index_token_initial_parenthesis - 1
            )))
          index_context_arg1[[pos_iCount]] <-
            list(c(1:(
              index_token_initial_parenthesis - 1
            )))
        } else {
          index_arg1[[pos_iCount]] <-
            list(c((index_token_comma[length(index_token_comma)] + 1):(index_token_initial_parenthesis -
                                                                         1)
            ))
          if (length(index_token_comma) > 1) {
            index_context_arg1[[pos_iCount]] <-
              list(c((index_token_comma[length(index_token_comma)] + 1):(index_token_initial_parenthesis -
                                                                           1)
              ))
            index_context_arg1[[pos_iCount]][2] <-
              list(c((index_token_comma[length(index_token_comma) - 1] + 1):(index_token_comma[length(index_token_comma)] -
                                                                               1)))
          } else
            index_context_arg1[[pos_iCount]] <-
              list(c((index_token_comma[length(index_token_comma)] + 1):(index_token_initial_parenthesis -
                                                                           1)
              ))
        }
      }
      
    } else if (grepl(" e| como| mas| porém| entretanto| contuto| todavia| no_entanto",
                     token,
                     fixed = F)) {
      if (pos_iCount != 1) {
        if (length(index_arg1[[pos_iCount - 1]]) &
            index_arg1[[pos_iCount - 1]][[1]][1] != 0) {
          index_arg1[[pos_iCount]] <- index_arg1[[pos_iCount - 1]]
          index_context_arg1[[pos_iCount]] <-
            index_arg1[[pos_iCount - 1]]
        } else {
          index_token_comma <-
            grep(" ,", sentence_infos[[pos_info]][1:(index_token - 1)], fixed = T)[1]
          if (is.na(index_token_comma)) {
            index_arg1[[pos_iCount]] <- list(c(1:(index_token - 1)))
            index_context_arg1[[pos_iCount]] <-
              list(c(1:(index_token - 1)))
          } else {
            index_token_comma <-
              grep(" ,", sentence_infos[[pos_info]][1:(index_token - 1)], fixed = T)
            index_arg1[[pos_iCount]] <-
              list(c(1:(index_token_comma[1] - 1)))
            if (length(index_token_comma) > 1) {
              index_context_arg1[[pos_iCount]] <-
                list(c(1:(index_token_comma - 1)))
              index_context_arg1[[pos_iCount]][2] <-
                list(c((index_token_comma[1] + 1):(index_token_comma[2] - 1)
                ))
            } else
              index_context_arg1[[pos_iCount]] <-
              list(c(1:(index_token_comma - 1)))
          }
        }
      } else {
        index_token_comma <-
          grep(" ,", sentence_infos[[pos_info]][1:(index_token - 1)], fixed = T)[1]
        if (is.na(index_token_comma)) {
          index_arg1[[pos_iCount]] <- list(c(1:(index_token - 1)))
          index_context_arg1[[pos_iCount]] <-
            list(c(1:(index_token - 1)))
        } else{
          index_arg1[[pos_iCount]] <- list(c(1:(index_token_comma - 1)))
          index_token_comma <-
            grep(" ,", sentence_infos[[pos_info]][1:(index_token - 1)], fixed = T)
          if (length(index_token_comma) > 1) {
            index_context_arg1[[pos_iCount]] <-
              list(c(1:(index_token_comma[1] - 1)))
            index_context_arg1[[pos_iCount]][2] <-
              list(c((index_token_comma[1] + 1):(index_token_comma[2] - 1)))
          } else
            index_context_arg1[[pos_iCount]] <-
            list(c(1:(index_token_comma[1] - 1)))
        }
      }
    } else if (grepl(" que| se", sentence_infos[[pos_info]][index_token], fixed = F)) {
      index_token_comma <-
        grep(" ,", sentence_infos[[pos_info]][1:(index_token - 1)], fixed = T)
      index_token_comma <-
        index_token_comma[length(index_token_comma)]
      index_token_final_parenthesis <-
        grep(" )", sentence_infos[[pos_info]][1:(index_token - 1)], fixed = T)
      index_token_final_parenthesis <-
        index_token_final_parenthesis[length(index_token_final_parenthesis)]
      
      if (length(index_token_comma) == 0 ||
          is.na(index_token_comma))
        index_token_comma <- 0
      if (length(index_token_final_parenthesis) == 0 ||
          is.na(index_token_final_parenthesis))
        index_token_final_parenthesis <- 0
      
      if (index_token_comma == 0 &
          index_token_final_parenthesis == 0) {
        index_arg1[[pos_iCount]] <- list(c(1:(index_token - 1)))
        index_context_arg1[[pos_iCount]] <-
          list(c(1:(index_token - 1)))
      } else {
        if (index_token_comma > index_token_final_parenthesis) {
          index_token_comma <-
            grep(" ,", sentence_infos[[pos_info]][1:(index_token - 1)], fixed = T)
          if (length(index_token_comma) == 1) {
            index_arg1[[pos_iCount]] <-
              list(c((index_token_comma + 1):(index_token - 1)))
            index_context_arg1[[pos_iCount]] <-
              list(c((index_token_comma + 1):(index_token - 1)))
          } else {
            index_last_token_comma <-
              index_token_comma[length(index_token_comma)]
            index_previous_token_comma <-
              index_token_comma[length(index_token_comma) - 1]
            if ((index_token - index_last_token_comma) == 1) {
              if (index_previous_token_comma > index_token_final_parenthesis) {
                index_arg1[[pos_iCount]] <-
                  list(c((index_previous_token_comma + 1):(index_last_token_comma - 1)
                  ))
                if (length(index_token_comma) > 2) {
                  index_context_arg1[[pos_iCount]] <-
                    list(c((index_previous_token_comma + 1):(index_last_token_comma - 1)
                    ))
                  index_context_arg1[[pos_iCount]][2] <-
                    list(c((index_token_comma[length(index_token_comma) - 2] + 1):(index_token_comma[length(index_token_comma) -
                                                                                                       1] - 1)
                    ))
                } else
                  index_context_arg1[[pos_iCount]] <-
                    list(c((index_previous_token_comma + 1):(index_last_token_comma - 1)
                    ))
              } else {
                index_arg1[[pos_iCount]] <-
                  list(c((index_token_final_parenthesis + 1):(index_last_token_comma - 1)
                  ))
                index_context_arg1[[pos_iCount]] <-
                  list(c((index_token_final_parenthesis + 1):(index_last_token_comma - 1)
                  ))
                index_context_arg1[[pos_iCount]][2] <-
                  list(c((
                    index_last_token_comma + 1
                  ):(index_token - 1)))
              }
            } else {
              index_arg1[[pos_iCount]] <-
                list(c((index_last_token_comma + 1):(index_token - 1)))
              if (length(index_token_comma) > 1) {
                index_context_arg1[[pos_iCount]] <-
                  list(c((
                    index_last_token_comma + 1
                  ):(index_token - 1)))
                index_context_arg1[[pos_iCount]][2] <-
                  list(c((index_previous_token_comma + 1):(index_last_token_comma - 1)
                  ))
              } else
                index_context_arg1[[pos_iCount]] <-
                  list(c((index_last_token_comma + 1):(index_token - 1)))
            }
          }
        } else {
          index_arg1[[pos_iCount]] <-
            list(c((index_token_final_parenthesis + 1):(index_token - 1)
            ))
          index_context_arg1[[pos_iCount]] <-
            list(c((index_token_final_parenthesis + 1):(index_token - 1)
            ))
        }
      }
    } else {
      index_token_comma <-
        grep(" ,", sentence_infos[[pos_info]][1:(index_token - 1)], fixed = T)
      index_token_comma <-
        index_token_comma[length(index_token_comma)]
      index_token_final_parenthesis <-
        grep(" )", sentence_infos[[pos_info]][1:(index_token - 1)], fixed = T)
      index_token_final_parenthesis <-
        index_token_final_parenthesis[length(index_token_final_parenthesis)]
      
      if (length(index_token_comma) == 0)
        index_token_comma <- 0
      if (length(index_token_final_parenthesis) == 0)
        index_token_final_parenthesis <- 0
      
      if (index_token_final_parenthesis > index_token_comma) {
        index_token_initial_parenthesis <-
          grep(" (", sentence_infos[[pos_info]][1:(index_token - 1)], fixed = T)
        if (length(index_token_initial_parenthesis) == 0) {
          index_token_comma <-
            grep(" ,", sentence_infos[[pos_info]][1:(index_token_final_parenthesis -
                                                       1)], fixed = T)
          if (length(index_token_comma) == 0) {
            index_arg1[[pos_iCount]] <-
              list(c(1:(
                index_token_final_parenthesis - 1
              )))
            index_context_arg1[[pos_iCount]] <-
              list(c(1:(
                index_token_final_parenthesis - 1
              )))
          } else {
            index_arg1[[pos_iCount]] <-
              list(c((index_token_comma[length(index_token_comma)] + 1):(index_token_final_parenthesis -
                                                                           1)
              ))
            if (length(index_token_comma) > 1) {
              index_context_arg1[[pos_iCount]] <-
                list(c((index_token_comma[length(index_token_comma)] + 1):(index_token_final_parenthesis -
                                                                             1)
                ))
              index_context_arg1[[pos_iCount]][2] <-
                list(c((index_token_comma[length(index_token_comma) - 1] + 1):(index_token_comma[length(index_token_comma)] -
                                                                                 1)
                ))
            } else
              index_context_arg1[[pos_iCount]] <-
                list(c((index_token_comma[length(index_token_comma)] + 1):(index_token_final_parenthesis -
                                                                             1)
                ))
          }
        } else {
          index_token_initial_parenthesis <-
            index_token_initial_parenthesis[1]
          index_token_comma <-
            grep(" ,", sentence_infos[[pos_info]][1:(index_token_initial_parenthesis -
                                                       1)], fixed = T)
          if (length(index_token_comma) == 0) {
            index_arg1[[pos_iCount]] <-
              list(c(
                1:(index_token_initial_parenthesis - 1),
                (index_token_final_parenthesis + 1):(index_token)
              ))
            index_context_arg1[[pos_iCount]] <-
              list(c(
                1:(index_token_initial_parenthesis - 1),
                (index_token_final_parenthesis + 1):(index_token)
              ))
            index_context_arg1[[pos_iCount]][2] <-
              list(c((index_token_initial_parenthesis + 1):(index_token_final_parenthesis -
                                                              1)
              ))
          } else {
            index_arg1[[pos_iCount]] <-
              list(c((index_token_comma[length(index_token_comma)] + 1):(index_token_initial_parenthesis -
                                                                           1),
                     (index_token_final_parenthesis + 1):(index_token)
              ))
            index_context_arg1[[pos_iCount]] <-
              list(c((index_token_comma[length(index_token_comma)] + 1):(index_token_initial_parenthesis -
                                                                           1),
                     (index_token_final_parenthesis + 1):(index_token)
              ))
            if (length(index_token_comma) > 1) {
              index_context_arg1[[pos_iCount]][2] <-
                list(c((index_token_comma[length(index_token_comma) - 1] + 1):(index_token_comma[length(index_token_comma)] -
                                                                                 1)
                ))
              index_context_arg1[[pos_iCount]][3] <-
                list(c((index_token_initial_parenthesis + 1):(index_token_final_parenthesis -
                                                                1)
                ))
            } else {
              index_context_arg1[[pos_iCount]][2] <-
                list(c((index_token_initial_parenthesis + 1):(index_token_final_parenthesis -
                                                                1)
                ))
            }
          }
        }
        
      } else if (index_token_comma > index_token_final_parenthesis) {
        if (length(c((index_token_comma + 1):index_token)) == 1 &
            grepl(" e| como| mas| por?m| entretanto| contuto| todavia| no_entanto",
                  token,
                  fixed = F)) {
          if (pos_iCount > 1) {
            if (length(index_arg1[[pos_iCount - 1]]))
              index_arg1[[pos_iCount]] <-
                index_arg1[[pos_iCount - 1]]
            else {
              aux_index_token_comma <-
                grep(" ,", sentence_infos[[pos_info]][1:(index_token_comma - 1)], fixed = T)
              if (length(aux_index_token_comma) == 0) {
                index_arg1[[pos_iCount]] <- list(c(1:(index_token_comma - 1)))
                index_context_arg1[[pos_iCount]] <-
                  list(c(1:(index_token_comma - 1)))
              } else {
                index_arg1[[pos_iCount]] <- list(c(1:(
                  aux_index_token_comma[1] - 1
                )))
                if (length(aux_index_token_comma) > 1) {
                  index_context_arg1[[pos_iCount]] <-
                    list(c(1:(
                      aux_index_token_comma[1] - 1
                    )))
                  index_context_arg1[[pos_iCount]][2] <-
                    list(c((aux_index_token_comma[1] + 1):(aux_index_token_comma[2] - 1)
                    ))
                } else
                  index_context_arg1[[pos_iCount]] <-
                    list(c(1:(
                      aux_index_token_comma[1] - 1
                    )))
              }
            }
          } else {
            aux_index_token_comma <-
              grep(" ,", sentence_infos[[pos_info]][1:(index_token_comma - 1)], fixed = T)
            if (length(aux_index_token_comma) == 0) {
              index_arg1[[pos_iCount]] <- list(c(1:(index_token_comma - 1)))
              index_context_arg1[[pos_iCount]] <-
                list(c(1:(index_token_comma - 1)))
            } else {
              index_arg1[[pos_iCount]] <-
                list(c(1:(aux_index_token_comma[1] - 1)))
              if (length(aux_index_token_comma) > 1) {
                index_context_arg1[[pos_iCount]] <-
                  list(c(1:(
                    aux_index_token_comma[1] - 1
                  )))
                index_context_arg1[[pos_iCount]][2] <-
                  list(c((aux_index_token_comma[1] + 1):(aux_index_token_comma[2] - 1)
                  ))
              } else
                index_context_arg1[[pos_iCount]] <-
                  list(c(1:(aux_index_token_comma[1] - 1)))
            }
          }
        } else {
          index_arg1[[pos_iCount]] <-
            list(c((index_token_comma + 1):index_token))
          index_token_comma <-
            grep(" ,", sentence_infos[[pos_info]][1:(index_token - 1)], fixed = T)
          if (length(index_token_comma) > 1) {
            index_context_arg1[[pos_iCount]] <-
              list(c((index_token_comma[length(index_token_comma)] + 1):index_token))
            index_context_arg1[[pos_iCount]][2] <-
              list(c((index_token_comma[length(index_token_comma) - 1] + 1):(index_token_comma[length(index_token_comma)] -
                                                                               1)))
          } else if (length(index_token_comma) == 1) {
            index_context_arg1[[pos_iCount]] <-
              list(c(1:(index_token_comma - 1)))
            index_context_arg1[[pos_iCount]][2] <-
              list(c((index_token_comma + 1):index_token))
          } else
            index_context_arg1[[pos_iCount]] <-
            list(c((index_token_comma + 1):index_token))
        }
      } else {
        index_arg1[[pos_iCount]] <- list(c(1:index_token))
        index_context_arg1[[pos_iCount]] <- list(c(1:index_token))
      }
    }
    pos_iCount <- pos_iCount + 1
  }
  output$index_arg1 <- index_arg1
  output$index_context_arg1 <- index_context_arg1
  output
}

getRightInterval <- function (sentence_infos, relations, pos_info)
{
  index_arg1 <- list()
  index_context_arg1 <- list()
  output <- NULL
  
  for (iCount in 1:length(relations$relations))
  {
    index_token <- max(relations$indexes[[iCount]]) + 1
    token <- sentence_infos[[1]][index_token]
    token_tag <- sentence_infos[[2]][index_token]
    max_num_tokens <- length(sentence_infos[[pos_info]])
    
    if (grepl(" ,", token, fixed = T)) {
      aux_index_token <-
        grep(" ,", sentence_infos[[pos_info]][(index_token + 1):max_num_tokens], fixed = T)
      if (length(aux_index_token) == 0) {
        index_token_initial_parenthesis <-
          grep(" (", sentence_infos[[pos_info]][c((index_token + 1):max_num_tokens)], fixed = T)[1]
        if (is.na(index_token_initial_parenthesis)) {
          index_arg1[[iCount]] <-
            list(c((index_token + 1):max_num_tokens))
          index_context_arg1[[iCount]] <-
            list(c((index_token + 1):max_num_tokens))
        } else {
          index_token_final_parenthesis <-
            grep(" )", sentence_infos[[pos_info]][c((index_token + 1):max_num_tokens)], fixed = T)
          index_token_final_parenthesis <-
            index_token_final_parenthesis[length(index_token_final_parenthesis)]
          if (length(index_token_final_parenthesis) == 0) {
            index_arg1[[iCount]] <-
              list(c((index_token + 1):(index_token + index_token_initial_parenthesis -
                                          1)
              ))
            index_context_arg1[[iCount]] <-
              list(c((index_token + 1):(index_token + index_token_initial_parenthesis -
                                          1)
              ))
          } else {
            index_token_verb <-
              grep("v-", sentence_infos[[2]][c((index_token + 1):(index_token + index_token_initial_parenthesis -
                                                                    1))], fixed = T)
            if (length(index_token_verb) == 0) {
              index_arg1[[iCount]] <-
                list(c((index_token + 1):(
                  index_token + index_token_initial_parenthesis - 1
                )
                ))
              index_context_arg1[[iCount]] <-
                list(c((index_token + 1):(
                  index_token + index_token_initial_parenthesis - 1
                )
                ))
              if (!grepl("( ,| ;| :| .)", sentence_infos[[pos_info]][index_token +
                                                                     index_token_final_parenthesis + 1], fixed = F)) {
                index_arg1[[iCount]] <-
                  list(c((index_token + 1):(
                    index_token + index_token_initial_parenthesis - 1
                  ),
                  (
                    index_token + index_token_final_parenthesis + 1
                  ):(max_num_tokens - 1)
                  ))
                index_context_arg1[[iCount]] <-
                  list(c((index_token + 1):(
                    index_token + index_token_initial_parenthesis - 1
                  ),
                  (
                    index_token + index_token_final_parenthesis + 1
                  ):(max_num_tokens - 1)
                  ))
                index_context_arg1[[iCount]][2] <-
                  list(c((
                    index_token + index_token_initial_parenthesis + 1
                  ):(
                    index_token + index_token_final_parenthesis - 1
                  )
                  ))
              }
            } else {
              index_arg1[[iCount]] <-
                list(c((index_token + 1):(
                  index_token + index_token_initial_parenthesis - 1
                )
                ))
              index_context_arg1[[iCount]] <-
                list(c((index_token + 1):(
                  index_token + index_token_initial_parenthesis - 1
                )
                ))
              if (!grepl("( ,| ;| :| .)", sentence_infos[[pos_info]][index_token +
                                                                     index_token_final_parenthesis + 1], fixed = F)) {
                index_arg1[[iCount]] <-
                  list(c((index_token + 1):(
                    index_token + index_token_initial_parenthesis - 1
                  ),
                  (
                    index_token + index_token_final_parenthesis + 1
                  ):(max_num_tokens - 1)
                  ))
                index_context_arg1[[iCount]] <-
                  list(c((
                    index_token + index_token_initial_parenthesis + 1
                  ):(
                    index_token + index_token_final_parenthesis - 1
                  )
                  ))
              }
            }
          }
        }
      } else {
        index_token_initial_parenthesis <-
          grep(" (", sentence_infos[[pos_info]][(index_token + 1):(max_num_tokens)], fixed = T)
        if (length(index_token_initial_parenthesis) == 0) {
          aux_index_token <- aux_index_token[length(aux_index_token)]
          index_arg1[[iCount]] <-
            list(c((index_token + aux_index_token + 1):(max_num_tokens - 1)
            ))
          index_context_arg1[[iCount]] <-
            list(c((index_token + aux_index_token + 1):(max_num_tokens - 1)
            ))
        } else {
          index_token_initial_parenthesis <-
            index_token_initial_parenthesis[1]
          index_token_final_parenthesis <-
            grep(" )", sentence_infos[[pos_info]][(index_token + 1):(max_num_tokens -
                                                                       1)], fixed = T)
          if (length(index_token_final_parenthesis) == 0) {
            aux_index_token <-
              grep(" ,", sentence_infos[[pos_info]][(index_token + 1):(index_token + index_token_initial_parenthesis -
                                                                         1)], fixed = T)
            if (length(aux_index_token) == 0) {
              index_arg1[[iCount]] <-
                list(c((index_token + 1):(
                  index_token + index_token_initial_parenthesis - 1
                )
                ))
              index_context_arg1[[iCount]] <-
                list(c((index_token + 1):(
                  index_token + index_token_initial_parenthesis - 1
                )
                ))
            } else {
              index_arg1[[iCount]] <-
                list(c((index_token + aux_index_token[length(aux_index_token)] + 1):(
                  index_token + index_token_initial_parenthesis - 1
                )
                ))
              if (length(aux_index_token) > 1) {
                index_context_arg1[[iCount]] <-
                  list(c((index_token + aux_index_token[length(aux_index_token)] + 1):(
                    index_token + index_token_initial_parenthesis - 1
                  )
                  ))
                index_context_arg1[[iCount]][2] <-
                  list(c((index_token + aux_index_token[length(aux_index_token) - 1] + 1):(index_token +
                                                                                             aux_index_token[length(aux_index_token)] - 1)
                  ))
              } else
                index_context_arg1[[iCount]] <-
                  list(c((index_token + aux_index_token + 1):(
                    index_token + index_token_initial_parenthesis - 1
                  )
                  ))
            }
          } else {
            aux_index_token <-
              grep(" ,", sentence_infos[[pos_info]][(index_token + index_token_final_parenthesis +
                                                       1):(max_num_tokens)], fixed = T)
            if (length(aux_index_token) == 0) {
              aux_index_token <-
                grep(" ,", sentence_infos[[pos_info]][(index_token + 1):(index_token + index_token_initial_parenthesis -
                                                                           1)], fixed = T)
              if (length(aux_index_token) == 0) {
                index_arg1[[iCount]] <-
                  list(c((index_token + 1):(
                    index_token + index_token_initial_parenthesis - 1
                  )
                  ))
                index_context_arg1[[iCount]] <-
                  list(c((index_token + 1):(
                    index_token + index_token_initial_parenthesis - 1
                  )
                  ))
                if (!grepl("( \\,| \\;| \\:| \\.)",
                           sentence_infos[[pos_info]][index_token + index_token_final_parenthesis +
                                                      1],
                           fixed = F)) {
                  index_arg1[[iCount]][2] <-
                    list(c((
                      index_token + index_token_final_parenthesis + 1
                    ):(max_num_tokens - 1)
                    ))
                  index_context_arg1[[iCount]][2] <-
                    list(c((
                      index_token + index_token_final_parenthesis + 1
                    ):(max_num_tokens - 1)
                    ))
                }
              } else {
                index_arg1[[iCount]] <-
                  list(c((index_token + aux_index_token[length(aux_index_token)] + 1):(
                    index_token + index_token_initial_parenthesis - 1
                  )
                  ))
                index_context_arg1[[iCount]] <-
                  list(c((index_token + aux_index_token[length(aux_index_token)] + 1):(
                    index_token + index_token_initial_parenthesis - 1
                  )
                  ))
                if (!grepl("( \\,| \\;| \\:| \\.)",
                           sentence_infos[[pos_info]][index_token + index_token_final_parenthesis +
                                                      1],
                           fixed = F)) {
                  index_arg1[[iCount]][2] <-
                    list(c((
                      index_token + index_token_final_parenthesis + 1
                    ):(max_num_tokens - 1)
                    ))
                  index_context_arg1[[iCount]][2] <-
                    list(c((
                      index_token + index_token_final_parenthesis + 1
                    ):(max_num_tokens - 1)
                    ))
                  if (length(aux_index_token) > 1)
                    index_context_arg1[[iCount]][3] <-
                    list(c((index_token + aux_index_token[length(aux_index_token) - 1] + 1):(index_token +
                                                                                               aux_index_token[length(aux_index_token)] - 1)
                    ))
                }
              }
            } else {
              index_arg1[[iCount]] <-
                list(c((index_token + aux_index_token[length(aux_index_token)] + 1):(max_num_tokens -
                                                                                       1)
                ))
              if (length(aux_index_token) > 1) {
                index_context_arg1[[iCount]] <-
                  list(c((index_token + aux_index_token[length(aux_index_token)] + 1):(max_num_tokens -
                                                                                         1)
                  ))
                index_context_arg1[[iCount]][2] <-
                  list(c((index_token + aux_index_token[length(aux_index_token) - 1] + 1):(index_token +
                                                                                             aux_index_token[length(aux_index_token)] - 1)
                  ))
              } else
                index_context_arg1[[iCount]] <-
                  list(c((index_token + aux_index_token[length(aux_index_token)] + 1):(max_num_tokens -
                                                                                         1)
                  ))
            }
          }
        }
      }
    } else if (grepl(" (", token, fixed = T)) {
      index_token_final_parenthesis <-
        grep(" )", sentence_infos[[pos_info]][c((index_token + 1):(max_num_tokens))], fixed = T)
      if (length(index_token_final_parenthesis) == 0) {
        index_token_comma <-
          grep(" ,", sentence_infos[[pos_info]][c((index_token + 1):(max_num_tokens))], fixed = T)
        if (length(index_token_comma) == 0) {
          index_arg1[[iCount]] <-
            list(c((index_token + 1):(max_num_tokens - 1)))
          index_context_arg1[[iCount]] <-
            list(c((index_token + 1):(max_num_tokens - 1)))
        } else {
          index_arg1[[iCount]] <-
            list(c((index_token + 1):(index_token_comma[1] - 1)))
          if (length(index_token_comma) > 1) {
            index_context_arg1[[iCount]] <-
              list(c((index_token + 1):(index_token_comma[1] - 1)))
            index_context_arg1[[iCount]][2] <-
              list(c((index_token_comma[1] + 1):(index_token_comma[2] - 1)))
          } else
            index_context_arg1[[iCount]] <-
              list(c((index_token + 1):(index_token_comma[1] - 1)))
        }
      } else {
        index_token_final_parenthesis <-
          index_token_final_parenthesis[length(index_token_final_parenthesis)]
        index_token_comma <-
          grep(" ,", sentence_infos[[pos_info]][c((index_token + index_token_final_parenthesis +
                                                     1):(max_num_tokens))], fixed = T)
        if (length(index_token_comma) == 0) {
          index_arg1[[iCount]] <-
            list(c((index_token + index_token_final_parenthesis + 1):(max_num_tokens -
                                                                        1)
            ))
          index_context_arg1[[iCount]] <-
            list(c((index_token + index_token_final_parenthesis + 1):(max_num_tokens -
                                                                        1)
            ))
        } else {
          index_token_comma <- index_token_comma[1]
          index_arg1[[iCount]] <-
            list(c((index_token + index_token_final_parenthesis + 1):(index_token +
                                                                        index_token_comma[1] - 1)
            ))
          if (length(index_token_comma) > 1) {
            index_context_arg1[[iCount]] <-
              list(c((index_token + index_token_final_parenthesis + 1):(index_token +
                                                                          index_token_comma[1] - 1)
              ))
            index_context_arg1[[iCount]][2] <-
              list(c((index_token + index_token_comma[1] + 1):(index_token + index_token_comma[2] -
                                                                 1)
              ))
          } else
            index_context_arg1[[iCount]] <-
            list(c((index_token + index_token_final_parenthesis + 1):(index_token +
                                                                        index_token_comma[1] - 1)
            ))
        }
      }
    } else if (grepl(" que| se", sentence_infos[[pos_info]][index_token], fixed = F)) {
      index_token_comma <-
        grep(" ,", sentence_infos[[pos_info]][(index_token):(max_num_tokens - 1)], fixed = T)
      index_token_comma <- index_token_comma[1]
      index_token_initial_parenthesis <-
        grep(" (", sentence_infos[[pos_info]][(index_token):(max_num_tokens - 1)], fixed = T)
      index_token_initial_parenthesis <-
        index_token_initial_parenthesis[1]
      
      if (length(index_token_comma) == 0 ||
          is.na(index_token_comma))
        index_token_comma <- 1000
      if (length(index_token_initial_parenthesis) == 0 ||
          is.na(index_token_initial_parenthesis))
        index_token_initial_parenthesis <- 1000
      
      if (length(index_token_comma) == 1000 &
          index_token_initial_parenthesis == 1000) {
        index_arg1[[iCount]] <-
          list(c((index_token + 1):(max_num_tokens - 1)))
        index_context_arg1[[iCount]] <-
          list(c((index_token + 1):(max_num_tokens - 1)))
      } else {
        if (index_token_comma < index_token_initial_parenthesis) {
          index_token_comma <-
            grep(" ,", sentence_infos[[pos_info]][(index_token):(max_num_tokens - 1)], fixed = T)
          if (length(index_token_comma) == 1) {
            index_arg1[[iCount]] <-
              list(c((index_token + 1):((index_token - 1) + index_token_comma - 1
              )))
            index_context_arg1[[iCount]] <-
              list(c((index_token + 1):((index_token - 1) + index_token_comma - 1
              )))
          } else {
            index_previous_token_comma <- index_token_comma[1]
            index_last_token_comma <- index_token_comma[2]
            if ((index_previous_token_comma - index_token) == 1) {
              if (index_last_token_comma < index_token_initial_parenthesis) {
                index_arg1[[iCount]] <-
                  list(c(((index_token - 1) + index_previous_token_comma + 1
                  ):((index_token - 1) + index_last_token_comma - 1)
                  ))
                index_context_arg1[[iCount]] <-
                  list(c(((index_token - 1) + index_previous_token_comma + 1
                  ):((index_token - 1) + index_last_token_comma - 1)
                  ))
                if (length(index_token_comma) > 2)
                  index_context_arg1[[iCount]][2] <-
                  list(c(((index_token - 1) + index_last_token_comma + 1):((index_token -
                                                                              1) + index_token_comma[3] - 1)
                  ))
              } else {
                index_arg1[[iCount]] <-
                  list(c(((index_token - 1) + index_previous_token_comma + 1
                  ):((index_token - 1) + index_token_initial_parenthesis - 1
                  )
                  ))
                index_context_arg1[[iCount]] <-
                  list(c(((index_token - 1) + index_previous_token_comma + 1
                  ):((index_token - 1) + index_token_initial_parenthesis - 1
                  )
                  ))
              }
            } else {
              index_arg1[[iCount]] <-
                list(c((index_token + 1):(index_token - 1) + index_previous_token_comma -
                         1
                ))
              if (length(index_token_comma) > 1) {
                index_context_arg1[[iCount]] <-
                  list(c((index_token + 1):(index_token - 1) + index_previous_token_comma -
                           1
                  ))
                index_context_arg1[[iCount]][2] <-
                  list(c((index_token - 1) + index_previous_token_comma + 1:(index_token -
                                                                               1) + index_last_token_comma - 1
                  ))
              } else
                index_context_arg1[[iCount]] <-
                  list(c((index_token + 1):(index_token - 1) + index_previous_token_comma -
                           1
                  ))
            }
          }
        } else {
          index_arg1[[iCount]] <-
            list(c((index_token + 1):((index_token - 1) + index_token_initial_parenthesis -
                                        1
            )
            ))
          index_context_arg1[[iCount]] <-
            list(c((index_token + 1):((index_token - 1) + index_token_initial_parenthesis -
                                        1
            )
            ))
        }
      }
    } else {
      index_token_comma <-
        grep(" ,", sentence_infos[[pos_info]][(index_token):(max_num_tokens - 1)], fixed = T)
      index_token_comma <- index_token_comma[1]
      index_token_initial_parenthesis <-
        grep(" (", sentence_infos[[pos_info]][(index_token):(max_num_tokens - 1)], fixed = T)
      index_token_initial_parenthesis <-
        index_token_initial_parenthesis[1]
      
      if (length(index_token_comma) == 0 ||
          is.na(index_token_comma))
        index_token_comma <- 1000
      if (length(index_token_initial_parenthesis) == 0 ||
          is.na(index_token_initial_parenthesis))
        index_token_initial_parenthesis <- 1000
      
      if (index_token_initial_parenthesis < index_token_comma) {
        index_token_final_parenthesis <-
          grep(" )", sentence_infos[[pos_info]][(index_token):(max_num_tokens - 1)], fixed = T)
        index_token_final_parenthesis <-
          index_token_final_parenthesis[1]
        if (length(index_token_final_parenthesis) == 0 ||
            is.na(index_token_final_parenthesis)) {
          index_arg1[[iCount]] <-
            list(c(
              index_token:(index_token + index_token_initial_parenthesis - 1)
            ))
          index_context_arg1[[iCount]] <-
            list(c(
              index_token:(index_token + index_token_initial_parenthesis - 1)
            ))
        } else {
          if (length(grep(" \\,| \\.", sentence_infos[[1]][(index_token - 1) + index_token_final_parenthesis +
                                                           1], fixed = F)) > 0 ||
              length(grep(" adv| conj-s| conj-c| intj", sentence_infos[[2]][(index_token -
                                                                             1) + index_token_final_parenthesis + 1], fixed = F)) > 0) {
            index_arg1[[iCount]] <-
              list(c(
                index_token:((index_token - 1) + index_token_initial_parenthesis - 1
                )
              ))
            index_context_arg1[[iCount]] <-
              list(c(
                index_token:((index_token - 1) + index_token_initial_parenthesis - 1
                )
              ))
          } else {
            index_token_comma <-
              grep(" ,", sentence_infos[[pos_info]][((index_token - 1) + index_token_final_parenthesis +
                                                       1):(max_num_tokens - 1)], fixed = T)
            if (length(index_token_comma) == 0) {
              index_arg1[[iCount]] <-
                list(c(
                  index_token:((index_token - 1) + index_token_initial_parenthesis - 1
                  ),
                  ((index_token - 1) + index_token_final_parenthesis + 1
                  ):(max_num_tokens - 1)
                ))
              index_context_arg1[[iCount]] <-
                list(c(
                  index_token:((index_token - 1) + index_token_initial_parenthesis - 1
                  ),
                  ((index_token - 1) + index_token_final_parenthesis + 1
                  ):(max_num_tokens - 1)
                ))
              index_context_arg1[[iCount]][2] <-
                list(c(((index_token - 1) + index_token_initial_parenthesis + 1
                ):((index_token - 1) + index_token_final_parenthesis - 1
                )
                ))
            } else{
              index_arg1[[iCount]] <-
                list(c(
                  index_token:((index_token - 1) + index_token_initial_parenthesis - 1
                  ),
                  ((index_token - 1) + index_token_final_parenthesis + 1
                  ):((index_token - 1) + (index_token_final_parenthesis + 1) + index_token_comma[1] -
                       1
                  )
                ))
              index_context_arg1[[iCount]] <-
                list(c(
                  index_token:((index_token - 1) + index_token_initial_parenthesis - 1
                  ),
                  ((index_token - 1) + index_token_final_parenthesis + 1
                  ):((index_token - 1) + (index_token_final_parenthesis + 1) + index_token_comma[1] -
                       1
                  )
                ))
              index_context_arg1[[iCount]] <-
                list(c(((index_token - 1) + index_token_initial_parenthesis + 1
                ):((index_token - 1) + index_token_final_parenthesis - 1
                )
                ))
            }
          }
        }
      } else if (index_token_comma < index_token_initial_parenthesis) {
        index_arg1[[iCount]] <-
          list(c(index_token:((index_token - 1) + index_token_comma - 1
          )))
        index_token_comma <-
          grep(" ,", sentence_infos[[pos_info]][(index_token):(max_num_tokens - 1)], fixed = T)
        if (length(index_token_comma) > 1) {
          index_context_arg1[[iCount]] <-
            list(c(index_token:((index_token - 1) + index_token_comma[1] - 1
            )))
          index_context_arg1[[iCount]][2] <-
            list(c(((index_token - 1) + index_token_comma[1] + 1):((index_token - 1) +
                                                                     index_token_comma[2] - 1)
            ))
        } else if (length(index_token_comma) == 1) {
          index_context_arg1[[iCount]] <-
            list(c(index_token:((index_token - 1) + index_token_comma - 1
            )))
          index_context_arg1[[iCount]][2] <-
            list(c(((index_token - 1) + index_token_comma + 1
            ):(max_num_tokens - 1)))
        } else
          index_context_arg1[[iCount]] <-
          list(c(index_token:((index_token - 1) + index_token_comma[1] - 1
          )))
      } else if (grepl(" ,", sentence_infos[[pos_info]][relations$indexes[[iCount]][1] +
                                                        1])) {
        new_comma <-
          grep(" \\,| \\(", sentence_infos[[pos_info]][(relations$indexes[[iCount]][1] +
                                                          2):(length(sentence_infos[[pos_info]]))])
        if (length(new_comma) > 0) {
          index_arg1[[iCount]] <- list(c(index_token:(max_num_tokens - 1)))
          index_context_arg1[[iCount]] <-
            list(c((relations$indexes[[iCount]][1] + 2):(relations$indexes[[iCount]][1] +
                                                           new_comma[1])
            ))
        } else{
          index_arg1[[iCount]] <- list(c(index_token:(max_num_tokens - 1)))
          index_context_arg1[[iCount]] <-
            list(c(index_token:(max_num_tokens - 1)))
        }
      } else {
        index_arg1[[iCount]] <- list(c(index_token:(max_num_tokens - 1)))
        index_context_arg1[[iCount]] <-
          list(c(index_token:(max_num_tokens - 1)))
      }
    }
  }
  output$index_arg2 <- index_arg1
  output$index_context_arg2 <- index_context_arg1
  output
}

clearTriples <-
  function(normal_triples,
           left_context_indexes,
           right_context_indexes)
  {
    output <- NULL
    
    aux_left_arg <- list()
    aux_rel <- list(NULL, NULL)
    aux_right_arg <- list()
    aux_left_context_indexes <- list()
    aux_right_context_indexes <- list()
    
    lim <- 0
    pos_iCount <- 1
    
    if (length(normal_triples$left_args) < length(normal_triples$right_args))
      lim <- length(normal_triples$left_args)
    else
      lim <- length(normal_triples$right_args)
    
    if (lim != 0) {
      for (iCount in 1:lim)
      {
        if (!(
          identical(normal_triples$left_args[[iCount]], "") ||
          identical(normal_triples$left_args[[iCount]], NULL) ||
          identical(normal_triples$left_args[[iCount]], as.character(NA))
        ) &
        !(
          identical(normal_triples$right_args[[iCount]], "") ||
          identical(normal_triples$right_args[[iCount]], NULL) ||
          identical(normal_triples$right_args[[iCount]], as.character(NA))
        )) {
          aux_left_arg[[pos_iCount]] <- normal_triples$left_args[[iCount]]
          aux_right_arg[[pos_iCount]] <-
            normal_triples$right_args[[iCount]]
          aux_left_context_indexes[[pos_iCount]] <-
            left_context_indexes[[iCount]]
          aux_right_context_indexes[[pos_iCount]] <-
            right_context_indexes[[iCount]]
          aux_rel[[1]][pos_iCount] <- relations$relations[iCount]
          aux_rel[[2]][pos_iCount] <- relations$indexes[iCount]
          pos_iCount <- pos_iCount + 1
        }
      }
    }
    
    if (length(aux_left_arg) == 0 ||
        length(aux_right_arg) == 0 || length(aux_rel) == 0)
      output <- NULL
    else{
      output$left_args <- aux_left_arg
      output$relations <- aux_rel[[1]]
      output$indexes <- aux_rel[[2]]
      output$right_args <- aux_right_arg
      output$index_context_arg1 <- aux_left_context_indexes
      output$index_context_arg2 <- aux_right_context_indexes
    }
    
    output
  }

clearInfTriples <-
  function(left_arg,
           relations,
           right_arg,
           left_context,
           right_context)
  {
    output <- c()
    
    for (iCount in 1:length(relations))
    {
      if ((
        identical(left_arg[iCount], "") ||
        identical(left_arg[iCount], NULL) ||
        identical(left_arg[iCount], as.character(NA))
      ) ||
      (
        identical(right_arg[iCount], "") ||
        identical(right_arg[iCount], NULL) ||
        identical(right_arg[iCount], as.character(NA))
      )) {
        left_arg <- left_arg[-iCount]
        right_arg <- right_arg[-iCount]
        relations <- relations[-iCount]
        left_context <- left_context[-iCount]
        right_context <- right_context[-iCount]
      }
    }
    
    if (length(left_arg) == 0 ||
        length(right_arg) == 0 || length(relations[[1]]) == 0) {
      output$status <- 0
    } else{
      output$left_arg <- left_arg
      output$relations <- relations
      output$right_arg <- right_arg
      output$left_context <- left_context
      output$right_context <- right_context
      output$status <- 1
    }
    output
  }
