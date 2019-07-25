contextExtraction <-
  function (normal_triples, sentence_infos, pos_info)
  {
    if (!is.null(normal_triples$left_sym_con)) {
      left_arg <- normal_triples$left_sym_arg
      relations <- normal_triples$sym_rel
      right_arg <- normal_triples$right_sym_arg
      left_context_indexes <- list(normal_triples$left_sym_con)
      right_context_indexes <- list(normal_triples$right_sym_con)
    } else if (!is.null(normal_triples$left_tran_con)) {
      left_arg <- normal_triples$left_tran_arg
      relations <- normal_triples$tran_rel
      right_arg <- normal_triples$right_tran_arg
      left_context_indexes <- list(normal_triples$left_tran_con)
      right_context_indexes <- list(normal_triples$right_tran_con)
    } else {
      left_context_indexes <- normal_triples$index_context_arg1
      right_context_indexes <- normal_triples$index_context_arg2
      left_arg <- normal_triples$left_args
      relations <- normal_triples$relations
      right_arg <- normal_triples$right_args
    }
    
    left_context <- list()
    right_context <- list()
    output <- NULL
    
    #left context analyses
    if (!is.null(left_context_indexes)) {
      for (iCount in 1:length(left_context_indexes)) {
        for (jCount in 1:length(left_context_indexes[[iCount]])) {
          for (kCount in 1:length(left_arg[[iCount]])) {
            tk_postag_context <-
              gsub(" ", "", sentence_infos[[pos_info]][left_context_indexes[[iCount]][[jCount]][1]])
            tk_lex_context <-
              gsub(" ", "", sentence_infos[[1]][left_context_indexes[[iCount]][[jCount]][1]])
            tk_lex_context <- paste0(" ", tk_lex_context, " ")
            
            if ((
              grepl("conj-s", tk_postag_context) ||
              grepl(CONDITIONAL_CONTEXT, tolower(tk_lex_context))
            )) {
              context <-
                paste0(sentence_infos[[1]][left_context_indexes[[iCount]][[jCount]]], collapse = "")
              context <-
                strsplit(context, str_trim(left_arg[[iCount]][kCount], "right"), fixed = T)[[1]]
              splited_context <- strsplit(context, " ")[[1]]
              splited_context <- splited_context[splited_context != ""]
              splited_context <- splited_context[splited_context != " "]
              
              if (grepl("pron|prp|art", getPostags(splited_context[length(splited_context)]))) {
                splited_context <- splited_context[-length(splited_context)]
                context <- paste0(splited_context, collapse = " ")
              }
              
              if (length(context) > 1 & length(splited_context) > 1) {
                if (!is.null(left_context_indexes[[iCount]]) &
                    !is.null(normal_triples$left_sym_con)) {
                  context <-
                    strsplit(context,
                             str_trim(left_arg[[iCount]][kCount], "right"),
                             fixed = T)[[1]]
                  splited_context <- strsplit(context, " ")[[1]]
                  if (grepl("pron|prp|art",
                            getPostags(splited_context[length(splited_context)]))) {
                    splited_context <- splited_context[-length(splited_context)]
                    context <- paste0(splited_context, collapse = " ")
                    left_context[[iCount]] <- context
                  }
                } else
                  left_context[[iCount]] <-
                    context[context != "" || context != " "]
              } else if (length(splited_context) > 1) {
                if (!is.null(left_context_indexes[[iCount]]) &
                    !is.null(normal_triples$left_sym_con)) {
                  context <-
                    strsplit(context,
                             str_trim(left_arg[[iCount]][kCount], "right"),
                             fixed = T)[[1]]
                  splited_context <- strsplit(context, " ")[[1]]
                  if (grepl("pron|prp|art",
                            getPostags(splited_context[length(splited_context)]))) {
                    splited_context <- splited_context[-length(splited_context)]
                    context <- paste0(splited_context, collapse = " ")
                    left_context[[iCount]] <- context
                  }
                } else
                  left_context[[iCount]] <- context
              }
              break
            } else if (length(grep("v-", sentence_infos[[pos_info]][left_context_indexes[[iCount]][[jCount]]], fixed = T))) {
              index_verbs <-
                grep("v-", sentence_infos[[pos_info]][left_context_indexes[[iCount]][[jCount]]], fixed = T)
              for (lCount in 1:length(index_verbs)) {
                if (grepl(VERBAL_CONTEXT, getLemmas(sentence_infos[[1]][left_context_indexes[[iCount]][[jCount]][index_verbs[lCount]]]))) {
                  context <-
                    paste0(sentence_infos[[1]][left_context_indexes[[iCount]][[jCount]]], collapse = "")
                  context <-
                    strsplit(context,
                             str_trim(left_arg[[iCount]][kCount], "right"),
                             fixed = T)[[1]]
                  splited_context <- strsplit(context, " ")[[1]]
                  splited_context <-
                    splited_context[splited_context != ""]
                  splited_context <-
                    splited_context[splited_context != " "]
                  
                  if (grepl("pron|prp|art",
                            getPostags(splited_context[length(splited_context)]))) {
                    splited_context <- splited_context[-length(splited_context)]
                    context <- paste0(splited_context, collapse = " ")
                  }
                  
                  if (length(context) > 1 &
                      length(splited_context) > 1) {
                    if (!is.null(left_context_indexes[[iCount]]) &
                        !is.null(normal_triples$left_sym_con)) {
                      context <-
                        strsplit(context,
                                 str_trim(left_arg[[iCount]][kCount], "right"),
                                 fixed = T)[[1]]
                      splited_context <- strsplit(context, " ")[[1]]
                      if (grepl("pron|prp|art",
                                getPostags(splited_context[length(splited_context)]))) {
                        splited_context <- splited_context[-length(splited_context)]
                        context <-
                          paste0(splited_context, collapse = " ")
                        left_context[[iCount]] <- context
                      }
                    } else
                      left_context[[iCount]] <-
                        context[context != "" || context != " "]
                  } else if (length(splited_context) > 1) {
                    if (!is.null(left_context_indexes[[iCount]]) &
                        !is.null(normal_triples$left_sym_con)) {
                      context <-
                        strsplit(context,
                                 str_trim(left_arg[[iCount]][kCount], "right"),
                                 fixed = T)[[1]]
                      splited_context <- strsplit(context, " ")[[1]]
                      if (grepl("pron|prp|art",
                                getPostags(splited_context[length(splited_context)]))) {
                        splited_context <- splited_context[-length(splited_context)]
                        context <-
                          paste0(splited_context, collapse = " ")
                        left_context[[iCount]] <- context
                      }
                    } else
                      left_context[[iCount]] <- context
                  }
                  break
                }
              }
            }
          }
        }
      }
    }
    
    #right context analyses
    if (!is.null(right_context_indexes)) {
      for (iCount in 1:length(right_context_indexes)) {
        for (jCount in 1:length(right_context_indexes[[iCount]])) {
          for (kCount in 1:length(right_arg[[iCount]])) {
            tk_postag_context <- strsplit(right_arg[[iCount]], " ")[[1]]
            tk_postag_context <-
              tk_postag_context[length(tk_postag_context)]
            first_tk_index <-
              which(str_trim(sentence_infos[[1]][right_context_indexes[[iCount]][[jCount]]], "left") == tk_postag_context)
            
            if (length(first_tk_index))
              if (!is.na(right_context_indexes[[iCount]][[jCount]][first_tk_index[1] +
                                                                   1]))
                right_context_indexes[[iCount]][[jCount]] <-
              c(right_context_indexes[[iCount]][[jCount]][first_tk_index[1] + 1]:right_context_indexes[[iCount]][[jCount]][length(right_context_indexes[[iCount]][[jCount]])])
            
            tk_postag_context <-
              gsub(" ", "", sentence_infos[[pos_info]][right_context_indexes[[iCount]][[jCount]][1]])
            tk_lex_context <-
              gsub(" ", "", sentence_infos[[1]][right_context_indexes[[iCount]][[jCount]][1]])
            tk_lex_context <- paste0(" ", tk_lex_context, " ")
            
            if ((
              grepl("conj-s", tk_postag_context) ||
              grepl(CONDITIONAL_CONTEXT, tolower(tk_lex_context))
            )) {
              context <-
                paste0(sentence_infos[[1]][right_context_indexes[[iCount]][[jCount]]], collapse = "")
              context <-
                strsplit(context, str_trim(right_arg[[iCount]][kCount]), "right")[[1]]
              splited_context <- strsplit(context, " ")[[1]]
              splited_context <- splited_context[splited_context != ""]
              splited_context <- splited_context[splited_context != " "]
              
              if (grepl("pron|prp|art", getPostags(splited_context[length(splited_context)]))) {
                splited_context <- splited_context[-length(splited_context)]
                context <- paste0(splited_context, collapse = " ")
              }
              
              if (length(context) > 1 & length(splited_context) > 1) {
                if (!is.null(right_context_indexes[[iCount]]) &
                    !is.null(normal_triples$left_sym_con)) {
                  context <-
                    strsplit(context,
                             str_trim(right_arg[[iCount]][kCount], "right"),
                             fixed = T)[[1]]
                  splited_context <- strsplit(context, " ")[[1]]
                  if (grepl("pron|prp|art",
                            getPostags(splited_context[length(splited_context)]))) {
                    splited_context <- splited_context[-length(splited_context)]
                    context <- paste0(splited_context, collapse = " ")
                    right_arg[[iCount]] <- context
                  }
                } else
                  right_context[[iCount]] <-
                    context[context != "" || context != " "]
              } else if (length(splited_context) > 1) {
                if (!is.null(right_context_indexes[[iCount]]) &
                    !is.null(normal_triples$left_sym_con)) {
                  context <-
                    strsplit(context,
                             str_trim(right_arg[[iCount]][kCount], "right"),
                             fixed = T)[[1]]
                  splited_context <- strsplit(context, " ")[[1]]
                  if (grepl("pron|prp|art",
                            getPostags(splited_context[length(splited_context)]))) {
                    splited_context <- splited_context[-length(splited_context)]
                    context <- paste0(splited_context, collapse = " ")
                    right_arg[[iCount]] <- context
                  }
                } else
                  right_context[[iCount]] <- context
              }
              break
            } else if (length(grep("v-", sentence_infos[[pos_info]][right_context_indexes[[iCount]][[jCount]]], fixed = T))) {
              index_verbs <-
                grep("v-", sentence_infos[[pos_info]][right_context_indexes[[iCount]][[jCount]]], fixed = T)
              for (lCount in 1:length(index_verbs)) {
                if (grepl(VERBAL_CONTEXT, getLemmas(sentence_infos[[pos_info]][right_context_indexes[[iCount]][[jCount]][index_verbs[lCount]]]))) {
                  context <-
                    paste0(sentence_infos[[1]][right_context_indexes[[iCount]][[jCount]]], collapse = "")
                  context <-
                    strsplit(context, str_trim(right_arg[[iCount]][kCount]), "right")[[1]]
                  splited_context <- strsplit(context, " ")[[1]]
                  splited_context <-
                    splited_context[splited_context != ""]
                  splited_context <-
                    splited_context[splited_context != " "]
                  
                  if (grepl("pron|prp|art",
                            getPostags(splited_context[length(splited_context)]))) {
                    splited_context <- splited_context[-length(splited_context)]
                    context <- paste0(splited_context, collapse = " ")
                  }
                  
                  if (length(context) > 1 & length(splited_context)) {
                    if (!is.null(right_context_indexes[[iCount]]) &
                        !is.null(normal_triples$left_sym_con)) {
                      context <-
                        strsplit(context,
                                 str_trim(right_arg[[iCount]][kCount], "right"),
                                 fixed = T)[[1]]
                      splited_context <- strsplit(context, " ")[[1]]
                      if (grepl("pron|prp|art",
                                getPostags(splited_context[length(splited_context)]))) {
                        splited_context <- splited_context[-length(splited_context)]
                        context <-
                          paste0(splited_context, collapse = " ")
                        right_arg[[iCount]] <- context
                      }
                    } else
                      right_arg[[iCount]] <-
                        context[context != "" || context != " "]
                  } else if (length(splited_context) > 1) {
                    if (!is.null(right_context_indexes[[iCount]]) &
                        !is.null(normal_triples$left_sym_con)) {
                      context <-
                        strsplit(context,
                                 str_trim(right_arg[[iCount]][kCount], "right"),
                                 fixed = T)[[1]]
                      splited_context <- strsplit(context, " ")[[1]]
                      if (grepl("pron|prp|art",
                                getPostags(splited_context[length(splited_context)]))) {
                        splited_context <- splited_context[-length(splited_context)]
                        context <-
                          paste0(splited_context, collapse = " ")
                        right_arg[[iCount]] <- context
                      }
                    } else
                      right_arg[[iCount]] <- context
                  }
                  break
                }
              }
            }
          }
        }
      }
    }
    output$left_contexts <- left_context
    output$right_contexts <- right_context
    output
  }

getConntextualTriples <- function(triples, contexts) {
  output <- NULL
  left_context <- contexts$left_contexts
  right_context <- contexts$right_contexts
  
  if (!is.null(triples$sym_status)) {
    if (triples$sym_status == 1) {
      aux_output <-
        clearContexts(
          triples$left_sym_arg,
          triples$sym_rel,
          triples$right_sym_arg,
          left_context,
          right_context
        )
      output$left_sym_arg <- aux_output$left_arg
      output$rel_sym <- aux_output$relations
      output$right_sym_arg <- aux_output$right_arg
      output$left_sym_context <- aux_output$left_context
      output$right_sym_context <- aux_output$right_context
      if (length(output$left_sym_context) == 0 &
          length(output$right_sym_context) == 0)
        output$sym_status <- 0
      else
        output$sym_status <- triples$sym_status
      
    }
  }
  
  if (!is.null(triples$tran_status)) {
    if (triples$tran_status == 1) {
      aux_output <-
        clearContexts(
          triples$left_tran_arg,
          triples$tran_rel,
          triples$right_tran_arg,
          left_context,
          right_context
        )
      output$left_tran_arg <- aux_output$left_arg
      output$rel_tran <- aux_output$relations
      output$right_tran_arg <- aux_output$right_arg
      output$left_tran_context <- aux_output$left_context
      output$right_tran_context <- aux_output$right_context
      if (length(output$left_tran_context) == 0 &
          length(output$right_tran_context) == 0)
        output$tran_status <- 0
      else
        output$tran_status <- triples$tran_status
    }
  }
  
  if (!is.null(triples$left_args)) {
    output <-
      clearContexts(
        triples$left_args,
        triples$relations,
        triples$right_args,
        left_context,
        right_context
      )
  }
  
  output
  
}

clearContexts <-
  function(left_arg,
           relations,
           right_arg,
           left_context,
           right_context)
  {
    output <- NULL
    if (!is.null(left_arg) &
        !is.null(relations) & !is.null(right_arg)) {
      left_context <- left_context[1:length(relations)]
      right_context <- right_context[1:length(relations)]
      for (iCount in 1:length(relations)) {
        if (length(left_context) >= iCount) {
          for (jCount in 1:length(left_context)) {
            if (length(left_context[[jCount]]) > 0) {
              if (!is.null(left_context[[jCount]]) & length(relations) >= iCount) {
                splited_rel <- str_split(relations[iCount], " ")[[1]]
                splited_rel <-
                  splited_rel[splited_rel != "" & splited_rel != " "]
                
                if (grepl("pron|prp|art", getPostags(splited_rel[length(splited_rel)])))
                  splited_rel <- splited_rel[-length(splited_rel)]
                
                splited_con <-
                  str_split(left_context[[jCount]], " ")[[1]]
                splited_con <-
                  splited_con[splited_con != "" & splited_con != " "]
                status_con <- splited_rel %in% splited_con
                status_con <- status_con[status_con != F]
                
                if (length(status_con) >= length(splited_rel)) {
                  left_arg <- left_arg[-iCount]
                  relations <- relations[-iCount]
                  right_arg <- right_arg[-iCount]
                  left_context <- left_context[-iCount]
                  break
                }
              }
            }
          }
        }
        if (length(right_context) >= iCount) {
          for (jCount in 1:length(right_context)) {
            if (length(right_context[[iCount]]) > 0) {
              if (!is.null(right_context[[jCount]]) &
                  length(relations) >= iCount) {
                splited_rel <- str_split(relations[iCount], " ")[[1]]
                splited_rel <-
                  splited_rel[splited_rel != "" & splited_rel != " "]
                
                if (grepl("pron|prp|art", getPostags(splited_rel[length(splited_rel)])))
                  splited_rel <- splited_rel[-length(splited_rel)]
                
                splited_con <-
                  str_split(right_context[[jCount]], " ")[[1]]
                splited_con <-
                  splited_con[splited_con != "" & splited_con != " "]
                status_con <- splited_rel %in% splited_con
                status_con <- status_con[status_con != F]
                
                if (length(status_con) >= length(splited_rel)) {
                  left_arg <- left_arg[-iCount]
                  relations <- relations[-iCount]
                  right_arg <- right_arg[-iCount]
                  right_context <- right_context[-iCount]
                  break
                }
              }
            }
          }
        }
      }
    }
    
    if (length(relations) == 1) {
      if (length(left_context) > 0) {
        for (iCount in 1:length(left_context[[1]])) {
          if (iCount > length(left_context[[1]]))
            break
          else if (is.null(left_context[[1]][iCount]))
            left_context <- left_context[[1]][-iCount]
        }
      }
    }
    
    if (length(relations) == 1) {
      if (length(right_context) > 0) {
        for (iCount in 1:length(right_context[[1]])) {
          if (iCount > length(right_context[[1]]))
            break
          else if (is.null(right_context[[1]][iCount]))
            right_context <- right_context[[1]][-iCount]
        }
      }
    }
    
    if (length(relations) == 0) {
      left_arg <- NULL
      relations <- NULL
      right_arg <- NULL
      left_context <- NULL
      right_context <- NULL
    }
    
    output$left_arg <- left_arg
    output$relations <- relations
    output$right_arg <- right_arg
    output$left_context <- left_context
    output$right_context <- right_context
    output
  }
