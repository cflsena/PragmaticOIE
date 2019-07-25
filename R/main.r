## Sets workspace
work_space <- "/home/cflsena/Documentos/Github/R/PragmaticOIE"
setwd(work_space)

## List of used packages
#list.of.packages <- c("rJava", "stringr", "NLP", "stringi")

## Returns a list of uninstalled packages
#new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

## Install new packages
#if(length(new.packages))
#  install.packages(new.packages)

## Load sentences
dataset <-
  read.table(
    "Sentences/test.txt",
    sep = "\t",
    header = F,
    encoding = "UTF-8",
    quote = ""
  )
# dataset <-
#   read.table(
#     "Sentences/centenfolha_final_200.txt",
#     sep = "\t",
#     header = F,
#     encoding = "UTF-8",
#     quote = ""
#   )
# dataset <-
#   read.table(
#     "Sentences/wikipedia_final_200.txt",
#     sep = "\t",
#     header = F,
#     encoding = "UTF-8",
#     quote = ""
#   )
# dataset <-
#   read.table(
#     "Sentences/wiki_inferential.txt",
#     sep = "\t",
#     header = F,
#     encoding = "UTF-8",
#     quote = ""
#   )
# dataset <-
#   read.table(
#     "Sentences/wiki_context.txt",
#     sep = "\t",
#     header = F,
#     encoding = "UTF-8",
#     quote = ""
#   )
# dataset <-
#   read.table(
#     "Sentences/wiki_intention.txt",
#     sep = "\t",
#     header = F,
#     encoding = "UTF-8",
#     quote = ""
#   )

path_save_file = "Output/"
file_name = "output.csv"

## Create folder to save csv file
if (!file.exists(path_save_file))
  dir.create(file.path(work_space, path_save_file))

## Load used packages
library(rJava)
library(stringr)
library(stringi)
library(NLP)
library(SnowballC)

## Load R sources files
source(file = "R/sentence_information.r")
source(file = "R/extractor.r")
source(file = "R/extraction_tasks.r")
source(file = "R/inferential_extraction.r")
source(file = "R/patterns.r")
source(file = "R/context_extraction.r")
source(file = "R/intentional_extraction.r")

for (iCount in 1:length(dataset[, ]))
{
  result <- getSentenceInformation(dataset, iCount)
  result <- sepInfo(result[1, ])
  relations <- getRelation(result, 2)
  
  if (relations$status == 0) {
    saveEmpytExtractions(c(as.String(dataset[iCount, ])), iCount, path_save_file, file_name)
    next
  }
  
  relations <- mergeRelation(relations, result)
  
  all_left_indexes <- getLeftInterval(result, relations, 1)
  
  all_right_indexes <- getRightInterval(result, relations, 1)
  
  normal_triples <-
    getNormalTriples(result, all_left_indexes, relations, all_right_indexes)
  
  if (length(normal_triples$left_args) == 0 ||
      length(normal_triples$right_args) == 0) {
    saveEmpytExtractions(c(as.String(dataset[iCount, ])), iCount, path_save_file, file_name)
    next
  }
  
  #inferences triples
  inference_triples <-
    getInferencialTriples(normal_triples, result)
  
  normal_contexts <- contextExtraction(normal_triples, result, 2)
  inferential_contexts <-
    contextExtraction(inference_triples, result, 2)
  
  #normal triples with context
  normal_triples <-
    getConntextualTriples(normal_triples, normal_contexts)
  
  #inference triples with context
  inference_triples <-
    getConntextualTriples(inference_triples, inferential_contexts)
  
  intentional_triples <-
    getIntentionalTriples(normal_triples, inference_triples)
  
  #save triples in the csv file
  if (length(normal_triples) == 0 &&
      (length(inference_triples) == 0 ||
       length(intentional_triples) == 0)) {
    saveEmpytExtractions(c(as.String(dataset[iCount, ])), iCount, path_save_file, file_name)
    next
  } else {
    saveExtractions (
      normal_triples,
      inference_triples,
      intentional_triples,
      c(as.String(dataset[iCount, ])),
      path_save_file,
      file_name,
      iCount
    )
  }
}