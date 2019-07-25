## Expressoes regulares para relacao e arugmentos

VERB  <- "(v-fin |v-inf |v-pcp |v-ger )"
ADV   <- "(adv )"
NOUN  <- "(n |prop )"
ADJ   <- "(adj |n-adj |num )"
PRON  <- "(pron-pers |pron-det |pron-indp )"

DET         <- "(det |art | spec )"
PARTICLE    <- "(conj-s |conj-c |intj )"
PREP        <- "(prp )"
INF_MARKER  <- "(EC |- )"

V <- paste0("(", VERB, PARTICLE, "?", ADV, "?)")

W <- paste0("(", NOUN, "|", ADJ, "|", ADV, "|", PRON, "|", DET, ")")

P <- paste0("(", PREP, "|", PARTICLE, "|", INF_MARKER, ")")

VP <- paste0("(", V, P, ")")

VWP <- paste0("(", V, W, "*", P, ")")

PATTERN <- paste0("(", VWP, "|", VP, "|", V, ")")

CHUNK <- "B-NP (I-NP )*(B-PP B-NP (I-NP )*)*"

## Expressoes regulares para inferencia e simetria

#Inferencia

####
# 
# Regra 1: |ARG1| |E-UM    | |ARG2| |QUALQUER RELACAO  | |ARG3| OU |ARG1| |QUALQUER RELACAO| |ARG2| |E-UM    | |ARG3|
# Regra 2: |ARG1| |PARTE-DE| |ARG2| |SIN               | |ARG3| OU |ARG1| |SIN             | |ARG2| |PARTE-DE| |ARG3|
# Regra 3: |ARG1| |LOC     | |ARG2| |PARTE-DE          | |ARG3| OU |ARG1| |LOC             | |ARG2| |PARTE-DE| |ARG3|
# Regra 4: |ARG1| |LOC     | |ARG2| |SIN               | |ARG3| OU |ARG1| |SIN             | |ARG2| |LOC     | |ARG3|
#
####

IS_A    <- "((ser )(um ))"
SIN     <- "((ser )((chamar )|(apelidar )|(conhecer ))((de ){1}|(como ){1}))"
PART_OF <- "(((ser )|(fazer ))(parte )(de ))"
LOC     <- "((ser )(o ))|(((ser ){0,1}|(estar ){0,1}))((localizar )|(situar )|(sediar ))(em )"

#Simetria

####
#
# Regra 1: |ARG1| |SYM| |ARG2|
#
###

# Deixei de fora o verbo transar
SYM_VERBS <-"((acordar )|(arguir )|(argumentar )|(brigar )|(brindar )|(cochichar )|(comerciar )|(conchavar )|(confabular )|(confraternizar )|(contracenar )|(conversar )|(copular )|(dialogar )|(duelar )|(ficar )|(flertar )|(interagir )|(lutar )|(mancomunar )|(noivar )|(pactuar )|(reatar )|(tabelar )|(casar ))+"

SYM_1 <- "((ser )(um ))"
SYM_2 <- "((ser )(o ))"
SYM_3 <- "((ser )((o )|(um ))(de )(o ))"
SYM_4 <- paste0("(", "(ser ){0,1}", SYM_VERBS, "((com ){1}|(de ){1})","(o ){0,1}", ")")

SYM <- paste0("(", SYM_1, "|", SYM_2, "|", SYM_3, "|", SYM_4, ")")
  
## Context

## Conditional Context (Subordinating Context)

#Conjun??es subordinativas causais - por isso, j? que, uma vez que, visto que, visto como
SUBORD_CONJ_CAUSAL<-"( porque | pois | porquanto )"

#Conjun??es subordinativas comparativas - (mais/menos/maior/menor/melhor/pior) do que, (tal) qual, (tanto) quanto
#assim como, bem como, como se
SUBORD_CONJ_COMPARATIVE<-"( como | quanto )"

#Conjun??es subordinativas concessivas - muito embora, ainda que, mesmo que, 
#posto que, bem que, se bem que, apesar de que, nem que.
SUBORD_CONJ_CONCESSIVE<-"( embora | conquanto )"

#Conjun??es subordinativas condicionais - contanto que, salvo se, desde que, a menos que, a n?o ser que
SUBORD_CONJ_CONDITIONAL<-"( se | caso )"

#Conjun??es subordinativas conformativas - etc.
SUBORD_CONJ_CONFORMATIVE<-"( conforme | segundo | consoante )"

#Conjun??es subordinativas consecutivas - tanto que, tal que,  de forma que, de maneira que, de modo que, de sorte que
SUBORD_CONJ_CONSECUTIVE<-"( tanto que )"

#Conjun??es subordinativas finais - para que, a fim de que, porque [para que], que
SUBORD_CONJ_FINALS<-"( porque )"

#Conjun??es subordinativas proporcionais - ? medida que, ao passo que, ? propor??o que, 
#quanto mais . (mais), quanto mais (tanto mais), quanto mais . (menos), quanto mais . 
#(tanto menos), quanto menos . (menos), quanto menos . (tanto menos), 
#quanto menos . (mais), quanto menos . (tanto mais)
SUBORD_CONJ_PROPORTIONAL<-"( enquanto )"

#Conjun??es subordinativas temporais - quando, antes que, depois que, 
#at? que, logo que, sempre que, assim que, desde que, enquanto, 
#todas as vezes que, cada vez que, apenas, mal, que [= desde que], etc.
SUBORD_CONJ_TEMPORAL<-"( quando | enquanto | apenas | que )"

CONTEXT_INTENT<-"( mas | todavia | após | porém | para | em )"

ADV_LEX<-("(bastante |demais |mais |menos |muito |quanto |quão |quase |tanto |pouco |como )")

CONDITIONAL_CONTEXT <- paste0("(", SUBORD_CONJ_CAUSAL, "|",
                              SUBORD_CONJ_COMPARATIVE, "|",
                              SUBORD_CONJ_CONCESSIVE, "|",
                              SUBORD_CONJ_CONDITIONAL, "|",
                              SUBORD_CONJ_CONFORMATIVE, "|",
                              SUBORD_CONJ_CONSECUTIVE, "|",
                              SUBORD_CONJ_FINALS, "|",
                              SUBORD_CONJ_PROPORTIONAL, "|",
                              CONTEXT_INTENT, "|",
                              SUBORD_CONJ_TEMPORAL, ")")

## Context Verbals

VERBAL_CONTEXT <- "(acreditar|pensar|significar|dizer|falar|considerar|conceber)"

## Intention

INT_1<-"(v-fin v-inf v-pcp)"
INT_2<-"(v-fin v-inf)"
