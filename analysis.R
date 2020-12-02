#############################################################
#                                                           #
# This script analyzes Twitter data for variation of the    #
# linguistic variable (lol). Script should be run from the  #
# LaTeX write-up using knitr                                #
#                                                           #
# -Joshua McNeill (joshua dot mcneill at uga dot edu)       #
#                                                           #
#############################################################

## ---- load_data_packages_functions ----
library(knitr)
library(tools)
library(vegan)
library(ggplot2)

tweets <- read.csv("data/tweetsAnon.csv")
# Subset by orthographic variants of <lol> only
lol <- subset(tweets, grepl("l+(o+|u+|e+|a+w+)l+",
                            lol, perl = TRUE, ignore.case = TRUE))


## Functions
# Get the Simpson's index of diversity for tokens of (lol)
getDiversity <- function(variants){
  divSimpson <- diversity(table(variants), index = "simpson")
  return(divSimpson)
}

# Get Simpson's index of diversity and centrality for each user individually
getDivByCent <- function(df, speaker, variable, centrality){
  diversities <- aggregate(eval(parse(text = paste0(df, "$", variable))),
                           list(eval(parse(text = paste0(df, "$", speaker)))),
                           getDiversity)
  centralities <- aggregate(eval(parse(text = paste0(df, "$", centrality))),
                            list(eval(parse(text = paste0(df, "$", speaker)))),
                            unique)
  divByCent <- merge(centralities, diversities, by = 1)
  colnames(divByCent) <- c(toTitleCase(speaker), toTitleCase(centrality), "Diversity")
  return(divByCent)
}

# Draw a scatter plot comparing Simpon's index of diversity and a centrality measure
graphDivByCent <- function(df, centrality) {
  divByCent <- getDivByCent(df, "utilisateur", "lol", centrality)
  colnames(divByCent) <- c("User", colnames(divByCent)[2:3])
  ggplot(divByCent,
         aes(x = log(eval(parse(text = toTitleCase(centrality)))),
             y = Diversity)) +
    theme_bw() +
    labs(x = paste(toTitleCase(centrality), "(log)")) +
    geom_point()
}

## Analysis
## ---- divByPR_graph ----
graphDivByCent("lol", "PageRank")

## ---- divByPR_above_med_degree ----
lolHighDegree <- lol[lol$degre >= quantile(lol$degre)[3],]
graphDivByCent("lolHighDegree", "PageRank")

## ---- get_modes ----
data.frame(table(lol$lol))[which.max(data.frame(table(lol$lol))[,2]),1]
