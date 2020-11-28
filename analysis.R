## ---- load_data_packages_functions ----
library(knitr)
library(tools)
library(vegan)
library(ggplot2)

tweets <- read.csv("data/tweets.csv")

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

## Analysis

## ---- divByPR_graph ----
divByPR <- getDivByCent("tweets", "utilisateur", "lol", "PageRank")
colnames(divByPR) <- c("User", colnames(divByPR)[2:3])
ggplot(divByPR,
       aes(x = log(PageRank),
           y = Diversity)) +
  geom_point()
