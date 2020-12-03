#############################################################
#                                                           #
# This script analyzes Twitter data for variation of the    #
# linguistic variable (lol). Script should be run from the  #
# LaTeX write-up using knitr                                #
#                                                           #
# -Joshua McNeill (joshua dot mcneill at uga dot edu)       #
#                                                           #
#############################################################

## ---- load_packages_functions_data ----
library(knitr)
library(tools)
library(vegan)
library(ggplot2)

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

# Get the mode of a vector
getMode <- function(vector) {
  types <- unique(vector)
  types[which.max(tabulate(match(vector, types)))]
}

## Data
tweets <- read.csv("data/tweetsAnon.csv")
# Subset by orthographic variants of <lol> only
lol <- subset(tweets, grepl("l+(o+|u+|e+|a+w+)l+",
                            lol, perl = TRUE, ignore.case = TRUE))

# Identify active users, producing at least 10 tokens
Active <- data.frame(table(lol$utilisateur) >= 10)
Active$User <- row.names(Active)
usersActive <- Active[Active$table.lol.utilisateur.....10 == TRUE, 2]
rm(Active)
lolActive <- lol[is.element(lol$utilisateur, usersActive),]

# Create a data frame summarizing each community
communitiesModes <- aggregate(lol$lol, list(lol$communaute), getMode)
communitiesDivs <- aggregate(lol$lol, list(lol$communaute), getDiversity)
communitiesSize <- aggregate(lol$utilisateur, list(lol$communaute), unique)
communitiesSize[,3] <- sapply(communitiesSize$x, length)
communitiesModesDivs <- merge(communitiesModes, communitiesDivs, by = 1)
communitiesSummary <- merge(communitiesModesDivs, communitiesSize, by = 1)
rm(list = c("communitiesModes", "communitiesDivs", "communitiesSize", "communitiesModesDivs"))
colnames(communitiesSummary) <- c("Community", "Mode", "Diversity", "Users", "Size")

# Create a data frame summarizing community 2265
lolCommunity2265 <- lol[lol$communaute == 2265,]
lol2265Modes <- aggregate(lolCommunity2265$lol, list(lolCommunity2265$utilisateur), getMode)
lol2265DivByCent <- getDivByCent("lolCommunity2265", "utilisateur", "lol", "PageRank")
lol2265Summary <- merge(lol2265Modes, lol2265DivByCent, by = 1)
lol2265Summary$Tokens <- table(lolCommunity2265$utilisateur)
colnames(lol2265Summary) <- c("User", "Mode", "PageRank", "Diversity", "Tokens")
rm(list = c("lolCommunity2265", "lol2265Modes", "lol2265DivByCent"))

# Create a data frame summarizing each user
usersCommunities <- aggregate(lol$communaute, list(lol$utilisateur), function(x) head(x, n = 1))
usersModes <- aggregate(lol$lol, list(lol$utilisateur), getMode)
usersDivByCent <- getDivByCent("lol", "utilisateur", "lol", "PageRank")
usersCommsModes <- merge(usersCommunities, usersModes, by = 1)
usersSummary <- merge(usersCommsModes, usersDivByCent, by = 1)
rm(list = c("usersCommunities", "usersModes", "usersDivByCent", "usersCommsModes"))
colnames(usersSummary) <- c("User", "Community", "Mode", "PageRank", "Diversity")

## Analysis
## ---- divByPR_graph ----
graphDivByCent("lol", "PageRank")

## ---- divByPR_graph_active ----
graphDivByCent("lolActive", "PageRank")