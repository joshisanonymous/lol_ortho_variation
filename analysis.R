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

# Create a data frame summarizing each user
usersCommunities <- aggregate(lol$communaute, list(lol$utilisateur), function(x) head(x, n = 1))
usersModes <- aggregate(lol$lol, list(lol$utilisateur), getMode)
usersDivByCent <- getDivByCent("lol", "utilisateur", "lol", "PageRank")
usersCommsModes <- merge(usersCommunities, usersModes, by = 1)
usersSummary <- merge(usersCommsModes, usersDivByCent, by = 1)
usersSummary$Tokens <- table(lol$utilisateur)
rm(list = c("usersCommunities", "usersModes", "usersDivByCent", "usersCommsModes"))
colnames(usersSummary) <- c("User", "Community", "Mode", "PageRank", "Diversity", "Tokens")

# Create a data frame summarizing community 2265
usersSummary2265 <- usersSummary[usersSummary$Community == 2265,]

# Create a data frame summarizing active users, producing at least 10 tokens
usersSummaryActive <- usersSummary[usersSummary$Tokens >= 10,]

# Subset data frame including only communities that users with the 5 highest and
# 5 lowest PageRanks belong to
lolDistComms <- lol[lol$communaute == 2265 |
                    lol$communaute == 1291 |
                    lol$communaute == 1032,]
# Grab only variants of (lol) over 1
variants <- table(lolDistComms$lol)
variantsGT <- names(variants[variants > 5])
variantsLT <- names(variants[variants <= 5])
lolDistComms <- lolDistComms[lolDistComms$lol == "lol" |
                             lolDistComms$lol == "Lol" |
                             lolDistComms$lol == "LOL",]

# Subset data frame including only users with the 5 highest PageRanks
lolDistUsersHigh <- lol[lol$utilisateur == "Rithanya" |
                        lol$utilisateur == "Amair" |
                        lol$utilisateur == "Rheya" |
                        lol$utilisateur == "Saadiya" |
                        lol$utilisateur == "Seprina",]
lolDistUsersHigh <- lolDistUsersHigh[lolDistUsersHigh$lol == "lol" |
                                     lolDistUsersHigh$lol == "Lol" |
                                     lolDistUsersHigh$lol == "LOL",]

# Subset data frame including only users with the 5 lowest PageRanks
lolDistUsersLow <- lol[lol$utilisateur == "Kentoria" |
                       lol$utilisateur == "Leyann" |
                       lol$utilisateur == "Weatherly" |
                       lol$utilisateur == "Yamira" |
                       lol$utilisateur == "Yogi",]
lolDistUsersLow <- lolDistUsersLow[lolDistUsersLow$lol == "lol" |
                                   lolDistUsersLow$lol == "Lol" |
                                   lolDistUsersLow$lol == "LOL",]

## Analysis
## ---- divByPR_graph ----
graphDivByCent("lol", "PageRank")

## ---- divByPR_graph_active ----
graphDivByCent("lolActive", "PageRank")


## ---- lol_dist_comms ----
ggplot(lolDistComms,
       aes(x = lol)) +
  facet_wrap(.~lolDistComms$communaute, ncol = 3) +
  labs(x = "(lol)", y = "Relative Frequency") +
  theme_bw() +
  geom_bar(aes(y = ..count.. / sapply(PANEL, FUN=function(x) sum(count[PANEL == x]))))

## ---- lol_dist_users_high ----
ggplot(lolDistUsersHigh,
       aes(x = lol)) +
  facet_wrap(.~lolDistUsersHigh$utilisateur, ncol = 5) +
  labs(x = "(lol)", y = "Relative Frequency") +
  theme_bw() +
  geom_bar(aes(y = ..count.. / sapply(PANEL, FUN=function(x) sum(count[PANEL == x]))))

## ---- lol_dist_users_low ----
ggplot(lolDistUsersLow,
       aes(x = lol)) +
  facet_wrap(.~lolDistUsersLow$utilisateur, ncol = 5) +
  labs(x = "(lol)", y = "Relative Frequency") +
  theme_bw() +
  geom_bar(aes(y = ..count.. / sapply(PANEL, FUN=function(x) sum(count[PANEL == x]))))
