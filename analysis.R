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
library(ggplot2)
library(igraph)
library(vegan)
library(sentimentr)

# Data
source("data_cleaning.R")

# Remove scientific notation
options(scipen = 999)

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
graphDivByCent <- function(df, centrality, xlab) {
  divByCent <- getDivByCent(df, "User", "lol", centrality)
  ggplot(divByCent,
         aes(x = eval(parse(text = toTitleCase(centrality))),
             y = Diversity)) +
    theme_bw() +
    labs(x = xlab) +
    geom_point()
}

# Draw a bar graph for the distribution of (lol) for a set of users
graphlolDistUsers <- function(df) {
  ggplot(df,
         aes(x = lol)) +
    facet_wrap(. ~ df$Community + df$User, ncol = 5) +
    labs(x = "(lol)", y = "Relative Frequency") +
    theme_bw() +
    geom_bar(aes(y = ..count.. / sapply(PANEL, FUN = function(x) sum(count[PANEL == x]))))
}

# Get the mode of a vector
getMode <- function(vector) {
  types <- unique(vector)
  types[which.max(tabulate(match(vector, types)))]
}

## Summary data frames and subsets
# Get sentiments for tweets with (lol) using default polarity dictionary (doesn't include <lol>)
lol$Sentiment <- sentiment_by(lol$Text)$ave_sentiment
# Data frame for only non-zero sentiments for the three main (lol) variants
lolSentMajorVars <- lol[lol$lol == "lol" | lol$lol == "LOL" | lol$lol == "Lol",]
lolSentMajorVars <- lolSentMajorVars[lolSentMajorVars$Sentiment != 0,]
# Get the means for each variant
lolSentMeans <- tapply(lolSentMajorVars$Sentiment, lolSentMajorVars$lol, mean)

# Identify active users, producing at least 10 tokens
Active <- data.frame(table(lol$User) >= 10)
Active$User <- row.names(Active)
usersActive <- Active[Active$table.lol.User.....10 == TRUE, 2]
rm(Active)
lolActive <- lol[is.element(lol$User, usersActive),]

# Create a data frame summarizing each community
communitiesModes <- aggregate(lol$lol, list(lol$Community), getMode)
communitiesDivs <- aggregate(lol$lol, list(lol$Community), getDiversity)
communitiesSize <- aggregate(usersAll$User, list(usersAll$Community), unique)
communitiesSize[,3] <- sapply(communitiesSize$x, length)
communitiesModesDivs <- merge(communitiesModes, communitiesDivs, by = 1)
communitiesSummary <- merge(communitiesModesDivs, communitiesSize, by = 1, all.y = FALSE)
rm(list = c("communitiesModes", "communitiesDivs", "communitiesSize", "communitiesModesDivs"))
colnames(communitiesSummary) <- c("Community", "Mode", "Diversity", "Users", "Size")

# Create a data frame summarizing each province
provincesModes <- aggregate(lol$lol, list(lol$Province), getMode)
provincesDivs <- aggregate(lol$lol, list(lol$Province), getDiversity)
provincesSize <- aggregate(lol$User, list(lol$Province), unique)
provincesSize[,3] <- sapply(provincesSize$x, length)
provincesModesDivs <- merge(provincesModes, provincesDivs, by = 1)
provincesSummary <- merge(provincesModesDivs, provincesSize, by = 1, all.y = FALSE)
rm(list = c("provincesModes", "provincesDivs", "provincesSize", "provincesModesDivs"))
colnames(provincesSummary) <- c("Province", "Mode", "Diversity", "Users", "Size")
provincesSummary <- provincesSummary[provincesSummary$Province != "Undefined",]

# Create a data frame summarizing each user
usersCommunities <- aggregate(lol$Community, list(lol$User), function(x) head(x, n = 1))
usersModes <- aggregate(lol$lol, list(lol$User), getMode)
usersDivByCent <- getDivByCent("lol", "User", "lol", "PageRank")
usersCommsModes <- merge(usersCommunities, usersModes, by = 1)
usersSummary <- merge(usersCommsModes, usersDivByCent, by = 1)
usersSummary$Tokens <- table(lol$User)
rm(list = c("usersCommunities", "usersModes", "usersDivByCent", "usersCommsModes"))
colnames(usersSummary) <- c("User", "Community", "Mode", "PageRank", "Diversity", "Tokens")
usersSummary <- merge(usersSummary, communitiesSummary[, c("Community", "Diversity")], by = "Community")
colnames(usersSummary) <- c("Community", "User", "Mode", "PageRank", "Diversity", "Tokens", "Diversity_Comm")
# Add the PageRank percentile for each user
for (community in unique(usersSummary$Community)) {
  sub <- subset(usersSummary, usersSummary$Community == community)
  for (user in unique(sub$User)) {
    if (!exists("usersPRPercentile")) {
      usersPRPercentile <- data.frame(User = user,
                                      PR_Percentile = ecdf(sub$PageRank)(sub[sub$User == user,]$PageRank))
    } else {
      usersPRPercentile <- rbind(usersPRPercentile,
                                 data.frame(User = user,
                                            PR_Percentile = ecdf(sub$PageRank)(sub[sub$User == user,]$PageRank)))
    }
  }
}
rm(list = c("community", "sub", "user"))
usersSummary <- merge(usersSummary, usersPRPercentile, by = "User")

# Add the PageRank Percentiles to the (lol) observations
lol <- merge(lol, usersPRPercentile, by = "User")
lolActive <- merge(lolActive, usersPRPercentile, by = "User")
rm(usersPRPercentile)

# Create a data frame summarizing community 2265
usersSummary2265 <- usersSummary[usersSummary$Community == 2265,]

# Create a data frame summarizing active users, producing at least 10 tokens
usersSummaryActive <- usersSummary[usersSummary$Tokens >= 10,]

# Subset data frame including only communities that users with the 5 highest and
# 5 lowest PageRanks belong to
lolDistComms <- lol[lol$Community == 2265 |
                    lol$Community == 1291 |
                    lol$Community == 1032,]
# Grab only variants of (lol) over 1
variants <- table(lolDistComms$lol)
variantsGT <- names(variants[variants > 5])
variantsLT <- names(variants[variants <= 5])
lolDistComms <- lolDistComms[lolDistComms$lol == "lol" |
                             lolDistComms$lol == "Lol" |
                             lolDistComms$lol == "LOL",]

# Subset data frame including only users with the 5 highest PageRanks
lolDistUsersHigh <- lol[lol$User == usersSummaryActive[order(usersSummaryActive$PageRank, decreasing = TRUE),][1, "User"] |
                        lol$User == usersSummaryActive[order(usersSummaryActive$PageRank, decreasing = TRUE),][2, "User"] |
                        lol$User == usersSummaryActive[order(usersSummaryActive$PageRank, decreasing = TRUE),][3, "User"] |
                        lol$User == usersSummaryActive[order(usersSummaryActive$PageRank, decreasing = TRUE),][4, "User"] |
                        lol$User == usersSummaryActive[order(usersSummaryActive$PageRank, decreasing = TRUE),][5, "User"],]
lolDistUsersHigh <- lolDistUsersHigh[lolDistUsersHigh$lol == "lol" |
                                     lolDistUsersHigh$lol == "Lol" |
                                     lolDistUsersHigh$lol == "LOL",]

# Subset data frame including only users with the 5 lowest PageRanks
lolDistUsersLow <- lol[lol$User == usersSummaryActive[order(usersSummaryActive$PageRank),][5, "User"] |
                       lol$User == usersSummaryActive[order(usersSummaryActive$PageRank),][4, "User"] |
                       lol$User == usersSummaryActive[order(usersSummaryActive$PageRank),][3, "User"] |
                       lol$User == usersSummaryActive[order(usersSummaryActive$PageRank),][2, "User"] |
                       lol$User == usersSummaryActive[order(usersSummaryActive$PageRank),][1, "User"],]
lolDistUsersLow <- lolDistUsersLow[lolDistUsersLow$lol == "lol" |
                                   lolDistUsersLow$lol == "Lol" |
                                   lolDistUsersLow$lol == "LOL",]

## Analysis
## ---- example_community ----
par(mar = c(2, 4, 3, 2))
plot.igraph(
  graph(edges = c("Ted", "Bob", "Bob", "Joe", "Joe", "Kelly", "Kelly", "Bob"), directed = FALSE),
  layout = layout_with_kk,
  vertex.color = "yellow",
  vertex.label.dist = 4
)

## ---- divByPR_graph_active ----
lolActive$PageRankLog <- log(lolActive$PageRank)
graphDivByCent("lolActive", "PageRankLog", "PageRank (log)")

## ---- divByPRPerc_graph_active ----
graphDivByCent("lolActive", "PR_Percentile", "PageRank Percentile")

## ---- lol_dist_comms ----
lolDistComms$community_ordered <- factor(lolDistComms$Community, levels = c("1291", "2265", "1032"))
ggplot(lolDistComms,
       aes(x = lol)) +
  facet_wrap(.~lolDistComms$community_ordered, ncol = 3) +
  labs(x = "(lol)", y = "Relative Frequency") +
  theme_bw() +
  geom_bar(aes(y = ..count.. / sapply(PANEL, FUN = function(x) sum(count[PANEL == x]))))

## ---- lol_dist_users_high ----
graphlolDistUsers(lolDistUsersHigh)

## ---- lol_dist_users_low ----
graphlolDistUsers(lolDistUsersLow)

## ---- div_individual_community ----
ggplot(usersSummaryActive,
       aes(x = Diversity,
           y = Diversity_Comm)) +
  scale_x_continuous(limits = c(0, 0.75)) +
  scale_y_continuous(limits = c(0, 0.75)) +
  labs(x = "Individual Diversity", y = "Community Diversity") +
  theme_bw() +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)

## ---- sentiment_lol_hist ----
ggplot(lolSentMajorVars,
       aes(x = Sentiment, y = stat(density * width))) +
  facet_grid(lol ~ .) +
  labs(y = "Density") +
  theme_bw() +
  geom_histogram()
