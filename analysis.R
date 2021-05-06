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

# Draw a bar graph for the distribution of (lol) for a set of users
graphlolDistUsers <- function(df) {
  ggplot(df,
         aes(x = lol)) +
    facet_wrap(. ~ df$communaute + df$utilisateur, ncol = 5) +
    labs(x = "(lol)", y = "Relative Frequency") +
    theme_bw() +
    geom_bar(aes(y = ..count.. / sapply(PANEL, FUN=function(x) sum(count[PANEL == x]))))
}

# Get the mode of a vector
getMode <- function(vector) {
  types <- unique(vector)
  types[which.max(tabulate(match(vector, types)))]
}

## Data
tweets <- read.csv("data/tweetsAnon.csv", encoding = "UTF-8")
usersAll <- read.csv("data/usersAllAnon.csv", encoding = "UTF-8")
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
communitiesSize <- aggregate(usersAll$Id, list(usersAll$modularity_class), unique)
communitiesSize[,3] <- sapply(communitiesSize$x, length)
communitiesModesDivs <- merge(communitiesModes, communitiesDivs, by = 1)
communitiesSummary <- merge(communitiesModesDivs, communitiesSize, by = 1, all.y = FALSE)
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
rm(usersPRPercentile)

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
lolDistUsersLow <- lol[lol$utilisateur == "Leyann" |
                       lol$utilisateur == "Dellanira" |
                       lol$utilisateur == "Jocques" |
                       lol$utilisateur == "Kentoria" |
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
lolDistComms$communaute_ordered <- factor(lolDistComms$communaute, levels = c("1291", "2265", "1032"))
ggplot(lolDistComms,
       aes(x = lol)) +
  facet_wrap(.~lolDistComms$communaute_ordered, ncol = 3) +
  labs(x = "(lol)", y = "Relative Frequency") +
  theme_bw() +
  geom_bar(aes(y = ..count.. / sapply(PANEL, FUN=function(x) sum(count[PANEL == x]))))

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