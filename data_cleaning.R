############################################################
# This script cleans up the corpus, mostly to translate it #
# into English where it's in French.                       #
#                                                          #
# -Joshua McNeill, joshua dot mcneill at uga dot edu       #
############################################################

# Load data
tweets <- read.csv("data/tweetsAnon.csv", encoding = "UTF-8")
usersAll <- read.csv("data/usersAllAnon.csv", encoding = "UTF-8")

# Translate column names
colnames(tweets) <- c("Token ID", "User", "Betweenness", "Community", "Indegree",
                      "Outdegree", "Degree", "Weighted Indegree", "Weighted Outdegree",
                      "Weighted Degree", "Eccentricity", "Closeness", "Harmonic Closeness",
                      "PageRank", "Eigenvector", "Clustering", "Text", "lol", "Category",
                      "Language Before", "Language After", "My Language", "Auto Language",
                      "More Than One Token", "Source", "Tweets", "Follows", "Followers",
                      "Country", "Province", "City", "Time Zone", "Coder")
colnames(usersAll) <- c("User ID", "User", "Timeset", "Indegree", "Outdegree", "Degree",
                        "Weighted Indegree", "Weighted Outdegree", "Weighted Degree",
                        "Eccentricity", "Closeness", "Harmonic Closeness", "Betweenness",
                        "PageRank", "Community", "Eigenvector", "Clustering")

# Translate province names
tweets$Province <- gsub("Angleterre", "England", tweets$Province)
tweets$Province <- gsub("Brabant-Septentrional", "North Brabant", tweets$Province)
tweets$Province <- gsub("Californie", "California", tweets$Province)
tweets$Province <- gsub("Ile-du-Prince-Edouard", "Prince Edward Island", tweets$Province)
tweets$Province <- gsub("Iles Vierges des Etats-Unis", "US Virgin Islands", tweets$Province)
tweets$Province <- gsub("Nouveau-Brunswick", "New Brunswick", tweets$Province)
tweets$Province <- gsub("Nouvelle-Ecosse", "Nova Scotia", tweets$Province)
tweets$Province <- gsub("Provence-Alpes-Cote d'Azur", "Provence-Alps-French Riviera", tweets$Province)
tweets$Province <- gsub("indefini", "Undefined", tweets$Province)

# Reformat usernames to get rid of underscores
tweets$User <- gsub("_", " ", tweets$User, fixed = TRUE)

# Subset by orthographic variants of <lol> only
lol <- subset(tweets, grepl("l+(o+|u+|e+|a+w+)l+", lol,
                            perl = TRUE, ignore.case = TRUE))