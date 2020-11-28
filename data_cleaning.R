# Read in tweets
tweets <- read.csv("data/tweets.csv")

# Subset by orthographic variants of <lol> only
lol <- subset(tweets, grepl("l+(o+|u+|e+|a+w+)l+",
                            lol, perl = TRUE, ignore.case = TRUE))
