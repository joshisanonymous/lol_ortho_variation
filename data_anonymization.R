library(babynames)

# Read in tweets
tweets <- read.csv("data/tweets.csv")

realNames <- c("Joe", "Joe", "Paul", "Mary", "Claire", "Frank", "Joe")
fakeNames <- sample(unique(babynames$name), length(unique(realNames)))

key <- data.frame("realNames" = unique(realNames),
                  "fakeNames" = fakeNames)

key[1, 2] <- "Claire"

lapply(unique(realNames), function(x) gsub(x, key[key$realNames == x, 2], realNames))
gsub("Joe", key[key$realNames == "Joe", 2], realNames)
key[key$realNames == "Joe", 2]
