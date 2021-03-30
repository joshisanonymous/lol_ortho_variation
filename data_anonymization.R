#############################################################
#                                                           #
# This script automatically assigns pseudonyms for speakers #
# everywhere their names show up in a data frame, including #
# in utterances.                                            #
#                                                           #
# -Joshua McNeill (joshua dot mcneill at uga dot edu)       #
#                                                           #
#############################################################

# Load packages
library(randomNames)

# Read in raw data
tweets <- read.csv("data/tweets.csv")
usersAll <- read.csv("data/usersAll.csv")

## Functions
# Take a data frame and an anonymization key data frame and attempt to pseudonymize
pseudonymize <- function(df, key) {
  for(name in key$realNames) {
    # Show progress
    if (exists("iteration")) {
      iteration <- iteration + 1
    } else {
      iteration <- 1 
    }
    print(paste("Current iteration:", iteration))
    # Perform task
    if(is.element(key[key$realNames == name, 2], key$realNames)) {
      if(exists("makeNewPseudo")) {
        makeNewPseudo <- c(makeNewPseudo, name)
      } else {
        makeNewPseudo <- name
      }
    } else {
      df <- as.data.frame(apply(df, 2, function(column) gsub(name, key[key$realNames == name, 2], column)))
    }
  }
  if(exists("makeNewPseudo")) {
    print("Your check $makeNewPseudo for people to rename.")
    return(list("newDf" = df, "makeNewPseudo" = makeNewPseudo))
  } else {
    print("All names successfully anonymized.")
    return(df)
  }
}

## Anonymize
# Remove tweet URLs
tweets <- tweets[, 2:ncol(tweets)]

# Remove redundant ID column for users list
usersAll <- usersAll[, c(1, 3:ncol(usersAll))]

# Create key
# fakeNames <- sample(unique(babynames$name), length(unique(tweets$utilisateur)))
fakeNames <- randomNames(length(unique(usersAll$Id)), name.order = "first.last", name.sep = "_")
userKey <- data.frame("realNames" = unique(usersAll$Id),
                      "fakeNames" = fakeNames)

# Apply pseudonym function
tweetsAnon <- pseudonymize(tweets, userKey)
usersAllAnon <- pseudonymize(usersAll, userKey)

# Record key and results
write.csv(userKey, "data/userKey.csv")
write.csv(tweetsAnon, "data/tweetsAnon.csv")
write.csv(usersAllAnon, "data/usersAllAnon.csv")

# Clean up
rm(list = ls())
