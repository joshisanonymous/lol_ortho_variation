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
  matrix <- as.matrix(df)
  iteration <- 1 # For progress indicator
  for(name in key$realNames) {
    # Show progress
    print(paste(iteration, "people renamed | renaming", name))
    iteration <- iteration + 1
    # Perform task
    if(is.element(key[key$realNames == name, "fakeNames"], matrix)) {
      if(exists("makeNewPseudo")) {
        makeNewPseudo <- c(makeNewPseudo, name)
      } else {
        makeNewPseudo <- name
      }
    } else {
      # df <- gsub(name, key[key$realNames == name, 2], df, fixed = TRUE)
      matrix <- gsub(name, key[key$realNames == name, "fakeNames"], matrix, fixed = TRUE)
    }
  }
  dfProcessed <- as.data.frame(matrix)
  if(exists("makeNewPseudo")) {
    print("Your check $makeNewPseudo for people to rename.")
    return(list("newDf" = dfProcessed, "makeNewPseudo" = makeNewPseudo))
  } else {
    print("All names successfully anonymized.")
    return(dfProcessed)
  }
}

## Anonymize
# Remove tweet URLs
tweets <- tweets[, 2:ncol(tweets)]

# Remove redundant ID column for users list
usersAll <- usersAll[, c(1, 3:ncol(usersAll))]

# Create key
fakeNames <- randomNames(length(unique(tweets$utilisateur)), name.order = "first.last", name.sep = "_")
userKey <- data.frame("realNames" = unique(tweets$utilisateur),
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
