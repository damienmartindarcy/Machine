# Association Rule Analysis using Titanic data

## load dataframe titanic.raw
load("C:/Users/psitlap13/Desktop/titanic.raw.rdata")
## draw a sample of 5 records
idx <- sample(1:nrow(titanic.raw), 5)
titanic.raw[idx, ]

summary(titanic.raw)

# apriori using default settings for support, confidence and lift
library(arules)
rules.all <- apriori(titanic.raw)
inspect(rules.all)

# Rules for 'survived' only - ordered by lift
rules <- apriori(titanic.raw,
                 control = list(verbose=F),
                 parameter = list(minlen=2, supp=0.005, conf=0.8),
                 appearance = list(rhs=c("Survived=No",
                                         "Survived=Yes"),
                                   default="lhs"))
## keep three decimal places
quality(rules) <- round(quality(rules), digits=3)
## order rules by lift
rules.sorted <- sort(rules, by="lift")

# Run - all is revealed
inspect(rules.sorted)

# Some of the rules are redundant - they add nothing to what we know - rules 1 and 2 are an example of this
inspect(rules.sorted[1:2])

## find redundant rules
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag = T)] <- NA
redundant <- colSums(subset.matrix, na.rm = T) >= 1
## which rules are redundant
which(redundant)
## [1] 2 4 7 8
## remove redundant rules
rules.pruned <- rules.sorted[!redundant]

# These are the remaining rules
inspect(rules.pruned)

# Interpreting
inspect(rules.pruned[1])

# In more depth - rules for children
rules <- apriori(titanic.raw, control = list(verbose=F),
                 parameter = list(minlen=3, supp=0.002, conf=0.2),
                 appearance = list(default="none", rhs=c("Survived=Yes"),
                                   lhs=c("Class=1st", "Class=2nd", "Class=3rd",
                                         "Age=Child", "Age=Adult")))
rules.sorted <- sort(rules, by="confidence")
inspect(rules.sorted)

# Visualization
library(arulesViz)
plot(rules.all)

# Grouped Matrix Plot
plot(rules.all, method = "grouped")

# Graph Plot
plot(rules.all, method = "graph")

plot(rules.all, method = "graph", control = list(type = "items"))

# Parallel coordinates 
plot(rules.all, method = "paracoord", control = list(reorder = TRUE))




