
## 2.1 

# Loading the Melville text.
text.v <- scan("data/TextAnalysisWithR/data/plainText/melville.txt",
               what = "character",
               sep = "\n")

# The `scan` function has stored the text in a character vector.
text.v

# The lines of the text are indexed, so let's look at a single line
text.v[1]


## 2.2 Separate Content from Metadata

# The file we loaded has Project Gutenberg metadata. So the actual novel
# beings on line 408. 
start.v <- which(text.v == "CHAPTER 1. Loomings.")
end.v <- which(text.v == "orphan.")
start.v
end.v


# Now we can break things up.
start.metadata.v <- text.v[1:start.v - 1]
end.metadata.v <- text.v[(end.v+1):length(text.v)]
metadata.v <- c(start.metadata.v, end.metadata.v)
novel.lines.v <- text.v[start.v:end.v]

# Now the novel is contained in novel.lines.v, which is a vector with
# each position broken by new lines. Sometimes breaking a text up by
# new lines is helpful, but not in this case.
novel.v <- paste(novel.lines.v, collapse = " ")

# Now `novel.v` is a single string containing the whole novel.
length(novel.lines.v)
length(novel.v)

## 2.3 Reprocessing the Content

# let's convert everything to lower case
novel.lower.v <- tolower(novel.v)

# Now let's split this lowercased blob into words. We will split using the regex
# \W that matches non word characters.
moby.words.1 <- strsplit(novel.lower.v, split = "\\W")

# strsplit returns a "list" not a "vector". It does this because the
# function is designed to handle situations in which the object being
# split is more complex than our use case here.
str(moby.words.1)

# But we do not need a list here so...
moby.word.v <- unlist(moby.words.1)

str(moby.word.v)

# Looking at the result here, we can see that there are some times
# that are empty strings ("").  Let's get rid of those.
not.blanks.v <- which(moby.word.v != "")
moby.word.v <- moby.word.v[not.blanks.v]

# So now we have a vector of words. We can access those positions at will.
moby.word.v[10]
moby.word.v[c(1, 10, 100)]

# A more interesting example. Let's find all whale words.
moby.word.v[which(moby.word.v=="whale")]

## 2.4 Beginning the Analysis

# Let's count the number of occurrences of whale
length(moby.word.v[which(moby.word.v=="whale")])

# Then what about the total number of words or tokens?
length(moby.word.v)

# What percentage of these tokens are whale?
length(moby.word.v[which(moby.word.v=="whale")]) / length(moby.word.v)

# What about the number of unique words
length(unique(moby.word.v))

# But how often does Melville use these words? Which are his favorites?
# Does Moby Dick fit Zipf's law regarding the general frequency of words
# in English?

# We can answer some of these questions with a "contingency" table.
moby.freqs.t <- table(moby.word.v)
sorted.moby.freqs.t <- sort(moby.freqs.t, decreasing = TRUE)



## Practice

# Challenge is to plot the frequence of top ten words to seeing
# if the word usage conforms to Zipf's law.   
plot(sorted.moby.freqs.t[1:10])

# A nicer plot
plot(sorted.moby.freqs.t[1:10],
     type = "b",
     xlab = "Top Ten Words",
     ylab = "Word Count", xaxt = "n")
axis(1,1:10, labels = names(sorted.moby.freqs.t[1:10]))
