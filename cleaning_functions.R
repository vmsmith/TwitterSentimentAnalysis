# Text transformation functions
# http://www.endmemo.com/program/R/gsub.php

remove_emoticons <- function(x) iconv(x, to = "UTF-8-MAC", sub = "byte")
convert_bad_encoding <- function(x) iconv(x, to = "UTF-8-MAC", sub = "byte")
remove_converted_encoding <- function(x) gsub("<..>", "", x)
remove_bad_encoding <- function(x) gsub("\\x[^**]", "", x)

remove_URLs <- function(x) gsub("http[^[:space:]]*", " ", x)
change_dash_to_space <- function(x) chartr("-", " ", x)
remove_punctuation <- function(x) gsub("[[:punct:]]", " ", x)
remove_digits <- function(x) gsub("[[:digit:]]+", "", x)
remove_control_characters <- function(x) gsub("[[:cntrl:]]", " ", x)
collapse_spaces <- function(x) gsub("\\s{2,}", " ", x) # Collapse multiple spaces to one space

remove_pictwittercom <- function(x) gsub("pictwittercom\\w+ *"  , "", x, ignore.case = TRUE)

# Words beginning with "@"
# remove_at <- function(x) gsub("@\\w+", "", x)

# Words beginning with "#"
# remove_hashtag <- function(x) gsub("#\\w+", "", x)
#remove_miscellaneous <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)

convert_to_lower <- function(x) tolower(x)


# Custom Stopwords --------------------------------------------------------

myStopwords <- c("c", "can", "say", "one", "way", "use", "also", "howev", "tell",
                 "will", "much", "need", "take", "tend", "even", "like", "particular",
                 "rather", "said", "get", "well", "make", "ask", "come", "end", 
                 "first", "two", "help", "often", "may", "might", "see", "someth", 
                 "thing", "point", "post", "look", "right", "now", "think", "'ve",
                 "'re", "anoth", "put", "set", "new", "good", "want", "sure", "kind",
                 "larg", "yes", "day", "etc", "quit","sinc","attempt","lack","seen",
                 "awar", "littl","ever","moreov","though","found","abl", "enough",
                 "far","earli","away","achiev","draw", "last","never","brief","bit",
                 "entir","brief", "great","lot")