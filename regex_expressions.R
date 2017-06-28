# Remove URLs
sentence <- "Now we know it is at http://www.frozengravity.com."
remove_URLs <- function(x) gsub("http[^[:space:]]*", "", x)
sentence <- remove_URLs(sentence)
sentence

# Change dash to space
sentence <- "Hey! What's up? Is the third-base guy around (you know what I mean)?"
change_dash_to_space <- function(x) chartr("-", " ", x)
sentence <- change_dash_to_space(sentence)
sentence

# Remove punctuation
remove_punctuation <- function(x) gsub("[[:punct:]]", "", x)
sentence <- remove_punctuation(sentence)
sentence

# Remove digits
sentence <- "We have lived at 500 Colonial Avenue for 6 years and     my linked in name is rwjones23507"
sentence
remove_digits <- function(x) gsub("[[:digit:]]+", "", x)
sentence <- remove_digits(sentence)
sentence

# Remove control characters
sentence <- "Now we see it
Now we don't"
sentence
remove_control_characters <- function(x) gsub("[[:cntrl:]]", " ", x)
sentence <- remove_control_characters(sentence)
sentence

# Collapse spaces
collapse_spaces <- function(x) gsub("\\s{2,}", " ", x)
sentence <- collapse_spaces(sentence)
sentence

