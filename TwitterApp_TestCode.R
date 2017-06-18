# Check operating system and adapt unicode issues -------------------------

get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}

# Error function for tolower ----------------------------------------------

# http://gastonsanchez.com/visually-enforced/how-to/2012/05/29/Catching-errors-when-using-tolower/

tweets_text = c(
  "Motivation, philosophy and      technique in activism. #Assange and #Occupy: http://t.co/89PFkyjh via @RT_com",
  "No work today, slept through the classes I wanted at the gym. Now I need to find something to occupy my time \ud83d\udc4d\ud83d\ude09",
  "RT @jdavis4100: The Spirit of God and fear never occupy the same space. The presence of one automatically implies the absence of the other...",
  "Police given powers to enter homes http://t.co/VXmtfPV5 and tear down anti- #Olympics posters during Games #Occupy #Anonymous #wakeup #fb",
  "RT @OccupyWallSt: RT @WSOASP12: I quit my job to join the occupy movement. Time to stand up and speak out, I'm not here to make another man rich @Occupy #OWS")

tweets_text

tolower(tweets_text)

# Convert encoding of emoticons
# See: https://stackoverflow.com/questions/9637278/r-tm-package-invalid-input-in-utf8towcs
tweets_text <- iconv(tweets_text, to = "UTF-8-MAC", sub = "byte")
tweets_text

# Remove bad encoding
tweets_text <- gsub("\\x[^**]", "", tweets_text)                # <- To Do
tweets_text

# Remove URLs
tweets_text <- gsub("http[^[:space:]]*", "", tweets_text)
tweets_text

# Remove punctuation
tweets_text = gsub("[[:punct:]]", "", tweets_text)
tweets_text

# Remove control characters
tweets_text = gsub("[[:cntrl:]]", "", tweets_text)
tweets_text

# Remove digits
tweets_text = gsub('\\d+', '', tweets_text)
tweets_text

# Remove other than English or space
tweets_text <- gsub("[^[:alpha:][:space:]]*", "", tweets_text)
tweets_text

# Transform to lower case
tweets_text <- tolower(tweets_text)
tweets_text



