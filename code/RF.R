setwd(choose.dir())
HRC <- read.table(file.choose(), header = FALSE, stringsAsFactors = FALSE)
HRC_train <- HRC

stopwords_from_online = scan(paste0(getwd(), "/common-english-words.txt"), what = "character", sep = ",")
stopwords_builtin = stopwords()
stopwords = unique(append(stopwords_from_online, stopwords_builtin))

remove_extra = function(x) {
  # Removing multiple punctuations, numbers, single/two-lettered words and whitespaces. (In that order)
  x <- gsub("[[:punct:]]", "", x) # Removes punctuations
  x <- gsub("[[:digit:]]", "", x) # Removes numbers
  x <- gsub("*\\b[[:alpha:]]{1,2}\\b *", " ", x) # Removes any words with 1/2 letters
  #x <- gsub("\\b(\\S+?)\\1\\S*\\b", "", x, perl = TRUE) # Removes any words with repeating letters
  x <- gsub("\\s+", " ", x) # Removes multiple whitespaces.
  return(x)
}
exclude_redundancy = function(x) {
  # Create a character vector that consists of meaningless word chunks.
  redun <- c("unclassified u.s. department of state case no.", "doc no.", "date:",
             "state dept. - produced to house select benghazi comm. subject to agreement on sensitive information & redactions.", "no foia waiver.", "subject:", "sent:", "monday",
             "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday", "f-",
             "release in full", "release in part", "state-", "january", "february", 
             "march", "april", "may", "june", "july", "august", "september", "october", 
             "november", "december", "subject", "fvv", "sent", "scb")
  # Remove elements in redun from the email set
  x <- tolower(x)
  for (red in redun) {
    x <- gsub(red, "", x)
  }
  # Since we don't need any words more than once.
  x <- toString(unique(unlist(strsplit(x, split = " ", fixed = T))))
  return(x)
}
for (email in 1:nrow(HRC_train)) {
  HRC_train[email, 2] <- exclude_redundancy(HRC_train[email, 2])
  HRC_train[email, 2] <- remove_extra(HRC_train[email, 2])
  HRC_train[email,2] <- removeWords(HRC_train[email, 2], stopwords)
}

dtm = DocumentTermMatrix(Corpus(VectorSource(HRC_train$V2)),control = list(stemming=T))
dtm = removeSparseTerms(dtm, .97)
dtm = as.data.frame(as.matrix(dtm))
dtm[,(colnames(dtm) %in% stopwords)==TRUE] <- rm()
dtm$sender <- HRC$V1

rf.cv <- rfcv(dtm[,-ncol(dtm)], dtm$sender, cv.fold=5)

with(rf.cv, plot(n.var, error.cv, log="x", type="o", lwd=2))

