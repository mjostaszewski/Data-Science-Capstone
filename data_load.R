library(tm)
"%l%" <- function(a,b) paste(a,b,sep = "")
setwd("~/Work/Projects/Coursera/Capstone/project")

# Custom mapping of characters
global_latin1_map <- read.table("latin1_map.txt", stringsAsFactors = F, header = T, sep = "\t", quote = "", comment.char = "")
# Stopwords "'stop-words.txt' is constructed from internet sources, referenced in the file."
# global_stopwords <- unique(stopwords("SMART"), grep('^#', readLines("stop-words.txt"), invert = T, value = T))
global_stopwords <- stopwords()
# Bad words list, compiled from three sources, see file for references
global_badwords <- unique(grep('^#', readLines("obscene-words.txt"), invert = T, value = T))
# Words to skip is a manually curated list of most popular quirks after previous cleaning steps
global_skipwords <- c(readLines("words-to-skip.txt"), letters)

tm_rw_long <- function(x, strings_to_replace, batch_size = 100, verbose = F) {
  ret <- x
  iter <- c(seq(0, length(strings_to_replace), by = batch_size), length(strings_to_replace))
  for(i in 2:length(iter)) {
    if(verbose) { print(iter[i] %l% " of " %l% length(strings_to_replace) %l% ": " %l% date()) }
    ret <- removeWords(ret, strings_to_replace[(iter[i-1]+1):(iter[i])]) 
  }
  ret
}

reload_saved <- function(fx) {
  en_blog_lines <- readLines("./" %l% fx %l%  "/en_US/en_US.blogs.txt", encoding = "latin1")
  save(en_blog_lines, file = "./" %l% fx %l%  "/en_blog_lines.Rdata")
  en_news_lines <- readLines("./" %l% fx %l%  "/en_US/en_US.news.txt", encoding = "latin1")
  save(en_news_lines, file = "./" %l% fx %l%  "/en_news_lines.Rdata")
  en_twitter_lines <- readLines("./" %l% fx %l%  "/en_US/en_US.twitter.txt", encoding = "latin1", skipNul = TRUE)
  save(en_twitter_lines, file = "./" %l% fx %l%  "/en_twitter_lines.Rdata")
}

prep_text <- function(flines, clean_map = NULL, remove_stopwords = F, remove_skipwords = F, verbose = F) {
  if(verbose) { print("Cleaning a set of " %l% length(flines) %l% " lines") }
  ret <- gsub("([[:upper:]])", "\\L\\1", flines, perl = TRUE)
  if(verbose) { print("Clean special characters: " %l% date()) }
  if(!is.null(clean_map)) {
    for(term in 1:nrow(clean_map)) {
      ret <- gsub(clean_map[term,1], clean_map[term,2], ret, fixed = endsWith(clean_map[term,3], "fixed"))
    }
  }
  #Clean remaining special symbols
  ret <- gsub('<[a-z0-9][a-z0-9]>', '', ret)
  if(verbose) { print("Remove wordlists: " %l% date()) }
  if(remove_stopwords) {
    # Remove stopwords, "'stop-words.txt' is constructed from internet sources, referenced in the file."
    print("Remove stopwords")
    ret <- tm_rw_long(ret, global_stopwords) 
  }
  # Remove badwords, "'obscene-words.txt' is constructed from internet sources, referenced in the file."
  print("Remove badwords")
  ret <- tm_rw_long(ret, global_badwords)
  # Remove punctation, digits and fix the multi-space characters; concatenate apostrophes first
  ret <- gsub('\'', '', ret)
  ret <- gsub('[[:punct:]]', ' ', ret)
  ret <- gsub('[[:digit:]]', ' ', ret)
  if(remove_skipwords) {
    # Words to skip is a manually curated list of most popular quirks after previous cleaning steps
    print("Remove skipwords")
    ret <- tm_rw_long(ret, global_skipwords)
  }
  # Fix the spaces
  ret <- gsub(' +', ' ', ret)
  ret <- gsub('^ | $', '', ret)
  if(verbose) { print("Done: " %l% date()) }
  return(ret)
}

create_corpus <- function(remove_wordlists = F, prefix = "", save) {
  load("./final/en_blog_lines.Rdata")
  load("./final/en_news_lines.Rdata")
  load("./final/en_twitter_lines.Rdata")
  print("Creating clean corpus: " %l% date())
  clean_corpus <- list(
    blogs = prep_text(en_blog_lines, clean_map = global_latin1_map, verbose = T, remove_stopwords = remove_wordlists, remove_skipwords = remove_wordlists),
    news = prep_text(en_news_lines, clean_map = global_latin1_map, verbose = T, remove_stopwords = remove_wordlists, remove_skipwords = remove_wordlists),
    twitter = prep_text(en_twitter_lines, clean_map = global_latin1_map, verbose = T, remove_stopwords = remove_wordlists, remove_skipwords = remove_wordlists)
  )
  if(prefix != "")  {
    save(clean_corpus, file = "./final/" %l% prefix %l% "_" %l% "clean_corpus.Rdata")
    print("Corpus saved: " %l% date())
  }
  return(clean_corpus)
}

trim_by_grep <- function(fcorpus, pattern) {
  fcorpus$blogs <- fcorpus$blogs[grep(pattern, fcorpus$blogs)]
  fcorpus$news <- fcorpus$news[grep(pattern, fcorpus$news)]
  fcorpus$twitter <- fcorpus$twitter[grep(pattern, fcorpus$twitter)]
  return(fcorpus)
}

create_subset <- function(fcorpus, fraction = 0.05) {
  return(list(
    blogs = fcorpus$blogs[as.logical(rbinom(length(fcorpus$blogs),1,fraction))],
    news = fcorpus$news[as.logical(rbinom(length(fcorpus$news),1,fraction))],
    twitter = fcorpus$twitter[as.logical(rbinom(length(fcorpus$twitter),1,fraction))]
  ))
}

tokenize_to_table <- function(flines, splitstring = " |\t", fdecreasing = T) {
  ret <- unlist(strsplit(flines, split = " |\t"))
  ret <- sort(table(ret[ret != ""]), decreasing = fdecreasing)
  return(ret)
}

tokenize_corpus <- function(fcorpus, fdecreasing = T) {
  ret <- list(blogs = "", news = "", twitter = "", all = "")
  ret$blogs <- tokenize_to_table(fcorpus$blogs, fdecreasing)
  # unlist(strsplit(fcorpus$blogs, split = " |\t"))
  # ret$blogs <- sort(table(ret$blogs[ret$blogs != ""]), decreasing = fdecreasing)
  ret$news <- tokenize_to_table(fcorpus$news, fdecreasing)
  # ret$news <- unlist(strsplit(fcorpus$news, split = " |\t"))
  # ret$news <- sort(table(ret$news[ret$news != ""]), decreasing = fdecreasing)
  ret$twitter <- tokenize_to_table(fcorpus$twitter, fdecreasing)
  # ret$twitter <- unlist(strsplit(fcorpus$twitter, split = " |\t"))
  # ret$twitter <- sort(table(ret$twitter[ret$twitter != ""]), decreasing = fdecreasing)
  ret$all <- tokenize_to_table(unlist(fcorpus), fdecreasing)
  # ret$all <- unlist(strsplit(unlist(fcorpus), split = " |\t"))
  # ret$all <- sort(table(ret$all[ret$all != ""]), decreasing = fdecreasing)
  return(ret)
}

my_ngram <- function(lines, ngram) {
  ret <- sapply(strsplit(lines, " ", fixed = TRUE),
                function(x) vapply(ngrams(x, ngram), paste, "", collapse = " "))
  ret <- sort(table(unlist(ret)), decreasing = T)
  ret <- data.frame(ngrams = names(ret), freq = as.vector(ret), stringsAsFactors = F)
  return(ret)
}

ngramize_corpus <- function(fcorpus) {
  return(list(unigrams = tokenize_corpus(fcorpus)$all,
              digrams = my_ngram(unlist(fcorpus), n = 2),
              trigrams = my_ngram(unlist(fcorpus), n = 3),
              quadgrams = my_ngram(unlist(fcorpus), n = 4)))
}

stem_corpus <- function(fcorpus) {
  return(list(blogs = stemDocument(fcorpus$blogs),
              news = stemDocument(fcorpus$news),
              twitter = stemDocument(fcorpus$twitter)))
}

reduce_ngram_corpus <- function(fcorpus, unigram_percentage = 0.95, min_freqs = list(digram = 2, trigram = 2, quadgram = 2)) {
  ret <- fcorpus
  ret$unigrams <- ret$unigrams[which(cumsum(ret$unigrams) < 0.95*sum(ret$unigrams))]
  ret$digrams <- ret$digrams[ret$digrams[,2] >= min_freqs$digram,]
  ret$trigrams <- ret$trigrams[ret$trigrams[,2] >= min_freqs$trigram,]
  ret$quadgrams <- ret$quadgrams[ret$quadgrams[,2] >= min_freqs$quadgram,]
  return(ret)
}

ngram_to_lookup_fast <- function(fngrams, lookup_list) {
  tmp <- sapply(strsplit(fngrams, split = " "), unlist)
  if(is.null(dim(tmp))) { tmp <- t(tmp) }
  tmp <- t(tmp)
  ret <- array(0, dim = dim(tmp))
  for(lti in seq_along(lookup_list)) { ret[tmp == lookup_list[lti]] = lti }
  return(ret)
}

numerize_ngram_corpus <- function(fcorpus) {
  unigrams_df <- as.data.frame(fcorpus$unigrams)
  manygrams_df <- as.data.frame(table(unlist(strsplit(unlist(sapply(fcorpus[-1], function(x) x$ngrams)), " "))))
  colnames(manygrams_df) <- colnames(unigrams_df) <- c("word", "freq")
  allwords_df <- merge(unigrams_df, manygrams_df, by = "word", all = T)
  allwords_df <- cbind(allwords_df, sumfreq = rowSums(allwords_df[,2:3], na.rm = T))
  allwords_df <- as.character(allwords_df[order(allwords_df$sumfreq, decreasing = T),1])
  ret <- fcorpus
  ret$unigrams <- cbind(ngram_to_lookup_fast(names(ret$unigrams), allwords_df), freq = ret$unigrams)
  ret$digrams <- cbind(ngram_to_lookup_fast(ret$digrams[,1], allwords_df), freq = ret$digrams[,2])
  ret$trigrams <- cbind(ngram_to_lookup_fast(ret$trigrams[,1], allwords_df), freq = ret$trigrams[,2])
  ret$quadgrams <- cbind(ngram_to_lookup_fast(ret$quadgrams[,1], allwords_df), freq = ret$quadgrams[,2])
  ret = list(corpus = ret, lookup_table = allwords_df)
  ret
}

create_corpus_with_stopwords <- function(destdir = "final") {
  this_corpus <- create_corpus(remove_wordlists = F, "with_stopwords")
  set.seed(123)
  #Reduce clean corpus to at minimum ngrams
  "Removing lines with less than 5 words " %l% date()
  this_corpus <- trim_by_grep(this_corpus, "([a-z]+( )+){4,}[a-z]+")
  "Done " %l% date()
  small_corpus <- create_subset(this_corpus, fraction = 0.1)
  sc_ngrams <- ngramize_corpus(small_corpus)
  save(sc_ngrams, file = "./" %l% destdir %l% "/with_stopwords_short_corpus.Rdata")
  sc_red_ngrams <- reduce_ngram_corpus(sc_ngrams)
  save(sc_red_ngrams, file = "./" %l% destdir %l% "/with_stopwords_short_corpus_reduced.Rdata")
}

create_corpus_without_stopwords <- function(destdir = "final") {
  this_corpus <- create_corpus(remove_wordlists = T, "without_stopwords")
  set.seed(123)
  #Reduce clean corpus to at minimum ngrams
  "Removing lines with less than 5 words " %l% date()
  this_corpus <- trim_by_grep(this_corpus, "([a-z]+( )+){4,}[a-z]+")
  "Done " %l% date()
  small_corpus <- create_subset(this_corpus, fraction = 0.1)
  sc_ngrams <- ngramize_corpus(small_corpus)
  save(sc_ngrams, file = "./" %l% destdir %l% "/with_stopwords_short_corpus.Rdata")
  sc_red_ngrams <- reduce_ngram_corpus(sc_ngrams)
  save(sc_red_ngrams, file = "./" %l% destdir %l% "/with_stopwords_short_corpus_reduced.Rdata")
}

