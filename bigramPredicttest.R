predict_tri <- function(input) {
        input <- cleanInput(input)
        x <- TrigramProb[TrigramProb[,Bigram == input]]
        x$MLEProb <- sort(x$MLEProb, decreasing = TRUE)
        match <- head(x$Next,1)
        return(paste0(input, " ", match))
}

predict_bi <- function(input) {
        input <- cleanInput(input)
        x <- BigramProb[BigramProb[,Prev == input]]
        x$MLEProb <- sort(x$MLEProb, decreasing = TRUE)
        matches <- head(x$Next,3)
        return(matches)
}

predict_bi("The")

predict_uni <- function(input) {
        input <- cleanInput(input)
        x <- UnigramProb
        x$freq <- sort(x$freq, decreasing = TRUE)
        matches <- head(x$Next,3)
        return(matches)
}

predict_quad <- function(input) {
        input <- cleanInput(input)
        x <- QuadgramProb[QuadgramProb[,Trigram == input]]
        x$MLEProb <- sort(x$MLEProb, decreasing = TRUE)
        matches <- head(x$Next, 3)
        return(matches)
}
