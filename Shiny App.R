library(shiny)
library(quanteda)
library(dplyr)
library(stringr)
library(data.table)
library(tm)
#Read in ngram models
unigram_prob <- fread("./UnigramProb.csv", header = T, sep = ",")
bigram_prob <- fread("./BigramProb.csv", header = T, sep = ",")
trigram_prob <- fread("./TrigramProb.csv", header = T, sep = ",")
quadgram_prob <- fread("./QuadgramProb.csv", header = T, sep = ",")

cleanInput <- function(input) {
        
        input <- tolower(input)
        
        # remove URL, email addresses, Twitter handles and hash tags
        input <- gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", input, ignore.case = FALSE, perl = TRUE)
        input <- gsub("\\S+[@]\\S+", "", input, ignore.case = FALSE, perl = TRUE)
        input <- gsub("@[^\\s]+", "", input, ignore.case = FALSE, perl = TRUE)
        input <- gsub("#[^\\s]+", "", input, ignore.case = FALSE, perl = TRUE)
        
        # remove ordinal numbers
        input <- gsub("[0-9](?:st|nd|rd|th)", "", input, ignore.case = FALSE, perl = TRUE)
        
        
        # remove punctuation
        input <- gsub("[^\\p{L}'\\s]+", "", input, ignore.case = FALSE, perl = TRUE)
        
        # remove punctuation (leaving ')
        input <- gsub("[.\\-!]", " ", input, ignore.case = FALSE, perl = TRUE)
        
        # trim leading and trailing whitespace
        input <- gsub("^\\s+|\\s+$", "", input)
        input <- stripWhitespace(input)
        
        return(input)
        
}


predictNextWord <- function(input) {
        sentence <- cleanInput(input)
        sentence_len <- ntoken(input)
        #Check if entered text is valid and display a message
        if (sentence_len == 0) {
                return("Please Enter Words in the Sidebar Panel")
        } 
        else {
                #Quadgram Prediction
                if (sentence_len == 3) {
                        x <- quadgram_prob[quadgram_prob[,Trigram == sentence]]
                        x$MLEProb <- sort(x$MLEProb, decreasing = TRUE)
                        matches <- head(x$Next, 3) 
                }
                
                
                #Trigram Prediction
                if (sentence_len == 2) {
                        x <- trigram_prob[trigram_prob[,Bigram == sentence]]
                        x$MLEProb <- sort(x$MLEProb, decreasing = TRUE)
                        matches <- head(x$Next,3)
                }
                
                #Bigram Prediction
                if (sentence_len == 1) {
                        x <- bigram_prob[bigram_prob[,Prev == sentence]]
                        x$MLEProb <- sort(x$MLEProb, decreasing = TRUE)
                        matches <- head(x$Next,3)
                }
                #If there is no matches then I used the highest frequency unigrams to fill in the words
                else{
                        if (length(matches) == 0) {
                                x <- unigram_prob
                                x$freq <- sort(x$freq, decreasing = TRUE)
                                matches <- head(x$Next,3)
                        }
                }
        }
        return(paste0(sentence, ": ", matches))
}




ui <- fluidPage(
        titlePanel("Data Science Capstone Word Prediction"),
        sidebarLayout(
                sidebarPanel(
                        textInput('words', label="Input Words", width = "100%"),
                        br(),
                        h3("Hello, welcome to my Word Prediction App."),
                        br(),
                        "This App is able to make predictions for groups of up to 3 words",
                        "I chose to remove swear words from the word prediction.",
                        br(),
                        "This App will give you up to 3 word predictions.",
                        br(),
                        h4("Made by Louis Bouwer")
                
                        
                ),
                mainPanel(tabsetPanel(type = "tabs", 
                                tabPanel("Word Predictions",
                                br(),
                                "Word Prediction Output",
                                br(),
                                verbatimTextOutput('predictedsentence')

                        )))))


server <- function(input, output) {
        output$predictedsentence <- renderText({ 
                text <- predictNextWord(input$words) 
                paste(text, collapse = "\n")
        })}

shinyApp(ui = ui, server = server)