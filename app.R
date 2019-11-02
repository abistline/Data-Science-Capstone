#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# 
library(shiny)
library(shinyWidgets)
if(!"shinydashboard" %in% installed.packages()) {
  install.packages("shinydashboard")
  library(shinydashboard)
}

#--------------------------------------------------------------------
# Define UI for application that predicts a next word from user input string

ui <- fluidPage(

setBackgroundImage(src = "landscape2.jpg"),

  # Application title
  titlePanel(span("Next-Word Prediction App", style = "color:blanchedalmond")),

# layout the ui with fluidRows and columns
fluidRow(
    column(width = 4,
      h4(span(strong("Input:")), style = "color:blanchedalmond"),
      textInput(inputId = "userText", label = " ", value = " "),
      tags$head(tags$style("#container * {display: inline; color:blanchedalmond}")),
      div(id = "container", h4("Next Word Prediction:", em(strong(textOutput("nxtwrd")))))
          ),
    column(width = 6,
      p(span("This App uses a series of n-grams to predict the next
        word that a user will type.  These n-grams were developed from
        twitter posts, blogs, and news sources - all provided by",
        strong("SwiftKey")), style = "color:blanchedalmond; font-size:16px"),
      strong(span("Instructions:"), style = "color:blanchedalmond; font-size:20px"),
      p(span("Enter text in the sidebar to the left.  The text along with the
        predicted next-word will be displayed below.  The prediction is also displayed 
             beneath the input"), style = "color:blanchedalmond; font-size:16px"),
      strong(span("Important:"), style = "color:blanchedalmond; font-size:20px"),
      em(span("The App may take up to 30 seconds to initialize.  Thank you for your
         patience."), style = "color:blanchedalmond; font-size:16px"),
      hr()
          )
        ),
  fluidRow(
    column(width = 6, offset = 4,
          h4(strong("Compilation:"), style = "color:blanchedalmond; font-size:16px"),
          h5(strong(textOutput("wordPred"), align = "left"), 
             style = "color:blanchedalmond; font-size:16px"),
          "Next Word Tool"
          )
        )
      ) 


#-------------------------------------------------------------------
# Define server logic required to predict the next word in a sentence

# load libraries
library(quanteda)
library(data.table)

# Load dataset
stats_table <- fread("stats_table.csv")

# server function
server <- function(input, output) {

  # define function for quering a word that follows an input string
  nxtword <- function(string, datatable, max_length = 5) {

    # standardize the string
    corp <- corpus(string)
    toks <- unlist(tokens(
      x = tolower(corp),
      remove_numbers = TRUE, remove_punct   = TRUE,
      remove_symbols = TRUE, remove_twitter = TRUE,
      remove_hyphens = TRUE, remove_url     = TRUE)
    )

    # verify the current input
    cat("Current input:", paste(toks, collapse=" "), "\n")
    cat("# of words in input:", length(toks), "\n\n")

    # define nextword to be the word in the data table (column 3)
    # on the same row as the user input
    nextword = datatable[ngrams == paste(toks, collapse=" "), 3][1]

    # if the first definition of nextword is NA because the user input
    # does not match anything in the ngram column, start removing the
    # first word from the user input one by one.
    for (i in max_length:2) {
      if (is.na(nextword) & length(toks) > i)
        nextword = datatable[ngrams == paste(toks[(length(toks) - (i - 1)):length(toks)],
                                             collapse = " "), 3][1]
    }

    # at this point, if nextword is still NA, then we need to just neet
    # to find a match in the ngram column to the last word of the user input
    if (is.na(nextword) & length(toks) > 1)
      nextword = datatable[ngrams == paste(toks[length(toks)],
                                           collapse = " "), 3][1]

    # if nextword is still NA, then just return a random word and move on
    if (is.na(nextword))
      nextword = datatable[sample(1:nrow(datatable), 1), 3][1]

    # verify the nextword prediction
    cat("We predict that the next word should be: ", unlist(nextword), "\n\n")

    # return the user input combined with nextword
    paste(string, nextword)
  }

  # compile the user input and nextword predictions
  output$wordPred <- renderText({

    nxtword(input$userText, stats_table)

  })

  # show only the nextword
  output$nxtwrd <- renderText({

    words = nxtword(input$userText, stats_table)
    toks = unlist(strsplit(words, " "))
    nextword = toks[length(toks)]

  })
}

#-------------------------------------------------------------------

# Run the application

shinyApp(ui = ui, server = server)

