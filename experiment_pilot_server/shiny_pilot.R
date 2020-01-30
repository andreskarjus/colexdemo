
################################################ #
library(shiny)
library(shinyjs) # for delay
library("shinyWidgets")

# Params
BREAKTIME = 2000    # ms
SAVEDIR = getwd()   # change
NROUNDS = "?"


# Demo stimuli
lang = c("jupike", "nubla", "vidin")
vocab = c("hand", "arm", "mouse", "sun")
nv = length(vocab)
pairs = data.frame(pair1 = sample(vocab, 20, T),
                   stringsAsFactors = F)
pairs$pair2 = sapply(pairs$pair1, function(x) setdiff(vocab, x)[sample(1:(nv-1), 1)] )
pairs$say = sample(1:2, nrow(pairs), T)
# demo, create list and preload for each experiment ID later
pairs = pairs[1:4,]


# Most of the texts, except the feedback ones which are interactive
texts = list(
  titlewelcome = "Welcome to a game of Espionage! <br>(cue Bond music)",
  titletext = "
  In the following game, you and your partner will be taking turns sending 
  each other encripted messages using a secret language.
  To make it impossible for the Enemy to decode your correspondence,
  even you won't know at the start what each word means.
  The aim of the game is to communicate the secret messages as accurately
  as possible, reflected by a score counter in the corner.
  Please do not talk or gesture during the game and communicate only using the game interface.
  Click the button below to start.
  ",
  titleconfirm =  "I have carefully read the instructions above.",
  feedback_correct = 'Correct guess! "',
  feedback_incorrect = "Incorrect guess.",
  titlewait = "Waiting for partner to be ready...",
  endtitle = "That's it. Thanks for participating!"
)


# Global reactive values
global <- reactiveValues(
  amountUser = 0, 
  userIdToPlay = 1,
  userIdToGuess = 2,
  ipair = 1,
  pairsmax = nrow(pairs),
  word = "",
  lastguess = "",
  correctguess = logical(),
  feedbackbreak = F,
  score = 0,
  guessvector = rep(F, nrow(pairs)),
  titleconfirmed=0,
  titlescreen=T
)

# UI elements and styling
ui <- fluidPage(
  useShinyjs(), # for delay
  tags$head(
    tags$style(HTML("
      .centered {
        margin: auto; 
        width: 400px;
        margin-top: 200px;
      }
      h1 {
        font-size: 15px;
        line-height: 1.1;
        color: gray;
        text-align: right;
        font-style: normal;
      }
      h2 {
        font-size: 20px;
        line-height: 1.5;
        color: black;
        text-align: left;
        font-style: normal;
      }
      h3 {
        font-size: 18px;
        line-height: 1.5;
        color: black;
        text-align: left;
        font-style: normal;
        font-weight: normal;
        text-align: justify;
        text-justify: inter-word;
      }
      .form-group .control-label {
        font-size: 18px;
        line-height: 1.2;
        color: black;
        text-align: left;
        font-style: normal;
        font-weight: normal;
      }

    "))
  ),
  tags$div(class="centered",   {uiOutput("moreControls")} )
  
)

server <- function(input, output, session){
  
  local <- reactiveValues()
  
  observe({
    isolate(global$amountUser <- global$amountUser + 1)
    isolate(local$userId <- global$amountUser)
  })
  
  
  
  observeEvent(eventExpr =  #input$send, 
                 input$wordchoice,
               handlerExpr = {
                 if(length(input$wordchoice) > 0 ){
                   global$userIdToPlay = 3 - global$userIdToPlay  
                   # assumes two players
                   # ---this might be easy to break if 2+ windows open
                   global$word = input$wordchoice
                 }
               }
  )
  observeEvent(eventExpr = #input$guess
                 input$vocabchoice,  
               # without actionbutton use: input$vocabchoice
               handlerExpr = {
                 if(length(input$vocabchoice)>0){ # this also makes it so clicking the button doesn't do anything if nothing selected
                   global$userIdToGuess = 3 - global$userIdToGuess
                   global$lastguess = input$vocabchoice
                   # print( paste(global$lastguess, 
                   #              pairs[global$ipair, pairs[global$ipair,3] ], input$wordchoice) )
                   global$correctguess = global$lastguess == 
                     pairs[global$ipair, pairs[global$ipair,3] ]
                   if(global$correctguess){
                     global$score = global$score + 1
                     global$guessvector[global$ipair] = T
                   }
                   global$feedbackbreak = T
                   delay(BREAKTIME, {      # pause to display feedback
                     #print(global$guessvector)   # debug
                     global$feedbackbreak = F
                     global$ipair = global$ipair + 1
                   } )
                 }
               }
  )
  
  observeEvent(eventExpr = input$titleconfirm,
               handlerExpr = {
                 disable(id = "titleconfirm")
                 updateActionButton(session, "titleconfirm",
                    label = texts$titlewait)
                 global$titleconfirmed = global$titleconfirmed + 1
               }
  )
  observeEvent(global$titleconfirmed, {
    if(global$titleconfirmed > 1){
      isolate ({ global$titlescreen = F } )
    }
  })
  
  
  # This part decides which screen to show 
  # when one of the reactive values change
  ##
  output$moreControls <- renderUI({
    # the mentions here make the reactivity work properly
    global$userIdToPlay   
    global$userIdToGuess
    global$feedbackbreak
    global$titlescreen
    isolate({
      ###
      ## title screen with instructions:
      if(global$ipair > global$pairsmax){
        return(
          tagList(
            h2(texts$endtitle),
            h3(paste0("The overall score between you and your partner is ", 
                      global$score, "/", global$ipair, 
                      " or ", round(global$score/global$ipair*100),
                      "% communicative accuracy."
                      ) 
               ),
            h3(paste0(
              "In the last quarter of the game, accuracy was ",
              round(sum(tail(global$guessvector, 
                   round(length(global$guessvector)/4) ))/
                round(length(global$guessvector)/4)*100),
               "%."
              ))
            )
        )
      }
      if(global$titlescreen){
        return(
          tagList(
            h2(HTML(texts$titlewelcome)),
            h3(HTML(texts$titletext)),
            h3(""),
            actionButton(inputId = "titleconfirm",
                         label = texts$titleconfirm)
          )
        )
      } else {
      # feedback break screen:
      if(global$feedbackbreak){
        # if(global$titleconfirmed < 2){
        #   return(h2("Waiting for partner. Stand by..."))
        # } else {
          return(   
            tagList(
              h1(paste0("Score: ", global$score, "/", global$ipair)),
              h2(   # feedback
                ifelse(
                  global$correctguess,
                  paste0(texts$feedback_correct, global$word, '" meant "', 
                         global$lastguess,'"'), 
                  texts$feedback_incorrect
                )
              )
            )
          )
#        }
      } else {
        
        # send message:
        if(local$userId == global$userIdToPlay & local$userId != global$userIdToGuess){
          #print(paste(pairs[global$ipair, 1:2, drop=T], collapse=" " ) ) # debug
          return(
            tagList(
              h1(paste0("Score: ", global$score, "/", global$ipair)),
              h2(paste0(pairs[global$ipair, 1], " | ", pairs[global$ipair, 2]
                        #  , '  | send "', pairs[global$ipair, pairs[global$ipair,3] ], '"' 
              ) ),
              ## Use radio or actionButtons?
              radioButtons(inputId = "wordchoice",
                           label = paste0('Communicate "',
                                          pairs[global$ipair, pairs[global$ipair,3] ],
                                          '" using...' ),
                           choices = lang[sample(1:length(lang))],
                           # randomize order? harder to play, but then cannot make pattern
                           selected = character(0)
              )
              # , actionButton(inputId="send", label="Send") # not needed anymore
              
              # h3(paste0('Communicate "',
              #           pairs[global$ipair, pairs[global$ipair,3] ], 
              #            '" using...' )),
              # actionGroupButtons(
              #   inputIds = rep("wordchoice", length(lang)),
              #   labels = lang,
              #   status = "danger",
              #   size = "normal",
              #   direction = "vertical",
              #   fullwidth = FALSE
              # )
            )
          )
        }
        # take a guess:
        if(local$userId == global$userIdToPlay & local$userId == global$userIdToGuess){
          return(
            tagList(
              h1(paste0("Score: ", global$score, "/", global$ipair)),
              h2(paste0(pairs[global$ipair, 1], " | ", pairs[global$ipair, 2]) ) ,
              h3( paste0('Message: "', global$word, '"') ) ,
              radioButtons(inputId =  "vocabchoice", 
                           label =  "This means:",
                           choices = c(pairs[global$ipair, 1], pairs[global$ipair, 2]),
                           selected = character(),
                           inline=T
                           # if radio buttons, should disable submitbutton until selection made! (using shinyjs disable/enable toggle)
                           
              )
              #,actionButton(inputId="guess", label="Continue")
              
            )
          )
        }
        # wait for message
        if(local$userId != global$userIdToPlay & local$userId == global$userIdToGuess){
          return(
            tagList(
              h1(paste0("Score: ", global$score, "/", global$ipair)),
              h2(paste0(pairs[global$ipair, 1], " | ", pairs[global$ipair, 2] ) ),
              h3("Waiting for message...")
            )
          )
        }
        # wait for guess:
        if(local$userId != global$userIdToPlay & 
           local$userId != global$userIdToGuess){
          return(
            tagList(
              h1(paste0("Score: ", global$score, "/", global$ipair)),
              h2(paste0(pairs[global$ipair, 1], " | ", pairs[global$ipair, 2])), 
              h3(paste0('Sent "', 
                        pairs[global$ipair, pairs[global$ipair,3] ], 
                        '" using "',global$word, '"'  ) ),
              h3("Stand by..." )
            )
          )
        }
        
      } # end else
      } # end else 
    }) ### end isolate
  })
  
}
#shinyApp(ui, server)
#runApp(list(ui = ui, server = server),host="192.168.xx.xx",port=5013, launch.browser = TRUE)
runApp(list(ui = ui, server = server), launch.browser = TRUE)


