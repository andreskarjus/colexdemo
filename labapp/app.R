



# setwd("C:/Users/s1364178/Dropbox/phd/experiment/labapp")   # only for local testing, comment out

# rsconnect::deployApp("C:/Users/s1364178/Dropbox/phd/experiment/labapp",forceUpdate = T, launch.browser = F)

# if using local server, should restart after every game. currently the endgame stopApp() makes it impossible to start a new game. If running online, either manually restart, or figure out some server restarter thing.
# look up the command to do it via ssh

# local testing:
# shiny::runApp("C:/Users/s1364178/Dropbox/phd/experiment/labapp/app.R", launch.browser = TRUE)

################################################ #
library(shiny)
library(shinyjs) # for delay
library(shinyWidgets)
library(rdrop2)



# Params #
BREAKTIME = 1800    # ms
DEMO = F         # run in demo mode?
DROPDIR = "experimentdb"


# Data storage #
starttime = Sys.time()
fn=NULL
# droptoken <- rdrop2::drop_auth(); saveRDS(droptoken, file = "droptoken.rds") 
# commented out; run once to generate dropbox access token for rdrop2 to work
# security risk: take care to not share the droptoken!
drop_auth(rdstoken = "droptoken.rds")

saveData <- function(data, filename, mode, datadir=DROPDIR) {
  filePath <- file.path(tempdir(), paste0(filename, ".RDS") )
  saveRDS(data, file = filePath)
  drop_upload(filePath, path = datadir, mode=mode)
}

loadData <- function(filename, datadir=DROPDIR) {
  dd <- drop_download(path = file.path(datadir,paste0(filename,".RDS")), overwrite = T )
  readRDS(paste0(filename,".RDS"))
}

generated_stims = loadData("generated_stims")
# load("generated_stims.RData") # contains stims and global parameters list
if(!DEMO){
  #load("bookkeeping.RData")     # contains table which keeps track of experiment stims and times
  bookkeeping = loadData("bookkeeping")
  whichstim = which(is.na(bookkeeping$starttime))[1] # determine which stimset to preload
  bookkeeping[whichstim, "starttime"] = starttime
} else {
  whichstim = sample(1:length(generated_stims), 1)
}
# experiment data will get added on here
pairs = generated_stims[[whichstim]]$pairs  # this will sit in the global environment; therefore later <<-'s
lang  = generated_stims[[whichstim]]$stims
vocab = generated_stims[[whichstim]]$words

# ----------debug------------------ -
#whichstim=2
#BREAKTIME = 1000  
 pairs = pairs[1,,drop=F]
# --------------------------------- -



# Most of the texts, except the feedback ones which are interactive (see below)
texts = list(
  titlewelcome = "Welcome to a game of Espionage! <br>(cue Bond music)",
  titletext = "
  In the following game, you and your partner will be taking turns sending 
  each other encrypted messages using a secret language.
  The possible messages appear as pairs of words on the screen (in random order).
  To make it impossible for the Enemy to decode your correspondence, even you won't know at the start how the secret language works - your job is to find a way to use it for successful communication. 
  Every time you guess the meaning of a message correctly, your score increases.
  Tip: when sending messages and guessing their meanings, try to remember how you and your partner used them in the previous rounds.
  <br>
  <br>
  Please communicate only using the game interface - do not talk or gesture during the game.
  Only play using the mouse pointer; please do not use your keyboard in any way.
  <br>
  Click the button below to start.
  ",
  titleconfirm =  "I have carefully read the instructions above.",
  titlewait = "Other player still reading instructions. Please wait...",
  feedback_correct = "Correct guess! &nbsp;&nbsp;&nbsp; ",
  feedback_incorrect = "Incorrect guess.",
  endtitle = "That's it. Thanks for participating!",
  feedbackask = "Please describe how you picked the words to communicate the messages. Any other feedback is also welcome.",
  finishbutn = "Click here to submit the feedback and finish the game",
  finishbutn2= "Thanks. Goodbye!"
)


# determines which word is on which line to add randomness, and line placement:
# update: don't make it jump lines, just random order
rndposition = function(wrds, wo){
  #we = sample(wrds) 
  we = wrds[wo] # now doing one random throw outside
  #lb = sample(c(T,T,T,F),1) # 3/4 of times words on separate lines
  #top = sample(c(T,F), 1)
  HTML(paste0(
    #ifelse(!lb & top, "<br>&nbsp;&nbsp;", ""),
    #paste(rep("&nbsp;", sample(0:15,1)),collapse=""),
    we[1] , 
    #ifelse(lb, "<br>", paste(rep("&nbsp;", sample(3:8,1)),collapse="")),
    #paste(rep("&nbsp;", sample(0:15,1)),collapse=""),
    paste(rep("&nbsp;", 4),collapse=""),
    we[2]
    #ifelse(!lb & !top, "<br>&nbsp;&nbsp;", "")
  ) )
}
coloredposition = function(wrds){
  HTML(paste0(
    "<span style='background-color: #114200;'>", #  #DAF7A6
    wrds[1],
    "</span>",
    paste(rep("&nbsp;", 4),collapse=""),
    wrds[2]
  )
  )
}



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
  titleconfirmed=c(F,F),  # 0
  finish=c(F,F),   # when both are finished, save files
  titlescreen=T,
  wo = c(2,1),  # random word order rememberer for persistent GUI
  playerspresent=c()
)

# UI elements and styling
ui <- fluidPage(
  useShinyjs(), # for delay
  tags$head(
    tags$style(HTML("
      body {
        background-color: black;
        color: white;
        font-family: 'Lucida Console', Courier, sans-serif;
      }
      .centered {
        margin: auto; 
        width: 450px;
        margin-top: 100px;
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
        /* color: black; */
        text-align: left;
        font-style: normal;
      }
      h3 {
        font-size: 18px;
        line-height: 1.5;
        /* color: black; */
        text-align: left;
        font-style: normal;
        font-weight: normal;
      }
      h4 {
        font-size: 16px;
        line-height: 1.5;
        /* color: black; */
        font-style: normal;
        font-weight: normal;
        text-align: justify;
        text-justify: inter-word;
      }
      .form-group .control-label {
        font-size: 18px;
        line-height: 1.5;
        /* color: black; */
        text-align: left;
        font-style: normal;
        font-weight: normal;
      }
      input[type='radio']{
        display:none;
      } 
      .radio {
        font-size: 18px;
        line-height: 2;
      }

    ")) # color: #357a00;
  ),
  tags$div(class="centered",   {uiOutput("moreControls")} )
  
)



server <- function(input, output, session){
  
  # manages players and their roles
  local <- reactiveValues()
  observe({
    isolate({global$amountUser <- global$amountUser + 1 }  )
    isolate({
      if(length(global$playerspresent)<1){
        p=1
      } else {
        p=setdiff(1:2, global$playerspresent)
      }
      global$playerspresent = c(global$playerspresent, p)
      local$userId = p
      #local$userId <- (2-(global$amountUser %% 2))
    })   
    # (2-(global$amountUser %% 2)) # (global$amountUser %% 2)+1 # global$amountUser
  })
  #session$onSessionEnded(function(x=global$amountUser){isolate({global$amountUser=x-1}) })
  session$onSessionEnded(function(x=global$playerspresent){isolate({
    global$playerspresent = setdiff(x, local$userId) 
    }) })
  
  
  ### action observers ###
  #
  # when word clicked, swap userIdToPlay
  observeEvent(eventExpr =  #input$send, 
                 input$wordchoice,
               handlerExpr = {
                 if(length(input$wordchoice) > 0 ){
                   pairs[global$ipair, "sendtime"] <<- Sys.time() # needs <<-
                   global$userIdToPlay =  3 - global$userIdToPlay  
                   # assumes two players
                   # ---this might be easy to break if 2+ windows open
                   # solution, display empty screen to any 3rd player/accidental login
                   # pairs$sender[global$ipair] # depends on the "sender" column in stims
                   global$word = input$wordchoice
                 }
               }
  )
  observeEvent(eventExpr = #input$guess
                 input$vocabchoice,  
               # without actionbutton use: input$vocabchoice
               handlerExpr = {
                 if(length(input$vocabchoice)>0){ 
                   # this also makes it so clicking the button doesn't do anything if nothing selected
                   # (was only needed for the radio+button gui, but might as well let it stay)
                   pairs[global$ipair, "guesstime"] <<- Sys.time() #  timestamp
                   global$userIdToGuess = 3 - global$userIdToGuess
                   global$lastguess = input$vocabchoice
                   # print( paste(global$lastguess, 
                   #              pairs[global$ipair, pairs[global$ipair,3] ], input$wordchoice) )
                   global$correctguess = 
                     global$lastguess == pairs$say[global$ipair]
                   if(global$correctguess){
                     global$score = global$score + 1
                     global$guessvector[global$ipair] = T
                   }
                   global$feedbackbreak = T
                #   delay(BREAKTIME, {      # pause to display feedback
                     #print(global$guessvector)   # debug
                     
                     # save round results
                     pairs[global$ipair, "sent"]    <<- global$word
                     pairs[global$ipair, "guess"]   <<- global$lastguess
                     pairs[global$ipair, "correct"] <<- global$correctguess
                     #if(DEMO){print(global$ipair);print(pairs)} # debug
                     
                     # carry on
                     global$ipair = global$ipair + 1
                     
                      # first save attempt of 2 redundant ones
                     if(global$ipair > global$pairsmax){
                       fn <<- paste0(whichstim,  # make it global so can reuse
                                     ifelse(bookkeeping[whichstim, "isbaseline",drop=T],"b","a"), 
                                     "_",
                                     format(starttime, "%Y-%m-%d_%H-%M-%S"),
                                     "--",
                                     format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
                                     ".RData" )
                       saveData(pairs, fn, "overwrite")
                     }
                     
                   #  global$feedbackbreak = F
                     
                  # } ) # end delay
                 }
               }
  )
  observe({
    global$feedbackbreak
    if(global$feedbackbreak){
      delay(BREAKTIME, { isolate({global$feedbackbreak=F}) })
    }
  })
  
  # title screen
  observeEvent(eventExpr = input$titleconfirm,
               handlerExpr = {
                 disable(id = "titleconfirm")                # all
                 updateActionButton(session, "titleconfirm", # this is now redundant, showing text instead
                                    label = texts$titlewait) #
                 #global$titleconfirmed = global$titleconfirmed + 1
                 global$titleconfirmed[local$userId] = T  # this is important
               }
  )
  observe({
    global$titleconfirmed
    isolate({
    # if(global$titleconfirmed[local$userId]){ # not needed anymore
    #   disable(id = "titleconfirm")
    #   updateActionButton(session, "titleconfirm",
    #                      label = texts$titlewait)
    # }
    if(all(global$titleconfirmed)){
      global$titlescreen = F   # this starts the game proper
    }
    })
  })
  
  # observeEvent(global$titleconfirmed, {
  #   if(global$titleconfirmed > 1){
  #     isolate ({ global$titlescreen = F })
  #   }
  # })
  
  
  # Important part: if done, save experiment results
  observeEvent(global$finish, {
    #print(global$finish) # debug
    if( all(global$finish)){
      if(!DEMO){
        bookkeeping$endtime[whichstim] <<- Sys.time()
        saveData(bookkeeping, "bookkeeping", "add")       # add a copy for backup
        saveData(bookkeeping, "bookkeeping", "overwrite") # updated; will be read next time
        # redundant re-save to be on the safe side:
        if(is.null(fn)){
          fn <<- paste0(whichstim,  # make it global so can reuse
                        ifelse(bookkeeping[whichstim, "isbaseline",drop=T],"b","a"), 
                        "_",
                        format(starttime, "%Y-%m-%d_%H-%M-%S"),
                        "--",
                        format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
                        ".RData" )
        }
        saveData(pairs, fn, "overwrite") # overwrite if first attempt was successful
        global$finish = c(F,F) # extra caution to avoid more saves
        
        delay(2000, {stopApp()} ) # delay so last player can also see the thankyou message
      }
    }
  })
  
  observeEvent(eventExpr = input$finishgame,
               handlerExpr = {
                 bookkeeping[whichstim, paste0("f", local$userId)] <<- input$feedback  # store feedback
                 isolate({
                   disable(id = "finishgame")
                   disable(id = "feedback")
                   updateActionButton(session, "finishgame",
                                      label = texts$finishbutn2)
                 })
                 global$finish[local$userId] = T # will trigger saving when both done
               }
  )
  

  
  
  # This part decides which screen to show 
  # when one of the reactive values change
  ##
  output$moreControls <- renderUI({
    # the mentions here make the reactivity work properly
    global$userIdToPlay   
    global$userIdToGuess
    global$feedbackbreak
    global$titlescreen
    global$playerspresent
    global$titleconfirmed
    isolate({
      ###
      # if somehow 3rd player connected, lock them out; could be used to monitor though.
      # no longer works, forcing players to be 1 or 2 - but now refreshing won't break game!
      # if(local$userId > 2){
      #   p=pairs[1:(global$ipair),c(1,2,4,6,7,9),drop=F]; p$sendtime=format(p$sendtime, "%H:%M:%S")
      #   return(
      #     tagList(
      #     h3("Something is wrong, it seems 2 players are already connected. 
      #        Please ask the experimenter for help."),
      #     HTML("<br>"),
      #     renderPrint({ print(p, row.names=F) })
      #     )
      #   )
      # }
      #
      #
      # Game over screen:
      if(global$ipair > global$pairsmax){
        # show end screen:
        return(
          tagList(
            h2(texts$endtitle),
            h3(paste0("The overall score between you and your partner was ", 
                      global$score, "/", global$pairsmax, 
                      " or ", round(global$score/(global$pairsmax)*100),
                      "% communicative accuracy."
                      # -1 because server already incremented ipair
            ) 
            ),
            h3(paste0(
              "In the last quarter of the game, accuracy was ",
              round(sum(tail(global$guessvector, 
                             round(length(global$guessvector)/4) ))/
                      round(length(global$guessvector)/4)*100),
              "%."
            )),
            h3(paste0("The game lasted for ", 
                      as.character(round( difftime(Sys.time(),starttime,units="mins"))), 
                      " minutes.")),
            HTML("<br>"),
            textAreaInput("feedback", 
                          texts$feedbackask, 
                          width = "399px", height="100px"),
            actionButton("finishgame",  texts$finishbutn)
          )
        )
      }
      ## Title screen with instructions:
      if(global$titlescreen){
        if(global$titleconfirmed[local$userId]){
            but = h4(texts$titlewait)
        } else {
          but = actionButton(inputId = "titleconfirm",
                       label = texts$titleconfirm)
        }
        return(
          tagList(
            h1(HTML(paste0(
              "<span style='font-size: 9px;'>",
              "Players connected: ", length(global$playerspresent),
              ". Running experiment no. ",
              whichstim, 
              ifelse(pairs$isbaseline[1], "b", "a"),
              "</span>"
            ))), # tiny text with stim n and b=baseline
            h2(HTML(texts$titlewelcome)),
            h4(HTML(texts$titletext)),
            h3(""),
            but
          )
        )
      } else {
        # Feedback break screen:
        if(global$feedbackbreak){
          # if(global$titleconfirmed < 2){
          #   return(h2("Waiting for partner. Stand by..."))
          # } else {
          return(   
            tagList(
              h1(paste0("Score: ", global$score, "/", global$pairsmax)),
              h2( HTML(  # feedback
                ifelse(
                  global$correctguess,
                  paste0(texts$feedback_correct, 
                         "<span style='color: #357a00;'>",
                         global$word, 
                         "</span>",
                         " = ", 
                         "<span style='font-style: italic;'>",
                         global$lastguess,
                         "</span>"
                         ), 
                  texts$feedback_incorrect
                )
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
                h1(paste0("Score: ", global$score, "/", global$pairsmax)),
                
                # old:
                # h2(paste0(pairs[global$ipair, 1], " | ", pairs[global$ipair, 2]
                #           #  , '  | send "', pairs[global$ipair, pairs[global$ipair,3] ], '"' 
                # ) ),
                #
                h2( 
                  #rndposition( c(pairs$pair1[global$ipair], pairs$pair2[global$ipair]) )
                  coloredposition( c(pairs$say[global$ipair],  # new: one to send always left+colored
                                     setdiff(c(pairs$pair1[global$ipair], pairs$pair2[global$ipair]),
                                             pairs$say[global$ipair]
                                     )
                  )
                  )
                ),
                ## Use radio or actionButtons?
                radioButtons(inputId = "wordchoice",
                             label = HTML( paste0("Communicate ",
                                            "<span style='font-style: italic;'>",
                                            pairs$say[global$ipair],
                                            "</span>",
                                            " using..." 
                                            )),
                             # replace with fixed
                             choices = lang,
                             # randomize order? harder to play, but then cannot make pattern
                             # better: now the message placement is randomized instead
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
                h1(paste0("Score: ", global$score, "/", global$pairsmax)),
                #h2(paste0(pairs[global$ipair, 1], " | ", pairs[global$ipair, 2]) ) ,
                h2(rndposition( c(pairs$pair1[global$ipair], pairs$pair2[global$ipair]), global$wo) ),
                h3(HTML( paste0(
                           "Message: ",
                           "<span style='color: #357a00;'>",
                           global$word, 
                           "</span>"
                           ) ) ) ,
                radioButtons(inputId =  "vocabchoice", 
                             label =  "This means:",
                             choices = c(pairs[global$ipair, 1], pairs[global$ipair, 2])[global$wo],
                             selected = character(),
                             inline = F
                             # if radio buttons, should disable submitbutton until selection made! (using shinyjs disable/enable toggle)
                             )
                #,actionButton(inputId="guess", label="Continue")
                
              )
            )
          }
          # wait for message
          if(local$userId != global$userIdToPlay & local$userId == global$userIdToGuess){
            global$wo = sample(1:2)
            return(
              tagList(
                h1(paste0("Score: ", global$score, "/", global$pairsmax)),
                h2(rndposition( c(pairs$pair1[global$ipair], pairs$pair2[global$ipair]) , global$wo)),
                h3("Waiting for message...")
              )
            )
          }
          # wait for guess:
          if(local$userId != global$userIdToPlay & 
             local$userId != global$userIdToGuess){
            return(
              tagList(
                h1(paste0("Score: ", global$score, "/", global$pairsmax)),
                #h2(paste0(pairs[global$ipair, 1], " | ", pairs[global$ipair, 2])), 
                h2(" "),h2(" "), # just blank
                h3(HTML(paste0(
                  "Sent ", 
                  "<span style='font-style: italic;'>", 
                  pairs$say[global$ipair], 
                  "</span>",
                  " using ",
                  "<span style='color: #357a00;'>",
                  global$word, 
                  "</span>"
                  )) 
                  ),
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
# runApp(list(ui = ui, server = server),host="192.168.xx.xx",port=5013, launch.browser = TRUE)
# runApp(list(ui = ui, server = server), launch.browser = TRUE)
# runApp(list(ui = ui, server = server), host="0.0.0.0",port=5050, launch.browser = F )
# runApp( "C:/Users/s1364178/Dropbox/phd/experiment/shiny_pilot.R" , host="192.168.1.10",port=3838, launch.browser = F )
# http://127.0.0.1:5050/
# http://192.168.1.10:5050


# shiny::runApp("C:/Users/s1364178/Dropbox/phd/experiment/labapp/app.R", launch.browser = TRUE)
shinyApp(ui, server)

