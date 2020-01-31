



#### shiny app ####

# setwd("C:/Users/s1364178/Dropbox/phd/experiment/app") # remove when uploaded; needed for includeHTML on local

# updater: 
#   rsconnect::deployApp("C:/Users/s1364178/Dropbox/phd/experiment/app"  )

source("expgen_scripts.R")
load("appdata.RData")

library(shiny)

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

ui =   fluidPage(
  tags$head(
    tags$style(HTML( "
                     label{font-style: normal; font-weight: normal; color: black; }
                     .container-fluid {  max-width: 1000px;}
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
    "
    )
    )
  ),
  tabsetPanel(
    tabPanel("Experiment overview", 
             fluidPage(
               withMathJax( includeHTML("overview.html") 
               )  # rmarkdown must be output: html_fragment !!!
             )
    ), # end first tab
    tabPanel("Interactive stimuli generator",
             fluidPage(
               titlePanel("Stimuli generator"),
               
               # Sidebar layout with input and output definitions ----
               fluidRow(
                 
                 column(4,
                        titlePanel("Settings for Simlex lexicon:"), 
                        numericInput("wordlen_min", "Min length of words from Simlex999 to use:",3, min = 2), #wordlen_min  = 3,    # length of words from simlex to use
                        numericInput("wordlen_max", "Max length of words from Simlex999 to use:",7), #wordlen_max  = 7,
                        numericInput("assocmax", 'Filter out "free-associated" words ([0,10] but most are <1):', 1, min=0,max=10,step=0.5),
                        # assocmax = 1        # filter out "free-associated" words ([0,10] but most are <1)
                        textInput("poslist", "Which POS to use:", "N"),
                        # poslist  = c("N")   # which POS to use
                        numericInput("simthreshold_high",'Above this is a "similar" word [0,10] from Simlex:', 8, 
                                     max = 10,step = 0.5),
                        # simthreshold_high = 8  # above this is a "similar" word [0,10] - this is for simlex
                        numericInput("simthreshold_low", 'Below this is a "not similar" word [0,1], embeddings vector cosine:', 0.3, max=1, step=0.1)
                        # simthreshold_low  = 0.2  # below this is a "not similar" word [0,1] - this is for the vector cosine
                        # # with nwords=20 and nsimilar=2 0.3 was minimum feasible, can do less if smaller lexicon
                 ),
                 column(4,
                        titlePanel("Settings for stims:"),
                        numericInput("nwords", "Size of English lexicon to use per stim/game:", 10 ),
                        numericInput("nlang","Size of artificial language lexicon per stim/game (probably want less than size of English lexicon):", 9),
                        numericInput("nsimilar", "...out of which this many will be 'similar' pairs:.",1),
                        # nsimilar = 1           # how many pairs to have which are > simthreshold_high
                        numericInput("nstims", "How many preset stim sets to create (i.e. how many dyads will be run):", 50),
                        # nstims = 40            # how many stim sets to create (i.e. how many dyads will be run)
                        # allowduplicates = 3    # how many max duplicate target pairs across stims 
                        numericInput("allowduplicates","Max how many duplicate target pairs allowed across stims (min 3 for 40 stims if 1 pair, 5 if 2):", 3),
                        # # 3 for 40 stims if 1 pair, 5 if 2)
                        # # making it equal to nstims will disable the check alltogether
                        numericInput("editmin", "Min edit distance steps, for both(!) natural and artificial words:", 3, min=0),
                        # editmin           = 3  # edit distance threshold for both natural and artificial words; currently ~ min(wordlen) & langlen-1
                        # # uses Optimal String Alignment (Levenshtein + transposition of adjacent characters)
                        checkboxInput("allowsameinitial", "Allow target pairs to have the same initial letter? (allows eg hero-heroine which are 3 edit dist):", value = TRUE),
                        # allowsameinitial = T   # allow target pairs to have the same initial letter? (allows eg hero-heroine)
                        actionButton("makewords", "(1) Generate English stims")
                 ),
                 column(4,
                        titlePanel("Language generator"),
                        # lang gen
                        numericInput("langlen", "Uniform length of artificial words, in number of CV syllables:", 2),
                        textInput("vowels", "String of allowed vovels", "aeoui"),
                        textInput("cons","String of allowed consonants", "qwtpsfhnmrl"),
                        textInput("trfrom","Letters in Eng wordlist to homogenize, to avoid similar-sounding artif.lang words; translate from:", "vdbz"),
                        textInput("trto","...to: (needs to be strings of equal length; latter should overlap with stimuli letters)", "wtps"),
                        # transl = c("vdbz", "wtps") # letters in the English wordlist to homogenize to catch stims that are 
                        # # similar-sounding o English words; must be 2 strings of equal(!) length, 
                        # # those in 1 transformed into those in 2
                        numericInput("maxinitial",
                                     "Times each consonant is allowed to start an artificial word per stim set; high value effectively disables:", 100),
                        checkboxInput("constraininitials", "Constrain artificial words: initial letter cannot overlap with English stims (conficts with low values of the times option above!)", T),
                        actionButton("makelang", "(2) Generate an artificial language"),
                        HTML("<br><br>"),
                        actionButton("makestims", HTML("(3) Generate stims by combining English <br>and artificial words (+scroll down)"))
                        # # setting it to ==nlang disables; (if less consonants than nlang or otherwise)
                        # # cannot find solution, raise to 2)
                 )
               ),  # fluidrow
               HTML("<br>"),
                tags$style(type='text/css', '#errorbar {color: red;}'),
               verbatimTextOutput("errorbar", placeholder = T),
               verbatimTextOutput("wordexample"),
               verbatimTextOutput("langexample"),
               verbatimTextOutput("stimsexample")
             ) # fluidpage
    ) # 2nd tab
    ,
    tabPanel("Experiment demo view",  # experiment demo (not interactive)
             fluidPage(
               tags$div(class="centered",   {uiOutput("moreControls")} )
             )
    )
  ) # all tabs
) # page


server <- function(input, output) {
  
  e = reactiveValues(
    e=NULL
  )
  stims <- reactiveValues(
    lang=NULL,
    words=NULL,
    stims=NULL
  )
  global = reactiveValues(
    ready=NULL,
    demo=NULL
  ) # for experiment demo
  
  observeEvent(eventExpr = input$makewords,
               handlerExpr = {
                 stims$words=NULL
                 tryCatch({
                   withProgress(message="Generating English stims; tip: if this seems to take forever, refresh the app, relax constraints, and try again", expr =  {
                     stims$words = wordsampler(
                       input$nwords, 
                       input$nstims, 
                       input$nsimilar, 
                       input$simthreshold_high, 
                       input$simthreshold_low, 
                       input$assocmax, 
                       input$editmin,     simlex, simmat, 
                       input$allowduplicates, 
                       input$allowsameinitial,
                       input$poslist, 
                       input$wordlen_min, 
                       input$wordlen_max)
                   })
                 }, error = function(e){e$e <<- e})
               })
  
  observeEvent(eventExpr = input$makelang,
               handlerExpr = {
                 stims$lang = NULL
                 if(nchar(input$vowels) == 0 | nchar(input$cons) == 0 ){
                   e$e$message = "Please provide consonants AND vowels"
                 }
                 tryCatch({
                   stims$lang = langgen(input$langlen, engdict, 
                                        input$maxinitial, 
                                        input$vowels, 
                                        input$cons, 
                                        c(input$trfrom, input$trto)
                   )
                 }, error = function(e){e$e <<- e})
               })
  
  
  observeEvent(eventExpr = input$makestims,
               handlerExpr = {
                 if(is.null(stims$lang) | is.null(stims$words) ){
                   e$e$message = "Please do steps (1) and (2) first."
                 } else {
                   stims$stims=NULL
                   tryCatch({
                     withProgress(message="Combining stims, will take a moment", expr =  {
                       stims$stims = stimgen(stims$words, 
                                             stims$lang, 
                                             input$nlang, 
                                             input$editmin, 
                                             input$cons, 
                                             input$maxinitial,
                                             input$constraininitials)
                       global$ready = sample(1:length(stims$stims), 1) # for demo view
                     })
                   }, error = function(e){e$e <<- e})
                 }
               })
  # this is for the experiment demo:
  observeEvent(eventExpr = input$wordchoice,
               handlerExpr = {
                 global$demo = runif(1)
               }
  )
  
  output$errorbar <- renderPrint({
    if(!is.null(e$e)) print(e$e$message, quote=F)
  })
  
  output$langexample <- renderPrint({
    if(length(stims$lang)>0) {
      print(paste("Generated a language of", length(stims$lang), "non-English words, for example:"),quote=F)
    } else { 
      print("Artificial language not generated (yet).", quote=F)
    }
    print(paste(head(sample(stims$lang), 30), collapse=" "), quote=F)
  })
  
  output$wordexample <- renderPrint({
    tryCatch({
      if(!is.null(stims$words)) {
        print(paste("Generated", length(stims$words), 
                    "English word samples, where the target (similar) pairs and their frequencies are:"),quote=F)
        x = sort(table( attr( stims$words, "globalx" )))
        print(paste(names(x),":",x, sep="", collapse="  "), quote=F)
      } else {
        print("English lexicon samples not generated (yet).", quote=F)
      }
    }, error = function(e){e$e <<- e})
  })
  
  output$stimsexample <- renderPrint({
    tryCatch({
      if(!is.null(stims$stims)) {
        print(paste("Generated stims for", length(stims$stims), 
                    "games; the first", input$nsimilar, "pair(s) in each stim = the 'similar'/target ones:"),quote=F)
        
        for(i in order(sapply(stims$words, "[[", 1)) ){
          print(" ", quote=F)
          print(paste(stims$words[[i]], collapse=" "), quote=F)
          print(paste(stims$stims[[i]], collapse=" "), quote=F)
        }
      } else {
        print("Game stims not generated (yet).", quote=F)
      }
    }, error = function(e){e$e <<- e})
  })
  
  
  # experiment demo view
  output$moreControls <- renderUI({
    global$demo
    global$ready
    if(!is.null(stims$stims)){
      i=global$ready
      we=sample(stims$words[[i]], 2)
      le=stims$stims[[i]] #sample(stims$stims[[i]])
      lb=sample(c(T,T,T,F),1)
      return(
        tagList(
          h1(paste0("Score: ", 0, "/", 450)),
          h2(HTML(paste0(
            paste(rep("&nbsp;", sample(0:15,1)),collapse=""),
            we[1] , 
            ifelse(lb, "<br>", paste(rep("&nbsp;", sample(3:8,1)),collapse="")),
            paste(rep("&nbsp;", sample(0:15,1)),collapse=""),
            we[2],
            ifelse(!lb,"<br>&nbsp;&nbsp;", "")
            
            ) )),
          ## Use radio or actionButtons?
          radioButtons(inputId = "wordchoice",
                       label = paste0('Communicate "',
                                      we[sample(1:2,1)],
                                      '" using...' ),
                       choices = le,
                       selected = character(0)
          ),
          HTML("<br><br><br><br>"),
          h1(HTML(paste0("(this shows the view of the sender; clicking on the options refreshes the demo)<br>
                  (the English words in this game: ", paste0(stims$words[[i]], collapse=" "))))
        )
      )
    } else {
      return(
        h2("When example stims are generated (middle tab), the experiment demo will appear here.")
      )
    }
  })
  
  
  
}
shinyApp(ui, server)
