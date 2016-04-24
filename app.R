#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
options(shiny.maxRequestSize=30*1024^2)

# Define UI
ui <- shinyUI(navbarPage("What's My Next Word App!",
         tabPanel("The App",
            sidebarLayout(
               sidebarPanel(width = 3,
                  h3("Instructions"),
                  HTML("Possible predicted words will be shown as you type words in the input box.<p>"),
                  HTML("App will either try to predict next full word or auto-complete word, 
                       depending on whether inputted word is followed by a space or not, respectively.<p>"),
                  HTML("Supported language is English.<p>"),
                  HTML("Please wait until \"Ready!\" is shown below.<p>"),
                  HTML("<hr>"),
                  h3("App status"),
                  htmlOutput("status")
               ),
               mainPanel(
                  tabsetPanel(type="tabs",
                     tabPanel("Input",
                        column(12, align="center",
                           HTML("<p>&nbsp;<p>&nbsp;<p>"),
                           textInput("inputText", "Please type your text input here:", value = "", width = 800, placeholder = NULL),
                           htmlOutput("prediction")
                           
                        )
                     ),
                     tabPanel("Details",
                        column(12, align="left",
                            h4("Prediction performance:"),
                            htmlOutput("status2"),
                            hr(),
                            h4("Predicted word details:"),
                            dataTableOutput("prediction2")
                        )
                     ),
                     tabPanel("Settings",
                        column(12, align="left",
                            HTML("<p>&nbsp;<p>&nbsp;<p>"),
                            sliderInput("numberPredict", "Enter maximum number of words to predict:", min=1, max=20, value=10, step=1)
                        )
                     )
                  )
               )
            )
            
         ),
         tabPanel("How it works",
            #sidebarPanel(width = 3
            #),
            mainPanel(
               tabsetPanel(type="tabs",
                  tabPanel("Prediction Algorithm",
                     h3("Introduction"),
                     HTML("Text prediction algrithm applied in this App utilizes N-Gram model, 
                        a type of probablistic natural language processing method, 
                        and assumtion of Markov model, a theory that assumes prediction can be based on historical data.<p>"),
                     HTML("Prediction algorith is time-consuming and thus performed outside of this App to minimize performance for the UI.<p>"),
                     hr(),
                     
                     h3("Flow"),
                     img(src="ModelFlow.png"),
                     hr(),
                     
                     h3("Description"),
                     h4("Input:"),
                     HTML("Algorithm is trained from 5% random sample of corpus from news, blogs, and twitters provided by HC Corpora.  
                          Sample size was optimized to balance App load time and time required to pre-calculate Kneser-Key probabiliy.
                          At 5% sample size, the App load time is around 5 seconds, while pre-calculation time is around 4 days (using Intel i5-4250U processor)."),
                     h4("Condition:"),
                     h5("Sampled data is conditioned by changing to lowercase, removing word to ignore, and remove punctions (excluding apostrophe)."),
                     HTML("Ignore words are connecting words: <code>a</code>, <code>an</code>,  <code>the</code>, 
                           <code>and</code>, <code>to</code>, <code>of</code>, <code>at</code>, 
                           <code>in</code>, <code>on</code>, and <code>from</code>."),
                     HTML("Consequently, these words will not be produced as output of the prediction."),
                     h4("Generate N-Gram"),
                     HTML("<h5>1, 2, 3, and 4 grams are produced using R package <code>quanteda</code>."),
                     h4("Pre-calculate Probability"),
                     HTML("Kneser-Ney smoothing is applied to assign probability for each N-gram elements. For 1-gram, a standard count probability is applied."),
                     h4("Output"),
                     HTML("The produced N-gram is ordered by highest probability and saved to R data files, loaded by this App when initializing.")
                     
                  ),
                  tabPanel("App Flow",
                      h3("Introduction"),
                      hr(),
                      
                      h3("Flow"),
                      img(src="AppFlow.png"),
                      hr(),
                      
                      h3("Description"),
                      h4("Input"),
                      HTML("When this App is first started, N-Gram with pre-calculated probabilities are loaded as part of an initialization routine."),
                      HTML("The App then waits for user to input phrase into the textbox."),
                      h4("Condition"),
                      HTML("The user inputted phrase is then cleaned by changing to lower case, and removing ingored words,
                         then split into words based on space as the separator."),
                      HTML("If the final word is followed by a space, then the App will try to predict the next whole word."),
                      HTML("If the final word is not followed by a space, the App assumes that the user is in a midst of typing a word, and will try to auto-complete the word."),
                      h4("Prediction/Auto-complete"),
                      HTML("The prediction and auto-complete will attempt to use as much of the already inputted words to perform prediction using Backoff method, where
                         higher order N-Gram is prioritized before backing-off to lower order N-Gram until up to 20 matches are found (in case of duplicated predictions)"),
                      h4("Output"),
                      HTML("First 10 unique word predictions will then be displayed on the App as output to the user.")
                  
                  )
               )
            )
         ),
         tabPanel("Reports",
            tabsetPanel(type="tabs",
               tabPanel("Interim Milestone Report",
                  includeHTML("report.html")
               ),
               tabPanel("Final Presentation",
                  h5("Final presentation is a 5 slide deck intended to be a pitch for this app."),
                  hr()
               )
            )

         ),
         tabPanel("About",
            column(12, align="center",
                   
               h3("This App is built for:"),
               HTML("<p><h5><a href='https://www.coursera.org/course/dsscapstone' target='_blank'>Data Science Capstone Project</a><p>&nbsp;<p>"),
               HTML("Class session: March 7 - May 1, 2016<p>&nbsp;"),
               HTML("<img src='logo.png' style='height:50px;'><p>"),
               hr(),
               HTML("<h3>Developed by Hiroto Miyake<p>&nbsp;</h3><p>"),
               HTML("<a href='https://github.com/hirotom/DataScienceCapstoneProject' target='_blank'><img src='GitHub-Mark.png' style='width:30px;height:30px;'>GitHub repository</a>"),
               HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"),
               HTML("<a href='https://www.linkedin.com/in/hirotomiyake' target='_blank'><img src='linkedin.png' style='width:26px;height:26px;'>&nbsp;LinkedIn</a>")
            )
            
         )

))

# Define server logic
server <- shinyServer(function(input, output) {
   
   source(file="PredictNextWord.R")
   
   output$status <- renderUI({HTML("<p style='color:green'>Ready!")})
   
   output$prediction <- renderUI({
      
      if (nchar(gsub(" ", "", input$inputText))>0) {
         ptm <- proc.time()
         if (grepl("*[[:space:]]$", input$inputText)) {
            # input text ends with a space, so predict next word
            df.predict <- fun.predictnext(input$inputText, autocomplete=FALSE, n=input$numberPredict)
            type <- "Next word prediction."
            
         } else {
            # input text does not end with a space, so predict auto-complete word
            df.predict <- fun.predictnext(input$inputText, autocomplete=TRUE, n=input$numberPredict)
            type <- "Auto-complete."
         }
         df.predict <- df.predict[! is.na(df.predict$Term1),]
         df.predict$Row <- row.names(df.predict)
         df.predict <- df.predict[, c("Row", "Term1", "NGramLevel","Prob_KN")]
         ptm2 <- proc.time()
         a <- ptm2 - ptm
         output$prediction2 <- renderDataTable(df.predict)
         txtPredict = paste0("<code>", df.predict$Term1, "</code>")
         txtPredict = paste0(txtPredict, sep = "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;", collapse="")
         output$status2 <- renderUI({HTML(paste0("<p>Mode: ", type, "<p>Computation time: ", round(as.numeric(a[3]),2), " seconds.</p>"))})
         HTML(paste0("<p>&nbsp;<p>", txtPredict))
      }
      
   })
   
})

# Run the application 
shinyApp(ui = ui, server = server)

