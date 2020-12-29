runShinyImbTreeAUC <- function( launch.browser = T ){
  
  app <- shinyApp(
    
    #### UI #### 
    ui = navbarPage( "ImbTreeAUC",
      
      #### Description ####                
      tabPanel( "Description",
        
        fluidPage(
          h2("An R package for building classification trees using Area Under the ROC Curve (AUC) on the imbalanced datasets"),
          
          br(),
          
          h3("Description"),
          "An R package for building binary and multiclass decision tree algorithms using Area Under the Receiver Operating Characteristic (ROC) Curve, 
          to measure impurity of a node. The package provides non-standard measures to select an optimal split point for an attribute 
          as well as the optimal attribute for splitting through the application of local, semi-global and global AUC measures. 
          Additionally, ImbTreeAUC is able to handle imbalanced data which is a challenging issue in many practical applications. 
          The package supports cost-sensitive learning by defining a misclassification cost matrix and weight sensitive learning. 
          It accepts all types of attributes, including continuous, ordered and nominal.",
          br(),
          
          h3("Author"),
          
          a(href="http://krzysztof_gajowniczek.users.sggw.pl/", "Krzysztof Gajowniczek, PhD"),
          h3("Uploading Files"),
          
          p(strong("data:"), "Data.frame in which to interpret the parameters Target and Attributes.
          Accepted extensions: text/csv, text/comma-separated-values,text/plain, .csv, .arff"),
          h3("Fit Model"),
          
          p(strong("Target:"), "Name of the target variable. Character vector of one element."),
          p(strong("Attributes:"), "Attribute names used for target (Target) modelling. Character vector of many elements."),
          p(strong("Method:"), "Method used for learning. Character vector of one element with one of the: AUCl, AUCs, AUCg."),
          p(strong("Depth:"), "Set the maximum depth of any node of the final tree, with the root node counted as depth 0. 
            Numeric vector of one element which is greater or equal to 1."),
          p(strong("Min obs:"), "The minimum number of observations that must exist in any terminal node (leaf). 
            Numeric vector of one element which is greater or equal to 1."),
          p(strong("Level Positive	:"), "Name of the positive class (label) used in AUC calculation, 
            i.e. predictions being the probability of the positive event; character vector of one element"),
          p(strong("Overfitting method:"), "Character vector of one element with one of the: none, leafcut, prune, avoid 
            specifying which method overcoming overfitting should be used. leafcut method is used when the full tree is built, 
            it reduces the subtree when both siblings choose the same class label. avoid method is incorporated during 
            the recursive partitioning, it prohibit the split when both sibling chose the same class. 
            prune method employs pessimistic error pruning procedure, it should be specified along with the cf parameter."),
          p(strong("Cp:"), "Complexity parameter, i.e. any split that does not decrease the overall lack of fit by a factor of cp is not attempted.
            It refers to miss-classification error. If cost or weights are specified aforementioned measure takes these parameter into account. 
            Works for overfitting methods: none, leafcut, avoid. Numeric vector of one element which is greater or equal to 0."),
          p(strong("Cf:"), "Numeric vector of one element with the number in (0, 0.5] for the optional pessimistic-error-rate-based pruning step"),
          p(strong("AUCweight:"), "Method used for AUC weighting in multiclass classification problems; character vector of one element with one of the: none, bySize, byCost."),
          p(strong("Weights:"), "Numeric vector of cases weights. It should have as many elements as the number of observation in the data.frame passed to the data parameter."),
          p(strong("Cost:"), "Matrix of costs associated with the possible errors. The matrix should have k columns and rows, 
            where k is the number of class levels. Rows contain true classes while columns contain predicted classes. 
            Rows and columns names should take all possible categories (labels) of the target variable."),
          p(strong("Class threshold:"), "Method used for determining thresholds based on which the final class for each node is derived. 
            If cost is specified it can take one of the following: theoretical, tuned, otherwise it takes equal. Character vector of one element."),
          p(strong("Ambiguous probability:"), "Ambiguity threshold for the difference between the highest class probability and the second highest class 
            probability per node, below which the expert has to make a decision regarding the future tree structure. 
            Logical vector with one element. It works when the Ambiguous class parameter is NULL."),
          p(strong("Top splits:"), "Number of best splits, i.e. final trees structure to be presented. 
            Splits are sorted in descending order according to the information gain. Numeric vector with one element."),
          p(strong("Attribute level:"), "Decision indicating whether possible best splits are derived on 
            the attribute level (higher) or on the split point for each attribute (lower). 
            TRUE means that the expert gets the best splits, one for each variable. 
            FALSE means the best splits at all where it might happen that the expert receives Top splits splits coming from only one variable. 
            Logical vector with one element."),
          p(strong("Ambiguous class:"), "Labels of classes for which the expert will make a decision during the learning. 
            Character vector of many elements (from 1 up to number of classes). 
            Should have the same number of elements as vector passed to the Ambiguous class frequencies parameter."),
          p(strong("Ambiguous class frequencies:"), "Classes frequencies per node above which the expert will make a 
            decision. Numeric vector of many elements (from 1 up to number of classes). 
            Should have the same number of elements as vector passed to the Ambiguous class parameter."),
          h3("Accuracy"),
          
          "Accuracy measures.",
          
          h3("Rules"),
          
          "Data frame with the extracted decision rules along with the following performance measures: Support, Confidence, Lift, Conviction, AddedValue, Cosine, Jaccard, Laplace, Leverage."
        
        )
      ),
      
      #### Static Model ####
      tabPanel("Static Model",
               
       shinyjs::useShinyjs(),
       
        tabsetPanel(
          
          tabPanel( "Uploading Files", 
                    
            sidebarLayout(
              
              sidebarPanel(
                
                fileInput("fileS", "Choose Table", multiple = TRUE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv", ".arff")),
                
                hr(),
                
                checkboxInput("headerS", "Header", TRUE),
                
                radioButtons("sepS", "Separator", choices = c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = ","),
                
                radioButtons("quoteS", "Quote", choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"), selected = '"')
                
              ),
              
              mainPanel(
                
                tableOutput("InputFileS")
                
              )
              
            )
            
          ),
          
          tabPanel( "Fit Model",
                    
            sidebarLayout(
              
              sidebarPanel(
                
                actionButton( "startS", "Start Learning"),
                
                hr(),
                
                textOutput("wrongTargetS"),
                
                selectInput("Y_nameS", "Target", ""),
                
                selectInput("X_namesS", "Attributes", "", multiple = TRUE),
                
                selectInput("typeS", "Method", choices = list("AUCs", "AUCg")),
                
                numericInput("depthS", "Depth", value = 5, 1, 32, 1),
                
                numericInput("min_obsS", "Min obs", value = 10, 1, Inf, 1),
                
                textOutput("wrongAUCweightS"),
                
                uiOutput("LevelPosAUCweightS"),
                
                selectInput("overfitS", "Overfitting method", choices = list("none", "leafcut", "prune", "avoid"), selected = "leafcut"),
                
                uiOutput("overfittingS"),
                
                textOutput("wrongThresholdS"),
                
                selectInput("class_thS", "Classification threshold", choices = list("equal", "theoretical", "tuned"), selected = "equal"),
                
                selectInput("weightscostS", "Cost-sensitive type", choices = list("none", "weights", "cost matrix"), selected = "none"),
                
                div(
    
                  id = "AreaWeightsS",
                  selectInput("WeightsS", "Weights", ""),
                  textOutput("wrongWeightsS")
                  
                )
                
              ),
              
              mainPanel(
                
                tabsetPanel(id = "tabsestS",
                  
                  tabPanel( "Tree",   
                            
                    br(),
                            
                    shinyjs::hidden(div(
                      
                      id = "AreaSaveS",
                      downloadButton("saveS", "Save Tree")
                      
                    )),
                    
                    hr(),
                    
                    verbatimTextOutput("plotS")
                    
                  
                  ),
    
                  tabPanel( "Cost matrix", 
                            
                    DTOutput( "CostMatS" )
                            
                  )
    
                )
              
              )
            
            )
            
          ),
          
          
          tabPanel("Accuracy",
              
            verbatimTextOutput("AccuracyS")
            
          ),
    
          tabPanel("Rules",
              
            br(),
            
            shinyjs::hidden(div(
              
              id = "AreaRulesS",
              downloadButton("saveRulesS", "Save Rules")
              
            )),
            
            hr(),
            
            verbatimTextOutput("RulesS")
            
          )
          
        )
       
      ),
      
      #### Tune Model ####
      tabPanel("Tune Static Model",
               
      shinyjs::useShinyjs(), 
      
       tabsetPanel(
         
         tabPanel( "Uploading Files", 
                   
           sidebarLayout(
             
             sidebarPanel(
               
               
               fileInput("fileT", "Choose Table", multiple = TRUE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv", ".arff")),
               
               hr(),
               
               checkboxInput("headerT", "Header", TRUE),
               
               radioButtons("sepT", "Separator", choices = c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = ","),
               
               radioButtons("quoteT", "Quote", choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"), selected = '"')
               
             ),
             
             mainPanel(
               
               tableOutput("InputFileT")
               
             )
             
           )
           
         ),
         
         tabPanel( "Fit Models",
                   
           sidebarLayout(
             
             sidebarPanel(
               
               actionButton( "startT", "Start Learning"),
               
               hr(),
               
               numericInput("kfoldT", "Number of Folds", value = 10, 2, 100, 1),
               
               numericInput("seedT", "Seed for PRNG", value = 666, 1, .Machine$integer.max, 1),
               
               hr(),
               
               textOutput("wrongTargetT"),
               
               selectInput("Y_nameT", "Target", ""),
               
               selectInput("X_namesT", "Attributes", "", multiple = TRUE),
               
               selectInput("typeT", "Method", multiple = TRUE, choices = list("AUCs", "AUCg")),
               
               textOutput("wrongAUCweightT"),
               
               uiOutput("LevelPosAUCweightT"),
               
               sliderInput("depthT", "Depth", 1, 32, c(5,5), 1),
               
               sliderInput("min_obsT", "Min obs", 1, Inf, c(5,5), 1),
               
               selectInput("overfitT", "Overfitting method", multiple = TRUE, choices = list("none", "leafcut", "prune", "avoid"), selected = "leafcut"),
               
               numericInput("stepcpT", "Step for Cp", value = 0.1, 0.001, 1, 0.001),
               
               sliderInput("cpT", "Cp", 0, 1, c(0,0), 0.001),
               
               numericInput("stepcfT", "Step for Cf", value = 0.05, 0.001, 1, 0.001),
               
               sliderInput("cfT", "Confidence intervals", 0.001, 0.5, c(0.25,0.25), 0.001),
               
               selectInput("class_thT", "Classification threshold", choices = list("equal", "theoretical", "tuned"), selected = "equal"),
               
               selectInput("weightscostT", "Cost-sensitive type", choices = list("none", "weights", "cost matrix"), selected = "none"),
               
               div(
                 
                 id = "AreaWeightsT",
                 selectInput("WeightsT", "Weights", ""),
                 textOutput("wrongWeightsT")
                 
               )
               
             ),
             
             mainPanel(
               
               tabsetPanel( id = "tabsestT", 
                 
                 tabPanel( "Cost matrix", 
                       
                    DTOutput( "CostMatT" )
                       
                 ),
                 
                 tabPanel( "Train Average",
                           
                   hr(),
                   
                   shinyjs::hidden(div(
                     
                     id = "AreaTrainAggT",
                     downloadButton("saveTrainAggT", "Save Results")
                     
                   )),
                   
                   hr(),
                   
                   DTOutput("TrainAggT")
                   
                 ),
                 
                 tabPanel( "Valid Average",
                           
                   hr(),
                   
                   shinyjs::hidden(div(
                     
                     id = "AreaValidAggT",
                     downloadButton("saveValidAggT", "Save Results")
                     
                   )),
                   
                   hr(),
                   
                   DTOutput("ValidAggT")
                   
                 ),
                 
                 tabPanel( "Train Detailed",
                           
                   hr(),
                   
                   shinyjs::hidden(div(
                     
                     id = "AreaTrainT",
                     downloadButton("saveTrainT", "Save Results")
                     
                   )),
                   
                   hr(),
                   
                   DTOutput("TrainT")
                   
                 ),
                 
                 tabPanel( "Valid Detailed",
                           
                   hr(),
                   
                   shinyjs::hidden(div(
                     
                     id = "AreaValidT",
                     downloadButton("saveValidT", "Save Results")
                     
                   )),
                   
                   hr(),
                   
                   DTOutput("ValidT")
                   
                 )
                 
               )
               
             )
             
           )
           
         )
         
       )
       
      ),
      
      #### Interactive Model ####
      tabPanel( "Interactive Model",
                
        shinyjs::useShinyjs(),
        
        tabsetPanel(
          
          tabPanel( "Uploading Files",
                    
            sidebarLayout(
              
              sidebarPanel(
                
                fileInput("fileI", "Choose Table", multiple = TRUE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv", ".arff")),
                
                hr(),
                
                checkboxInput("headerI", "Header", TRUE),
                
                radioButtons("sepI", "Separator", choices = c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = ","),
                
                radioButtons("quoteI", "Quote", choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"), selected = '"')
              ),
              
              mainPanel(
                
                tableOutput("InputFileI")
                
              )
              
            )
            
          ),
    
          tabPanel( "Fit Model",
                    
            sidebarLayout(
              
              sidebarPanel(
                
                actionButton( "startI", "Start Learning"),
                
                hr(),
                
                shinyjs::hidden(div(
                  
                  id = "decisionAreaTree",
                  uiOutput("Ntree"),
                  actionButton("decisionI","Confirm Decision and Resume Learning"),
                  
                )),
                
                hr(),
                
                textOutput("wrongTargetI"),
                
                selectInput("Y_nameI", "Target", ""),
                
                selectInput("X_namesI", "Attributes", "", multiple = TRUE),
                
                selectInput("typeI", "Method", choices = list("AUCs")),
                
                numericInput("depthI", "Depth", value = 5, 1, 32, 1),
                
                numericInput("min_obsI", "Min obs", value = 10, 1, Inf, 1),
                
                textOutput("wrongAUCweightI"),
                
                uiOutput("LevelPosAUCweightI"),
                
                numericInput("top_splitI", "Top splits", value = 2, 1, 10, 1),
                
                selectInput("attr_levI", "Attribute level", choices = list("TRUE", "FALSE"), selected = "TRUE"),
                
                selectInput("amb_decI", "Ambiguity type", choices = list("Probability", "Class"), selected = "Probability"),
                
                uiOutput("amb_typeI"),
                
                selectInput("overfitI", "Overfitting method", choices = list("none", "leafcut", "prune", "avoid"), selected = "leafcut"),
                
                uiOutput("overfittingI"),
                
                selectInput("class_thI", "Classification threshold", choices = list("equal", "theoretical", "tuned"), selected = "equal"),
                
                selectInput("weightscostI", "Cost-sensitive type", choices = list("none", "weights", "cost matrix"), selected = "none"),
                
                div(
                  
                  id = "AreaWeightsI",
                  selectInput("WeightsI", "Weights", ""),
                  textOutput("wrongWeightsI")
                  
                )
                
              ),
              
              mainPanel(
                
                tabsetPanel( id = "tabsestI",
                             
                  tabPanel( "Tree", 
                            
                    br(),
                    
                    shinyjs::hidden(div(
                      
                      id = "AreaSaveI",
                      downloadButton("saveI", "Save Tree")
                      
                    )),
                    
                    hr(),
                    
                    verbatimTextOutput("plotI")
                    
                  ),
                  
                  tabPanel( "Cost matrix", 
                            
                    DTOutput( "CostMatI" )
                            
                  ),
                  
                  tabPanel( "Ambiguity matrix", 
                            
                    DTOutput( "AmbMatI" )
                            
                  )
                  
                )
                
              )
              
            )
            
          ),
    
          tabPanel("Accuracy",
              
            verbatimTextOutput("AccuracyI")
            
          ),
    
          tabPanel("Rules",
            
            br(),
                   
            shinyjs::hidden(div(
              
              id = "AreaRulesI",
              downloadButton("saveRulesI", "Save Rules")
              
            )),
            
            hr(),
            
            verbatimTextOutput("RulesI")
            
          )
          
        )
        
      ),
      
      #### Predict ####
      tabPanel( "Predict New Data",
                
        tabsetPanel(
          
          tabPanel( "Uploading Data",
                    
            sidebarLayout(
              
              sidebarPanel(
                
                fileInput("fileP", "Choose Table", multiple = TRUE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv", ".arff")),
                
                hr(),
                
                checkboxInput("headerP", "Header", TRUE),
                
                radioButtons("sepP", "Separator", choices = c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = ","),
                
                radioButtons("quoteP", "Quote", choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"), selected = '"')
                
              ),
              
              mainPanel(
                
                tableOutput("InputFileP")
                
              )
              
            )
            
          ),
          
          tabPanel( "Uploading Tree",
                    
            sidebarLayout(
              
              sidebarPanel(
                
                fileInput("treeP", "Choose Tree", multiple = TRUE, accept = ".rds")
                
              ),
              mainPanel(
                
                verbatimTextOutput("plotP")
                
              )
              
            )
            
          ),
          
          tabPanel("Predict Data",
                   
            fluidPage(
              
              br(),
              
              actionButton("startP", "Predict observations"),
              
              hr(),
              
              shinyjs::hidden(div(
                
                id = "AreaSaveP",
                downloadButton("saveP", "Save Table")
                
              )),
              
              hr(),
              
              tableOutput("Pred")
              
            )
            
          )
        
        )
        
      )
      
    ),
  
    #### Server ####
    server <- function( input, output, session ) {
      
      #### Init #### 
      options( width = 10000 )
      
      sapply( c("OkTargetS","OkWeightsS","OkfileS","costMatS","costMatProxyS","TarLevelsS",
                "OkTargetT","OkWeightsT","OkfileT","costMatT","costMatProxyT","TarLevelsT",
                "OkTargetI","OkWeightsI","OkfileI","costMatI","costMatProxyI","TarLevelsI","ambMatI","ambMatProxyI"), 
              function(x){ assign(x, T, envir = .GlobalEnv) } )
      
      #### Input files ####
      input_fileS <- reactive({
        
        req( input$fileS )
        
        OkfileS <<- T
        
        if( length( grep("arff",input$fileS$name) ) ){
          
          dfS <- read.arff( input$fileS$datapath )
          
        }else{
          
          dfS <- read.table( input$fileS$datapath, header = input$headerS, sep = input$sepS, quote = input$quoteS, check.names = F, stringsAsFactors = T )
          
        }
        
        if( any( sapply( dfS, anyNA ) ) ){
          
          OkfileS <<- F
          dfS <- data.frame( " " = "*** Missing values are not allowed ***", check.names = F )
          
        }
        
        return( dfS )
        
      })
      
      input_fileT <- reactive({
        
        req( input$fileT )
        
        if( length( grep("arff",input$fileT$name) ) ){
          
          dfT <- read.arff( input$fileT$datapath )
          
        }else{
          
          dfT <- read.table( input$fileT$datapath, header = input$headerT, sep = input$sepT, quote = input$quoteT, check.names = F, stringsAsFactors = T )
          
        }
        
        if( any( sapply( dfT, anyNA ) ) ){
          
          dfT <- data.frame( " " = "*** Missing values are not allowed ***", check.names = F )
          
        }
        
        return( dfT )
        
      })
      
      input_fileI <- reactive({
        
        req( input$fileI )
        
        if( length( grep("arff",input$fileI$name) ) ){
          
          dfI <- read.arff( input$fileI$datapath )
          
        }else{
          
          dfI <- read.table( input$fileI$datapath, header = input$headerI, sep = input$sepI, quote = input$quoteI, check.names = F, stringsAsFactors = T )
          
        }
        if( any( sapply( dfI, anyNA ) ) ){
          
          dfI <- data.frame( " " = "*** Missing values are not allowed ***", check.names = F )
          
        }
        
        return( dfI )
        
      })
      
      input_fileP <- reactive({
        
        req( input$fileP )
        
        if( length( grep("arff",input$fileP$name) ) ){
          
          dfP <- read.arff( input$fileP$datapath )
          
        }else{
          
          dfP <- read.table( input$fileP$datapath, header = input$headerP, sep = input$sepP, quote = input$quoteP, check.names = F, stringsAsFactors = T )
          
        }
        if( any( sapply( dfP, anyNA ) ) ){
          
          dfP <- data.frame( " " = "*** Missing values are not allowed ***", check.names = F )
          
        }
        
        return( dfP )
        
      })
      
      #### Input tree Predict ####
      input_treeP <- reactive({
        
        req( input$treeP )
        
        treeP <- readRDS( input$treeP$datapath )
        
        return( treeP )
        
      })
      
      #### Dynamic parameters Static ####
      observe({
        
        if( input$weightscostS %in% c("weights") ){
          
          shinyjs::show("AreaWeightsS")
          
        }else{
          
          shinyjs::hide("AreaWeightsS")
          
        }
        
      })
      
      output$overfittingS <- renderUI({
        
        if( input$overfitS!= "prune" ){
          
          numericInput("cpS", "Cp", value = 0, 0, 1, 0.01)
          
        }else{
          
          numericInput("cfS", "Confidence intervals", value = 0.25, 0.01, 0.5, 0.01)
          
        }
        
      })
      
      #### Dynamic parameters Tune ####
      observe({
        
        if( input$weightscostT %in% c("weights") ){
          
          shinyjs::show("AreaWeightsT")
          
        }else{
          
          shinyjs::hide("AreaWeightsT")
          
        }
        
      })
      
      #### Dynamic parameters Interactive ####
      output$paramsI <- renderUI({
        
        if( input$typeI %in% c("Renyi", "Tsallis") ){
          
          numericInput("qI", "Q", value = 1, -10, 10, 0.01)
          
        }else if( input$typeI %in% c("Sharma-Mittal", "Sharma-Taneja", "Kapur") ){
          
          tabPanel("AlphaBetaI", 
                   
                   numericInput("aI", "Alpha", value = 1, -10, 10, 0.01),
                   numericInput("bI", "Beta", value = 1, -10, 10, 0.01)
                   
          )
          
        }
        
      })
      
      output$amb_typeI <- renderUI({
        
        if( input$amb_decI == "Probability" ){
          
          numericInput("amb_probI", "Ambiguous probability", value = 1, 0, 1, 0.01)
          
        }
        
      })
      
      observe({
        
        if( input$weightscostI %in% c("weights") ){
          
          shinyjs::show("AreaWeightsI")
          
        }else{
          
          shinyjs::hide("AreaWeightsI")
          
        }
        
      })
      
      output$overfittingI <- renderUI({
        
        if( input$overfitI != "prune" ){
          
          numericInput("cpI", "Cp", value = 0, 0, 1, 0.01)
          
        }else{
          
          numericInput("cfI", "Confidence intervals", value = 0.25, 0.01, 0.5, 0.01)
          
        }
        
      })
      
      #### Dynamic names ####
      observeEvent( input_fileS(), ignoreInit = T, { 
        
        input_namesS <- colnames( input_fileS() )
        
        updateSelectInput( session, "Y_nameS", choices = input_namesS, selected = "" )
        updateSelectInput( session, "X_namesS", choices = input_namesS )
        updateSelectInput( session, "WeightsS", choices = input_namesS, selected = "" )
        
      })
      
      observe({
        
        input_namesT <- colnames( input_fileT() )
        
        n_row <- nrow(input_fileT())
        
        updateSelectInput( session, "Y_nameT", choices = input_namesT, selected = "" )
        updateSelectInput( session, "X_namesT", choices = input_namesT )
        updateSelectInput( session, "WeightsT", choices = input_namesT, selected = "" )
        
        updateSliderInput( session, "min_obsT", min = 1, max = n_row, value = c(floor(n_row*0.05),floor(n_row*0.05)), step = 1)
        updateSliderInput( session, "qT", step = input$stepT)
        updateSliderInput( session, "aT", step = input$stepT)
        updateSliderInput( session, "bT", step = input$stepT)
        updateSliderInput( session, "cpT", step = input$stepcpT)
        updateSliderInput( session, "cfT", step = input$stepcfT)
        
      })
      
      observe({
        
        input_namesI <- colnames( input_fileI() )
        
        updateSelectInput( session, "Y_nameI", choices = input_namesI, selected = "" )
        updateSelectInput( session, "X_namesI", choices = input_namesI )
        updateSelectInput( session, "WeightsI", choices = input_namesI, selected = "" )
        
      })
      
      #### Files render ####
      output$InputFileS <- renderTable( input_fileS() )
      output$InputFileT <- renderTable( input_fileT() )
      output$InputFileI <- renderTable( input_fileI() )
      output$InputFileP <- renderTable( input_fileP() )
      
      output$plotP <- renderPrint( PrintTree( input_treeP() ) )
      
      entropy_parI <- reactive( if( input$typeI == "Shannon" ){ 1 }else if( input$typeI %in% c("Renyi","Tsallis") ){ input$qI }else{ c( input$aI, input$bI ) } )
      
      #### Static Model ####
      observeEvent( input$WeightsS, ignoreInit = T, {
        
        if( OkfileS == T & input$WeightsS != "" ){
          
          if( !is.numeric( input_fileS()[,input$WeightsS] ) ){
            
            OkWeightsS <<- F
            output$wrongWeightsS <- renderText("Weights should not be a factor")
            
          }else if( any( input_fileS()[,input$WeightsS] < 1 ) ){

            OkWeightsS <<- F
            output$wrongWeightsS <- renderText("Weights should not be less than 1")
            
          }else{
            
            OkWeightsS <<- T
            output$wrongWeightsS <- renderText("")
            
          }
          
        } 
        
      })
      
      observeEvent( input$Y_nameS, ignoreInit = T, {
        
        req( OkfileS == T & input$Y_nameS != "" )
        
        if( is.numeric( input_fileS()[,input$Y_nameS] ) ){
          
          OkTargetS <<- F
          output$wrongTargetS <- renderText( "Target should be a factor" )
          
        }else{
          
          OkTargetS <<- T
          output$wrongTargetS <- renderText("")
          
          output$LevelPosAUCweightS <- renderUI({

            if( length( levels( input_fileS()[,input$Y_nameS] ) ) == 2 ){
              
              selectInput("LevelPosS", "Level Positive", choices = levels( input_fileS()[,input$Y_nameS] ) )
              
            }else{
              
              selectInput("AUCweightS", "Multiclass AUC weight", choices = c("none", "bySize", "byCost"))
              
            }
            
          })
          
        }
        
      })
      
      observeEvent( input$Y_nameS, ignoreInit = T, {  
        
        req( input$Y_nameS )
        
        TarLevelsS <<- levels( input_fileS()[,input$Y_nameS] )
        costMatS <<- 1 - diag( length( TarLevelsS ) )
        dimnames(costMatS) <<- list( TarLevelsS, TarLevelsS )
        
        output$CostMatS <- renderDT(costMatS, selection = 'none', server = F, editable = "cell",
                                    options = list(dom = 't', pageLength = 500) )
        
        costMatProxyS <<- dataTableProxy("costMatS")
        
      })
      
      observeEvent( input$CostMatS_cell_edit, {
        
        info <- input$CostMatS_cell_edit 
        
        if( info$row != info$col & if( is.na(as.numeric(info$value)) ){ F }else{ as.numeric(info$value) > 0} ){
          
          costMatS <<- editData( costMatS, info )
          replaceData( costMatProxyS, costMatS, resetPaging = FALSE )
          dimnames( costMatS ) <<- list( TarLevelsS, TarLevelsS )
          
        }else{
          
          output$CostMatS <- renderDT( costMatS, selection = "none", server = F, editable = "cell",
                                       options = list(dom = "t", pageLength = 500) )
          
        }
        
      })
      
      observeEvent( input$weightscostS, {
        
        if( input$weightscostS == "cost matrix" ) {
          
          showTab( inputId = "tabsestS", target = "Cost matrix" )
          
        }else{
          
          hideTab( inputId = "tabsestS", target = "Cost matrix" )
          
        }
        
      })
      
      observeEvent( input$weightscostS, ignoreInit = T, {

        if( (input$AUCweightS == "byCost" & input$weightscostS == "none") ){
          
          output$wrongAUCweightS <- renderText("Set up both parameters: Multiclass AUC weight = byCost, Cost-sensitive type = cost matrix.")
          
        }else if( input$weightscostS == "weights" & input$AUCweightS == "byCost" ){
          
          output$wrongAUCweightS <- renderText("Choose only one of the: Cost-sensitive type = weights or Multiclass AUC weight = byCost.")
          
        }else{
          
          output$wrongAUCweightS <- renderText("")
          
        }
        
      })
      
      observeEvent( input$AUCweightS, ignoreInit = T, {

        if( (input$AUCweightS == "byCost" & input$weightscostS == "none") ){
          
          output$wrongAUCweightS <- renderText("Set up both parameters: Multiclass AUC weight = byCost, Cost-sensitive type = cost matrix.")
          
        }else if( input$weightscostS == "weights" & input$AUCweightS == "byCost" ){
          
          output$wrongAUCweightS <- renderText("Choose only one of the: Cost-sensitive type = weights or Multiclass AUC weight = byCost.")
          
        }else{
          
          output$wrongAUCweightS <- renderText("")
            
        }
        
      })
      
      observeEvent( input$startS, {
        
        shinyjs::hide("AreaSaveS")
        shinyjs::hide("AreaRulesS")
        
        req( input$Y_nameS, input$X_namesS, OkTargetS == T,
             (OkWeightsS == T & input$weightscostS == "weights" & input$class_thS == "equal") |
               (input$weightscostS == "none" & input$class_thS == "equal") | 
               (input$weightscostS == "cost matrix"), 
               (input$AUCweightS == "byCost" & input$weightscostS == "cost matrix") |
               (input$AUCweightS != "byCost")
        )

        TreeS <- ImbTreeAUC( Y_name = input$Y_nameS, X_names = input$X_namesS, data = input_fileS(), 
                             type = input$typeS, depth = input$depthS, min_obs = input$min_obsS, 
                             levelPositive = if( is.null(input$LevelPosS) ){ "1" }else{ input$LevelPosS },
                             cp = input$cpS, n_cores = 1, 
                             weights = if( input$WeightsS == "" | !input$weightscostS == "weights" ){ NULL }else{ input_fileS()[,input$WeightsS] }, 
                             cost = if( input$weightscostS == "cost matrix" ){ costMatS }else{ NULL },
                             AUCweight = if( input$AUCweightS != "" ){ input$AUCweightS }else{ "none" }, 
                             class_th = input$class_thS, overfit = input$overfitS, 
                             cf = ifelse( is.null(input$cfS), 0.25, input$cfS ) )
        
        shinyjs::show("AreaSaveS")
        shinyjs::show("AreaRulesS")
        
        output$plotS <- renderPrint({
          
          PrintTreeInter(TreeS)
          
        })
        
        output$AccuracyS <- renderPrint({
          
          req( OkfileS == T & input$Y_nameS != "" )
          
          stop_pred <- ImbTreeEntropy:::StopPredict( TreeS, input_fileS() )
          if( stop_pred[1,] != "OK" ){
            
            stop_pred
            
          }else{
            
            predS <- PredictTree( TreeS, input_fileS() )$Class
            tarS <- input_fileS()[,input$Y_nameS]
            cat( paste0( capture.output( confusionMatrix( tarS, predS ) ), collapse = "\n") )
            
          }
          
        })
        
        rulesS <- ExtractRules( TreeS )
        output$RulesS <- renderPrint({
          
          req( OkfileS == T )
          
          cat( paste0( capture.output( rulesS ), collapse = "\n") )
          
        })
        
        output$saveS <- downloadHandler(
          
          filename = function(){
            paste0("Tree-", Sys.time(), ".rds")
          },
          
          content = function( file ){
            saveRDS( TreeS, file )
          }
          
        )
        
        output$saveRulesS <- downloadHandler(
          
          filename = function(){
            paste0("Rules-", Sys.time(), ".txt")
          },
          
          content = function( file ){
            write.table( rulesS, file, sep = ";", row.names = F )
          }
          
        )
        
      })
      
      #### Tune Model ####
      observeEvent( input$WeightsT, ignoreInit = T, {
        
        if( OkfileT == T & input$WeightsT != "" ){
          
          if( !is.numeric( input_fileT()[,input$WeightsT] ) ){
            
            OkWeightsT <<- F
            output$wrongWeightsT <- renderText("Weights should not be a factor")
            
          }else if( any( input_fileT()[,input$WeightsT] < 1 ) ){
            
            OkWeightsT <<- F
            output$wrongWeightsT <- renderText("Weights should not be less than 1")
            
          }else{
            
            OkWeightsT <<- T
            output$wrongWeightsT <- renderText("")
            
          }
          
        } 
        
      })
      
      observeEvent( input$Y_nameT, ignoreInit = T, {
        
        req( OkfileT == T & input$Y_nameT != "" )
        
        if( is.numeric( input_fileT()[,input$Y_nameT] ) ){
          
          OkTargetT <<- F
          output$wrongTargetT <- renderText( "Target should be a factor" )
          
        }else{
          
          OkTargetT <<- T
          output$wrongTargetT <- renderText("")
          
          output$LevelPosAUCweightT <- renderUI({
            
            if( length( levels( input_fileT()[,input$Y_nameT] ) ) == 2 ){
              
              selectInput("LevelPosT", "Level Positive", choices = levels( input_fileT()[,input$Y_nameT] ) )
              
            }else{
              
              selectInput("AUCweightT", "Multiclass AUC weight", choices = c("none", "bySize", "byCost"))
              
            }
            
          })
          
        }
        
      })
      
      observeEvent( input$Y_nameT, ignoreInit = T, {  
        
        req( input$Y_nameT )
        
        TarLevelsT <<- levels( input_fileT()[,input$Y_nameT] )
        costMatT <<- 1 - diag( length( TarLevelsT ) )
        dimnames( costMatT ) <<- list( TarLevelsT, TarLevelsT )
        
        output$CostMatT <- renderDT( costMatT, selection = 'none', server = F, editable = "cell",
                                     options = list(dom = 't', pageLength = 500) )
        
        costMatProxyT <<- dataTableProxy("costMatT")
        
      })
      
      observeEvent( input$CostMatT_cell_edit,{
        
        info <- input$CostMatT_cell_edit 
        
        if( info$row != info$col & if( is.na(as.numeric(info$value)) ){ F }else{ as.numeric(info$value) > 0} ){
          
          costMatT <<- editData( costMatT, info )
          replaceData( costMatProxyT, costMatT, resetPaging = FALSE )
          dimnames( costMatT ) <<- list( TarLevelsT, TarLevelsT )
          
        }else{
          
          output$CostMatT <- renderDT( costMatT, selection = "none", server = F, editable = "cell",
                                       options = list(dom = "t", pageLength = 500) )
          
        }
        
      })
      
      observeEvent( input$weightscostT, {
        
        if( input$weightscostT == "cost matrix") {
          
          showTab( inputId = "tabsestT", target = "Cost matrix" )
          
        }else{
          
          hideTab( inputId = "tabsestT", target = "Cost matrix" )
          
        }
        
      })
      
      observeEvent( input$weightscostT, ignoreInit = T, {
        
        if( (input$AUCweightT == "byCost" & input$weightscostT == "none") ){
          
          output$wrongAUCweightT <- renderText("Set up both parameters: Multiclass AUC weight = byCost, Cost-sensitive type = cost matrix.")
          
        }else if( input$weightscostT == "weights" & input$AUCweightT == "byCost" ){
          
          output$wrongAUCweightT <- renderText("Choose only one of the: Cost-sensitive type = weights or Multiclass AUC weight = byCost.")
          
        }else{
          
          output$wrongAUCweightT <- renderText("")
          
        }
        
      })
      
      observeEvent( input$AUCweightT, ignoreInit = T, {
        
        if( (input$AUCweightT == "byCost" & input$weightscostT == "none") ){
          
          output$wrongAUCweightT <- renderText("Set up both parameters: Multiclass AUC weight = byCost, Cost-sensitive type = cost matrix.")
          
        }else if( input$weightscostT == "weights" & input$AUCweightT == "byCost" ){
          
          output$wrongAUCweightT <- renderText("Choose only one of the: Cost-sensitive type = weights or Multiclass AUC weight = byCost.")
          
        }else{
          
          output$wrongAUCweightT <- renderText("")
          
        }
        
      })
      
      observeEvent( input$startT, {
        
        shinyjs::hide("AreaTrainAggT")
        shinyjs::hide("AreaValidAggT")
        shinyjs::hide("AreaTrainT")
        shinyjs::hide("AreaValidT")
        
        req( input$Y_nameT, input$X_namesT, input$typeT, OkTargetT == T,
             (OkWeightsT == T & input$weightscostT == "weights" & input$class_thT == "equal") |
               (input$weightscostT == "none" & input$class_thT == "equal") | 
               (input$weightscostT == "cost matrix") 
        )
        
        Tune <- ImbTreeAUC:::CrossValid( Y_name = input$Y_nameT, X_names = input$X_namesT, dat = input_fileT(), 
                                         type = input$typeT, 
                                         depth = seq(input$depthT[1], input$depthT[2]), 
                                         min_obs = seq(input$min_obsT[1],input$min_obsT[2]), 
                                         levelPositive = if( is.null(input$LevelPosT) ){ "1" }else{ input$LevelPosT }, 
                                         weights = if( input$WeightsT == "" | !input$weightscostT == "weights" ){ NULL }else{ input_fileT()[,input$WeightsT] }, 
                                         cost = if( input$weightscostT == "cost matrix" ){ costMatT }else{ NULL },
                                         overfit = input$overfitT, AUCweight = input$AUCweightT,
                                         cp = seq(input$cpT[1],input$cpT[2],input$stepcpT), 
                                         cf = seq(input$cfT[1],input$cfT[2],input$stepcfT),
                                         k_fold = input$kfoldT, seed = input$seedT )
        
        shinyjs::show("AreaTrainAggT")
        shinyjs::show("AreaValidAggT")
        shinyjs::show("AreaTrainT")
        shinyjs::show("AreaValidT")
        
        output$TrainT <- renderDT({
          
          Tune$Train
          
        })
        output$TrainAggT <- renderDT({
          
          Tune$TrainAgg
          
        })
        
        output$ValidT <- renderDT({
          
          Tune$Valid
          
        })
        
        output$ValidAggT <- renderDT({
          
          Tune$ValidAgg
          
        })
        
        output$saveTune <- downloadHandler(
          
          filename = function(){
            paste0("Tune-", Sys.time(), ".txt")
          },
          
          content = function( file ){
            write.table( rulesS, file, sep = ";", row.names = F )
          }
          
        )
        
        output$saveTrainAggT <- downloadHandler(
          
          filename = function(){
            paste0("TrainAgg-", Sys.time(), ".txt")
          },
          
          content = function( file ){
            write.table( Tune$TrainAgg, file, sep = ";", row.names = F )
          }
          
        )
        
        output$saveValidAggT <- downloadHandler(
          
          filename = function(){
            paste0("ValidAgg-", Sys.time(), ".txt")
          },
          
          content = function( file ){
            write.table( Tune$ValidAgg, file, sep = ";", row.names = F )
          }
          
        )
        
        output$saveTrainT <- downloadHandler(
          
          filename = function(){
            paste0("TrainDetail-", Sys.time(), ".txt")
          },
          
          content = function( file ){
            write.table( Tune$Train, file, sep = ";", row.names = F )
          }
          
        )
        
        output$saveValidT <- downloadHandler(
          
          filename = function(){
            paste0("ValidDetail-", Sys.time(), ".txt")
          },
          
          content = function( file ){
            write.table( Tune$Valid, file, sep = ";", row.names = F )
          }
          
        )
        
      })
      
      #### Interactive Model ####
      observeEvent( input$WeightsI, ignoreInit = T, {
        
        if( OkfileI == T & input$WeightsI != "" ){
          
          if( !is.numeric( input_fileI()[,input$WeightsI] ) ){
            
            OkWeightsI <<- F
            output$wrongWeightsI <- renderText("Weights should not be a factor")
            
          }else if( any( input_fileI()[,input$WeightsI] < 1 ) ){
            
            OkWeightsI <<- F
            output$wrongWeightsI <- renderText("Weights should not be less than 1")
            
          }else{
            
            OkWeightsI <<- T
            output$wrongWeightsI <- renderText("")
            
          }
          
        } 
        
      })
      
      observeEvent( input$Y_nameI, ignoreInit = T, {
        
        req( OkfileI == T & input$Y_nameI != "" )
        
        if( is.numeric( input_fileI()[,input$Y_nameI] ) ){
          
          OkTargetI <<- F
          output$wrongTargetI <- renderText( "Target should be a factor" )
          
        }else{
          
          OkTargetI <<- T
          output$wrongTargetI <- renderText("")
          
        }
        
        output$LevelPosAUCweightI <- renderUI({
          
          if( length( levels( input_fileI()[,input$Y_nameI] ) ) == 2 ){
            
            selectInput("LevelPosI", "Level Positive", choices = levels( input_fileI()[,input$Y_nameI] ) )
            
          }else{
            
            selectInput("AUCweightI", "Multiclass AUC weight", choices = c("none", "bySize", "byCost"))
            
          }
          
        })
        
      })
      
      observeEvent( input$Y_nameI, ignoreInit = T, {  
        
        req( input$Y_nameI )
        
        TarLevelsI <<- levels( input_fileI()[,input$Y_nameI] )
        costMatI <<- 1 - diag( length( TarLevelsI ) )
        dimnames( costMatI ) <<- list( TarLevelsI, TarLevelsI )
        
        output$CostMatI <- renderDT( costMatI, selection = 'none', server = F, editable = "cell",
                                     options = list(dom = 't', pageLength = 500) )
        
        costMatProxyI <<- dataTableProxy("costMatI")
        
        ambMatI <<- data.frame( Class = TarLevelsI, Frequencies = 1 )
        
        output$AmbMatI <- renderDT( ambMatI, selection = 'none', server = F, editable = "cell",
                                    options = list(dom = 't', pageLength = 500) )
        
        ambMatProxyI <<- dataTableProxy("ambMatI")
        
      })
      
      observeEvent( input$CostMatI_cell_edit,{
        
        info <- input$CostMatI_cell_edit 
        
        if( info$row != info$col & if( is.na(as.numeric(info$value)) ){ F }else{ as.numeric(info$value) > 0} ){
          
          costMatI <<- editData( costMatI, info )
          replaceData( costMatProxyI, costMatI, resetPaging = FALSE )
          dimnames( costMatI ) <<- list( TarLevelsI, TarLevelsI )
          
        }else{
          
          output$CostMatI <- renderDT( costMatI, selection = "none", server = F, editable = "cell",
                                       options = list(dom = "t", pageLength = 500) )
          
        }
        
      })
      
      observeEvent( input$AmbMatI_cell_edit,{
        
        info <- input$AmbMatI_cell_edit 
        
        if( (info$value %in% TarLevelsI) | if( is.na(as.numeric(info$value)) ){ F }else{ (as.numeric(info$value) <= 1 & as.numeric(info$value) >= 0) }){
          
          ambMatI <<- editData( ambMatI, info )
          replaceData( ambMatProxyI, ambMatI, resetPaging = FALSE )
          
        }else{
          
          output$AmbMatI <- renderDT( ambMatI, selection = "none", server = F, editable = "cell",
                                      options = list(dom = "t", pageLength = 500) )
          
        }
        
      })
      
      observeEvent( input$weightscostI, {
        
        if( input$weightscostI == "cost matrix") {
          
          showTab( inputId = "tabsestI", target = "Cost matrix" )
          
        }else{
          
          hideTab( inputId = "tabsestI", target = "Cost matrix" )
          
        }
        
      })
      
      observeEvent( input$weightscostI, ignoreInit = T, {
        
        if( (input$AUCweightI == "byCost" & input$weightscostI == "none") ){
          
          output$wrongAUCweightI <- renderText("Set up both parameters: Multiclass AUC weight = byCost, Cost-sensitive type = cost matrix.")
          
        }else if( input$weightscostI == "weights" & input$AUCweightI == "byCost" ){
          
          output$wrongAUCweightI <- renderText("Choose only one of the: Cost-sensitive type = weights or Multiclass AUC weight = byCost.")
          
        }else{
          
          output$wrongAUCweightI <- renderText("")
          
        }
        
      })
      
      observeEvent( input$AUCweightI, ignoreInit = T, {
        
        if( (input$AUCweightI == "byCost" & input$weightscostI == "none") ){
          
          output$wrongAUCweightI <- renderText("Set up both parameters: Multiclass AUC weight = byCost, Cost-sensitive type = cost matrix.")
          
        }else if( input$weightscostI == "weights" & input$AUCweightI == "byCost" ){
          
          output$wrongAUCweightI <- renderText("Choose only one of the: Cost-sensitive type = weights or Multiclass AUC weight = byCost.")
          
        }else{
          
          output$wrongAUCweightI <- renderText("")
          
        }
        
      })
      
      observeEvent( input$amb_decI, {
        
        if( input$amb_decI == "Class" ) {
          
          showTab( inputId = "tabsestI", target = "Ambiguity matrix" )
          
        }else{
          
          hideTab( inputId = "tabsestI", target = "Ambiguity matrix" )
          
        }
        
      })
      
      observeEvent( input$startI, {
        
        shinyjs::hide("AreaSaveI")
        shinyjs::hide("AreaRulesI")
        
        req( input$Y_nameI, input$X_namesI, OkTargetI == T,
             (OkWeightsI == T & input$weightscostI == "weights" & input$class_thI == "equal") |
              (input$weightscostI == "none" & input$class_thI == "equal") | 
              (input$weightscostI == "cost matrix"),
              (input$AUCweightI == "byCost" & input$weightscostI == "cost matrix") |
              (input$AUCweightI != "byCost")
        )
        
        ImbTreeAUC:::ImbTreeAUCInterShiny1( Y_name = input$Y_nameI, X_names = input$X_namesI, data = input_fileI(), 
                                            type = input$typeI, depth = input$depthI, min_obs = input$min_obsI, 
                                            levelPositive = if( is.null(input$LevelPosI) ){ "1" }else{ input$LevelPosI }, 
                                            cp = input$cpI, n_cores = 1,
                                            weights = if( input$WeightsI == "" | !input$weightscostI == "weights" ){ NULL }else{ input_fileI()[,input$WeightsI] }, 
                                            AUCweight = if( input$AUCweightI != "" ){ input$AUCweightI }else{ "none" },
                                            cost = if( input$weightscostI == "cost matrix" ){ costMatI }else{ NULL },
                                            class_th = input$class_thI, overfit = input$overfitI, 
                                            cf = ifelse( is.null(input$cfI), 0.25, input$cfI ), 
                                            amb_prob = if( input$amb_decI == "Probability" ){ input$amb_probI }else{ NULL }, 
                                            var_lev = input$attr_levI, top_split = input$top_splitI, 
                                            amb_class = if( input$amb_decI == "Class" ){ ambMatI$Class }else{ NULL }, 
                                            amb_class_freq = if( input$amb_decI == "Class" ){ ambMatI$Frequencies }else{ NULL }, 
                                            shiny = list( input = input, output = output ) )
        
        shinyjs::show("decisionAreaTree")
        
        if( all( imb$PROCESS$finish == T ) ){
          
          TreeI <- ImbTreeAUC:::ImbTreeAUCInterShiny2()
          
          shinyjs::hide("decisionAreaTree")
          shinyjs::show("AreaSaveI")
          shinyjs::show("AreaRulesI")
          
          output$plotI <- renderPrint({
            
            PrintTreeInter(TreeI)
            
          })
          
          output$AccuracyI <- renderPrint({
            
            req( OkfileI == T & input$Y_nameI != "" )
            
            stop_pred <- ImbTreeAUC:::StopPredict( TreeI, input_fileI() )
            if( stop_pred[1,] != "OK" ){
              
              stop_pred
              
            }else{
              
              predI <- PredictTree( TreeI, input_fileI() )$Class
              tarI <- input_fileI()[,input$Y_nameI]
              cat( paste0( capture.output( confusionMatrix( tarI, predI ) ), collapse = "\n") )
              
            }
            
          })
          
          rulesI <- ExtractRules( TreeI )
          output$RulesI <- renderPrint({
            
            req( OkfileI == T )
            
            cat( paste0( capture.output( rulesI ), collapse = "\n") )
            
          })
          
          output$saveI <- downloadHandler(
            
            filename = function(){
              paste0("Tree-", Sys.time(), ".rds")
            },
            
            content = function( file ){
              saveRDS( TreeI, file )
            }
            
          )
          
          output$saveRulesI <- downloadHandler(
            
            filename = function(){
              paste0("Rules-", Sys.time(), ".txt")
            },
            
            content = function( file ){
              write.table( rulesI, file, sep = ";", row.names = F )
            }
            
          )
          
        }
        
      })
      
      observeEvent( input$decisionI, {
        
        breakREPEAT <<- 0
        repeat{

          rollout <<- 0
          ImbTreeAUC:::BuildTreeInterShiny( get("n0", envir = imb)$Tree, get("n0", envir = imb)$Y_name,
                                            get("n0", envir = imb)$X_names, get("n0", envir = imb)$data,
                                            get("n0", envir = imb)$depth, get("n0", envir = imb)$min_obs,
                                            get("n0", envir = imb)$type, get("n0", envir = imb)$levelPositive,
                                            get("n0", envir = imb)$cp, get("n0", envir = imb)$n_cores,
                                            get("n0", envir = imb)$weights, get("n0", envir = imb)$AUCweight,
                                            get("n0", envir = imb)$cost,
                                            get("n0", envir = imb)$class_th, get("n0", envir = imb)$overfit,
                                            get("n0", envir = imb)$cf, get("n0", envir = imb)$amb_prob,
                                            get("n0", envir = imb)$top_split, get("n0", envir = imb)$var_lev,
                                            get("n0", envir = imb)$amb_class, get("n0", envir = imb)$amb_class_freq,
                                            1, shiny = list( input = input, output = output) )
          
          if( all( imb$PROCESS$finish == T ) | ( breakREPEAT == 2 ) ) break 
          
        }
        
        if( all( imb$PROCESS$finish == T ) ){
          
          TreeI <- ImbTreeAUC:::ImbTreeAUCInterShiny2()
          
          shinyjs::hide("decisionAreaTree")
          shinyjs::show("AreaSaveI")
          shinyjs::show("AreaRulesI")
          
          output$plotI <- renderPrint({
            
            PrintTreeInter(TreeI)
            
          })
          
          output$AccuracyI <- renderPrint({
            
            req( OkfileI == T & input$Y_nameI != "" )
            
            stop_pred <- ImbTreeAUC:::StopPredict( TreeI, input_fileI() )
            if( stop_pred[1,] != "OK" ){
              
              stop_pred
              
            }else{
              
              predI <- PredictTree( TreeI, input_fileI() )$Class
              tarI <- input_fileI()[,input$Y_nameI]
              cat( paste0( capture.output( confusionMatrix( tarI, predI ) ), collapse = "\n") )
              
            }
            
          })
          
          rulesI <- ExtractRules( TreeI )
          output$RulesI <- renderPrint({
            
            req( OkfileI == T )
            
            cat( paste0( capture.output( rulesI ), collapse = "\n") )
            
          })
          
          output$saveI <- downloadHandler(
            
            filename = function(){
              paste0("Tree-", Sys.time(), ".rds")
            },
            
            content = function( file ){
              saveRDS( TreeI, file )
            }
            
          )
          
          output$saveRulesI <- downloadHandler(
            
            filename = function(){
              paste0("Rules-", Sys.time(), ".txt")
            },
            
            content = function( file ){
              write.table( rulesI, file, sep = ";", row.names = F )
            }
            
          )
          
        }
        
      })
      
      #### Predict ####
      observeEvent( input$startP, {
        
        shinyjs::hide("AreaSaveP")
        
        req( input_treeP(), input$treeP )
        
        stop_pred <- ImbTreeAUC:::StopPredict( input_treeP(), input_fileP() )
        if( stop_pred[1,] != "OK" ){
          
          predP <- stop_pred
          
        }else{
          
          predP <- PredictTree( input_treeP(), input_fileP() )
          
          
        }
        output$Pred <- renderTable({
          
          predP
          
        })
        
        shinyjs::show("AreaSaveP")
        
        output$saveP <- downloadHandler(
          
          filename = function(){
            paste0("Predict-", Sys.time(), ".txt")
          },
          
          content = function( file ){
            write.table( predP, file, sep = ";", row.names = F )
          }
          
        )
        
      })
      
      
    }
    
  )
  
  #### RunApp #### 
  runApp( app, launch.browser = launch.browser )
  
}