library(tidyverse)
library(shiny)
library(shinythemes)
library(rdrop2)
library(shinyjs)
library(lubridate)
library(ggplot2)

token <- read_rds(paste(getwd(),"/Documents/token.rds",sep=""))
drop_download(dtoken = token,path = "ChatSurvey/Documents/QuestionsData.csv",local_path = paste(getwd(),"/Documents/",sep=""),overwrite = T)

questions <- read_csv(paste(getwd(),"/Documents/QuestionsData.csv",sep=""))

currentchatids <- questions %>% distinct(chatid)  %>% as.list()
randomizedchatid <- questions %>% distinct(chatid) %>%  sample_n(1) %>% as.numeric()
currentchatorders <- questions %>% filter(chatid==randomizedchatid) %>% select(chatorder) %>% as.list()

# Define UI for application that draws a histogram
ui <- fluidPage(
        
    theme = shinytheme("flatly"),
    useShinyjs(),

    # Application title
    titlePanel(title ="User Emotion Survey in Chatbot Context",windowTitle = "Survey"),
    
    tags$head(
        tags$style(HTML("
                          @import url('//fonts.googleapis.com/css?family=Rancho');
                          
                          h2 {
                            font-family: 'Arial';
                            line-height: 1.1;
                            color: TEAL;
                            background-color:MINTCREAM;
                            text-align:center;
                          }
                          
                          h3 {
                            font-family: 'Arial';
                            line-height: 1.1;
                            color: WhiteSmoke;
                            background-color:LightSlateGray;
                            text-align:center;
                          }
                          
                          h4 {
                            font-family: 'Arial';
                            line-height: 1.1;
                            color: teal;
                            text-align:center;
                          }
                          
                          h5 {
                            font-family: 'Arial';
                            line-height: 1.1;
                            color: navy;
                            text-align:center;
                          }
                          
                          div .one{
                            font-family: Cursive;
                            font-size: 25px;
                            line-height: 1.1;
                            color: black;
                            text-align:center;
                            border: 1px solid grey;
                          }
                          
                          div .two{
                            font-family: Cursive;
                            font-size: 20px;
                            line-height: 1.1;
                            color: darkgrey;
                            text-align:center;
                            border: 1px grey;
                          }
                          
                          div .three{
                            font-family: Cursive;
                            font-size: 20px;
                            line-height: 1.1;
                            color: lightgrey;
                            text-align:center;
                            border: 1px grey;
                          }
                          
                          div .note{
                            font-family: Cursive;
                            font-size: 20px;
                            line-height: 1.1;
                            color: grey;
                            text-align:center;
                            border: 5px solid black;
                            background-color: black;
                          }
                          
                          .ima {
                          filter:invert(100%);
                          }
                          
                          .imar {
                          filter:hue-rotate(40deg)
                          }
        "))
    ),
    
    tags$h3(id="welcome","Welcome"),
    

    tags$h5(id="note","This survey shows you various Customer and Chatbot interaction texts. 
            You need to read the conversation and submit your opinion if the customer is happy or angry at each highlighted sentence, if so at what level (on 0-5 scale). 
            There is a timer set to rate each sentence in the chat. 
            Additionally, there is a 'Call the Manger' button that can be used only once at any time in the chat, through which you can express your extreme disapporval/anger to the way Bot is serving Customer (i.e., Customer no longer wants to chat with the Bot and needs a human for assistance).
             Your final score will be show after completing each chat survey, based on time taken to complete the survey and accuracy of the scores you provide."),
    
    fluidRow(
        column(
            actionButton(inputId = "begin",
                         label = "Begin Survey")
            ,offset = 10,width = 2,aling="center"),style = "background-color: MINTCREAM;"
    ),
    
    fluidRow(
        column(actionButton(inputId = "refreshbtn",
                            label = "Review Another Chat")
               ,width = 2),
        column(selectInput("currentchatid","Select Chat:",currentchatids,selected = randomizedchatid),width = 2),
        column(selectInput("currentchatorder","Chat Order:",currentchatorders,selected = 2),width = 2),
        #column(,align="center",width = 3),
        column(h4(textOutput("currentTime")),plotOutput("Chart",width = 100,height = 100),align="center",width = 5),style = "background-color: MINTCREAM;"
    ),
    
    fluidRow(style = "padding-top:10px;background-color: LightSlateGray;"),
    
    fluidRow(
        column(tags$div(id="top51",class="three",p(textOutput("usrln1"))),width = 2),
        column(tags$div(id="top52",class="three",p(textOutput("txtln1"))),width = 10)
    ),
    fluidRow(
        column(tags$div(id="top41",class="two",p(textOutput("usrln2"))),width = 2),
        column(tags$div(id="top42",class="two",p(textOutput("txtln2"))),width = 10)
    ),
    fluidRow(
        column(tags$div(id="top31",class="one",p(textOutput("usrln3"))),width = 2),
        column(tags$div(id="top32",class="one",p(textOutput("txtln3"))),width = 10)
    ),
    fluidRow(
        column(tags$div(id="top21",class="two",p(textOutput("usrln4"))),width = 2),
        column(tags$div(id="top22",class="two",p(textOutput("txtln4"))),width = 10)
    ),
    fluidRow(
        column(tags$div(id="top11",class="three",p(textOutput("usrln5"))),width = 2),
        column(tags$div(id="top12",class="three",p(textOutput("txtln5"))),width = 10)
    ),
    
    fluidRow(style = "padding-top:10px;background-color: LightSlateGray;"),

    
    fluidRow(
        column(
            sliderInput(inputId = "angerscl",
                        label = "Anger Scale:",
                        min = 0,
                        max = 5,
                        value = 0)
            ,align="center",width = 3
            ),
        column(
            sliderInput(inputId = "happyscl",
                        label = "Happy Scale:",
                        min = 0,
                        max = 5,
                        value = 0)
            ,align="center",width = 3
        ),
        column(
            tags$button(
                id = "stopchat_button",
                value ="call ",
                #class = "ima",
                img(src ="https://image.flaticon.com/icons/svg/2706/2706950.svg",
                    height = "100px",width="100px",filter= 100
                )
            )
            ,align="center",width = 3
        ),
        column(
            actionButton(inputId = "submit",
                        label = "Next Sentence")
            ,width = 3
        ),style = "padding-top:15px"
        ),
    
    fluidRow(
        column(
            imageOutput("angerpic")
            ,align="center",width = 3),
        column(
            imageOutput("happypic")
            ,align="center",width = 3),
        column(
            textOutput(outputId = "managertxt"),
            align="center",width = 3),
    ),
    fluidRow(
        column(
            actionButton(inputId = "finish",
                         label = "Submit Survey")
            ,width = 3)
    ),
    fluidRow(
        column(h4(textOutput("accuracytxt")),align="center"
               ,width = 3),
        column(h4(textOutput("timingtxt")),align="center"
               ,width = 3),
        column(
            h4(textOutput("nextroundtxt")),
            actionButton(inputId = "nextround",
                         label = "Next Round?")
            ,width = 3,align="center")
    )
    
)

server <- function(input, output, session) {
    
    rv <- reactiveValues()
    rv$mgr <- "Call the manager ?"
    rv$stop <- 0
    rv$max_chatorder <- questions %>% filter(chatid==randomizedchatid) %>% summarise(chatorder=max(chatorder)) %>% as.numeric()
    rv$timer <- 90
    rv$x0<- Sys.time() # time at the beginning the timer
    rv$usrtime <- Sys.time()
    rv$day <- format(Sys.time(), "%b %d %Y") %>% str_replace_all(" ","_")
    rv$filename <- ""
    rv$accuracy <- ""
    rv$timing <- ""
    rv$nextrndtxt <- "Do you like to challenge yourself to the next round?"
    rv$diff <- 0
    rv$norows <- 0
    hideElement("finish")
    hideElement("currentchatid")
    hideElement("currentchatorder")
    hideElement("accuracytxt")
    hideElement("timingtxt")
    hideElement("nextroundtxt")
    hideElement("nextround")
    
    hideElement("submit")
    hideElement("angerscl")
    hideElement("happyscl")
    hideElement("managertxt")
    hideElement("stopchat_button")
    hideElement("angerpic")
    hideElement("happypic")
    hideElement("happy2_button")
    hideElement("currentTime")
    hideElement("Chart")
    
    hideElement("refreshbtn")
    hideElement("usrln1")
    hideElement("usrln2")
    hideElement("usrln3")
    hideElement("usrln4")
    hideElement("usrln5")
    hideElement("txtln1")
    hideElement("txtln2")
    hideElement("txtln3")
    hideElement("txtln4")
    hideElement("txtln5")
    
    rv$local_opinion <- tibble(
        ChatID = numeric(),
        ChatOrder = numeric(),
        AngerScore = numeric(),
        HappyScore = numeric(),
        StopChat = numeric(),
        Timer = numeric()
    )
    
    onclick("begin",({
        #save and upload file
        hideElement("begin")
        refresh_survey()
    }))
    
    output$currentTime <- renderText({
        invalidateLater(1000, session)
        paste("Time left :", (round(((minute(rv$x0)-minute(Sys.time()))*60)+(second(rv$x0)-second(Sys.time())))+ rv$timer)," secs.")
    })
    
    output$Chart<-renderPlot({
        if (round(rv$x0+rv$timer-Sys.time())>0) {
            invalidateLater(1000, session) # refresh the chart every second
            print("loop")
        }
        
        t<-rv$timer
        x1<-Sys.time() # current time
        x2<-round(as.numeric((minute(x1)-minute(rv$x0))*60+(second(x1)-second(rv$x0))),0) 
        df<-data.frame(art_of_time=c("gone","left"), seconds=c(x2,t-x2))
        df$fraction<-df$seconds/t; df$ymax=cumsum(df$fraction); df$ymin = c(0, df$ymax[1])
        
        p = ggplot(df, aes(fill=art_of_time, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
            geom_rect(colour="grey30", fill=c("red","darkblue")) +
            coord_polar(theta="y") +
            xlim(c(0, 4)) + theme_bw() +
            theme(panel.grid=element_blank()) + theme(axis.text=element_blank()) +
            theme(axis.ticks=element_blank())+
            theme(
                panel.background = element_rect(fill = "mintcream",
                                                colour = "mintcream"),panel.border = element_blank())
        if (df[2,2]>0){p} else {print("stop")} 
    })
    
    output$usrln1 <- renderText({
        u1 <- questions %>% filter(chatid==input$currentchatid,chatorder==(as.numeric(input$currentchatorder)-2)) %>% select(user) %>% as.character()
        if (u1 == "character(0)") {
            u1 <- NULL
        }
        u1
    })
    output$usrln2 <- renderText({
        u2 <- questions %>% filter(chatid==input$currentchatid,chatorder==(as.numeric(input$currentchatorder)-1)) %>% select(user) %>% as.character()
        if (u2 == "character(0)") {
            u2 <- NULL
        }
        u2
    })
    output$usrln3 <- renderText({
        u3 <- questions %>% filter(chatid==input$currentchatid,chatorder==input$currentchatorder) %>% select(user) %>% as.character()
        if (u3 == "character(0)") {
            u3 <- NULL
        }
        u3
    })
    output$usrln4 <- renderText({
        u4 <- questions %>% filter(chatid==input$currentchatid,chatorder==(as.numeric(input$currentchatorder)+1)) %>% select(user) %>% as.character()
        if (u4 == "character(0)") {
            u4 <- NULL
        }
        u4
    })
    output$usrln5 <- renderText({
        u5 <- questions %>% filter(chatid==input$currentchatid,chatorder==(as.numeric(input$currentchatorder)+2)) %>% select(user) %>% as.character()
        if (u5 == "character(0)") {
            u5 <- NULL
        }
        u5
    })
    
    output$txtln1 <- renderText({
        q1 <- questions %>% filter(chatid==input$currentchatid,chatorder==(as.numeric(input$currentchatorder)-2)) %>% select(text) %>% as.character()
        if (q1 == "character(0)") {
            q1 <- NULL
        }
        q1
    })
    output$txtln2 <- renderText({
        q2 <- questions %>% filter(chatid==input$currentchatid,chatorder==(as.numeric(input$currentchatorder)-1)) %>% select(text) %>% as.character()
        if (q2 == "character(0)") {
            q2 <- NULL
        }
        q2
    })
    output$txtln3 <- renderText({
        q3 <- questions %>% filter(chatid==input$currentchatid,chatorder==input$currentchatorder) %>% select(text) %>% as.character()
        if (q3 == "character(0)") {
            q3 <- NULL
        }
        q3
    })
    output$txtln4 <- renderText({
        q4 <- questions %>% filter(chatid==input$currentchatid,chatorder==(as.numeric(input$currentchatorder)+1)) %>% select(text) %>% as.character()
        if (q4 == "character(0)") {
            q4 <- NULL
        }
        q4
    })
    output$txtln5 <- renderText({
        q5 <- questions %>% filter(chatid==input$currentchatid,chatorder==(as.numeric(input$currentchatorder)+2)) %>% select(text) %>% as.character()
        if (q5 == "character(0)") {
            q5 <- NULL
        }
        q5
    })
    
    output$managertxt <- renderText({
        if (rv$stop==0) {
            rv$mgr
        }
        else
        {
            rv$mgr <- "Manager is Informed"
            rv$mgr
        }
        })
    
    onclick("submit",({
        
        rv$local_opinion <- rv$local_opinion %>% add_row(ChatID=as.numeric(input$currentchatid),ChatOrder=as.numeric(input$currentchatorder),AngerScore=as.numeric(input$angerscl) ,HappyScore=as.numeric(input$happyscl) ,StopChat=rv$stop,Timer = round(Sys.time()-rv$usrtime))
        updateSliderInput(session = session,inputId = "angerscl",value = 0)
        updateSliderInput(session = session,inputId = "happyscl",value = 0)
        rv$timer <- 60
        rv$x0<- Sys.time()
        if (as.numeric(input$currentchatorder) == rv$max_chatorder | (as.numeric(input$currentchatorder)+1) == rv$max_chatorder) {
            finished_survey()
        }
        updateSelectInput(session,inputId ="currentchatorder",selected = as.numeric(input$currentchatorder)+2 )
        rv$usrtime <-  Sys.time()
    }))
    
    finished_survey <- function() {
        hideElement("submit")
        hideElement("angerscl")
        hideElement("happyscl")
        hideElement("managertxt")
        hideElement("stopchat_button")
        hideElement("angerpic")
        hideElement("happypic")
        hideElement("happy2_button")
        hideElement("currentTime")
        hideElement("Chart")
        hideElement("usrln1")
        hideElement("usrln2")
        hideElement("usrln3")
        hideElement("usrln4")
        hideElement("usrln5")
        hideElement("txtln1")
        hideElement("txtln2")
        hideElement("txtln3")
        hideElement("txtln4")
        hideElement("txtln5")
        showElement("finish")
        hideElement("refreshbtn")
        rv$timing <- paste("Your average time per sentence (Sec):",rv$local_opinion %>% summarise(mean(Timer)) %>% as.character())
        rv$diff <- rv$local_opinion %>% inner_join(questions,by=(c("ChatID"="chatid","ChatOrder"="chatorder"))) %>% mutate(angerdiff=abs(AngerScore-angrybaseline),happydiff=abs(HappyScore-happybaseline)) %>% mutate(diff=angerdiff+happydiff) %>% summarise(totdiff=sum(diff)) %>% as.numeric()
        rv$norows <- rv$local_opinion %>% inner_join(questions,by=(c("ChatID"="chatid","ChatOrder"="chatorder"))) %>% nrow() %>% as.numeric()
        if (rv$diff==(rv$norows*10)) {
            rv$accuracy <- paste("Your Overall Accuracy is 0%")
        }
        else if(rv$diff==0){
            rv$accuracy <- paste("Your Overall Accuracy is 100%")
        }
        else{
            rv$accuracy <- paste("Your Overall Accuracy is ",as.character((((rv$norows*10)-rv$diff)/(rv$norows*10))*100),"%")
        }
    }
    
    output$angerpic <- renderImage({
        filename <- normalizePath(file.path('./images',
                                            paste('a', input$angerscl, '.gif', sep='')))
        list(src = filename,
             width=100,
             height=100,
             alt = paste("Image number", input$angerscl))
    }, deleteFile = FALSE)
    
    output$happypic <- renderImage({
        filename <- normalizePath(file.path('./images',
                                            paste('h', input$happyscl, '.gif', sep='')))
        
        list(src = filename,
             width=100,
             height=100,
             alt = paste("Image number", input$happyscl))
        
    }, deleteFile = FALSE)
    
    onclick("stopchat_button",({
        rv$stop <- 1
        disable("stopchat_button")
    }))
    
    onclick("finish",({
        hideElement("finish")
        #local
        rv$filename <- paste("/Output/",as.character(input$currentchatid),"_",rv$day,"_",sample(1:999999,1),".csv")
        write_csv(rv$local_opinion,paste(getwd(),rv$filename,sep=""))
        #dropbox
        drop_upload(dtoken = token,paste(getwd(),rv$filename,sep=""), path = "ChatSurvey/Response",mode = "add")
        #refresh_survey()
        showElement("accuracytxt")
        showElement("timingtxt")
        showElement("nextroundtxt")
        showElement("nextround")
    }))
    
    onclick("refreshbtn",({
        #save and upload file
        refresh_survey()
    }))
    
    onclick("nextround",({
        #save and upload file
        refresh_survey()
    }))
    
    refresh_survey <- function() {
        rv$local_opinion <- rv$local_opinion %>% filter(ChatID==99999999999999999999)
        updateSliderInput(session = session,inputId = "angerscl",value = 0)
        updateSliderInput(session = session,inputId = "happyscl",value = 0)
        newchatid <- questions %>% distinct(chatid) %>%  sample_n(1) %>% as.numeric()
        newchatorders <- questions %>% filter(chatid==newchatid) %>% select(chatorder) %>% as.list()
        updateSelectInput(session,inputId ="currentchatid",selected = newchatid )
        updateSelectInput(session,inputId ="currentchatorder",choices = newchatorders,selected = 2)
        showElement("angerscl")
        showElement("submit")
        showElement("angerscl")
        showElement("happyscl")
        showElement("managertxt")
        showElement("stopchat_button")
        showElement("angerpic")
        showElement("happypic")
        showElement("happy2_button")
        showElement("currentTime")
        showElement("Chart")
        showElement("refreshbtn")
        showElement("usrln1")
        showElement("usrln2")
        showElement("usrln3")
        showElement("usrln4")
        showElement("usrln5")
        showElement("txtln1")
        showElement("txtln2")
        showElement("txtln3")
        showElement("txtln4")
        showElement("txtln5")
        hideElement("finish")
        hideElement("accuracytxt")
        hideElement("timingtxt")
        hideElement("nextroundtxt")
        hideElement("nextround")
        enable("stopchat_button")
        rv$mgr <- "Call the manager ?"
        rv$stop <- 0
        rv$x0<- Sys.time()
        rv$usrtime <- Sys.time()
        rv$timer <- 90
    }
    
    output$accuracytxt <- renderText({
        rv$accuracy
    })
    
    output$timingtxt <- renderText({
        rv$timing
    })
    
    output$nextroundtxt <- renderText({
        rv$nextrndtxt
    })
    
    output$emoticon <- renderText({
        paste("Time left :", getElement("collection"))
    })
    

}

# Run the application 
shinyApp(ui = ui, server = server)
