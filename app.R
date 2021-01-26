## app.R ##
library(shiny)
library(shinydashboard)
#library(shinydashboardPlus)
library(DT)
library(ggplot2)
library(dplyr)
library("tidyr")
library("expss")
#library(shinybusy)
library(scales)
library("tools")
library("xlsx")
library(readxl)

#add_busy_bar(color = "red", height = "8px")
ui <- dashboardPage(skin = "green",title="Rolling return app",
                    
                    dashboardHeader(title = h3("Rolling Return Application"),dropdownMenu(type = "messages", badgeStatus = "info",headerText = "Application created by - Arijit Sadhu",
                                                                                        
                                                                                          notificationItem(
                                                                                              text = "Linkedin",
                                                                                              icon("linkedin"),
                                                                                              href="https://www.linkedin.com/in/arijit-sadhu/"
                                                                                          ),
                                                                                          notificationItem(
                                                                                              text = "Whatsapp",
                                                                                              icon("whatsapp"),
                                                                                              href="https://wa.me/+918967686251?text=Hi%20Arijit"
                                                                                          )
                                                                                          
                    )),
                    
                    dashboardSidebar(
                        sidebarMenu(
                            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                            menuItem("About", tabName = "About", icon = icon("th")),
                            fileInput(
                                inputId = "filedata",
                                label = "Upload data. Choose csv file",
                                accept = c(".csv",".xlsx"),
                                placeholder = "No file selected"
                            )
                        ),actionButton(inputId="btn", label="Submit"),br()
                        # ,h5("Created by Arijit Sadhu")
                        
                    ),
                    
                    dashboardBody(
                        
                        tabItems( tabItem(tabName = "dashboard",
                                          
                                          fluidRow(
                                              box(title = "Your scheme is:",textOutput("my_csv_name"),width = 11,status="primary",solidHeader = TRUE),
                                              
                                              box(title = "1-year Rolling Return Statistics",status="primary",solidHeader = TRUE,
                                                  DT::dataTableOutput("roll_df"),width = 5,height = 460),
                                              
                                              box(title = "Plot of 1-year Rolling return distribution",status="primary",solidHeader = TRUE,
                                                  plotOutput("distPlot"),width = 6),
                                              
                                              valueBoxOutput("vbox1")
                                              
                                              #  box(
                                              #      title = "Mean Return over the years",
                                              #      textOutput("mean")
                                              #  )
                                              
                                              
                                          ),
                                          tags$head(
                                              tags$meta(name="author", content="My Name"),
                                              tags$meta(name="creation_date", content="21/08/2020")
                                          )
                                          
                                          
                                          
                                          
                        ),
                        tabItem(tabName = "About",
                                
                                h2("Few things about Rolling returns"),h4("Beta Version of App")
                                ,p("One of the ways to calculate a mutual fund's performance
                                                                           is by considering how consistently the fund has performed over 
                                                                           a period of time, because let's face it we all want our money to be invested in 
                                                                           quality schemes.For further depth on the topic of",strong("Rolling returns"), 
                                   em("click on the below link !")),br(),p("To carry out the analysis download the historical NAV values from the link given below from the website Advisorkhoj and 
                                                                                                              keep only NAV date and NAV Values and save in csv format in your local system.")
                                ,uiOutput("tab"),uiOutput("tab2")
                                
                        )
                        
                        
                        
                        )
                    )
                    
                    
)

server <- function(input, output) {
    
    
    output$my_csv_name <- renderText({
        # Test if file is selected
        if (!is.null(input$filedata$datapath)) {
            # Extract file name (additionally remove file extension using sub)
            return(sub(".csv$", "", basename(input$filedata$name)))
        } else {
            return(NULL)
        }
    })
    
    data <- reactive({
        
        ext <- file_ext(input$filedata$name)
        
        req(input$filedata)
        
        if(ext == "csv"){
        
        read.csv(input$filedata$datapath, stringsAsFactors = FALSE, header = TRUE,
                 col.names = c("NAV.Date","NAV.Value"))%>%
            mutate(NAV.Date = as.Date(NAV.Date, format = "%d-%m-%Y"))%>%
            complete(NAV.Date =seq.Date(min(NAV.Date),max(NAV.Date), by= "day") ) %>%
            fill(NAV.Value)
        }
        else{
            read.xlsx(input$filedata$datapath,
                      header = TRUE,
                      sheetIndex = 1)%>%`colnames<-`(c("NAV.Date","NAV.Value")) %>%
                mutate(NAV.Date = as.Date(NAV.Date, format = "%d-%m-%Y"))%>%
                complete(NAV.Date =seq.Date(min(NAV.Date),max(NAV.Date), by= "day") ) %>%
                fill(NAV.Value)
        }
        
    })
    
    observeEvent(input$btn,{
        cal_ret<- data.frame(Date = data()$NAV.Date, Rolling_Return = c(1:length(data()$NAV.Date)))
        r1<-min(data()$NAV.Date)
        nav1<-data()[data()$NAV.Date == r1,2]
        nav2<-data()[data()$NAV.Date == r1 + 365*1,2]
        
        
        
        cal_ret[1,2]<-round(100*((nav2/nav1)-1),digits = 3)
        for (i in 1:length(data()$NAV.Date)) {
            if(r1+i + 365*2==max(data()$NAV.Date)+1){
                break
            }
            nav1 = data()[data()$NAV.Date == r1+i,2]
            nav2 = data()[data()$NAV.Date == r1+i + 365*1 ,2]
            cal_ret[i+1,2] = round(100*((nav2/nav1)-1),digits = 3)
        } 
        cal_ret= cal_ret[!cal_ret$Rolling_Return>100,]   
        
        # output$textout <- renderDataTable(cal_ret)
    
        
        #Mean of  Rolling Retun
        #  output$mean<-renderPrint(mean(cal_ret$Rolling_Return))
        
        ######################################################################################
        
        
        roll_df<- data.frame(Category=c("<0%","0-5%","5-10%","10-15%","15-20%",">20%") , Rolling_Return=c(1:6))
        
        
        #No of times <0 % return
        roll_df[1,2]<- round(100*count_if(le(0),cal_ret$Rolling_Return)/length(cal_ret$Rolling_Return),digits = 0)
        
        # 0-5%
        roll_df[2,2]<-round(100*count_if(gt(0) & lt(5),cal_ret$Rolling_Return)/length(cal_ret$Rolling_Return),digits = 2)
        
        #5-10%
        roll_df[3,2]<-round(100*count_if(gt(5) & lt(10),cal_ret$Rolling_Return)/length(cal_ret$Rolling_Return),digits = 2)
        
        #10-15%
        roll_df[4,2]<-round(100*count_if(gt(10) & lt(15),cal_ret$Rolling_Return)/length(cal_ret$Rolling_Return),digits = 2)
        
        #15-20%
        roll_df[5,2]<-round(100*count_if(gt(15) & lt(20),cal_ret$Rolling_Return)/length(cal_ret$Rolling_Return),digits = 2)
        
        #>20%
        roll_df[6,2]<-round(100*count_if(ge(20),cal_ret$Rolling_Return)/length(cal_ret$Rolling_Return),digits = 2)  
        
        
        ###############################################################################
        
        output$roll_df<-DT::renderDataTable({
            
            
            datatable(roll_df,colnames = c("Category","Return Distribution (% of times)"))
        })
        
        output$distPlot<- renderPlot({
            
            #  ggplot(data=cal_ret)+geom_bar(aes(x=cal_ret$Rolling_Return))
            # ggplot(data=roll_df, aes(x=roll_df$Category , y=roll_df$Rolling_Return))+geom_count()
           # ggplot(roll_df)+geom_bar(aes(x=Category , y=Rolling_Return), stat = "identity",width = 0.5,fill="steelblue")+theme_minimal()+ylab("Frequency of Return")
            ggplot(data = cal_ret,aes(x=cal_ret$Date,y=cal_ret$Rolling_Return))+geom_line(color = "blue")+labs(x = "Timeline", y = "Rolling return %") +
                scale_x_date(labels = date_format("%b-%Y"))
            
            })
        
        
        
        
        
        
        m<-round(mean(cal_ret$Rolling_Return),digits = 3)
        output$vbox1 <- renderValueBox({ valueBox( subtitle = "Average Rolling Return over the years",m, width = 4,icon = icon("percent"),color = "light-blue")})
        
    }, ignoreNULL = FALSE)
    
    url <- a("Click Here!", href="https://zerodha.com/varsity/chapter/rolling-returns/")
    output$tab <- renderUI({
        tagList("Zerodha Varsity:", url)
    })
    url2 <- a("Click Here!", href="https://www.advisorkhoj.com/mutual-funds-research/historical-NAV/")
    output$tab2 <- renderUI({
        tagList("Download historical NAV values:", url2)
    })
    
}

shinyApp(ui, server)