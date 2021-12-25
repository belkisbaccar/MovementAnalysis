#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(readr)
library(tidyr)
library(RcppRoll)
library(ggplot2)
library(groupdata2)
ui <- dashboardPage( skin = "red",
    dashboardHeader(title ="Intro to DS"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Dataset", tabName = "Dataset", icon =icon("table"),
                     menuSubItem("X Mouvement", tabName = "XDataset", icon= icon("table")),
                     menuSubItem("O Mouvement", tabName = "ODataset", icon= icon("table")),
                     menuSubItem("Daily Mouvement", tabName = "DailyDataset", icon= icon("table"))
                     ),
            menuItem("Visualisation", tabName = "Visualisation", icon = icon("bar-chart-o")),
            menuItem("Processing", tabName = "Processing", icon =icon("list-alt"),
                     menuSubItem("X Mouvement", tabName = "X_processing", icon= icon("table")),
                     menuSubItem("O Mouvement", tabName = "O_processing", icon= icon("table")),
                     menuSubItem("Daily Mouvement", tabName = "N_processing", icon= icon("table")))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem("XDataset",
                    fluidPage(
                        h1("Datset X mouvement"),
                        fileInput(
                            "X",
                            "Choose X Movement CSV File",
                            accept = c(
                                "text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv"
                            )
                        ),
                        dataTableOutput("datasetX")
                    )),
            tabItem("ODataset",
                    fluidPage(
                        h1("Datset O mouvement"),
                        fileInput(
                            "O",
                            "Choose O Movement CSV File",
                            accept = c(
                                "text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv"
                            )
                        ),
                        dataTableOutput("datasetO")
                    )),
            tabItem("DailyDataset",
                    fluidPage(
                        h1("Datset daily mouvement"),
                        fileInput(
                            "N",
                            "Choose Noise Movement CSV File",
                            accept = c(
                                "text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv"
                            )
                        ),
                        dataTableOutput("datasetDaily")
                    )),
            tabItem("Visualisation",div(p("visualisation")),
                    plotlyOutput("plots",height = 800)),
            tabItem("X_processing",div(p(" X processing")),
                    
                    fluidRow(
                        box( plotOutput("X_LONG_plot", brush = "plot_brush")),
                        box(verbatimTextOutput("X_LONG_DATA"),downloadButton("Download_X_LONG", label = "Download X Long "))
                    ),
                    fluidRow(
                        box(verbatimTextOutput("X_LARGE_DATA"),
                            downloadButton("Download_X_LARGE", label = "Download X Large ")
                            ,
                            tags$head(tags$style("#X_LARGE_DATA {overflow-y:scroll; max-height: 300px;}"))
                            
                        
                            ),
                        box(sliderInput("X_t_thresh", "Time Threshhold:",
                                        min = 0, max = 1, value = 0.5
                        ),
                        sliderInput("X_m_thresh", "Mean Threshhold:",
                                    min = 0, max = 1, value = 0.2
                        ),
                        sliderInput("X_sd_thresh", "Standard Deviation Threshhold:",
                                    min = 0, max = 1, value = 0.1
                        ))
                    )
                    ),
            tabItem("O_processing",div(p("O processing")),
                    
                    fluidRow(
                        box( plotOutput("O_LONG_plot", brush = "plot_brush")),
                        box(verbatimTextOutput("O_LONG_DATA"),downloadButton("Download_O_LONG", label = "Download O Long "))
                    ),
                    fluidRow(
                        box(tags$head(tags$style("#O_LARGE_DATA {overflow-y:scroll; max-height: 300px;}")),verbatimTextOutput("O_LARGE_DATA"),downloadButton("Download_O_LARGE", label = "Download O Large ")),
                        box(sliderInput("O_t_thresh", "Time Threshhold:",
                                        min = 0, max = 1, value = 0.5
                        ),
                        sliderInput("O_m_thresh", "Mean Threshhold:",
                                    min = 0, max = 1, value = 0.2
                        ),
                        sliderInput("O_sd_thresh", "Standard Deviation Threshhold:",
                                    min = 0, max = 1, value = 0.1
                        ))
                    )
            ),
            tabItem("N_processing",div(p("Noise processing")),
                    
                    fluidRow(
                        box( plotOutput("N_LONG_plot")),
                        box(verbatimTextOutput("N_LONG_DATA"),downloadButton("Download_N_LONG", label = "Download N Long "))
                    ),
                    fluidRow(
                        box(tags$head(tags$style("#N_LARGE_DATA {overflow-y:scroll; max-height: 300px;}")),verbatimTextOutput("N_LARGE_DATA"),downloadButton("Download_N_LARGE", label = "Download N Large ")),
                        box(sliderInput("N_t_thresh", "Time Threshhold:",
                                        min = 0, max = 1, value = 0.5
                        ),
                        sliderInput("N_m_thresh", "Mean Threshhold:",
                                    min = 0, max = 1, value = 0.2
                        ),
                        sliderInput("N_sd_thresh", "Standard Deviation Threshhold:",
                                    min = 0, max = 1, value = 0.1
                        ))
                    )
            )
    ))
    
    

)

server <- function(input, output) {
    X_upload <- reactive({
        inFile <- input$X
        if (is.null(inFile))
            return(NULL)
        df <-
            read.csv(
                inFile$datapath,
                header = TRUE ,
                sep = ","
            )
        return(df)
    })
    O_upload <- reactive({
        inFile <- input$O
        if (is.null(inFile))
            return(NULL)
        df <-
            read.csv(
                inFile$datapath,
                header = TRUE ,
                sep = ","
            )
        return(df)
    })
    N_upload <- reactive({
        inFile <- input$N
        if (is.null(inFile))
            return(NULL)
        df <-
            read.csv(
                inFile$datapath,
                header = TRUE ,
                sep = ","
            )
        return(df)
    })
    output$datasetX = renderDataTable(X_upload() ,
                                      options = list(
                                          pageLength = 10
                                        
                                      ))
    output$datasetO <- renderDataTable(O_upload() ,
                                       options = list(
                                           pageLength = 10
                                           
                                       ))
    output$datasetDaily <-renderDataTable(N_upload() ,
                                          options = list(
                                              pageLength = 10
                                              
                                          ))

    
    


    
    output$plots <-renderPlotly({ 
        df1<-X_upload()
        df2<-O_upload()
        df3<-N_upload() 
        axx <- list(
            gridcolor='rgb(255, 255, 255)',
            zerolinecolor='rgb(255, 255, 255)',
            showbackground=TRUE,
            backgroundcolor='rgb(230, 230,230)'
        )
        plot_X1 <- plot_ly(
            x = df1$gFx,
            y = df1$gFy,
            z = df1$gFz,
            type = "scatter3d",
            mode = "lines", scene='scene1',name="X:G Force"
        )
        plot_X2 <-  plot_ly(
            x = df1$ax,
            y = df1$ay,
            z = df1$az,
            type = "scatter3d",
            mode = "lines", scene='scene2',name="X:Accelerometer"
        )
        plot_X3 <- plot_ly(
            x = df1$wx,
            y = df1$wy,
            z = df1$wz,
            type = "scatter3d",
            mode = "lines", scene='scene3',name="X:Gyroscope"
        )
        
        
        plot_O1 <-  plot_ly(
            x = df2$gFx,
            y = df2$gFy,
            z = df2$gFz,
            type = "scatter3d",
            mode = "lines", scene='scene4',name="O:G Force"
           
        )
        plot_O2 <-  plot_ly(
            x = df2$ax,
            y = df2$ay,
            z = df2$az,
            type = "scatter3d",
            mode = "lines", scene='scene5',name="O:Accelerometer"
            )
            
        
        plot_O3 <-  plot_ly(
            x = df2$wx,
            y = df2$wy,
            z = df2$wz,
            type = "scatter3d",
            mode = "lines", scene='scene6',name="O:Gyroscope"
        )
        
        
        plot_N1 <- plot_ly(
            x = df3$gFx,
            y = df3$gFy,
            z = df3$gFz,
            type = "scatter3d",
            mode = "lines", scene='scene7',name="Noise:G Force"
        )
        plot_N2 <- plot_ly(
            x = df3$ax,
            y = df3$ay,
            z = df3$az,
            type = "scatter3d",
            mode = "lines", scene='scene8',name="Noise:Accelerometer"
        )
        plot_N3 <- plot_ly(
            x = df3$wx,
            y = df3$wy,
            z = df3$wz,
            type = "scatter3d",
            mode = "lines", scene='scene9',name="Noise:Gyroscope"
        )
       p<- subplot (list(plot_X1,plot_X2,plot_X3, plot_O1, plot_O2,plot_O3,plot_N1,plot_N2,plot_N3),nrows = 3,shareX = FALSE,shareY = FALSE)
       p <- p %>% layout(title = "Plots",
                             scene = list(domain=list(x=c(0,1/3),y=c(2/3,1)),
                                          xaxis=axx, yaxis=axx, zaxis=axx,
                                         
                                          aspectmode='cube'),
                             scene2 = list(domain=list(x=c(1/3,2/3),y=c(2/3,1)),
                                           xaxis=axx, yaxis=axx, zaxis=axx,
                                           aspectmode='cube'),
                             scene3 = list(domain=list(x=c(2/3,1),y=c(2/3,1)),
                                           xaxis=axx, yaxis=axx, zaxis=axx,
                                          
                                           aspectmode='cube'),
                             scene4 = list(domain=list(x=c(0,1/3),y=c(1/3,2/3)),
                                           xaxis=axx, yaxis=axx, zaxis=axx,
                                           aspectmode='cube')
                         ,
                         scene5 = list(domain=list(x=c(1/3,2/3),y=c(1/3,2/3)),
                                      xaxis=axx, yaxis=axx, zaxis=axx,
                                      aspectmode='cube')
                         ,
                         scene6 = list(domain=list(x=c(2/3,1),y=c(1/3,2/3)),
                                      xaxis=axx, yaxis=axx, zaxis=axx,
                                      aspectmode='cube')
                         ,
                         scene7 = list(domain=list(x=c(0,1/3),y=c(0,1/3)),
                                      xaxis=axx, yaxis=axx, zaxis=axx,
                                      aspectmode='cube')
                         ,
                         scene8 = list(domain=list(x=c(1/3,2/3),y=c(0,1/3)),
                                      xaxis=axx, yaxis=axx, zaxis=axx,
                                      aspectmode='cube')
                         ,
                         scene9 = list(domain=list(x=c(2/3,1),y=c(0,1/3)),
                                      xaxis=axx, yaxis=axx, zaxis=axx,
                                      aspectmode='cube'))
       })
    extract_feature_long = function (data,threshold_mean,threshold_sd,threshold_t) {
        data$gN = abs((data$gFx) ^ 2 + ((data$gFy) ^ 2) + ((data$gFz) ^ 2) - 0.8)
        data$gNm = roll_meanl(data$gN, 10)
        data$gNsd = roll_sdl(data$gN, 10)
        data$X <- NULL
        data = drop_na(data)
        data$switch =0
        data$dt=0
        data$pid = 0
        rest = data[data$gNm <= threshold_mean |  data$gNsd <= threshold_sd, ]
        data = data[data$gNm > threshold_mean & data$gNsd > threshold_sd, ]
        data$dt = c(0, diff(data$time))
        data$switch[data$dt >= threshold_t] = 1
        data = group(data,n = rep(1, sum(data$switch)),method = 'l_starts',col_name = 'pid', starts_col = 'switch')
        data$pid = as.numeric(data$pid)
        data$pid[data$switch == 1] = 0
        full = bind_rows(data, rest)
        return (full)
    }
    extract_feature_large= function(data,threeshold_mean,threeshold_sd,threeshold_t){
        result = extract_feature_long(data, threeshold_mean, threeshold_sd, threeshold_t)
        result <- result %>% group_by(pid) %>% filter(sum(dt) <= 3 & sum(dt) >= 1)  %>% mutate(localtime = time - min(time)) %>% mutate(tbin = floor(localtime * 2))
        result = result %>% group_by(pid, tbin) %>% summarise(
            mean_gFx = mean(gFx),
            mean_gFy = mean(gFy),
            mean_gFz = mean(gFz),
            mean_wx = mean(wx),
            mean_wy = mean(wy),
            mean_wz = mean(wz),
            mean_ax = mean(ax),
            mean_ay = mean(ay),
            mean_az = mean(az)
        ) %>% pivot_wider( pid,tbin,values_from = c(
            mean_gFx,
            mean_gFy,
            mean_gFz,
            mean_ax,
            mean_ay,
            mean_az,
            mean_wx,
            mean_wy,
            mean_wz
        ),values_fill = 0
        )
        return (result)
    }
    output$X_LONG_DATA <- renderPrint({
        res <- extract_feature_long(X_upload(), input$X_m_thresh, input$X_sd_thresh, input$X_t_thresh)
        brushedPoints(str(res,give.attr = F), NULL)
       
    })
    output$X_LARGE_DATA <- renderPrint({
        res2 <- extract_feature_large(X_upload(), input$X_m_thresh, input$X_sd_thresh, input$X_t_thresh)
        brushedPoints(str(res2, give.attr = F), NULL)
        
    })
    output$X_LONG_plot <- renderPlot({
        res = extract_feature_long( X_upload(), input$X_m_thresh, input$X_sd_thresh, input$X_t_thresh)
        
        ggplot(res %>% filter(pid > 0)) + geom_line(aes(
            x = time,
            y = gNsd,
            color = as.factor(pid),
            group = pid
        )) + geom_point(data = res %>% filter(pid == 00),
                        aes(x = time, y = gNsd),
                        color = "black")
    })
    
    output$O_LONG_DATA <- renderPrint({
        Ores <- extract_feature_long(O_upload(), input$O_m_thresh, input$O_sd_thresh, input$O_t_thresh)
        brushedPoints(str(Ores,give.attr = F), NULL)
        
    })
    output$O_LARGE_DATA <- renderPrint({
        Ores2 <- extract_feature_large(O_upload(), input$O_m_thresh, input$O_sd_thresh, input$O_t_thresh)
        brushedPoints(str(Ores2, give.attr = F), NULL)
        
    })
    output$O_LONG_plot <- renderPlot({
        Ores = extract_feature_long( O_upload(), input$O_m_thresh, input$O_sd_thresh, input$O_t_thresh)
        
        ggplot(Ores %>% filter(pid > 0)) + geom_line(aes(
            x = time,
            y = gNsd,
            color = as.factor(pid),
            group = pid
        )) + geom_point(data = Ores %>% filter(pid == 00),
                        aes(x = time, y = gNsd),
                        color = "black")
    })
    
    output$N_LONG_DATA <- renderPrint({
        Nres <- extract_feature_long(N_upload(), input$N_m_thresh, input$N_sd_thresh, input$N_t_thresh)
        brushedPoints(str(Nres,give.attr = F), NULL)
        
    })
    output$N_LARGE_DATA <- renderPrint({
        Nres2 <- extract_feature_large(N_upload(), input$N_m_thresh, input$N_sd_thresh, input$N_t_thresh)
        brushedPoints(str(Nres2, give.attr = F), NULL)
        
    })
    output$N_LONG_plot <- renderPlot({
        Nres = extract_feature_long( N_upload(), input$N_m_thresh, input$N_sd_thresh, input$N_t_thresh)
        
        ggplot(Nres %>% filter(pid > 0)) + geom_line(aes(
            x = time,
            y = gNsd,
            color = as.factor(pid),
            group = pid
        )) + geom_point(data = Nres %>% filter(pid == 00),
                        aes(x = time, y = gNsd),
                        color = "black")
    })
    
    
    output$Download_X_LONG <- downloadHandler(
        filename = 'BACCAR_AGHDAEI_OUTPUT_LONG_X.csv'
        ,
        content = function(cont) {
            write.csv(extract_feature_long(X_upload(), input$X_m_thresh, input$X_sd_thresh, input$X_t_thresh), cont)
        }
    )
    output$Download_X_LARGE <- downloadHandler(
        filename = 'BACCAR_AGHDAEI_OUTPUT_LARGE_X.csv'
        ,
        
        content = function(cont) {
            write.csv(extract_feature_large(X_upload(), input$X_m_thresh, input$X_sd_thresh, input$X_t_thresh), cont)
        }
    )
    output$Download_O_LONG <- downloadHandler(
        filename = 'BACCAR_AGHDAEI_OUTPUT_LONG_O.csv'
        ,
        
        content = function(cont) {
            write.csv(extract_feature_long(O_upload(), input$O_m_thresh, input$O_sd_thresh, input$O_t_thresh), cont)
        }
    )
    output$Download_O_LARGE <- downloadHandler(
        filename = 'BACCAR_AGHDAEI_OUTPUT_LARGE_O.csv'
        ,
        
        content = function(cont) {
            write.csv(extract_feature_large(O_upload(), input$O_m_thresh, input$O_sd_thresh, input$O_t_thresh), cont)
        }
    )
    output$Download_N_LONG <- downloadHandler(
        filename = 'BACCAR_AGHDAEI_OUTPUT_LONG_N.csv'
        ,
        
        content = function(cont) {
            write.csv(extract_feature_long(N_upload(), input$N_m_thresh, input$N_sd_thresh, input$N_t_thresh), cont)
        }
    )
    output$Download_N_LARGE <- downloadHandler(
        filename = 'BACCAR_AGHDAEI_OUTPUT_LARGE_N.csv'
        ,
        
        content = function(cont) {
            write.csv(extract_feature_large(N_upload(), input$N_m_thresh, input$N_sd_thresh, input$N_t_thresh), cont)
        }
    )
    }

# Run the application 
shinyApp(ui = ui, server = server)





