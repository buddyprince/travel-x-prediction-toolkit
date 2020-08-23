library(shiny)
library(shinydashboard)
library(forecast)
library(rsconnect)
#rsconnect::deployApp('C:/Users/sundan/Desktop/ts/prediction/prediction')


ui = dashboardPage(
    dashboardHeader(
        title="airport prediction"
    ),
    dashboardSidebar(
        sidebarMenu(
            menuItem(text="模型构建与控制", tabName = "side1", icon = icon("sliders-h")),
            menuItem(text="预测结果图", tabName = "side2", icon = icon("camera")),
            menuItem(text="预测结果数据", tabName = "side3", icon = icon("table")),
            menuItem(text="帮助", tabName = "side4", icon = icon("question-circle")),
            menuItem(text="关于", tabName = "side5", icon = icon("address-card"))
            
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(
                tabName="side1",
                fluidRow(
                    box(fileInput("file",label="选择.csv文件上传",accept=".csv",buttonLabel="选择文件"),
                        numericInput("period",label="训练数据最小单位周期（默认为12，请勿轻易修改）",value=12,step=1),
                        width=12,title="模型训练数据导入",solidHeader=T,status = "primary"),
                    
                ),
                fluidRow(
                    box(
                        box(strong("1.只接受.cvs文件"),background="red"),
                        box(strong("2.文件中历史数据按时间从过去到现在从上至下按列排列，且务必以 history 作为列名"),background="red"),
                        width=12,title="警告",solidHeader=T,status = "danger"),
                ),
                fluidRow(
                    box(numericInput("h",label=NULL,value=12,step=1,width="100%"),width=12,title="预测时长（以导入数据最小单位为单位）",solidHeader=T,status = "primary"),
                ),
            ),
            
            tabItem(
                tabName="side2",
                fluidRow(
                    box(plotOutput("forecast"),title="预测结果图（黑色为历史数据，蓝色为预测结果）",solidHeader=T,status = "primary"),
                    box(plotOutput("stl"), width=6,title="历史数据分解",solidHeader=T,status = "primary"),
                ),
                fluidRow(
                    box(textOutput("model"),title="模型描述",solidHeader=T,status = "primary"),
                    box(textOutput("p.value"),title="模型检验：>0.05即认为模型可用",solidHeader=T,status = "primary"),
                ),
                
            ),
            
            tabItem(
                tabName="side3",
                box(dataTableOutput("predict"),
                    downloadButton("downloadfile", "下载预测结果数据"),
                    width=12,title="预测结果数据",solidHeader=T,status = "primary"),
                
            ),
            
            tabItem(
                tabName="side4",
                box(h5("
                        1、上传需要用于训练模型的历史数据.csv文件，其中历史数据要求按照从古至今，由上至下排成一列，且一定要以history作为该列列名。
                        
                       "),
                    title="使用流程",width=12,solidHeader=T,status = "primary"),
                box(h5("
                        2、然后设定“训练数据最小单位周期”为导入数据拥有的的周期性（比如导入数据为5年来每年每月旅客人数，则其拥有的最小单位周期为12个月，
                        即“训练数据最小单位周期”为12）
                       "),
                    title=NULL,width=12,solidHeader=T,status = "primary"),
                box(h5("
                        3、选择需要预测的时长（比如需要预测未来一年的数据，则选择预测时长为12）。
                        其中“预测时长”选项的单位与导入数据的最小单位相同（比如导入数据为每月人数，则“预测时长”单位为”月“）。
                       "),
                    title=NULL,width=12,solidHeader=T,status = "primary"),
                box(h5("
                        4、直接点击侧边栏的“预测结果图”或“预测结果数据”，便可直接查看结果（由于网络原因，可能需要等几秒钟才可显示结果）。
                        同时，在“预测结果数据”页面可以选择下载预测结果数据文件，格式为.csv。
                       "),
                    title=NULL,width=12,solidHeader=T,status = "primary"),
                box(h5("
                        5、需要改变训练数据时，直接点击侧边栏的“模型构建与控制”，重新上传训练数据文件即可。
                       "),
                    title=NULL,width=12,solidHeader=T,status = "primary"),
            ),
            
            tabItem(
                tabName="side5",
                box(h5("
                        本站基于R语言实现。预测模型使用的是经典时间序列模型ARIMA(p,d,q)(P,D,Q)[S]。
                       "),
                    title="关于",width=12,solidHeader=T,status = "primary"),
                box(strong("
                            Powered By TRAVEL—X
                          "),
                    title=NULL,width=12,solidHeader=T,status = "primary"),
            )
            
        )
    )
)


server <- function(input, output, session) {
    Data=reactive({
        read.csv(input$file$datapath)
    })
    TS=reactive({
        ts({Data()}$history,frequency=input$period)
    })
    Arima=reactive({
        auto.arima({TS()})
    })
    Forecast=reactive({
        forecast({Arima()},h=input$h,level=c(99.5))
    })
    
    output$stl=renderPlot(plot(stl({TS()},"periodic")))
    output$p.value=renderText({Box.test({Arima()}$residuals,type="Ljung-Box")$p.value})
    output$model=renderText({Forecast()}$method)
    output$forecast=renderPlot(plot({Forecast()}))
    output$predict=renderDataTable(
        data.frame("预测时长"=c(1:length({Forecast()}$mean)),
                   "预测人数"=as.integer({Forecast()}$mean)))
    output$downloadfile=downloadHandler(
        filename=paste("predict_duration=",input$h,".csv",sep=""),
        content=function(file){
            write.csv(data.frame("预测时长"=c(1:length({Forecast()}$mean)),
                                "预测人数"=as.integer({Forecast()}$mean))
                      ,file,row.names=F,)
            },
        contentType="text/csv")
}
shinyApp(ui, server)

