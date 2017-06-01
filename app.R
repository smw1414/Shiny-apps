library(shiny)
library(shinyjs)
library(ggplot2)
library(plotly)
library(googleVis)
library(data.table)
require(visNetwork)
library(magrittr)
library(DT)
library(shinydashboard)
##ss

header <- dashboardHeader()

sidebar <-   dashboardSidebar(
  useShinyjs(),
  sidebarMenu(
    menuItem("ANNOVAR annotation", tabName = "annovar" ,icon = icon("list-alt")),
    selectInput("annovar_folder_sel", "Folder",
                choices = c(unique(annovar_folder)), multiple=F, selectize=TRUE,selected =  NULL,
                width = '98%')

  )
)

body <-   dashboardBody(
  useShinyjs(),
   tabItems(
     
     tabItem(tabName = "annovar",
             fluidRow( column(6,
                              h3(paste0("1. Create a folder under RA_files\\app\\annovar \n")),
                              h3(paste0("2. Copy vcf files to that folder\n")),
                              h3(paste0("3. Select the the folder you created from sidebar menu,\n or press F5 if the folder was not existed in the drop down menu ")),
                              # actionButton("close", "Close connection"),
                              h3(paste0("4. Select the Baseapsce APP type \n")),
                              h3(paste0("5. Click the button: Apply annotation \n")),
                              h3(paste0("6. Collect the annoated vcf files, the progress indicator was showed under the Apply annotation button\n")),
                              h4(radioButtons("annovar_func", "Baseapsce APP type",
                                           c("BWA GATK hg19" = "bwa_gatk",
                                             "RNA alignment hg38" = "rnaalig"))),
                              verbatimTextOutput("file_preview"),
                              verbatimTextOutput("runapp"),
                              actionButton("apply_anno", "Apply annotation"),
                              textOutput("outlog")
                              )
                       )
  
     )
   )
)



# example
# sidebar<-  dashboardSidebar(
#   sidebarMenu(id = "sidebarmenu",
#     menuItem("Dashbssoard", tabName = "tcgaexpression", icon = icon("dashboard")),
#     sliderInput("tcgaexpression", "Under sidebarMenu", 1, 100, 50),
#     menuItem("Widgets", tabName = "annovar", icon = icon("th"))
#   )
# )
# body <-   dashboardBody(
#   tabItems(
#     # First tab content
#     tabItem(tabName = "tcgaexpression",
#             fluidRow(
#               box(plotOutput("plot1", height = 250)),
# 
#               box(
#                 title = "Controls",
#                 sliderInput("slider", "Number of observations:", 1, 100, 50)
#               )
#             )
#     ),
#     # Second tab content
#     tabItem(tabName = "annovar",
#             h2("Widgets tab content")
#     )
#   )
# )

ui <- dashboardPage(header, sidebar, body)

server <- shinyServer(function(input, output, session) {
  
  
######## aanovar 
  # vcf_list <- reactive({})
  observe({
    x<-basename(system("ls -d /home/wsm/RA_files/app/annovar/* ",intern = T))
    updateSelectInput(session, "annovar_folder_sel",
                      label = paste("Folder"),
                      choices = x,
                      selected = tail(x, 1))
    
  })
  
  
  output$file_preview <- renderPrint({
    cat("First 10 files were showed\n")
    cat(input$annovar_folder_sel)
    cat("\n")
    files<-system(paste0("ls /home/wsm/RA_files/app/annovar/",input$annovar_folder_sel,"/*.vcf"),intern = T)
    files<-basename(files[1:10][!is.na(files[1:10])])
    cat(files)
  })
  
  runapp_annovar<- eventReactive(input$apply_anno, {
    files<-system(paste0("ls /home/wsm/RA_files/app/annovar/",input$annovar_folder_sel,"/*.vcf"),intern = T)
    out=paste0("/home/wsm/RA_files/app/annovar/",input$annovar_folder_sel)
    annovar_app <- switch(input$annovar_func,
                         bwa_gatk = "/home/wsm/bam/scripts/annovar/annotate_allflow_BWA_GATK.sh",
                         rnaalig = "/home/wsm/bam/scripts/annovar/annotate_allflow_RNAaligment_hg38.sh")
    ##aa
    
 # aa <-system(paste0("cd /home/wsm/RA_files/app/annovar/",input$annovar_folder_sel,"; pwd"),intern = T)
  # aa<-system("pwd",intern = T)
   disable_act_but()
   #Sys.sleep(2)
   cat(paste("cd ",out,";",annovar_app,files[1],sep = " "))
   
   withProgress(message = 'Annotating', value = 0, {
     # Number of times we'll go through the loop
     n <- length(files)
     
     for (i in seq(1,length(files))){
       message(paste0(i," "))
       system(paste("cd ",out,";",annovar_app,files[i],sep = " "))
       # Increment the progress bar, and update the detail text.
       incProgress(1/n, detail = paste("File", i))
     }
   })

  
   enable_act_but()
  # cat(paste0(aa))
   
  })
  # output$runapp = renderPrint({
  #   runapp_annovar()  
  # })
  
  observeEvent(input$apply_anno, {
    withCallingHandlers({
      shinyjs::html("outlog", "")
      
      runapp_annovar()
    },
    message = function(m) {
      shinyjs::html(id = "outlog", html = m$message, add = TRUE)
    })
  })
  
  # observe({
  #   if (input$close > 0) stopApp() # stop shiny
  # })
  
  enable_act_but<-function(){
    #message("enable but")
   
    message("finished \n")
    shinyjs::enable('apply_anno')
    shinyjs::enable("annovar_folder_sel")
    shinyjs::enable("tcga_sel")
    shinyjs::enable("annovar_func")
  }
  disable_act_but<-function(){
   # message("disabling but\n")
    message("start run\n")
    shinyjs::disable('apply_anno')
    shinyjs::disable("annovar_folder_sel")
    shinyjs::disable("tcga_sel")
    shinyjs::disable("annovar_func")
  }
  session$allowReconnect(TRUE)
}
)
shinyApp(ui, server)







