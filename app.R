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
####sgdfgsdfssssasdasdass32
annovar_folder<-basename(system("ls -d /home/wsm/RA_files/app/annovar/* ",intern = T))
header <- dashboardHeader()
fastq_folder<-basename(system("ls -d /home/wsm/RA_files/app/cat_fastq/* ",intern = T))
bammergefd<-basename(system("ls -d /home/wsm/RA_files/app/bam_merge/* ",intern = T))
sidebar <-   dashboardSidebar(
  useShinyjs(),
  sidebarMenu(
    menuItem("ANNOVAR annotation", tabName = "annovar" ,icon = icon("list-alt")),
    selectInput("annovar_folder_sel", "Folder",
                choices = c(unique(annovar_folder)), multiple=F, selectize=TRUE,selected =  NULL,
                width = '98%'),
    menuItem("Merage fastq", tabName = "cat_fastq" ,icon = icon("list-alt")),
    selectInput("fastq_folder_sel", "Folder",
                choices = c(unique(fastq_folder)), multiple=F, selectize=TRUE,selected =  NULL,
                width = '98%'),
    menuItem("Merge bam", tabName = "merge_bam" ,icon = icon("list-alt")),
    selectInput("bammerge_folder_sel", "Folder",
                choices = c(unique(bammergefd)), multiple=F, selectize=TRUE,selected =  NULL,
                width = '98%')

  )
)

body <-   dashboardBody(
  tags$head(
    tags$style(HTML("
      @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
    "))
  ),
  useShinyjs(),
   tabItems(
     
     tabItem(tabName = "annovar",
             fluidRow( column(6,
                              h1("ANNOVAR annotation", 
                                 style = "font-family: 'Times New Roman', font-weight: 500; line-height: 1.1; color:  #ad1d28;"),
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
                              #verbatimTextOutput("runapp"),
                              actionButton("apply_anno", "Apply annotation"),
                              textOutput("outlog")
                              )
                       )
  
      )
     ,
     tabItem(tabName = "cat_fastq",
              fluidRow( column(6,
                               h1("FASTQ CAT", 
                                  style = "font-family: 'Times New Roman', font-weight: 500; line-height: 1.1; color:  #ad1d28;"),
                              h3(paste0("1. Create a folder under RA_files\\app\\cat_fastq \n")),
                              h3(paste0("2. Copy fastq files to that folder\n")),
                              h3(paste0("3. Select the the folder you created from sidebar menu,\n or press F5 if the folder was not existed in the drop down menu ")),
                              # actionButton("close", "Close connection"),
                              h3(paste0("4. Select the fastq type \n")),
                              h3(paste0("5. Click the button: Merge fastq \n")),
                              h3(paste0("6. Collect fastq files, the progress indicator was showed under the Merge fastq button\n")),
                              h4(radioButtons("fastq_radio", "Reads type",
                                              c("Single-end" = "SE",
                                                "Paired-end" = "PE"))),
                               verbatimTextOutput("file_preview_fastq"),
             #                 # verbatimTextOutput("runapp"),
                              actionButton("apply_fastq", "Merge fastq")
                             # textOutput("outlog")
              )
              )

     ),
     tabItem(tabName = "merge_bam",
             fluidRow( column(6,
                              h1("BAM merge", 
                                 style = "font-family: 'Times New Roman', font-weight: 500; line-height: 1.1; color:  #ad1d28;"),
                              h3(paste0("1. Create a folder under RA_files\\app\\merge_bam \n")),
                              h3(paste0("2. Copy bam files to the folder you just created\n")),
                              h3(paste0("3. Select the the folder you created from sidebar menu,\n or press F5 if the folder was not existed in the drop down menu ")),
                              h3(paste0("4. Fill the new merged bam below\n")),
                              h3(paste0("5. Click the button: Merge bam \n")),
                              h3(paste0("6. Access and the \"merged\" folder under the folder you just created and collect the merged bam file\n")),
                             
                              textInput("newbamname", label = h3("New file name (Do not add .bam)"), value = "Enter_new_name_here"),
                              verbatimTextOutput("file_preview_mergebam"),
                              #                 # verbatimTextOutput("runapp"),
                              actionButton("apply_mergebam", "Merge bam")
                              # textOutput("outlog")
             )
             )
             
     )
   )
)


ui <- dashboardPage(header, sidebar, body)

server <- shinyServer(function(input, output, session) {
  
  
######## aanovar 
  # vcf_list <- reactive({})
  ##### list folders
  observe({
    x<-basename(system("ls -d /home/wsm/RA_files/app/annovar/* ",intern = T))
    updateSelectInput(session, "annovar_folder_sel",
                      label = paste("Folder"),
                      choices = x,
                      selected = tail(x, 1))
    
  })
  
  observe({
    x<-basename(system("ls -d /home/wsm/RA_files/app/cat_fastq/* ",intern = T))
    updateSelectInput(session, "fastq_folder_sel",
                      label = paste("Folder"),
                      choices = x,
                      selected = tail(x, 1))

  })
  
  observe({
    x<-basename(system("ls -d /home/wsm/RA_files/app/bam_merge/* ",intern = T))
    updateSelectInput(session, "bammerge_folder_sel",
                      label = paste("Folder"),
                      choices = x,
                      selected = tail(x, 1))
    
  })
  
  ##### preview files in the given folder
  output$file_preview <- renderPrint({
    cat("First 10 files were showed\n")
    cat(input$annovar_folder_sel)
    cat("\n")
    files<-system(paste0("ls /home/wsm/RA_files/app/annovar/",input$annovar_folder_sel,"/*.vcf"),intern = T)
    files<-basename(files[1:10][!is.na(files[1:10])])
    cat(files)
  })
  
  output$file_preview_fastq <- renderPrint({
    cat("First 10 files were showed\n")
    cat(input$fastq_folder_sel)
    cat("\n")
    files<-system(paste0("ls -d /home/wsm/RA_files/app/cat_fastq/",input$fastq_folder_sel,"/* | grep -v \"cated\" "),intern = T)
    files<-basename(files[1:10][!is.na(files[1:10])])
    cat(files)
  })
  
  output$file_preview_mergebam <- renderPrint({
    cat("First 10 files were showed\n")
    cat(input$bammerge_folder_sel)
    cat("\n")
    files<-system(paste0("ls -d /home/wsm/RA_files/app/bam_merge/",input$bammerge_folder_sel,"/*.bam "),intern = T)
    files<-basename(files[1:10][!is.na(files[1:10])])
    cat(files)
  })
  
  
  #### APP function
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

   system(paste0("chmod 777 -R /home/wsm/RA_files/app/"))
   enable_act_but()
  # cat(paste0(aa))
   
  })
  
  runapp_catfastq<- eventReactive(input$apply_fastq, {
    files<-system(paste0("ls -d /home/wsm/RA_files/app/cat_fastq/",input$fastq_folder_sel,"/*"),intern = T)
    out=paste0("/home/wsm/RA_files/app/cat_fastq/",input$fastq_folder_sel)
    run_app <- switch(input$fastq_radio,
                          SE = "/home/wsm/bam/scripts/cat_fastq/catfastq.sh",
                          PE = "/home/wsm/bam/scripts/cat_fastq/catfastq_pe.sh")
    ##aa
    
    # aa <-system(paste0("cd /home/wsm/RA_files/app/annovar/",input$annovar_folder_sel,"; pwd"),intern = T)
    # aa<-system("pwd",intern = T)
    disable_act_but()
    #Sys.sleep(2)
    message("fastq")
    
    withProgress(message = 'cat fastq', value = 0, {
      # Number of times we'll go through the loop
      n <- length(files)
      
      for (i in seq(1,length(files))){
        message(paste0(i," "))
        system(paste(run_app,files[i],sep = " "))
        # Increment the progress bar, and update the detail text.
        incProgress(1/n, detail = paste(files[i]," done"))
      }
    })
    
    system(paste0("chmod 777 -R /home/wsm/RA_files/app/"))
    enable_act_but()
    # cat(paste0(aa))
    
  })
  
  runapp_mergebam<- eventReactive(input$apply_mergebam, {

    run_app <- "/home/wsm/bam/scripts/bammerge/bammerge.sh"
    ##aa
  
    disable_act_but()
    #Sys.sleep(2)
    message("merge bam")
    
    message(paste0("folder",input$bammerge_folder_sel))
    message(paste0("file: ",input$newbamname))
    wd=paste("/home/wsm/RA_files/app/bam_merge/",input$bammerge_folder_sel,sep = "/")
   
     message(paste(run_app,"-i",wd,"-o",input$newbamname,sep = " "))
    system(paste(run_app,"-i",wd,"-o",input$newbamname,sep = " "))  
    
    system(paste0("chmod 777 -R /home/wsm/RA_files/app/"))
    enable_act_but()

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
  
  observeEvent(input$apply_fastq, {
    runapp_catfastq()
  })
  
  observeEvent(input$apply_mergebam, {
    runapp_mergebam()
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
    shinyjs::enable("apply_fastq")
    shinyjs::enable("fastq_folder_sel")
    shinyjs::enable("fastq_radio")
    shinyjs::enable("apply_mergebam")
    shinyjs::enable("newbamname")
    shinyjs::enable("bammerge_folder_sel")
    
    
  
  }
  disable_act_but<-function(){
   # message("disabling but\n")
    message("start run\n")
    shinyjs::disable('apply_anno')
    shinyjs::disable("annovar_folder_sel")
    shinyjs::disable("tcga_sel")
    shinyjs::disable("annovar_func")
    shinyjs::disable("apply_fastq")
    shinyjs::disable("fastq_folder_sel")
    shinyjs::disable("fastq_radio")
    shinyjs::disable("apply_mergebam")
    shinyjs::disable("newbamname")
    shinyjs::disable("bammerge_folder_sel")
  }
  session$allowReconnect(TRUE)
}
)
shinyApp(ui, server)







