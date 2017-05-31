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

tcga_sets_df<-data.frame(name=c("TCGA-COADREAD_mirna","TCGA-COADREAD_mrna")
                         ,file=c("/home/wsm/bam/CRC/PGS/deseq2/TCGA_TN_de_mirna.rds","/home/wsm/bam/CRC/PGS/deseq2/TCGA_TN_de_rseq.rds")
                         ,stringsAsFactors = F)

annovar_folder<-basename(system("ls -d /home/wsm/RA_files/app/annovar/* ",intern = T))
selected_cols= c("gene","cor","FoldChange", "pvalue", "padj" )
allcn<-c("gene","type","log2FoldChange","FoldChange" ,"ratio","pvalue", "padj","baseMean" ,"N BaseMean" ,"T BaseMean" )
header <- dashboardHeader()

sidebar <-   dashboardSidebar(
  useShinyjs(),
  sidebarMenu(
    menuItem("TCGA Expression", tabName = "tcgaexpression" ,icon = icon("list-alt")),
    selectInput("tcga_sel", "Data sets",
                choices = c(unique(tcga_sets_df$name)), multiple=F, selectize=TRUE,selected =  NULL,
                width = '98%'),
    menuItem("ANNOVAR annotation", tabName = "annovar" ,icon = icon("list-alt")),
    selectInput("annovar_folder_sel", "Folder",
                choices = c(unique(annovar_folder)), multiple=F, selectize=TRUE,selected =  NULL,
                width = '98%'),
    menuItem("Widgets", tabName = "widgets", icon = icon("list-alt"))
  )
)

body <-   dashboardBody(
  useShinyjs(),
   tabItems(
     tabItem(tabName = "tcgaexpression",h2("Widgets taassdfasb content"),
             fluidRow( column(6,
                              h2(verbatimTextOutput('test')),
                              checkboxInput("selcols", tags$b("Modify Columns:")),
                              conditionalPanel(
                                condition = "input.selcols == true",
                                checkboxGroupInput('sel_cols', 'Columns to Display:',
                                                   # allcn[!allcn %in% c("type",
                                                   #                     "baseMean",
                                                   #                     "N BaseMean",
                                                   #                     "T BaseMean")],
                                                   allcn,
                                                   selected = selected_cols)
                              ),
                              DT::dataTableOutput('tbl')))
             
     ),
     tabItem(tabName = "annovar",
             fluidRow( column(6,
                              h3(paste0("1. Create a folder under RA_files\\app\\annovar \n")),
                              h3(paste0("2. Copy vcf files to that folder\n")),
                              h3(paste0("3. Select the the folder you created from sidebar menu,\n or press F5 button if the folder was not existed in the drop down menu ")),
                              h3(paste0("4. Select the Baseapsce APP type \n")),
                              h3(paste0("5. Click the button: Apply annotation \n")),
                              h3(paste0("6. Collect the annoated vcf files, the progress indicator was showed under the Apply annotation button\n")),
                              h4(radioButtons("annovar_func", "Baseapsce APP type",
                                           c("BWA GATK hg19" = "bwa_gatk",
                                             "RNA alignment hg38" = "rnaalig"))),
                              verbatimTextOutput("file_preview"),
                              verbatimTextOutput("runapp"),
                              actionButton("apply_anno", "Apply annotation"),
                              textOutput("outlog"),
                              h2("Widgets tab consdfasdtent")
                              )
                       )
  
     ),
     tabItem(tabName = "widgets",
             h2("Widgets tab content")
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
  data_f <- reactive({
    
      cancerlist<-readRDS(tcga_sets_df[tcga_sets_df$name == input$tcga_sel,]$file)
      # coexp_tbl = data[(cor < input$corr[2] & cor > input$corr[1]) & cor_p < input$pvalue & lncRNA==input$qgene & co_exp_gene != input$qgene,-c("lncRNA","lncRNA_start","lncRNA_end","lncRNA_strand","lncRNA_id"),with=F]
      list(res=cancerlist[["res"]],dds=cancerlist[["dds"]],res_tab=cancerlist[["res_tab"]],factor_list=cancerlist[["factor_list"]],words=cancerlist[["words"]])
  }) 
  
  
  res_tab<- reactive({
    dt<-data.table(data_f()$res_tab,stringsAsFactors = F)
    #allcn<<-colnames(dt)
    cols<-colnames(dt)[!colnames(dt) %in% c("gene","type")]
    data.table(data_f()$res_tab)[,(cols):=lapply(.SD, function(x) as.numeric(formatC(as.numeric(x),digits =5,format = "g"))),.SDcols=cols]
   # data.table(data_f()$res_tab)
  })
  
  output$tbl = DT::renderDataTable(
    #{
    #datatable(
    
    res_tab()[,input$sel_cols, with = F],
    #res_tab(),
    selection = list(
      mode = "single",
      target = "row",
      selected = c(1)
    ),
    filter = 'top',
    caption = '',
    options = list(
      # dom = 'Blfrtip',
      # # buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
      # #list(extend='csv',filename="coexpressed_gene_list.xlsx")
      # buttons = list(list(extend='copy'),
      #                list(extend='csv',filename=paste("coexp_gene",input$qgene,input$corr[2],input$corr[1],sep="_")),
      #                list(extend='excel',filename=paste("coexp_gene",input$qgene,input$corr[2],input$corr[1],sep="_"))
      #                ),
      pageLength = 10,
      lengthMenu = c(5, 10, 15, 20),
      autoWidth = TRUE, scrollX=TRUE,
      selection = 'single'
    ),
    rownames = FALSE
    # )
    #}
    , server = T)
  
  output$test = renderPrint({
  s=input$tbl_rows_selected
  cat(input$tcga_sel)
  cat("\n")
  cat(as.character(res_tab()[["gene"]])[s]) 
  })
  
  # s=input$tbl_rows_selected
  # #s=input$tbl_row
  
  
######## aanovar 
  # vcf_list <- reactive({})

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
    
    
 # aa <-system(paste0("cd /home/wsm/RA_files/app/annovar/",input$annovar_folder_sel,"; pwd"),intern = T)
  # aa<-system("pwd",intern = T)
   disable_act_but()
   #Sys.sleep(2)
   cat(paste("cd ",out,";",annovar_app,files[1],sep = " "))
   
   withProgress(message = 'Annotating', value = 0, {
     # Number of times we'll go through the loop
     n <- length(files)
     
     for (i in seq(1,length(files))){
       system(paste("cd ",out,";",annovar_app,files[i],sep = " "))
       message(paste0(i," "))
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
}
)
shinyApp(ui, server)







