
library(shiny)
library(shinyjs)
library(data.table)
library(DT)

library(shinydashboard)

tcga_sets_df<-data.frame(name=c("TCGA-COADREAD_mrna","TCGA-COADREAD_mirna")
                         ,file=c("/home/wsm/bam/CRC/PGS/deseq2/TCGA_TN_de_rseq.rds","/home/wsm/bam/CRC/PGS/deseq2/TCGA_TN_de_mirna.rds")
                         ,stringsAsFactors = F)


header <- dashboardHeader()

sidebar <-   dashboardSidebar(
  sidebarMenu(
    # menuItem("TCGA-COADREAD_deseq", tabName = "TCGA-COADREAD_deseq", icon = icon("dashboard")),
    # menuItem("ANNOVAR", tabName = "ANNOVAR", icon = icon("dashboard")),
    menuItem("TCGA_expression", tabName = "TCGA expression" ,icon = icon("list-alt"),
             # Input directly under menuItem
             selectInput("tcga_sel", "Data sets",
                         choices = c(unique(tcga_sets_df$name)), multiple=F, selectize=TRUE,
                         width = '98%')
    )
  )
)

body <-   dashboardBody(
  
  # tabItems(
  #   # First tab content
  #   tabItem(tabName = "TCGA-COADREAD_deseq",
  #           fluidRow(
  #             box(plotOutput("plot1", height = 250)),
  #             
  #             box(
  #               title = "Controls",
  #               sliderInput("slider", "Number of observations:", 1, 100, 50)
  #             )
  #           )
  #   ),
  #   
    # Second tab content
    tabItem(tabName = "TCGA_expression",
            fluidRow( h2(verbatimTextOutput('test')),
                      DT::dataTableOutput('tbl'))
           
    )
  # )
)

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {
  data_f <- reactive({
    
      cancerlist<-readRDS(tcga_sets_df[tcga_sets_df$name == input$tcga_sel,]$file)
      # coexp_tbl = data[(cor < input$corr[2] & cor > input$corr[1]) & cor_p < input$pvalue & lncRNA==input$qgene & co_exp_gene != input$qgene,-c("lncRNA","lncRNA_start","lncRNA_end","lncRNA_strand","lncRNA_id"),with=F]
      list(res=cancerlist[["res"]],dds=cancerlist[["dds"]],res_tab=cancerlist[["res_tab"]],factor_list=cancerlist[["factor_list"]],words=cancerlist[["words"]])
  }) 
  
  res_tab<- reactive({
    data.table(data_f()$res_tab)
  })
  
  output$tbl = DT::renderDataTable(
    #{
    #datatable(
    
    res_tab(),
    extensions = 'Buttons',
    class = 'compact',
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
      pageLength = 5,
      lengthMenu = c(5, 10, 15, 20),
      autoWidth = TRUE
    ),
    rownames = FALSE
    # )
    #}
    , server = T)
  
  output$test = renderPrint({
   cat(input$tcga_sel)  
  })
  
  
}

shinyApp(ui, server)







