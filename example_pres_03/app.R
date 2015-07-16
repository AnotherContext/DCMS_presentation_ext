library(shiny)
library(shinyapps)
library(rmarkdown)
library(ggplot2)
library(reshape2)
library(scales)
library(grid)
source("doc/graphics.R")
source("doc/source_files_pres_02.R")
source("doc/source_files_pres_03.R")

############################################################
## ui
############################################################

ui <- shinyUI(
  navbarPage(title = "Employment", 
             collapsible = TRUE, fluid = TRUE, 
             tabPanel("Value to the Economy",  
                      fluidRow(
                        column(8,
                              selectizeInput("desc", "Select", 
                                             choices = levels(factor(emp$Desc)),
                                             selected = "Arts, entertainment and recreation ",
                                             multiple = FALSE)), 
                        column(2,
                               radioButtons("format", "Report format", c("Word", "HTML"), inline = FALSE)
                        ),
                        column(2,
                               downloadButton("downloadReport")
                      )),
                      fluidRow(
                          column(12,
                                plotOutput("chart1", height = "350px"))
             )),
             tabPanel("No. of Employees",
                      fluidRow(
                          column(12,
                                 plotOutput("chart2", height = "400px"))
             )),
             tabPanel("Data", 
                      downloadButton("downloadData", "Download"),
                      HTML("<p> <br></p>"),
                      dataTableOutput("table")
             )
  )
)

#################################################################
## server
#################################################################
server <- shinyServer(function(input, output){
  
  #overview section: abs bar plot
  output$chart1 <- renderPlot({
     emp <- emp[ emp$variable != "No of Enterprises", ]
     emp$value <- emp$value/1e3
     desc <- input$desc #"Arts, entertainment and recreation " 
     emp <- emp[ emp$Desc == desc,]
    
    ggplot(data = emp, aes(x = Year, y = value)) + 
      geom_bar(stat = "identity", aes_string(fill= "Sizeband")) +
      fte_theme()  +
      facet_grid( variable  ~ Sizeband) +
      scale_fill_brewer(palette = "Set2", breaks = c("1 to 9", "10 to 49", "50 to 249", "250 and over")) +
      scale_y_continuous(labels = comma) +
      ylab("Value (£ Billion)") +
      ggtitle("The Annual Business Survey for Arts, Entertainment and Recreation, Information and Communication\ncovering the Non-Financial Business Economy which accounts for approx. 75% of the UK economy in terms of GVA*") +
      theme(legend.position="top") +
      guides(fill=guide_legend(title= c("No. of Employess"))) +
      theme(legend.text = element_text(size=7, color=brewer.pal("Greys", n=9)[7])) 
  })
  
  #overview section: no of ent bar plot
  output$chart2 <- renderPlot({
    emp <- emp[ emp$variable == "No of Enterprises", ]
    emp$value <- emp$value/1e3
    
    #desc <- input$desc #"Arts, entertainment and recreation " 
    #emp <- emp[ emp$Desc == desc,]
    
    ggplot(data = emp, aes(x = Year, y = value)) + 
      geom_bar(stat = "identity", aes_string(fill= "Sizeband")) +
      fte_theme()  +
      facet_grid( Sizeband  ~ Desc) +
      scale_fill_brewer(palette = "Set2", breaks = c("1 to 9", "10 to 49", "50 to 249", "250 and over")) +
      #scale_y_discrete(labels = comma)+
      ylab("No. of Enterprises (Thousand)") +
      ggtitle("The Annual Business Survey for Arts, Entertainment and Recreation, Information and Communication\ncovering the Non-Financial Business Economy which accounts for approx. 75% of the UK economy in terms of GVA*") +
      theme(legend.position="top") +
      guides(fill=guide_legend(title= c("No. of Employees"))) + 
      theme(legend.text = element_text(size=7, color=brewer.pal("Greys", n=9)[7])) 
    
  })
  
  output$table <- renderDataTable({
     emp
  }, options = list(aLengthMenu = c(10, 25, 50, 100, 150), iDisplayLength = 2)
  )
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste('DCMS Employment Trends', sep = '.', 
            switch(input$format, Word = 'docx', HTML = 'html')
      )
    },
    content = function(file) {
      src <- normalizePath('doc/spending_report_pres_03.Rmd')
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      #owd <- setwd(tempdir())
      #on.exit(setwd(owd))
      file.copy(src, 'doc/spending_report_pres_03.Rmd')
      library(rmarkdown)
      out <- render('doc/spending_report_pres_03.Rmd', 
                    switch(input$format,  
                           HTML = html_document(), 
                           Word = word_document()))
      file.rename(out, file)
    }
  )
  
  
  output$downloadData <- downloadHandler(
    filename = function() {"DCMS_ABSemployment_data.csv"},
    content = function(file) {write.csv(emp, file)
    })
  
  
}) # end server

#######################################################
## knit the app
#######################################################

shinyApp(ui = ui, server = server)