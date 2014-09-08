library(shiny)



shinyUI(
  navbarPage("CAT",theme="bootstrap.css",
             tabPanel("Info",
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons("info",label="",choices=c("Explanation" = "uit", "Copyright" = "copy")),
                          HTML("<b>Language</b> <br>"),
                          HTML('<a href="https://gettingthingsr.shinyapps.io/SensSpecD/"><img src="http://i.imgur.com/JoV5CWg.png" height= "60px" width="60px"></a>'),
                          HTML("     "),
                          HTML('<img src="http://i.imgur.com/4ktuGe1.png" height= "60px" width="60px"></a>'),
                          br(),br(),
                          HTML('<a href="http://www.maastrichtuniversity.nl/" target="_blank"><img src="logo.jpg" alt="Maastricht University"  border="0" style="border: #00A2DB solid 1px; border-radius: 5px;"/></a>')
                          
                        ),
                        mainPanel(htmlOutput("infoMD"))
                      )
             ),
             navbarMenu("RUN1",
                        tabPanel("Slider",
                                 sidebarLayout(
                                   sidebarPanel(
                                     # HTML opmaak e.d.
                                     tags$head(
                                       HTML('<body style="background-color: white;">'),
                                       tags$style(type="text/css", "select { color: #001C3D }"),
                                       tags$style(type="text/css", "textarea { max-width: 315px; color: #001C3D}"),
                                       tags$style(type="text/css", ".jslider { max-width: 99%; }"),
                                       tags$style(type="text/css", ".jslider { min-width: 99%; color: #001C3D}"),
                                       tags$style(type="text/css", ".jslider .jslider-label{color: #001C3D ; font-size: 12px;}"),
                                       tags$style(type="text/css", ".jslider .jslider-value{color: #001C3D ; font-weight:bold; font-size: 16px;}"),      
                                       tags$style(type='text/css', ".well { max-width: 340px; }"),
                                       tags$style(type='text/css', ".span4 .well { background-color: white; border-color: #00A2DB}"),
                                       tags$style(type='text/css', ".span12  { color: #001C3D; }"),
                                       tags$style(type='text/css', ".span4  { color: #001C3D; }") ,   
                                       tags$style(type="text/css", "select { max-width: 200px; }"),
                                       tags$style(type="text/css", "textarea { max-width: 185px; }"),
                                       tags$style(type='text/css', ".well { max-width: 310px; }"),
                                       tags$style(type='text/css', ".span4 { max-width: 310px; }")
                                     ),
                                     # Conditie
                                     selectInput("selfr", strong("Condition"), 
                                                 choices = c("Normal" = "nor", "Prior Low" = "priL", "Prior High" = "priH", "Perfect" = "per", "Worse" = "wor", "Diffuse" = "diff")),
                                     # Opties voor figuur
                                     wellPanel(strong("Population Figure"),
                                               checkboxInput("PF_cel","Cell",   value = FALSE),
                                               checkboxInput("PF_cor","Classification",value = FALSE)
                                     ),
                                     # Opties voor tabel
                                     wellPanel(strong("Table"), 
                                               checkboxInput("TA_inv","Fill",value=FALSE),
                                               checkboxInput("TA_uit","Compute"),value=FALSE),
                                     checkboxInput("expl",strong("Explanation")),
                                     conditionalPanel(condition = paste("input.expl == true"),
                                                      "For a particular disease 21 individuals are screened with a new instrument. The scores range from 0 to 105, where a higher score indicates a higher risk for the disease. In reality, the green people do not have the disease while the red people do The cut-off point may (above figure) be shifted in order to see what effect this has on the sensitivity, specificity, NPV and PPV. The Cell option under Figure shows the location of each person in the 2x2 table. The option Classification shows for the current cut-off point a person is classified correct or incorrect (X = Wrong; V = Good). The options Fill and Compute under the Table options, show, respectively, the numbers in the 2x2 table and the corresponding sensitivity, specificity, NPV and PPV values. The lower right figure shows the ROC curve, in which the sensitivity and specificity of the selected cutoff are circled. At conditions different situations can be selected with regard to the prior probability of the disease and the quality of the instrument.")
                                     
                                   ),
                                   
                                   mainPanel(
                                     sliderInput("afkap", "",min=0, max=105, value=50, step=5,animate=TRUE),
                                     plotOutput("basefig",height = "250px", width="99%"),
                                     fluidRow(
                                       column(4,plotOutput("tabfig",height = "250px")),
                                       column(4,plotOutput("sens_form",height = "250px")),
                                       column(4,plotOutput("ROC",height = "250px"))
                                     )
                                   )
                                 )
                        ),
                        tabPanel("Calculator",
                                 sidebarLayout(
                                   sidebarPanel(
                                     wellPanel(
                                     numericInput("cell_A", HTML("True Positive <b>(A)</b>"), 1,min=0),
                                     numericInput("cell_B", HTML("False Positive <b>(B)</b>"), 1,min=0),
                                     numericInput("cell_C", HTML("False Negative <b>(C)</b>"), 1,min=0),
                                     numericInput("cell_D", HTML("True Negative <b>(D)</b>"), 1,min=0)
                                     ),
                                     wellPanel(
                                     checkboxInput("calc_uit",strong("Compute"),value=FALSE),
                                     conditionalPanel(condition = paste("input.calc_uit == true"),
                                                      numericInput("dec","Decimals",2,min=1,max=5),
                                                      checkboxInput("calc_ci","Confidence Interval (%)",value=FALSE),
                                                      conditionalPanel(condition = paste("input.calc_ci == true"),           
                                                                       numericInput("calc_cib", "",value=95,min=0,max=100)
                                                      )
                                     )),
                                     checkboxInput("expl_calc",strong("Explanation")),
                                     conditionalPanel(condition = paste("input.expl_calc == true"),
                                                      "The cells of the 2x2 table can be filled. Then the specificity, sensitivity, negative predictive value (NPV) and positive predictive value (PPV) can be calculated. In addition, the number of decimals and the width of the confidence interval can be determined.")
                                     
                                   ),
                                   mainPanel(
                                     fluidRow(
                                       column(6,plotOutput("calcTab",height = "250px")),
                                       column(6,plotOutput("calc_form",height = "250px"))
                                     )
                                   )
                                 )
                        )
             )
             
  ))
