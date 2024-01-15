dashboardPage(
  dashboardHeader(disable=T),
  dashboardSidebar(disable=T),
  
  dashboardBody(
    # set background to white
    setBackgroundColor(color = "#FFFFFF", shinydashboard = FALSE),
    setBackgroundColor(color = "#FFFFFF", shinydashboard = TRUE),
    tags$head(
      tags$link(rel = "shortcut icon", href = "logo.ico")
    ),
    navbarPage(title=img(id="logo", src="Good logo.png", style="width:100%;height:150%"),
               theme = shinytheme("lumen"),
               windowTitle = "Saudi Arabia Population",
               tabPanel("عربي",
                        fluidRow(
                          column(width = 8, align = "center", offset = 2 , plotlyOutput("plot1"))),
                        br(),
                        fluidRow(
                          column(width = 12, align = "center",
                                 p("يتضح من الرسم البياني أن السكان في سن العمل من عمر ٢٥ إلى ٥٤ سنة يشكلون أعلى نسبة سكان في الممكلة العربية السعودية ثم يليهم فئة الأطفال من عمر ١٤ سنة وأقل
"))
                        ),
                        hr(),
                        fluidRow(column(width = 2,align = "right", offset = 10,
                                        tags$a(img(src = "website.png",style = "width:85%;height:85%;"),href = "https://portal.saudicensus.sa/portal", target="_blank"),
                        p("يهدف التعداد إلى توفير إحصاءات كاملة ودقيقة عن السكان علاوة على معلومات عن المساكن في جميع مناطق المملكة العربية السعودية والفئات الفرعية للسكان
",style = "text-align: justify;direction:rtl;")))
               ),
               tabPanel("English",
                               fluidRow(
                                 column(width = 8, align = "center", offset = 2 , plotlyOutput("plot2"))),
                        br(),
                        fluidRow(
                          column(width = 12, align = "center",
                                 p("The Working-Age population aged between 25 and 54 is the highest in Saudi Arabia, followed by children aged 14 and less."))
                        ),
                        hr(),
                        fluidRow(column(width = 2,
                                        tags$a(img(src = "website.png",style = "width:85%;height:85%;"),href = "https://portal.saudicensus.sa/portal", target="_blank"),
                        p("A comprehensive census is designed to provide a complete and accurate count of the population along with information on housing, across all locations in the Kingdom and sub-groups of the population.
",style = "text-align: justify;")))
                        )
               ),
    fluidRow(
      column(width = 1,align = "right", offset = 4,
                    tags$a(img(src = "GitHub.png",style = "width:30%;height:30%;"),href = "https://github.com/0abdullah0sami0", target="_blank")),
      column(width = 1,align = "center", offset = 0,
             tags$a(img(src = "LinkedIn.png",style = "width:30%;height:30%;"),href = "https://www.linkedin.com/in/abdullahalshalaan/", target="_blank")),
      column(width = 1,align = "left", offset = 0,
             tags$a(img(src = "twittter.jpeg",style = "width:30%;height:30%;"),href = "https://twitter.com/HR02030", target="_blank"))
      ))
    )
