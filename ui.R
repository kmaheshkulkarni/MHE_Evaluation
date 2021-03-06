bs4DashPage(
  sidebar_mini = FALSE,
  sidebar_collapsed = FALSE,
  enable_preloader = TRUE,
  loading_duration = 3,
  loading_background = "#273443",
  setBackgroundColor(color = "#273443"),
  navbar = bs4DashNavbar(sidebarIcon = NULL,
                         skin = "dark",
                         controlbarIcon = NULL,
                         fixed = TRUE,
                         leftUi = h3("MHE Evaluation")
                           # tagList(span(class = "logo-lg"),img(src = "logo.png", height= 40,
                           #                                            style = 'margin-left: -40px; margin-top: -1px;')),
                         # rightUi = bs4UserMenu(
                         #   src = "Prilogo.png", 
                         #   name = "V0MG1UF",
                         #   title = "Real Time Paint Tracker",
                         #   subtitle = "Version 1.0",
                         #   status = "warning",
                         #   footer = NULL
                         # )
  ),
  sidebar = bs4DashSidebar(disable = TRUE),
  body = bs4DashBody(
    fluidRow(column(1)),
    fluidRow(
      column(1, pickerInput(
        inputId = "FloorCond",
        label = "Select Floor", 
        choices = c("Concrete", "Rough", "Smooth", "Rough_Smooth", "Clean"),
        options = list(`live-search` = TRUE)
      ),align= "center"),
      column(2, textInput("Trips", label = "Trips Per Shift", value = ""),align= "center"),
      column(1, textInput("Dist", label = "Distance (Ft)", value = ""),align= "center"),
      column(1, textInput("AisleWidth", label = "Aisle Width (Ft)", value = ""),align= "center"),
      column(2, textInput("TurnRads", label = "Turning Radius (Ft)", value = ""),align= "center"),
      column(2, textInput("VertReach", label = "Vertical Reach (Ft)", value = ""),align= "center"),
      column(1, textInput("MaxWt", label = "Max Weight (lbs)", value = ""),align= "center"),
      column(2, align= "center", actionBttn(
        inputId = "Getinsights",
        label = "Get Insights",
        style = "fill"
      ))      
    ),
    fluidRow(
      # bs4Card(
      #   title = "Ratings", width = 12, status = "danger",
      #   closable = FALSE, maximizable = TRUE, collapsible = FALSE, height = "500px",
      column(12,
            fluidRow(
                        column(3,bs4ValueBoxOutput(outputId = "SChaser", width = 12)),
                        column(3,bs4ValueBoxOutput(outputId = "P_Cart", width = 12)),
                        column(3,bs4ValueBoxOutput(outputId = "MHT", width = 12)),
                        column(3,bs4ValueBoxOutput(outputId = "Tugger", width = 12))
                      ),
            fluidRow(column(3),
                     column(3,bs4ValueBoxOutput(outputId = "AGVQ", width = 12)),
                     column(3,bs4ValueBoxOutput(outputId = "PalStack", width = 12)),
                     column(3)
                     )
                 )
      ),
               
               
               # fluidRow(bs4ValueBox(value = 200, subtitle = "Surface Area", status = "success",
               #                      footer = "Unit in SQ.Ft", icon = "database", width = 12))
        fluidRow(
               column(12,
                      fluidRow(
                        column(3, bs4ValueBoxOutput(outputId = "ForkliftV", width = 12)),
                        column(3,bs4ValueBoxOutput(outputId = "Pallet_JH", width = 12)),
                        column(3,bs4ValueBoxOutput(outputId = "Pallet_JE", width = 12)),
                        column(3,bs4ValueBoxOutput(outputId = "Reach_Trucks", width = 12))
                      )
               )
               # fluidRow(bs4ValueBox(value = 200, subtitle = "Surface Area", status = "success",
               #                      footer = "Unit in SQ.Ft", icon = "database", width = 12))
        )
  ),
  tags$head(
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "primer.css"
    )
  ),
  footer = bs4DashFooter(
    fixed = TRUE,
    copyrights = tagList(span(class = "logo-lg"),img(src = "Prilogo.png", height= 57, 
                                                     style = 'vertical-align: initial;height: 65px;margin-left: -1px;margin-top: -2px;margin-bottom: -5px;')),
    right_text = tagList(span(class = "logo-lg"),img(src = "green.png", height= 63, style = 'vertical-align: initial;'))
    
    # tagList(span(class = "logo-lg"),img(src = "green.png", height= 63, style = 'vertical-align: initial;'))
  )
)