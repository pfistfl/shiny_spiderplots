library(shinyBS)
library(shinydashboard)

ui <- dashboardPage(skin = "yellow",
  dashboardHeader(title = "Create a Spiderplot from Input Data", disable = TRUE),
  dashboardSidebar(
    tags$head(
      tags$style(type='text/css',
        ".panel-default {color: #070707; padding: 5px;}",
        ".panel-title {color: #070707;}",
        ".shiny-bound-input {margin-bottom: 0px; margin-top: 0px; padding: 0px 0px 0px 0px;}",
        ".form-group {margin-top: 0px; margin-bottom: 0px; padding: 0px 0px 0px 0px;}"
        ),
      tags$style(HTML("
        h4 {
        font-weight: bold;
        color: #070707;
        line-height: 1.1;
        }

        .collapsed {
         color: #070707;
         opacity:1.0;
        }

        .panel-heading {
        opacity: 0.8;
        background: rgba(0, 0, 0, 0.1);
        }
        
        "))
      ),
    bsCollapse(multiple = FALSE,
    bsCollapsePanel("Skills",
      numericInput("nskills", "How many skills?", 6),
      uiOutput("getSkillNames")
    ),
    bsCollapsePanel("Colors",
      selectInput("plot_colors", "Select a colorset:",
        choices = c("blue", "green", "red", "yellow", "grey"),
        selected = "blue")
    ),
    bsCollapsePanel("Legend",
      checkboxInput("plot.legend", "Plot Legend?", value = TRUE, width = NULL),
      textInput("legend_title", "Legend Title", value = "Soft Skills"),
      textInput("skillset_1_name", "First Skillset", value = "Current Skillset"),
      textInput("skillset_2_name", "Second Skillset", value = "Projected Skillset")
    ),
    bsCollapsePanel("Plot parameters",
      sliderInput("text_x_sf", "Legend orientation", min = 0, max = 5, value = 1.6, step = .1),
      sliderInput("grid.label.size", "Label sizes", min = 0.5, max = 10, value = 3, step = .1),
      sliderInput("left.labels.hjust", "Left labels horizontal orientation",
        min = -3, max = 3, value = 0, step = .1),
      sliderInput("center.labels.vjust", "Center labels vertical orientation",
        min = -3, max = 3, value = 0, step = .1),
      sliderInput("right.labels.hjust", "Right labels horizontal orientation",
        min = -3, max = 3, value = 0, step = .1)
    ),
    bsCollapsePanel("Save Plot",
      sliderInput("save.scale", "Plot scale", min = 0.1, max = 5, value = 1, step = .1),
      sliderInput("save.width", "Plot width (inches)",
        min = 0, max = 12, value = 8, step = .1),
      sliderInput("save.height", "Plot height (inches)",
        min = 0, max = 12, value = 6, step = .1),
      selectInput("save.device", "Select an output format:",
        choices = c("pdf", "jpeg", "png", "tex", "svg", "bmp"),
        selected = "png")
    )
    )
    ),
  dashboardBody(uiOutput("MainBody"), plotOutput("plot") ,
    downloadButton("downloadData", "Download plot"))
)
