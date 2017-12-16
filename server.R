library(data.table)
library(DT)

# Set options for DT rendering
options(DT.options = list(pageLength = 5, dom = 't', language = list(search = 'Filter:')))
source("create_net_plot.R")


server <- function(input, output) {
  
  # Make Initial DF reactive
  vals = reactiveValues()
  
  # Initial Data
  df = data.table(
    "Creativity" = c(3, 5), 
    "Teamwork" = c(5, 5),
    "Individual Initiative" = c(4, 5),
    "Innovative" = c(3, 4),
    "Curious about Data" = c(5, 5),
    "Analytical Mindset" = c(5, 5)
  )
  vals$Data = df
  vals$rownames = c("Current Skills", "Projected Skillset")
  
  # Render UI
  output$MainBody = renderUI({
    fluidPage(
      box(width=12,
        column(12, dataTableOutput("Main_table")),
        tags$script(HTML('$(document).on("click", "input", function () {
            var checkboxes = document.getElementsByName("row_selected");
            var checkboxesChecked = [];
            for (var i=0; i<checkboxes.length; i++) {
            if (checkboxes[i].checked) {
            checkboxesChecked.push(checkboxes[i].value);
            }
            }
            Shiny.onInputChange("checked_rows",checkboxesChecked);
        })')),
        tags$script("$(document).on('click', '#Main_table button', function () {
        Shiny.onInputChange('lastClickId',this.id);
        Shiny.onInputChange('lastClick', Math.random())
        });")))
  })
  
  # Add modify buttons
  output$Main_table = renderDataTable({
    DT = cbind(vals$Data,
      Modify = paste0('
        <div class="btn-group" role="group" aria-label="Basic example">
        <button type="button" class="btn btn-secondary modify"id=modify_',
        seq_len(2),'>Modify</button></div>'))
    datatable(DT, escape = FALSE, rownames = get_skillset_names())
  })
  
  # Row modification
  modal_modify = modalDialog(
    fluidPage(
      h3(strong("Modify rows"), align = "center"),
      hr(),
      dataTableOutput('row_modif'),
      actionButton("save_changes","Save changes"),
      tags$script(HTML("$(document).on('click', '#save_changes', function () {
          var list_value=[]
          for (i = 0; i < $( '.new_input' ).length; i++)
          {
          list_value.push($( '.new_input' )[i].value)
          }
          Shiny.onInputChange('newValue', list_value)
          });")), width = 10), size="l"
    )
  
  # Observe lastClick
  observeEvent(input$lastClick, {
    if (input$lastClickId %like% "modify")
      showModal(modal_modify)
  }, ignoreInit = TRUE)
  
  # Row modification dialogue
  output$row_modif = renderDataTable({
    selected_row=as.numeric(gsub("modify_","",input$lastClickId))
    old_row=vals$Data[selected_row]
    row_change=list()
    for (i in colnames(old_row)) {
      if (is.numeric(vals$Data[[i]])) {
        row_change[[i]]<-paste0('<input class="new_input" type="number" id=new_',i,'><br>')
      } else {
        row_change[[i]]<-paste0('<input class="new_input" type="text" id=new_',i,'><br>')
      }
    }
    row_change=as.data.table(row_change)
    setnames(row_change,colnames(old_row))
    DT=rbind(old_row,row_change)
    rownames(DT)<-c("Current values","New values")
    DT
    }, escape = FALSE, options = 
      list(dom='t', ordering=FALSE, columnDefs = list(list(width = '10%')), selection="none",
        autoWidth = TRUE, scrollX=TRUE)
  )
  
  # Row modification observer
  observeEvent(input$newValue, {
    newValue=lapply(input$newValue, function(col) {
      if (suppressWarnings(all(!is.na(as.numeric(as.character(col)))))) {
        as.numeric(as.character(col))
      } else {
        col
      }
    })
    DF = data.frame(lapply(newValue, function(x) t(data.frame(x))))
    colnames(DF) = colnames(vals$Data)
    vals$Data[as.numeric(gsub("modify_", "", input$lastClickId))] = DF
  }, ignoreInit = TRUE)
  
  # Control number of skills
  observeEvent(input$nskills, {
    diff = as.integer(input$nskills - ncol(vals$Data))
    if (diff > 0) {
      df = cbind(vals$Data, data.table(matrix(1, nrow = 2, ncol = diff)))
    } else if (diff < 0) {
      df = vals$Data[, colnames(vals$Data)[seq_len(input$nskills)], with = FALSE]
    }
    vals$Data = data.table(df)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  # Dynamically allow to set a number of nskills colnames
  output$getSkillNames = renderUI({
    lapply(seq_len(input$nskills), function(i) {
      textInput(paste0("S", i), paste0("Skill ", i),
        value = c(colnames(df), paste0("X", 1:100))[i])
    })
  })
  
  # Dynamically observe all skill names 
  observeEvent(Map("[[", list(input), paste0("S", seq_len(input$nskills))),
    {
      cn = sapply(seq_len(input$nskills), function(i) {
        input[[paste0("S", i)]]
      })
      colnames(vals$Data)[seq_len(input$nskills)] = unlist(cn)[seq_len(input$nskills)]
    },
    ignoreInit = TRUE, ignoreNULL = TRUE)
  
  # Observe all rowname inputs
  observeEvent(c(input$skillset_1_name, input$skillset_2_name), {
      vals$rownames = get_skillset_names()
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  output$plot = renderPlot({
    createPlot()
  })
  
  createPlot = function() {
    
    pltdf = data.frame(vals$Data)
    pltdf = cbind("group" = as.factor(vals$rownames), pltdf)
    
    vals$p = CreateRadialPlot(pltdf,
      legend.title = input$legend_title,
      plot.extent.x.sf = input$text_x_sf,
      plot.color = get_plot_colors(),
      plot_legend = input$plot.legend,
      grid.label.size = input$grid.label.size,
      axis.label.size = input$grid.label.size,
      left.labels.hjust = - input$left.labels.hjust,
      center.labels.vjust = - input$center.labels.vjust,
      right.labels.hjust = - input$right.labels.hjust)
    return(vals$p)
  }
  
  # Set plot colors
  get_plot_colors = function() {
    colors = switch(input$plot_colors,
      "blue" = c("darkblue", "dodgerblue2"),
      "green" = c("seagreen", "springgreen3"),
      "red" = c("firebrick", "coral1"),
      "yellow" =  c("orange", "gold"),
      "grey" = c("darkgrey" , "darkgrey"))
    return(colors)
  }
  
  # Set rownames
  get_skillset_names = function() {
    c(input$skillset_1_name, input$skillset_2_name)
  }
  
  # Downloadable pdf of selected dataset
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Spiderplot_", input$legend_title, ".", input$save.device, sep="")
    },
    content = function(file) {
      ggsave(file, plot = createPlot(), device = input$save.device,
        scale = input$save.scale,
        width = input$save.width,
        height = input$save.height)
    },
    contentType = "image/png"
  )
}
