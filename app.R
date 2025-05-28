#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

#source files---------------------
source("src/packages.R")
source("src/functions.R")


ui <- fluidPage(titlePanel("Make STICS Files"),
                sidebarLayout(
                  sidebarPanel(
                    checkboxInput("obs_checkbox", "Make .obs Files", value = FALSE),
                    fileInput("excel_file", "Choose Excel File", accept = ".xlsx"),
                    uiOutput("sheet_ui_block"),
                    # dynamically generated
                    shinyDirButton("output_dir", "Choose Output Directory", "Select"),
                    verbatimTextOutput("dir_text"),
                    checkboxInput("csv_checkbox", "Save CSV copies", value = FALSE),
                    actionButton("run_btn", "Generate Files"),
                    actionButton("exit_btn", "Exit App")
                  ),
                  mainPanel(verbatimTextOutput("log"))
                ))

server <- function(input, output, session) {
  volumes <- c(Home = fs::path_home(),
               "R Installation" = R.home(),
               getVolumes()())
  sheets <- reactiveVal()
  log_text <- reactiveVal("")
  output$log <- renderText({
    log_text()
  })
    
  #if no output directory is chosen set as default
  output_dir <- reactive({
    volumes <- c(Home = fs::path_home(), getVolumes()())
    
    # Defensive fallback
    if (!is.null(input$output_dir)) {
      parsed <- parseDirPath(volumes, input$output_dir)
      if (length(parsed) == 1 && nzchar(parsed)) {
        return(parsed)
      }
    }
    
    default_path <- sprintf("files/%s", format(Sys.Date(), "%Y-%m-%d"))
    dir_create(default_path)
    default_path
  })
  
  #display chosen output directory
  shinyDirChoose(input, "output_dir", roots = volumes)
  output$dir_text <- renderText({
    dir_path <- output_dir()
    paste("Output directory:", dir_path)
  })
  
  # Dynamically list sheets
  observeEvent(input$excel_file, {
    req(input$excel_file)
    sheet_list <- readxl::excel_sheets(input$excel_file$datapath)
    sheets(sheet_list)
    
    output$sheet_ui_block <- renderUI({
      tagList(
        checkboxInput("toggle_all", "Select All", value = FALSE),
        checkboxGroupInput("selected_sheets", "Select Sheets", choices = sheet_list)
      )
    })
    
    updateCheckboxInput(session, "toggle_all", value = FALSE)
  })
  
  observeEvent(input$toggle_all, {
    if (isTRUE(input$toggle_all)) {
      updateCheckboxGroupInput(session, "selected_sheets", selected = sheets())
    } else {
      updateCheckboxGroupInput(session, "selected_sheets", selected = character(0))
    }
  })
  
  observe({
    all_selected <- identical(sort(input$selected_sheets), sort(sheets()))
    updateCheckboxInput(session,
                        "toggle_all",
                        label = if (all_selected)
                          "Deselect All"
                        else
                          "Select All")
  })
  
  # Process sheets
  observeEvent(input$run_btn, {
    req(input$excel_file,
        input$selected_sheets)
    
    excel_path <- input$excel_file$datapath
    sheets <- input$selected_sheets
    dir_path <- output_dir()
    

    
    bad_sheets <- invalid_sheets(sheets)
    
    if (any(bad_sheets) &&
        !isTRUE(input$obs_checkbox)) {
      showModal(
        modalDialog(
          title = "Invalid Sheet Names",
          HTML(
            paste0(
              "<b>The following sheets are invalid:</b><br>",
              paste(sheets[bad_sheets], collapse = "<br>"),
              "<br><br><b>Valid names: (not case sensitive)</b><br>",
              "usms, usm<br>",
              "init, ini<br>",
              "sol, sols, soils, soil<br>",
              "tec<br>",
              "sta, station")
            )
          ),
          easyClose = TRUE,
          footer = modalButton("OK")
        )
      return()  # stop execution here
    }
    
    log_text(paste(log_text(), sprintf("From: %s\n", input$excel_file$name )))
    
    for (sheet in sheets) {
      if (isTRUE(input$obs_checkbox) &
          !(sheet %in% c("usms", "init", "sol", "tec", "sta"))) {
        f <- make_obs(sheet,
                      excel_path,
                      dir_path,
                      save2csv = isTRUE(input$csv_checkbox))
      } else {
        f <- make_files(sheet,
                        excel_path,
                        dir_path,
                        save2csv = isTRUE(input$csv_checkbox))
      }
      
      log_text(paste(log_text(), sprintf("Saved: %s\n", f)))
    }
    write(log_text(), 
               sprintf("files/%s/log_%s.txt",
                       format(Sys.Date(), "%Y-%m-%d"),
                       format(Sys.Date(), "%Y-%m-%d")
                       ),
          append = T
    )
  }
  )
  
  observeEvent(input$exit_btn, {
    showModal(modalDialog(
      title = "Confirm Exit",
      "Are you sure you want to exit the app?",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_exit", "Yes")
      ),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$confirm_exit, {
    stopApp()
  })
  
}

shinyApp(ui, server)
