

library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(MtreeRing)
library(testthat)
library(magrittr)
library(png)
library(jpeg)
library(tiff)
library(bmp)
library(magick)
library(imager)
library(dplR)
library(spatstat)
library(measuRing)
library(dplyr)
library(rhandsontable)
library(xRing)
library(shinyMatrix)
library(openxlsx)
library(gridExtra)
# Run the application
createUI <- function()
{
  shiny.title <- dashboardHeader(title = 'MtreeRing')
  shiny.sider <- dashboardSidebar(
    sidebarMenu(
      menuItem('Image Loading',tabName = 'input_pre', 
        icon = icon('folder-open', lib = 'font-awesome'), selected = TRUE),
      menuItem('Measurement',tabName = 'mea_arg', 
        icon = icon('gear', lib = 'font-awesome'))
    )
  )
  page1 <- fluidPage(
    fluidRow(
    box(
      title = div(style = 'color:#FFFFFF;font-size:80%; 
        font-weight: bolder', 'Image Preview'),
      width = 12, status = 'primary', solidHeader = T, collapsible = T,
      prettyCheckbox(
        inputId = "wh_ratio", 
        label = div(style = 'color:black;font-weight: bolder;',
                    'Maintain original width/height ratio'), 
        shape = "curve", value = F, status = "success"),
      hr(),
      plotOutput('pre.img',
        brush = brushOpts(
          id = "plot1_brush",
          opacity = 0.25,
          resetOnNew = TRUE)
      )
      )),
    fluidRow(
    box(
      title = div(style = 'color:#FFFFFF;font-size:80%;
        font-weight: bolder', 'Image Upload'),
      width = 4, status = 'primary', solidHeader = T, collapsible = T,
      conditionalPanel(
        condition = '!input.inmethod',
        fileInput('selectfile', 'Choose an image file',
          buttonLabel = 'Browse...', width = '80%')
      ),
      prettySwitch(inputId = "magick.switch", label = "Magick ON",
        value = TRUE, fill = TRUE, status = "success"),
      helpText('Image upload is limited to 150 MB per file. Supported',
        ' formats include png, jpg, tif and bmp.',
        style = 'color:black;font-size:90%'),
      prettyCheckbox(
        inputId = "inmethod", 
        label = div(style = 'color:black;font-weight: bolder;','Image Path'), 
        shape = "curve", value = F, status = "success"),
      conditionalPanel(
        condition = 'input.inmethod',
        textInput('enter.path', 'Enter file path', ''),
        helpText('For example: C:/Users/shiny/img01.png',
          style = 'color:black;font-size:90%'),
        hr()
      ),
      actionButton(
        'buttoninputimage', 'Load ',
        class = "btn btn-primary btn-md",
        icon = icon('upload',  "fa-1x"),
        style = 'color:#FFFFFF;text-align:center;
        font-weight: bolder;font-size:110%;'),
      useSweetAlert()
      ),
    box(
      title = div(style = 'color:#FFFFFF;font-size:80%;
        font-weight: bolder', 'Image Cropping'),
      width = 3, status = 'primary', solidHeader = T, collapsible = T,
      helpText("To remove unwanted cores and irrelevant objects, ",
               "move the mouse to the core you wish to measure and",
               "create a rectangle by brushing, see details below.",
               style = 'color:black;font-size:90%;text-align:justify;'),
      prettyRadioButtons(inputId = "cropcondition", label = "",
                         choiceNames = 'UNCROPPED', choiceValues = list('a'),
                         status = "danger", shape = "square",
                         fill = FALSE, inline = FALSE),
      prettyCheckbox(
        inputId = "showcropp", 
        label = div(style = 'color:black;font-weight: bolder;', 'Show Help'),
        shape = "curve", value = F, status = "success"
      ),
      conditionalPanel(
        condition = 'input.showcropp',
        helpText(
          "The operation \"brush\" allows users to create a transparent ", 
          "rectangle on the image and drag it around. For cores scanned ", 
          "side by side, the user can choose a core of interest by brushing.", 
          style = 'color:black;text-align:justify;'),
        helpText(
          "After brushing, click on the button \"Crop\" to create a",
          " cropped area. The measurement will be performed within", 
          " this area, rather than the whole (uncropped) image.",
          style = 'color:black;text-align:justify;'),
        helpText(
          "To cancel this operation, click on the button \"Cancel\".",
          " If the transparent rectangle exists, the user should first ",
          "click on the outer region of the rectangle (this will make the",
          " rectangle disappear) and then click on the button \"Cancel\".",
          style = 'color:#FF0000;text-align:justify;')
      ),  
      
      hr(),
      actionButton(
        'buttoncrop', 'Crop',
        class = "btn btn-primary btn-md",
        icon = icon('crop',"fa-1x"),
        style = 'color:#FFFFFF;text-align:center;
        font-weight: bolder;font-size:110%;')
    ),
    box(
      title = div(style = 'color:#FFFFFF;font-size:80%;
        font-weight: bolder', 'Image Rotation'),
      width = 3, status = 'primary', solidHeader = T, collapsible = T,
      prettyRadioButtons(inputId = "rotatede", label = "",
                         choices = c("0 degrees" = "rotate0",
                                     "90 degrees" = "rotate90",
                                     "180 degrees" = "rotate180",
                                     "270 degrees" = "rotate270"),
                         shape = "curve", status = "success",
                         fill = TRUE, inline = TRUE),
      helpText("Rotation angle in degrees. Note that the bark ",
               "side should be placed at the left side of the ",
               "graphics window and the pith side at the right.",
               style = 'color:black;font-size:90%;text-align:justify;'),
      actionButton(
        'buttonrotate', 'Rotate',
        class = "btn btn-primary btn-md",
        icon = icon('repeat',"fa-1x"),
        style = 'color:#FFFFFF;text-align:center;
        font-weight: bolder;font-size:110%;'),
      
    ),),
    fluidRow(
    # New box to fill in data for light calibration
    box(
      title = div(style = 'color:#FFFFFF;font-size:80%;
        font-weight: bolder', 'Light calibration'), status = 'primary', solidHeader = T, collapsible = T, width = 5,
      helpText("Introduce thickness parameters",
               "as well as image intensity, number of steps",
               "and the material density for densitometry analysis",
               style = 'color:black;font-size:90%;text-align:justify;'),
      numericInput("density", "Density (g/cm3):", 1.20, step=0.1),
      conditionalPanel(
        condition = '!input.loadMatrix',
        tableOutput("static"),
        numericInput("nsteps", "Number of Steps:", 2, min = 1, max = 30),
        matrixInput("thickness_matrix",
                    value = matrix(0, 2, 2,dimnames = list(NULL,c("Thickness","Intensity"))),
                    rows = list(
                      editableNames = TRUE),
                    class = "numeric",
                    cols = list(names = TRUE)
        ),
        uiOutput("matrixcontrol")),
      conditionalPanel(
        condition = 'input.loadMatrix',
        fileInput('path_matrix', 'Choose a txt file of the matrix',
                  buttonLabel = 'Browse...', width = '80%'),
      ),
      conditionalPanel(
        condition = '!input.loadMatrix',
        prettyCheckbox(
          inputId = "saveMatrix", 
          label = div(style = 'color:black;font-weight: bolder;','Save Matrix'), 
          shape = "curve", value = F, status = "success")),
      conditionalPanel(
        helpText("Save matrix to a file",
                 style = 'color:black;font-size:90%;text-align:justify;'),
        textInput("filenameMatrix","Enter filename:"),
        condition = 'input.saveMatrix && !input.loadMatrix',actionButton(
          'savematrix', 'Save',
          class = "btn btn-primary btn-md",
          icon = icon('upload',  "fa-1x"),
          style = 'color:#FFFFFF;text-align:center;
        font-weight: bolder;font-size:110%;'),
        br(),
        br()),
      prettyCheckbox(
        inputId = "loadMatrix", 
        label = div(style = 'color:black;font-weight: bolder;','Load Matrix'), 
        shape = "curve", value = F, status = "success"),
      br(),
      br(),
      actionButton(
        'buttondensity', 'Plot',
        class = "btn btn-primary btn-md",
        icon = icon('upload',  "fa-1x"),
        style = 'color:#FFFFFF;text-align:center;
        font-weight: bolder;font-size:110%;'),
        hr()
    ), 
    # Box for diplaying light Calibration
    box(
      title = div(style = 'color:#FFFFFF;font-size:80%; 
        font-weight: bolder', 'Light Calibration'),width = 6, status = 'primary', solidHeader = T, collapsible = T,
      hr(),
      plotOutput("light")
    ),))
  page2.1 <- fluidRow(
    box(
      title = div(style = 'color:#FFFFFF;font-size:80%;
        font-weight: bolder', 'Sample Info'), height = "auto",
      width = 4, status = 'primary', solidHeader = T, collapsible = T,
      textInput('tuid', 'Series ID', '', width = '75%'),
      textInput('dpi', 'DPI', '', '75%'),
      textInput('sample_yr', 'Year', '', '75%'),
      prettyCheckbox(
        inputId = "moreinfo", 
        label = div(
          style = 'color:black;font-weight: bolder;font-size:90%', 
          'More Info'), 
        shape = "curve", value = F, status = "success"
      ),
      prettyCheckbox(
        inputId = "decades", 
        label = div(
          style = 'color:black;font-weight: bolder;font-size:90%', 
          'Years in Decades'), 
        shape = "curve", value = F, status = "success"
      ),
      conditionalPanel(
        condition = "input.moreinfo",
        textInput('sample_site', 'Site', '', '75%'),
        textInput('sample_parcel', 'Plot', '', '75%'),
        textInput('sample_species', 'Species', '', '75%')
      ),
      # textInput('m_line', 'Y-coordinate of path', '', '75%'),
      pickerInput(
        inputId = "sel_sin_mul", 
        div(
          style = 'color:black;font-weight:bolder;font-size:90%', 
          'Path Mode'), 
        width = '87%',
        choices = c("Single Segment", "Multi Segments"),
        options = list(style = "btn-primary")
      ),
      conditionalPanel(
        condition = 'input.sel_sin_mul == "Single Segment"',
        prettyCheckbox(
          inputId = "hor_path", 
          label = div(
            style = 'color:black;font-weight: bolder;font-size:90%', 
            'Horizontal path'), 
          shape = "curve", value = T, status = "success"
        )
      ),
      numericInput('num_seg', 
        div(style = 'color:black;font-weight:bolder;font-size:90%', 
            'Number of segments'),
        value = 1, min = 1, max = 1, step = 1, width = "75%"),
      conditionalPanel(
        condition = 'input.hor_path',
        prettyCheckbox(
          inputId = "incline", 
          label = div(
            style = 'color:black;font-weight: bolder;font-size:90%', 
            'Inclined tree rings'), 
          shape = "curve", value = F, status = "success"
        ),
        conditionalPanel(
          condition = 'input.incline',
          numericInput('h.dis', 'Distance between paths (mm)', 
                       1, 0.1, 30, 0.1, width = '75%')
        ) 
      )
      
    ),
    box(
      title = div(style = 'color:#FFFFFF;font-size:80%;
        font-weight: bolder', 'Label Options'), height = "auto",
      width = 4, status = 'primary', solidHeader = T, collapsible = T,
      sliderInput('linelwd', 'Path width', 
        0.2, 3, 1, 0.1, width = '80%'),
      sliderInput('label.cex', 'Magnification for labels',
        0.2, 3, 1.5, 0.1, width = '80%'),
      radioGroupButtons(
        inputId = "pch", 
        label = 'Symbol for borders', status = "btn btn-primary btn-md",
        size = 'sm',
        choiceNames = list(
          div(style = 'color:#FFFFFF;font-weight: bolder;',
            icon('circle', 'fa-lg')), 
          div(style = 'color:#FFFFFF;font-weight: bolder;',
            icon('circle', 'fa-1x')), 
          div(style = 'color:#FFFFFF;font-weight: bolder;',
            icon('circle-o', 'fa-1x')), 
          div(style = 'color:#FFFFFF;font-weight: bolder;',
            icon('times', 'fa-1x')),
          div(style = 'color:#FFFFFF;font-weight: bolder;',
            icon('plus', 'fa-1x'))
        ),
        selected = '20', 
        choiceValues = list('19', '20', '1', '4', '3'),
        width = '100%'
      ),
      colorSelectorInput(
        inputId = "border.color", label = "Color for borders",
        choices = c("black", "gray", "white", "red", "#FF6000", 
          "#FFBF00", "#DFFF00", "#80FF00", "#20FF00", 
          "#00FF40", "#00FF9F", "cyan", "#009FFF", "#0040FF",
          "#2000FF", "#8000FF", "#DF00FF", "#FF00BF"),
        selected = '#20FF00', mode = "radio", display_label = FALSE, ncol = 9
      ),
      colorSelectorInput(
        inputId = "label.color", label = "Color for labels",
        choices = c("black", "gray", "white", "red", "#FF6000", 
          "#FFBF00", "#DFFF00", "#80FF00", "#20FF00", 
          "#00FF40", "#00FF9F", "cyan", "#009FFF", "#0040FF",
          "#2000FF", "#8000FF", "#DF00FF", "#FF00BF"),
        selected = 'black', mode = "radio", display_label = FALSE, ncol = 9
      )
    ),
    box(
      title = div(style = 'color:#FFFFFF;font-size:80%;
                  font-weight: bolder', 'Detection Options'),  height = "auto",
      width = 4, status = 'primary', solidHeader = T, collapsible = T,
      numericInput('pixelspath', 
                   div(style = 'color:black;font-weight:bolder;font-size:90%', 
                       'Pixels for density profile'),
                   value = 5, min = 0, max = 20, step = 1, width = "75%"),
      prettyCheckbox(
        inputId = "isrgb", 
        label = div(
          style = 'color:black;;font-size:90%;font-weight:bolder;', 
          "Default RGB"), 
        shape = "curve", value = T, status = "success"
      ),
      conditionalPanel(
        condition = '!input.isrgb',
        textInput('customRGB', 'Custom RGB', '0.299,0.587,0.114'),
        helpText('Note:The three numbers correspond to',
                 'R, G and B components,respectively.',
                 style = 'color:black;font-weight: bolder'),
        hr()
      ),
      radioGroupButtons(
        inputId = "method",
        label = div(style = 'color:black;font-weight: bolder;font-size:85%',
                    'Ring detection method'),
        status = "btn btn-primary btn-md",
        #individual = T,
        selected = 'canny',
        size = 'normal',
        choiceNames = list(
          div(style = 'color:#FFFFFF;font-weight: bolder;font-size:85%',
              'Watershed'),
          div(style = 'color:#FFFFFF;font-weight: bolder;font-size:85%',
              'Canny'),
          div(style = 'color:#FFFFFF;font-weight: bolder;font-size:85%',
              'measuRing')
        ),
        choiceValues = list('watershed', 'canny', 'lineardetect'), 
        width = '100%'
      ),
      conditionalPanel(
        condition = 'input.method=="watershed"',
        selectInput('watershed.threshold',
                    'Otsu threshold',
                    c('Auto (Recommended)' = 'auto',
                      'Custom' = 'custom.waterthr'),
                    width = '75%'
        ),
        conditionalPanel(
          condition = 'input["watershed.threshold"]=="auto"',
          sliderInput('watershed.adjust',
                      'Threshold adjusment factor',
                      0.5, 1.5, 0.8, 0.05, width = '85%')
        ),
        conditionalPanel(
          condition = 'input["watershed.threshold"]=="custom.waterthr"',
          textInput('watershed.threshold2', 
                    'Threshold value', '', width = '75%'),
          'A value of the form XX% (e.g. 98%)',
          br(),
          br()
        )
      ),
      conditionalPanel(
        condition = 'input.method=="canny"',
        prettyCheckbox(
          inputId = "defaultcanny", 
          label = div(
            style = 'color:black;font-size:90%;font-weight: bolder;',
            "Auto threshold (Recommanded)"), 
          shape = "curve", value = T, status = "success"),
        conditionalPanel(
          condition = 'input.defaultcanny',
          sliderInput('canny.adjust',
                      'Threshold adjusment factor',
                      0.8, 1.8, 1.4, 0.05, width = '85%')
        ),
        conditionalPanel(
          condition = '!input.defaultcanny',
          textInput('canny.t2', 'Threshold for strong edges', '', '85%'),
          textInput('canny.t1', 'Threshold for weak edges', '', '85%')
        ),
        sliderInput('canny.smoothing', 'Degree of smoothing',
                    0, 5, 2, 1, width = '85%')
        # numericInput('canny.smoothing', 'Degree of smoothing',
        #   1, 0, 4, 1, width = '75%')
      ),
      conditionalPanel(
        condition = 'input.method!="lineardetect"',
        prettyCheckbox(inputId = "defaultse", 
                       label = div(
                       style = 'color:black;font-size:90%;font-weight:bolder;',
                       "Default structuring elements"), 
                       shape = "curve", value = T, status = "success"),
        conditionalPanel(
          condition = '!input.defaultse',
          numericInput('struc.ele1', 'First structuring element', 
                       3, 1, 100, 1, "75%"),
          numericInput('struc.ele2', 'First structuring element', 
                       9, 1, 100, 1, "75%")
        ),
        hr()
      ),
      conditionalPanel(
        condition = 'input.method=="lineardetect"',
        textInput('origin', ' Origin in smoothed gray', '0', '75%'),
        'If you use the linear detection, don\'t ',
        'tick the checkbox "Inclined tree rings".',
        hr()
      ),
      helpText('Automatic detection may take a few seconds.',
               # 'depending on the image size and complexity of the sample.',
               # style = 'color:black;font-size:95%;text-align:justify;')
               style = 'color:black;font-size:90%;')
    ),
    box(
      title = div(style = 'color:#FFFFFF;font-size:100%;
        font-weight: bolder', 'Main Window'),
      width = 12, status = 'primary', solidHeader = T, collapsible = T,
      radioGroupButtons(inputId = "sel_mode", status = "primary",
        label = 
          div(style = 'color:black;font-weight: bolder;font-size:110%',
              'Working mode selector'),
        choiceNames = list(
          div(style = 'color:#FFFFFF;font-weight: bolder;font-size:110%',
              'Path Creation'),
          div(style = 'color:#FFFFFF;font-weight: bolder;font-size:110%',
              'Ring Detection'),
          div(style = 'color:#FFFFFF;font-weight: bolder;font-size:110%',
              'Ring Editing')
        ),
        # direction = "vertical",
        choiceValues = list('sel_path', 'sel_det', 'sel_edit')
      ),
      conditionalPanel(
        condition = "input.sel_mode == 'sel_path'",
        actionButton(
          'rm_last', 'Remove Last',
          class = "btn btn-warning btn-md", icon = icon('reply'),
          style = 'color:#FFFFFF;text-align:center;font-weight: bolder'
        ),
        useSweetAlert(),
        actionButton(
          'rm_all', 'Remove All',
          class = "btn btn-danger btn-md", icon = icon('trash'),
          style = 'color:#FFFFFF;text-align:center;font-weight: bolder'
        ),
        useSweetAlert(),
        br(),
        br(),
        prettyCheckbox(
          inputId = "pre_path", 
          label = div(style = 'color:black;font-weight: bolder;',
                      'Show the preview path'), 
          shape = "curve", value = F, status = "success")
      ),
      conditionalPanel(
        condition = "input.sel_mode == 'sel_det'",
        actionButton(
          'button_run_auto', 'Run Detection',
          class = "btn btn-success btn-md", icon = icon('play'),
          style = 'color:#FFFFFF;text-align:center;font-weight: bolder'
        ),
        actionButton(
          'button_run_auto_xray', 'Run Detection for X-RAY',
          class = "btn btn-success btn-md", icon = icon('play'),
          style = 'color:#FFFFFF;text-align:center;font-weight: bolder'
        ),
        useSweetAlert(),
        br(),
        br(),
        actionButton(
          'button_run_auto_early', 'Run Detection for Early-Late Wood',
          class = "btn btn-success btn-md", icon = icon('play'),
          style = 'color:#FFFFFF;text-align:center;font-weight: bolder'
        ),
        
        br(),
        br(),
      ),
      conditionalPanel(
        condition = "input.sel_mode == 'sel_edit'",
        actionButton(
          'buttonzoomdel', 'Delete Border',
          class = "btn btn-warning btn-md",
          icon = icon('eraser'),
          style = 'color:#FFFFFF;text-align:center;font-weight: bolder'
        ),
        useSweetAlert(),
        actionButton(
          'rm_all_border', 'Remove All',
          class = "btn btn-danger btn-md", icon = icon('trash'),
          style = 'color:#FFFFFF;text-align:center;font-weight: bolder'
        ),
        useSweetAlert(),
        br(),
        br()
      ),
      prettyCheckbox(
        inputId = "wh_ratio2", 
        label = div(style = 'color:black;font-weight: bolder;',
                    'Maintain original width/height ratio'), 
        shape = "curve", value = F, status = "success"),
      conditionalPanel(
        condition = "input.sel_mode == 'sel_det'",
        prettyCheckbox(
          inputId = "show_profile", 
          label = div(style = 'color:black;font-weight: bolder;',
                      'Show Density Profile'), 
          shape = "curve", value = F, status = "success")
      ),
      conditionalPanel(
        condition = "input.sel_mode == 'sel_det'",
        prettyCheckbox(
          inputId = "show_wood", 
          label = div(style = 'color:black;font-weight: bolder;',
                      'Show Early/Late Wood'), 
          shape = "curve", value = F, status = "success")
      ),
      conditionalPanel(
        condition = "input.sel_mode == 'sel_edit'",
        prettyCheckbox(
          inputId = "edit_wood", 
          label = div(style = 'color:black;font-weight: bolder;',
                      'Edit Early/Late Wood'), 
          shape = "curve", value = F, status = "success")
      ),
      hr(),
      fluidPage(
        fluidRow(
          conditionalPanel(condition="input.show_profile",
                           column(width = 11,
                                  plotOutput('profile_edit',height="200px")
                           )
          )
          ,
          column(width = 11,
                 plotOutput('ring_edit', height = "310px",
                            dblclick = "plot2_dblclick",
                            brush = brushOpts(
                              id = "plot2_brush", resetOnNew = TRUE
                            ),
                            hover = hoverOpts(
                              id = "plot2_hover", delay = 300,
                              delayType = "debounce"
                            )
                 )
          ),
          column(width = 1,
                 br(), br(),
                 noUiSliderInput(
                   width = "100px", height = "250px",
                   inputId = "img_ver", label = NULL, tooltips = F,
                   min = 0, max = 1000, step = 10,
                   value = c(0, 1000), margin = 10,
                   orientation = "vertical", behaviour = "drag"
                 )
          )
        ),
        br(),
        fluidRow(
          column(
            width = 11, offset = 0,
            sliderInput(
              inputId = "img_hor", label = NULL,
              min = 0, max = 100, value = c(0, 100), step = 1, 
              round = T, ticks = F, dragRange = T, post = "%"
            )
          )
        )
      )
    )
  )
  page2.2 <- fluidRow(
    column(width = 12,
      conditionalPanel(
        condition = '!input.tuheader',
        box(
          title = div(style = 'color:#FFFFFF;font-size:80%;
            font-weight: bolder', 'Delete Borders'),
          width = 3, status = 'primary', solidHeader = T, collapsible = T,
          conditionalPanel(
            condition = 'input.incline',
            textInput('del.u', 'Border number in the upper portion', '', '75%'),
            textInput('del.l', 'Border number in the lower portion', '', '75%')
          ),
          conditionalPanel(
            condition = '!input.incline',
            textInput('del', 'Border number', '', '75%')
          ),
          helpText(
            "To perform a mass deletion of ring borders, use commas ",
            "to separate border numbers, e.g. 1, 2, 3, 4",
            style = 'color:black;text-align:justify;'
          ),
          br(),
          br(),
          actionButton(
            'button_del', 'Delete Border',
            class = "btn btn-danger btn-md", icon = icon('eraser'),
            style = 'color:#FFFFFF;text-align:center;font-weight: bolder'
          )
        )
      ),
      tabBox(
        #title = tagList(shiny::icon("gear"), 'Output'),
        title = div(
          style = 'color:black;font-weight: bolder;',
          icon('cog', class = 'fa-spin', lib = 'font-awesome'), 'Output'),
        width = 6,
        tabPanel(
          div(style = 'color:black;font-weight: bolder;',
            icon('list-ol', 'fa-1x'), ' Results'),
          #HTML("<p style = 'color:black;'><b>Results</b></p>"),
          actionButton(
            'button_results', 'Generate Series',
            class = "btn btn-primary btn-md",
            style = 'color:#FFFFFF;text-align:center;font-weight:bolder;'
          ),
          useSweetAlert(),
          actionButton(
            'button_hide', 'Hide Series',
            class = "btn btn-primary btn-md",
            style = 'color:#FFFFFF;text-align:center;font-weight:bolder;'
          ),
          useSweetAlert(),
          br(),
          tableOutput('results')
        ),
        tabPanel(
          div(style = 'color:black;font-weight: bolder;',
            icon('arrow-down', 'fa-1x'), ' CSV'
          ),
          textInput('csv.name', 'Name of the csv file', '', width = '50%'),
          helpText(
            style = 'color:black;font-weight: normal;',
            'The filename extension is not required. ',
            'Leave blank to use the current series ID.'
          ),
          helpText(
            style = 'color:#FF0000;font-weight: normal;',
            'Attention: if running the app within an RStudio window',
            ', the rename operation doesn\'t work. Please run the app',
            ' within a browser.'
          ),
          hr(),
          #HTML("<p style = 'color:black;'><b>CSV</b></p>"),
          downloadButton(
            'RingWidth.csv', 'Download CSV',
            class = "btn btn-primary btn-md",
            style = 'color:#FFFFFF;text-align:center;font-weight:bolder;'
          )
        ),
        tabPanel(
          div(style = 'color:black;font-weight: bolder;',
              icon('arrow-down', 'fa-1x'), ' Excel'),
          textInput('excel.name', 'Name of the excel file', '', width = '50%'),
          downloadButton(
            'RingWidth.xlsx', 'Download excel',
            class = "btn btn-primary btn-md",
            style = 'color:#FFFFFF;text-align:center;font-weight:bolder;'
          )
        ),
        tabPanel(
          div(style = 'color:black;font-weight: bolder;',
              icon('arrow-down', 'fa-1x'), 'Image'),
          textInput('image.name', 'Name of the image file', '', width = '50%'),
          downloadButton(
            'imageCore', 'Save image',
            class = "btn btn-primary btn-md",
            style = 'color:#FFFFFF;text-align:center;font-weight:bolder;'
          )
        ),
        tabPanel(
          div(style = 'color:black;font-weight: bolder;',
              icon('arrow-down', 'fa-1x'), 'Path'),
          textInput('imgpath.name', 'Name of the image file', '', width = '50%'),
          downloadButton(
            'imagePath', 'Save image',
            class = "btn btn-primary btn-md",
            style = 'color:#FFFFFF;text-align:center;font-weight:bolder;'
          )
        ),
        tabPanel(
          div(style = 'color:black;font-weight: bolder;',
            icon('arrow-down', 'fa-1x'), ' RWL'),
          textInput('rwl.name', 'Name of the rwl file', '', width = '50%'),
          helpText(style = 'color:black;font-weight: normal;',
            'The filename extension is not required. ',
            ' Leave blank to use the current series ID.'),
          helpText(style = 'color:#FF0000;font-weight: normal;',
            'Attention: if running the app within an RStudio window',
            ', the rename operation doesn\'t work. Please run the app',
            ' within a browser.'),
          hr(),
          selectInput('tuprec', 'Precision of the rwl file',
            c('0.01' = '0.01', '0.001' = '0.001'),
            selected = '0.01', width = '50%'),
          helpText(style = 'color:black;font-weight: normal;', 
            'Units are in mm.'),
          hr(),
          checkboxInput('tuheader', 'Header of the File', F),
          conditionalPanel(  
            condition = 'input.tuheader',
            actionButton(
              'reset.hdr', 'Reset Header',
              class = "btn btn-danger btn-md",
              icon = icon('trash'),
              style = 'color:#FFFFFF;text-align:center;font-weight: bolder'
            )
          ),
          helpText(style = 'color:black;font-weight: normal;',
            'For more details about the header, please', 
            'read reference manual of the R package dplR.', 
            'The output file is Tucson format.'),
          hr(),
          #HTML("<p style = 'color:black;'><b>RWL</b></p>"),
          downloadButton(
            'RingWidth.rwl', 'Download RWL',
            class = "btn btn-primary btn-md",
            style = 'color:#FFFFFF;text-align:center;font-weight:bolder;'
          )
        )
      ),
      conditionalPanel(  
        condition = 'input.tuheader',
        box(
          title = 'Header',width = 3, 
          status = 'primary', solidHeader = T, collapsible = T,
          textInput('tuhdr1', 'Site ID', ''),
          textInput('tuhdr2', 'Site Name', ''),
          textInput('tuhdr3', 'Species Code', ''),
          textInput('tuhdr4', 'State or Country', ''),
          textInput('tuhdr5', 'Species', ''),
          textInput('tuhdr6', 'Elevation', '')
        )
      ),   
      conditionalPanel(  
        condition = 'input.tuheader',
        box(
          title = 'Header',width = 3, 
          status = 'primary', solidHeader = T, collapsible = T,
          textInput('tuhdr7', 'Latitude', ''),
          textInput('tuhdr8', 'Longitude', ''),
          textInput('tuhdr9', 'First Year', ''),
          textInput('tuhdr10', 'Last Year', ''),
          textInput('tuhdr11', 'Lead Investigator', ''),
          textInput('tuhdr12', 'Completion Date', '')
        )
      )   
    )
  )
  shiny.body <- dashboardBody(
    tabItems(
      tabItem(tabName = 'input_pre', page1),
      tabItem(tabName = 'mea_arg', page2.1, page2.2)
    )
  )
  ui <- dashboardPage(
    shiny.title,
    shiny.sider,
    shiny.body
  )
  return(ui)
}

createServer <- function(input, output, session) 
{
  f.morphological <- function(seg.data, struc.ele1, struc.ele2, x.dpi) {
    cim <- as.cimg(seg.data)
    cim2 <- erode_rect(cim, sx = struc.ele1[1], sy = struc.ele1[2], sz = 1L)
    cim2 <- dilate_rect(cim2, sx = struc.ele1[1], sy = struc.ele1[2], sz = 1L)
    cim2 <- dilate_rect(cim2, sx = struc.ele1[1], sy = struc.ele1[2], sz = 1L)
    cim2 <- erode_rect(cim2, sx = struc.ele1[1], sy = struc.ele1[2], sz = 1L)
    cim2 <- erode_rect(cim2, sx = struc.ele2[1], sy = struc.ele2[2], sz = 1L)
    cim2 <- dilate_rect(cim2, sx = struc.ele2[1], sy = struc.ele2[2], sz = 1L)
    return(cim2)
  }
  hat <- function(seg.mor, x.dpi, watershed.threshold, watershed.adjust) {
    black.hat <- mclosing_square(seg.mor, size = round(x.dpi / 10))
    black.hat <- black.hat - seg.mor
    black.hat <- threshold(black.hat, thr = watershed.threshold, 
      approx = FALSE, adjust = watershed.adjust)
    black.hat <- 1 - black.hat
    black.hat.mat <- black.hat[, , 1, 1]
    return(black.hat.mat)
  }
  normalize <- function(x) {
    return((x - min(x))/(max(x) - min(x)))
  }
  correct.color <- function(water.c2) {
    color.adj <- function(i, water.c2, diff.m) {
      color.position <- which(water.c2 == i, arr.ind = T)
      row.range <- range(color.position[, 1])
      row.range <- row.range[1]:row.range[2]
      color.adjacent <- integer()
      for (j in row.range) {
        row.p <- which(color.position[, 1] == j)
        min.column <- color.position[row.p, 2] %>% min
        color.diff <- which(diff.m[, j] != 0)
        color.pre.p <- color.diff[which(color.diff == min.column) - 1] - 1
        color.pre <- water.c2[j, color.pre.p]
        color.adjacent <- c(color.adjacent, color.pre)
      }
      max(color.adjacent)
    }  
    water.c3 <- cbind(matrix(-1, nrow(water.c2), 1), 
      matrix(0, nrow(water.c2), 1), 
      water.c2)
    diff.m <- apply(water.c3, 1, function(x) c(0, diff(x)))
    color.max <- max(water.c2)
    df.color <- data.frame(color = c(1:color.max), 
      adj = rep(NA, times = color.max))
    for (i in 1:color.max) {
      test.c <- color.adj(i, water.c3, diff.m)
      df.color[i, 2] <- test.c
    }
    for (i in -1:color.max) {
      adj.c <- which(df.color[, 2] == i) 
      if (length(adj.c) >= 2) {   
        max.c <- max(df.color[adj.c, 1])  
        covered.c <- sort(df.color[adj.c, 1])
        covered.c <- covered.c[-length(covered.c)]
        for (j in covered.c) {
          cl <- which(water.c3 == j, arr.ind = T)
          water.c3[cl] <- max.c
          df.color[which(df.color == j, arr.ind = T)] <- max.c
        }
      } 
    }
    return(water.c3[, -c(1, 2)])
  }
  water.im <- function(black.hat, correct) {
    water.c <- connected(im(black.hat), background = 0, method = "C")
    water.c2 <- apply(water.c$v, 2, function(x){
      x[is.na(x)]<- 0
      return(x)
    })
    if (correct)
      water.c2 <- correct.color(water.c2)
    return(water.c2)
  }
  watershed.im <- function(water.seg, seg.data) {
    normalize <- function(x) return((x - min(x))/(max(x) - min(x)))
    imgra <- imgradient(as.cimg(seg.data), axes = "y", scheme = 2)
    watershed.seg <- watershed(as.cimg(water.seg), imgra, fill_lines = F)
    # watershed.seg <- normalize(watershed.seg[, , 1, 1])
    return(watershed.seg[,, 1, 1])
  }

  f.sort <- function(bor_xy, dp) {
    filter.col <- diff(bor_xy$x) >= dp/10
    filter.col <- c(TRUE, filter.col)
    bor_xy <- bor_xy[filter.col,]
    return(bor_xy)
  }
  # Plots borders on top of img
  plot.marker <- function(path.info, hover.xy, sample_yr, l.w, pch,
                          bor.color, lab.color, label.cex, el_wood)
  {
    if(is.null(path.info$x))
      return()
    p.max <- path.info$max
    p.x <- path.info$x - crop.offset.xy$x
    p.y <- path.info$y - crop.offset.xy$y
    p.type <- path.info$type
    p.hor <- path.info$horizontal
    incline <- path.info$incline
    h.dis <- path.info$h
    dpi <- path.info$dpi
    len <- length(p.x)
    # plot path
    if (len == 1)
      points(p.x, p.y, pch = 16, col = lab.color)
    if (len >= 2 & !incline) 
      points(p.x, p.y, type = 'l', col = lab.color, lty = 1, lwd = l.w)
    if(incline){
      dp <- dpi/25.4
      d <- h.dis*dp/2
      points(p.x, p.y + d, type = 'l', col = lab.color, lty = 1, lwd = l.w)
      points(p.x, p.y - d, type = 'l', col = lab.color, lty = 1, lwd = l.w)
      if(len == 2) {
        points(p.x, p.y, type = 'l', col = lab.color, lty = 2, lwd = l.w)
        points(c(p.x[1], p.x[1]), c(p.y[1] + d, p.y[1] - d), 
               type = 'l', col = lab.color, lty = 2, lwd = l.w)
        points(c(p.x[len], p.x[len]), c(p.y[len] + d, p.y[len] - d), 
               type = 'l', col = lab.color, lty = 2, lwd = l.w)
      }
    }
    if(input$sel_mode == 'sel_path' & len < p.max & len >= 1 & input$pre_path){
      y <- ifelse(p.hor, p.y[len], hover.xy$y)
      points(c(p.x[len], hover.xy$x), c(p.y[len], y), 
             type = 'l', col = lab.color, lty = 2, lwd = l.w)
    }
    if(!is.null(el_wood)){
      if(input$show_wood){
        points(el_wood$x, el_wood$y, col = 'red', type = "p", 
               pch = pch, cex = label.cex * 0.75)
    }}
    # plot border point
    if(is.null(df.loc$data))
      return()
    df.loc <- df.loc$data
    # Here we have the data of the borders and path pixels and yr printing
    if (nrow(df.loc) >= 1) {
      bx <- df.loc$x - crop.offset.xy$x
      by <- df.loc$y - crop.offset.xy$y
      bz <- df.loc$z
      bz <- bz[order(bx)]
      by <- by[order(bx)]
      bx <- sort(bx)
      if (incline) {
        up <- which(bz == 'u')
        lenup <- length(up)
        if (lenup >= 1) {
          points(bx[up], by[up], col = bor.color, type = "p", 
            pch = pch, cex = label.cex * 0.75)
          if(input$decades){
            oddvalsx <- seq(1, length(bx[up]), by=10)
            oddvalsy <- seq(1, length(by[up]), by=10)
            year.u <- c(seq(sample_yr,(sample_yr - lenup + 1),by=-10))
            bx_modu <- bx[up][oddvalsx]
            by_modu <- by[up][oddvalsy]
            text(bx_modu, by_modu, year.u, adj = c(-0.5, 0.5), 
                 srt = 90, col = lab.color, cex = label.cex)
          }
          else{
            year.u <- c(sample_yr:(sample_yr - lenup + 1))
            text(bx[up], by[up], year.u, adj = c(-0.5, 0.5), 
                 srt = 90, col = lab.color, cex = label.cex)
          }
          border.num <- 1:lenup
          text(bx[up], by[up], border.num, adj = c(0.5, 2.25), 
               col = lab.color, cex = label.cex)
        }
        lower <- which(bz == 'l')
        lenlo <- length(lower)
        if (lenlo >= 1) {
          points(bx[lower], by[lower], col = bor.color, type = "p", 
            pch = pch, cex = label.cex * 0.75)
          if(input$decades){
            oddvalsx <- seq(1, length(bx[lower]), by=10)
            oddvalsy <- seq(1, length(by[lower]), by=10)
            year.l <- c(seq(sample_yr,(sample_yr - lenlo + 1),by=-10))
            bx_modl <- bx[lower][oddvalsx]
            by_modl <- by[lower][oddvalsy]
            text(bx_modl, by_modl, year.l, adj = c(1.5, 0.5), 
                 srt = 90, col = lab.color, cex = label.cex)
          }
          else{
            year.l <- c(sample_yr:(sample_yr - lenlo + 1))
            text(bx[lower], by[lower], year.l, adj = c(1.5, 0.5), 
                 srt = 90, col = lab.color, cex = label.cex)
          }
          border.num <- 1:lenlo
          text(bx[lower], by[lower], border.num, adj = c(0.5, -1.25), 
               col = lab.color, cex = label.cex)
        }
      } else { 
        if (length(bx) >= 1) {
          lenbx <- length(bx)
          points(bx, by, col = bor.color, type = "p", 
            pch = pch, cex = label.cex * 0.75)
          if(input$decades){
            oddvalsx <- seq(1, length(bx), by=10)
            oddvalsy <- seq(1, length(by), by=10)
            year.u <- c(seq(sample_yr,(sample_yr - length(by)  + 1),by=-10))
            bx_mod <- bx[oddvalsx]
            by_mod <- by[oddvalsy]
            text(bx_mod, by_mod, year.u, adj = c(1.5, 0.5), 
                 srt = 90, col = lab.color, cex = label.cex)
          }
          else{
          year.u <- c(sample_yr:(sample_yr - length(by) + 1))
          text(bx, by, year.u, adj = c(1.5, 0.5), 
               srt = 90, col = lab.color, cex = label.cex)
          }
          
          border.num <- 1:lenbx
          text(bx, by, border.num, adj = c(0.5, -1.25), 
               col = lab.color, cex = label.cex)
        }
      }
    }
  }
  
  f.rw <- function(outfile, sample_yr, incline, dpi, h.dis) {
    df.loc <- outfile
    bx <- df.loc$x
    by <- df.loc$y
    bz <- df.loc$z
    by <- by[order(bx)]
    bz <- bz[order(bx)]
    bx <- sort(bx)
    dp <- dpi/25.4
    if (!incline) {
      lenbx <- length(bx)
      dx <- diff(bx)
      dy <- diff(by)
      d <- sqrt(dx^2 + dy^2)
      rw <- c(NA, round(d / dp, 2))
      if(input$decades){
        years <- c(seq(sample_yr,(sample_yr - (lenbx)*10 + 1),by=-10))
      }
      else{
        years <- c(sample_yr:(sample_yr - lenbx + 1))
      }
      df.rw <- data.frame(year = years, x = bx, y = by, ring.width = rw)
    } else { 
      up <- which(bz == 'u')
      lenup <- length(up)
      bx.up <- bx[up]
      diff.col.num.up <- diff(bx.up)
      rw.up <- round(diff.col.num.up/dp, 2)
      
      lower <- which(bz == 'l')
      lenlo <- length(lower)
      bx.lower <- bx[lower]
      diff.col.num.lower <- diff(bx.lower)
      rw.lower <- round(diff.col.num.lower/dp, 2)
      if(input$decades){
        years <- c(seq(sample_yr,(sample_yr - (lengup)*10 + 1),by=-10))
      }
      else{
        years <- c(sample_yr:(sample_yr - lenup + 1))
      }
      mean.bor <- (diff.col.num.lower + diff.col.num.up)/2
      x.cor <- abs(bx.lower - bx.up)
      x.cor <- x.cor[-length(x.cor)]
      correct.rw <- mean.bor * cos(atan(x.cor/(dp * h.dis)))
      correct.rw <- c(NA, correct.rw)
      correct.rw <- round(correct.rw / dp, 2)
      df.rw <- data.frame(year = years, 
                          original = round(c(NA, mean.bor)/dp, 2), 
                          corrected = correct.rw)
    }
    return(df.rw)
  }
  
  calc.se <- function(se, dpi, order) {
    if (is.null(se)) {
      if(order == 1)
        se1 <- dpi/400
      if(order == 2)
        se1 <- dpi/80
      se <- c(se1, se1) %>% round
    }
    return(se)
  }
  
  # 0804
  # Here img contains the img cropped
  automatic.det <- function(
    img, incline, method, h.dis, dpi, RGB, px, py, path.hor, path.df,
    watershed.threshold, watershed.adjust, struc.ele1, struc.ele2,
    default.canny, canny.t1, canny.t2, canny.adjust, canny.smoothing, origin
  )
  {   
    dp <- dpi/25.4
    dimt <- image_info(img) %>% '['(1, 2:3) %>% as.numeric
    dimcol <- dimt[1]
    dimrow <- dimt[2]
    struc.ele1 <- calc.se(struc.ele1, dpi, 1)
    struc.ele2 <- calc.se(struc.ele2, dpi, 2)
    
    # X direction
    pxmin <- min(px) - round(1.5 * struc.ele2[1])
    if (pxmin <= 0)
      pxmin <- 0
    pxmax <- max(px) + round(1.5 * struc.ele2[1])
    if (pxmax >= dimcol)
      pxmax <- dimcol
    # Y direction
    pymin <- min(py) - 2 * struc.ele2[1]
    if (incline & path.hor)
      pymin <- pymin - round(h.dis * dp / 2)
    if (pymin <= 0)
      pymin <- 0
    pymax <- max(py) + 2 * struc.ele2[1]
    if (incline & path.hor)
      pymax <- pymax + round(h.dis * dp / 2)
    if (pymax >= dimrow)
      pymax <- dimrow
    # crop an image
    img.range <- paste0(as.character(pxmax - pxmin), 'x', 
                        as.character(pymax - pymin), '+',
                        as.character(pxmin), '+', 
                        as.character(dimrow - pymax))
    img <- image_crop(img, img.range)
    rd.martix <- img[[1]]
    hex2dec <- function(rd.martix) apply(rd.martix, 1, as.numeric)
    rd.channel <- dim(rd.martix)[1]
    if (rd.channel == 1) {
      rd.m.array <- hex2dec(rd.martix[1, , ])
    } else {
      rd.m.array <- array(0, dim = rev(dim(rd.martix)))
      for (i in 1:rd.channel) {
        rd.m.array[, , i] <- hex2dec(rd.martix[i, , ])
      }
    }
    rd.m.array <- rd.m.array/255
    if (rd.channel == 1)
      seg.data <- rd.m.array[, ]
    if (rd.channel == 2)
      seg.data <- rd.m.array[, , 1]
    if (rd.channel >= 3)
      seg.data <- apply(rd.m.array[, , 1:3], 1, function(x) x %*% RGB) %>% t
    tdata <- seg.data
    if (method == 'watershed') {
      seg.mor <- f.morphological(seg.data, struc.ele1, struc.ele2, dpi)
      black.hat <- hat(seg.mor, dpi, watershed.threshold, watershed.adjust)
      marker.img <- water.im(black.hat, T)
      seg.data <- watershed.im(marker.img, seg.mor)
      s2 <- seg.data[, -1]
      s2 <- cbind(s2, matrix(max(s2), ncol = 1, nrow = nrow(s2)))
      seg.data <- as.cimg(s2 - seg.data)
      
    }  
    if (method == 'canny') {
      seg.mor <- f.morphological(seg.data, struc.ele1, struc.ele2, dpi)
      if (default.canny) {
        seg.data <- cannyEdges(as.cimg(seg.mor), alpha = canny.adjust, 
          sigma = canny.smoothing)
      } else {
        seg.data <- cannyEdges(as.cimg(seg.mor), t1=canny.t1, t2=canny.t2,
          alpha = canny.adjust, sigma = canny.smoothing)
      }
      # seg.data <- canny.seg[, , 1, 1]
    } 
    
    # intersection operations
    if (method != 'lineardetect') {
      bor_xy <- where(seg.data == TRUE)
      bor_xy <- bor_xy[, c(2, 1)]
      colnames(bor_xy) <- c('x', 'y')
      bor_xy$x <- bor_xy$x + pxmin - 1
      bor_xy$y <- nrow(seg.data) - bor_xy$y + pymin
      if (path.hor & incline) {
        df.upper <- path.df
        df.lower <- path.df
        df.upper$y <- df.upper$y + round(h.dis * dp / 2)
        df.lower$y <- df.lower$y - round(h.dis * dp / 2)
        bor_xy_u <- intersect(bor_xy, df.upper)
        bor_xy_u <- f.sort(bor_xy_u, dp)
        bor_xy_u$z <- 'u'
        bor_xy_l <- intersect(bor_xy, df.lower)
        bor_xy_l <- f.sort(bor_xy_l, dp)
        bor_xy_l$z <- 'l'
        bor_xy <- rbind(bor_xy_u, bor_xy_l)
      } else {
        bor_xy <- intersect(bor_xy, path.df)
        bor_xy <- f.sort(bor_xy, dp)
        bor_xy$z <- 'u'
      }
      # filter falsely identified borders
      filter_edge <- function(bor_xy, tdata, pxmin, pymin, dp) {
        bor_row <- nrow(tdata) - bor_xy$y + pymin
        bor_col <- bor_xy$x - pxmin
        num_dp <- dp * 0.2
        num_dp <- ifelse(num_dp %% 2 ==0, num_dp + 1, num_dp)
        mat <- matrix(c(bor_row, bor_col - (num_dp - 1) / 2), ncol = 2)
        pixel_mat <- matrix(nrow = length(bor_row), ncol = 0)
        
        # calculate slope
        for(i in 1:num_dp) {
          pixel_mat <- cbind(pixel_mat, tdata[mat])
          mat[,2] <- mat[,2] + 1
        }
        calc_slope <- function(x){
          lm(x ~ c(1:num_dp)) %>% coef %>% as.numeric
        }
        slope <- apply(pixel_mat, 1, calc_slope)
        bor_xy <- bor_xy[slope[2,] < 0,]
      }
      
      bor_xy <- filter_edge(bor_xy, tdata, pxmin, pymin, dp)
    }
    if (method == 'lineardetect') {
      attributes(seg.data)['image'] <- 'img'
      smoothed <- graySmoothed(seg.data, ppi = dpi, rgb = RGB)
      borders <- linearDetect(smoothed, origin = origin)
      borders <- borders + pxmin
      bor_xy <- data.frame(x = borders, y = py[1], z = 'u')
      first_column <- bor_xy[,1]
      second_column <- bor_xy[,2]
      bor_row <- nrow(tdata) - bor_xy$y + pymin
      bor_col <- bor_xy$x - pxmin
      pix <- 255*tdata[bor_row,bor_col][1,]
    }
    # Calculates the density profile
    if(is.null(calibration$data)){}
      else{
        bor_row <- nrow(tdata) - bor_xy$y + pymin
        bor_row_plus <- bor_row + 5
        bor_row_less <- bor_row - 5
        bor_col <- bor_xy$x - pxmin
        path_pixes<- 255*tdata[bor_row,][1,]
        bor_pix <- 255*tdata[bor_row,bor_col][1,]
        path_matrix <- matrix(data=255*tdata[bor_row,][1,],nrow=1,ncol=length(255*tdata[bor_row,][1,]))
        if (nrow(tdata) < bor_row[1] + input$pixelspath){
          top <- nrow(tdata) - bor_row[1] - 1
        }
        else{
          top <- input$pixelspath
        }
        if (0 > bor_row[1] - input$pixelspath){
          bot <- bor_row[1] - 1
        }
        else{
          bot <- input$pixelspath
        }
        for (i in 1:min(bot,top)-1){
          path_matrix <- rbind(path_matrix,255*tdata[bor_row+i,][1,])
          path_matrix <- rbind(path_matrix,255*tdata[bor_row-i,][1,])
        }
        pathposition$data <- bor_row
        calibration_profile$data <-predict(calibration$data, (colMeans(path_matrix)))
        calibration_profile$data[is.na(calibration_profile$data)] <- 0
        calibration_profile$data <- append(calibration_profile$data,integer(pxmin),0)
        calibration_profile$data <- append(calibration_profile$data,integer(dimcol-pxmax))
      }
    return(bor_xy)
  } 
  readImg <- function(img, img.name, magick.switch = TRUE) {
    img.size <- file.size(img)/1024^2
    options(warn = -1)
    if(img.size <= 10 | !magick.switch){ 
      if (grepl("\\.tif", img))
        tree.data <- readTIFF(img, native = FALSE, info = TRUE)
      if (grepl("\\.png", img))
        tree.data <- readPNG(img, native = FALSE, info = TRUE)
      if (grepl("\\.jpg", img) | grepl("\\.jpeg", img))
        tree.data <- readJPEG(img, native = FALSE)
      if (grepl("\\.bmp", img)) {
        tree.data <- read.bmp(img)
        tree.data <- tree.data/255
      }
      td.dim <- dim(tree.data)
      if (!is.matrix(tree.data)) {
        if(any(td.dim[3]== c(2,4)))
          tree.data <- tree.data[, , -td.dim[3]]
      }
      if(is.matrix(tree.data)){
        tdata <- as.raster(tree.data) %>%
          image_read %>%
          image_convert(colorspace = 'gray')
      } else {
        tdata <- image_read(tree.data)
      }
      rm(tree.data)
      gc()
    } else {
      tdata <- image_read(img)
    }
    options(warn = 0)
    dim.tdata <- image_info(tdata) %>% '['(1, 2:3) %>% as.numeric
    attributes(tdata) <- c(attributes(tdata), 
      list(img.name = img.name, dimt = dim.tdata))
    return(tdata)
  }
  imgInput <- function(tdata, tdata.copy, plot1_rangesx, plot1_rangesy) {
    img.name <- attributes(tdata)$img.name
    dim.tdata <- image_info(tdata.copy) %>% '['(1,2:3) %>% as.numeric
    xleft <- 0
    ybottom <- 0
    xright <- dim.tdata[1]
    ytop <- dim.tdata[2]
    par(mar = c(2.5, 2, 2, 0))
    # 0729
    if(input$wh_ratio){
      plot(tdata.copy, xlim = c(xleft, xright), ylim = c(ybottom, ytop),
           main = img.name, xlab = "", ylab = "", cex.main = 1.2)
    } else {
      plot(x = c(xleft, xright), y = c(ybottom, ytop),
           xlim = c(xleft, xright), ylim = c(ybottom, ytop),
           main = img.name, xlab = "", ylab = "",
           type = "n", axes = F, cex.main = 1.2)
      rasterImage(as.raster(tdata.copy), xleft, ybottom,
                  xright, ytop, interpolate = FALSE)
    }
    axis(1, col = "grey", cex.axis = 1)
    axis(2, col = "grey", cex.axis = 1)
    if (!is.null(plot1_rangesx)) {
      xmin <- plot1_rangesx[1]
      xmax <- plot1_rangesx[2]
      ymin <- plot1_rangesy[1]
      ymax <- plot1_rangesy[2]
      dimt <- image_info(tdata) %>% '['(1, 2:3) %>% as.numeric
      if (dimt[1] * dimt[2] >= 1.2e+07) {
        xmin <- xmin/4
        xmax <- xmax/4
        ymin <- ymin/4
        ymax <- ymax/4
      }
      x <- c(xmin, xmax, xmax, xmin, xmin)
      y <- c(ymin, ymin, ymax, ymax, ymin)
      points(x, y, type = 'l', lty = 2, lwd = 1.5)
    }
  }
  imgInput_crop <- function(tdata, ver, hor) {
    # crop an image based on slider info
    dim.tdata <- image_info(tdata) %>% '['(1, 2:3) %>% as.numeric
    dimcol <- dim.tdata[1]
    dimrow <- dim.tdata[2]
    crop.x <- round(diff(hor)*dimcol/100)
    crop.y <- round(diff(ver)*dimrow/1000)
    ini.x <- round(hor[1]*dimcol/100)
    ini.y <- round(ver[1]*dimrow/1000)
    img.range <- paste0(as.character(crop.x), 'x', 
                        as.character(crop.y), '+',
                        as.character(ini.x), '+', 
                        as.character(ini.y))
    tdata <- image_crop(tdata, img.range)
    # new dimension
    dim.tdata <- image_info(tdata) %>% '['(1, 2:3) %>% as.numeric
    xleft <- 0
    ybottom <- 0
    xright <- dim.tdata[1]
    ytop <- dim.tdata[2]
    par(mar = c(0, 0, 0, 0), mai = c(0, 0, 0, 0))
    # 0730
    if(input$wh_ratio2) {
      par(mar = c(0, 0, 0, 0), xaxs='i', yaxs='i')
      plot(tdata, xlim = c(xleft, xright), ylim = c(ybottom, ytop),
           main = '', xlab = "", ylab = "", cex.main = 1.2)
    } else {
      par(mar = c(0, 0, 0, 0), xaxs='i', yaxs='i')
      plot(x = c(xleft, xright), y = c(ybottom, ytop),
           xlim = c(xleft, xright), ylim = c(ybottom, ytop),
           main = '', xlab = "", ylab = "",
           type = "n", axes = F, cex.main = 1.2)
      rasterImage(as.raster(tdata), xleft, ybottom,
                  xright, ytop, interpolate = FALSE)
    }
    return(tdata)  
  }
  rotateImg <- function(tdata, degree) {
    tdata <- image_rotate(tdata, degree)
    dim.tdata <- image_info(tdata) %>% '['(1, 2:3) %>% as.numeric
    attributes(tdata) <- c(attributes(tdata), list(dimt = dim.tdata))
    return(tdata)
  }
  # Functions listed above are used for shiny app

  options(shiny.maxRequestSize = 150*(1024^2))
  pathposition <- reactiveValues(data = NULL)
  calibration <- reactiveValues(data = NULL)
  calibration_profile <- reactiveValues(data = NULL)
  el_wood <- reactiveValues(x = NULL, y = NULL)
  img.file <- reactiveValues(data = NULL)
  img.file.crop <- reactiveValues(data = NULL)
  img.file.copy <- reactiveValues(data = NULL)
  
  # It modifies the entries of the matrix based on the number of steps the user wants
  output$matrixcontrol <- renderDataTable({
    if(!input$loadMatrix){
    updateMatrixInput(session=session, "thickness_matrix", matrix(0, input$nsteps, 2, dimnames = list(NULL,c("Thickness","Intensity"))))}})
  
  observeEvent(input$inmethod, {
    img.file$data <- NULL
    img.file.copy$data <- NULL
    img.file.crop$data <- NULL
    df.loc$data <- NULL
    df.loc$ID <- NULL
    plot1_ranges$x <- NULL
    plot1_ranges$y <- NULL
    plot2_ranges$x <- NULL
    plot2_ranges$y <- NULL
    path.info$x <- NULL
    path.info$y <- NULL
    path.info$type <- NULL
    path.info$ID <- NULL
    path.info$horizontal <- NULL
    path.info$incline <- NULL
    path.info$h <- NULL
    path.info$dpi <- NULL
    path.info$max <- NULL
    path.info$df <- NULL
    rw.dataframe$data <- NULL
    
    updatePrettyRadioButtons(
      session = session, inputId = "cropcondition",
      choiceNames = 'UNCROPPED', choiceValues = list('a'),
      prettyOptions = list(shape = "curve", status = "danger",
        fill = F, inline = F)
    )
    updateActionButton(session, "buttoncrop", label = "Crop")
    updatePrettyRadioButtons(
      session = session, inputId = "rotatede",
      label = "Clockwise Rotation",
      choices = c("0 degrees" = "rotate0",
        "90 degrees" = "rotate90",
        "180 degrees" = "rotate180",
        "270 degrees" = "rotate270"),
      prettyOptions = list(shape = "curve", status = "success",
        fill = F, inline = F)
    )
  })
  observeEvent(input$savematrix, {
    write.table(input$thickness_matrix, file = paste(input$filenameMatrix,".txt"), col.names = FALSE,quote=FALSE,row.names =FALSE)
  })
  observeEvent(input$buttonrotate, {
    if (!input$inmethod)
      img <- input$selectfile["datapath"] %>% as.character
    if (input$inmethod)
      img <- input$enter.path
    img.check1 <- ifelse(length(img) >= 1, TRUE, FALSE)
    img.check2 <- FALSE
    if (img.check1)
      img.check2 <- ifelse(nchar(img) > 1, TRUE, FALSE)
    if (any(!img.check1, !img.check2, is.null(img.file$data))) {
      et <- paste('The preview image has not been generated')
      sendSweetAlert(
        session = session, title = "Error", text = et, type = "error"
      )
      return()
    }
    degree <- input$rotatede %>% substring(7) %>% as.numeric
    img.file$data <- rotateImg(img.file$data, degree)
    img.file.crop$data <- img.file$data
    img.file.copy$data <- rotateImg(img.file.copy$data, degree)
    # new.dimt <- attributes(img.file$data)[["dimt"]]
    # attributes(img.file.copy$data)[["dimt"]] <- new.dimt
    plot1_ranges$x <- NULL
    plot1_ranges$y <- NULL
    plot2_ranges$x <- NULL
    plot2_ranges$y <- NULL
    df.loc$data <- NULL
    df.loc$ID <- NULL
    # path
    path.info$x <- NULL
    path.info$y <- NULL
    path.info$type <- NULL
    path.info$ID <- NULL
    path.info$horizontal <- NULL
    path.info$incline <- NULL
    path.info$h <- NULL
    path.info$dpi <- NULL
    path.info$max <- NULL
    path.info$df <- NULL
    rw.dataframe$data <- NULL
    updateTextInput(session, "m_line", value = '',
      label = 'Y-coordinate of the path')
    updatePrettyRadioButtons(
      session = session, inputId = "cropcondition",
      choiceNames = 'UNCROPPED', choiceValues = list('a'),
      prettyOptions = list(shape = "curve", status = "danger",
        fill = F, inline = F)
    )
    updateActionButton(session, "buttoncrop", label = "Crop")
  })
  
  observeEvent(input$magick.switch, {
    if(input$magick.switch){
      updatePrettySwitch(session, inputId = 'magick.switch', 
        label = 'Magick ON', value = TRUE)
    } else {
      updatePrettySwitch(session, inputId = 'magick.switch', 
        label = 'Magick OFF', value = FALSE)
    }
  })
  # This event reacts to activating button plot from light calibration, when activated it saves 
  # the plot in variable lightplot
  lightplot <- eventReactive(input$buttondensity,{
    #  It first determines if the img is loaded
    if (!input$inmethod) {
      imgf <- input$selectfile
      if (is.null(imgf)) {
        et <- paste('The image file has not been uploaded')
        sendSweetAlert(
          session = session, title = "Error", text = et, type = "error"
        )
        return()
      }
      img <- as.character(imgf["datapath"])
      img.name <- as.character(imgf["name"])
    }
    if (input$inmethod) {
      img <- input$enter.path
      if (img == '') {
        et <- paste('The file path has not been entered')
        sendSweetAlert(
          session = session, title = "Error", text = et, type = "error"
        )
        return()
      }
      img.name <- basename(img)
    }
    # If its loaded it reads it with imRead(black and white)
    im <- imRead(img)
    if(!input$loadMatrix){
      if (max(input$thickness_matrix[,2]) != 255 && max(input$thickness_matrix[,2]) != 65536){
        showNotification(paste("WARNING: max intensity inserted is lower than possible max"), duration = 5)
      }
      if (min(input$thickness_matrix[,2]) != 0){
        showNotification(paste("WARNING: min intensity inserted is higher than 0"), duration = 5)
      }
      # Runs the calibration with the input data from thickness_matrix and density
      calibration$data <- fitCalibrationModel(input$thickness_matrix[,2], input$thickness_matrix[,1], density = input$density, plot = TRUE)}
    if(input$loadMatrix){
      density_matrix = read.table(input$path_matrix["datapath"] %>% as.character)
      if (max(density_matrix[,2]) != 255 && max(density_matrix[,2]) != 65536){
        showNotification(paste("WARNING: max intensity inserted is lower than possible max"), duration = 5)
      }
      if (min(density_matrix[,2]) != 0){
        showNotification(paste("WARNING: min intensity inserted is higher than 0"), duration = 5)
      }
      calibration$data <- fitCalibrationModel(density_matrix[,2], density_matrix[,1], density = input$density, plot = TRUE)
    }
  })
  
  output$light <- renderPlot({
    lightplot()
  })
  
  observeEvent(input$buttoninputimage, {
    magick.switch <- input$magick.switch
    if (!input$inmethod) {
      imgf <- input$selectfile
      if (is.null(imgf)) {
        et <- paste('The image file has not been uploaded')
        sendSweetAlert(
          session = session, title = "Error", text = et, type = "error"
        )
        return()
      }
      img <- as.character(imgf["datapath"])
      img.name <- as.character(imgf["name"])
    }
    if (input$inmethod) {
      img <- input$enter.path
      if (img == '') {
        et <- paste('The file path has not been entered')
        sendSweetAlert(
          session = session, title = "Error", text = et, type = "error"
        )
        return()
      }
      img.name <- basename(img)
    }
    updatePrettyRadioButtons(
      session = session, inputId = "rotatede",
      label = "Clockwise Rotation",
      choices = c("0 degrees" = "rotate0",
        "90 degrees" = "rotate90",
        "180 degrees" = "rotate180",
        "270 degrees" = "rotate270"),
      prettyOptions = list(shape = "curve", status = "success",
        fill = F, inline = F)
    )
    plot1_ranges$x <- NULL
    plot1_ranges$y <- NULL
    plot2_ranges$x <- NULL
    plot2_ranges$y <- NULL
    df.loc$data <- NULL
    df.loc$ID <- NULL
    # path
    path.info$x <- NULL
    path.info$y <- NULL
    path.info$type <- NULL
    path.info$ID <- NULL
    path.info$horizontal <- NULL
    path.info$incline <- NULL
    path.info$h <- NULL
    path.info$dpi <- NULL
    path.info$max <- NULL
    path.info$df <- NULL
    rw.dataframe$data <- NULL
    #cur.time <- as.character(Sys.time())
    updateTextInput(session, "tuid", value = '',
      label = 'Series ID')
    updateTextInput(session, "sample_yr", value = '',
      label = 'Sampling year')
    updateTextInput(session, "dpi", value = '',
      label = 'DPI of the image')
    updatePrettyRadioButtons(
      session = session, inputId = "cropcondition",
      choiceNames = 'UNCROPPED', choiceValues = list('a'),
      prettyOptions = list(shape = "curve", status = "danger",
        fill = F, inline = F)
    )
    updateActionButton(session, "buttoncrop", label = "Crop")
    img.file$data <- readImg(img, img.name, magick.switch)
    img.file.crop$data <- img.file$data
    #img.file.crop.copy$data <- as.raster(img.file$data)
    dim.tdata <- attributes(img.file$data)[["dimt"]]
    dimcol <- dim.tdata[1]
    dimrow <- dim.tdata[2]
    if ((dimcol*dimrow) >= 1.2e+07) {
      resize.ratio <- 0.25
      resize.str <- paste0(round(dimcol*resize.ratio), 'x', 
                           round(dimrow*resize.ratio))
      img.file.copy$data <- image_resize(img.file$data, resize.str)
    } else {
      img.file.copy$data <- img.file$data
    }
  })
  
  plot1_ranges <- reactiveValues(x = NULL, y = NULL)
  
  observeEvent(input$buttoncrop, {
    if(is.null(img.file$data)){
      et <- paste('The preview image have not been generated')
      sendSweetAlert(
        session = session, title = "Error", text = et, type = "error"
      )
      return()
    }
    plot1_brush <- input$plot1_brush
    plot1_ranges$x <- NULL
    plot1_ranges$y <- NULL
    plot2_ranges$x <- NULL
    plot2_ranges$y <- NULL
    df.loc$data <- NULL
    df.loc$ID <- NULL
    # path
    path.info$x <- NULL
    path.info$y <- NULL
    path.info$type <- NULL
    path.info$ID <- NULL
    path.info$horizontal <- NULL
    path.info$incline <- NULL
    path.info$h <- NULL
    path.info$dpi <- NULL
    path.info$max <- NULL
    path.info$df <- NULL
    rw.dataframe$data <- NULL
    updateTextInput(session, "tuid", value = '', label = 'Series ID')
    if (!is.null(plot1_brush)) {
      plot1_ranges$x <- c(round(plot1_brush$xmin), round(plot1_brush$xmax))
      plot1_ranges$y <- c(round(plot1_brush$ymin), round(plot1_brush$ymax))
      #0730
      dimt <- attributes(img.file$data)[["dimt"]]
      dimcol <- dimt[1]
      dimrow <- dimt[2]
      if (dimcol * dimrow >= 1.2e+07) {
        plot1_ranges$x <- plot1_ranges$x * 4
        plot1_ranges$y <- plot1_ranges$y * 4
      }

      if (plot1_ranges$x[1] <= 0) plot1_ranges$x[1] <- 0
      if (plot1_ranges$y[1] <= 0) plot1_ranges$y[1] <- 0
      if (plot1_ranges$x[2] >= dimcol) plot1_ranges$x[2] <- dimcol
      if (plot1_ranges$y[2] >= dimrow) plot1_ranges$y[2] <- dimrow
      xmin <- plot1_ranges$x[1]
      ymin <- plot1_ranges$y[1]
      xmax <- plot1_ranges$x[2]
      ymax <- plot1_ranges$y[2]
      img.range <- paste0(as.character(xmax-xmin), 'x', 
                          as.character(ymax-ymin), '+',
                          as.character(xmin), '+',
                          as.character(dimrow-ymax))
      img.file.crop$data <- image_crop(img.file$data, img.range)
      updateActionButton(session, "buttoncrop", label = "Cancel")
      updatePrettyRadioButtons(
        session = session, inputId = "cropcondition",
        choiceNames = 'CROPPED', choiceValues = list('a'),
        prettyOptions = list(shape = "curve", status = "success",
          fill = F, inline = F)
      )
    } else {
      img.file.crop$data <- img.file$data
      updateActionButton(session, "buttoncrop", label = "Crop")
      updatePrettyRadioButtons(
        session = session, inputId = "cropcondition",
        choiceNames = 'UNCROPPED', choiceValues = list('a'),
        prettyOptions = list(shape = "curve", status = "danger",
          fill = F, inline = F)
      ) 
    }
  })
  
  output$pre.img <- renderPlot({
    if (is.null(img.file$data)) return()
    imgInput(img.file$data, img.file.copy$data, plot1_ranges$x, plot1_ranges$y)
  })

  plot2_ranges <- reactiveValues(x = NULL, y = NULL)
  df.loc <- reactiveValues(data = NULL, ID = NULL)
  
  # update path options
  observeEvent(input$sel_sin_mul, {
    if(input$sel_sin_mul == "Single Segment") {
      updateNumericInput(session = session, inputId = 'num_seg', 
                         value = 1, min = 1, max = 1, step = 1)
      updatePrettyCheckbox(
        session = session, inputId = "hor_path", value = TRUE)
      updatePrettyCheckbox(
        session = session, inputId = "incline", value = FALSE)
    } else {
      updateNumericInput(session = session, inputId = 'num_seg', 
                         value = 2, min = 1, max = 10, step = 1)
      updatePrettyCheckbox(
        session = session, inputId = "hor_path", value = FALSE)
      updatePrettyCheckbox(
        session = session, inputId = "incline", value = FALSE)
    }
  })

  # 0803 delete a segment
  observeEvent(input$rm_last, {
    if(is.null(path.info$x)) {
      et <- 'The path to be removed does not exist.'
      sendSweetAlert(
        session = session, title = "Error", text = et, type = "error"
      )
      return()
    }
    
    if(length(path.info$x) == 1) {
      path.info$x <- NULL
      path.info$y <- NULL
      path.info$type <- NULL
      path.info$ID <- NULL
      path.info$horizontal <- NULL
      path.info$incline <- NULL
      path.info$h <- NULL
      path.info$dpi <- NULL
      path.info$max <- NULL
      et <- 'The path has been removed. You need to recreate a path.'
      sendSweetAlert(
        session = session, "Success", et, "success"
      )
      return()
    }
    
    if(length(path.info$x) >= 2) {
      path.info$x <- path.info$x[-length(path.info$x)]
      path.info$y <- path.info$y[-length(path.info$y)]
      et <- 'The last endpoint added has been removed.'
      sendSweetAlert(
        session = session, "Success", et, "success"
      )
      return()
    }
  })
  # 0803 delete all segments
  observeEvent(input$rm_all, {
    if(is.null(path.info$x)) {
      et <- 'The path to be removed does not exist.'
      sendSweetAlert(
        session = session, title = "Error", text = et, type = "error"
      )
      return()
    }
    if(length(path.info$x) >= 1) {
      el_wood$x <- NULL
      el_wood$y <- NULL
      calibration_profile$data <- NULL
      path.info$x <- NULL
      path.info$y <- NULL
      path.info$type <- NULL
      path.info$ID <- NULL
      path.info$horizontal <- NULL
      path.info$incline <- NULL
      path.info$h <- NULL
      path.info$dpi <- NULL
      path.info$max <- NULL
      df.loc$data <- NULL
      df.loc$ID <- NULL
      plot2_ranges$x <- NULL
      plot2_ranges$y <- NULL
      et <- 'The path has been removed. You need to recreate a path.'
      sendSweetAlert(
        session = session, "Success", et, "success"
      )
      return()
    }
  })

  # del border points
  observeEvent(input$rm_all_border, {
    if(input$edit_wood){
      if(is.null(el_wood$x)) {
        et <- 'Early/Late borders were not found'
        sendSweetAlert(
          session = session, title = "Error", text = et, type = "error"
        )
        return()
      }
      el_wood$x <- NULL
      el_wood$y <- NULL
      et <- 'All early-wood borders have been removed'
      sendSweetAlert(
        session = session, "Success", et, "success"
      )
      return()
    }
    else{
      if(is.null(df.loc$data)) {
        et <- 'Ring borders were not found'
        sendSweetAlert(
          session = session, title = "Error", text = et, type = "error"
        )
        return()
      }
      
      df.loc$data <- NULL
      et <- 'All ring borders have been removed'
      sendSweetAlert(
        session = session, "Success", et, "success"
      )
      return()
    }
  })
  
  # record slider info
  crop.offset.xy <- reactiveValues(x = NULL, y = NULL)
  observeEvent(input$img_ver, {
    if (is.null(img.file.crop$data))
      return()
    dimt <- image_info(img.file.crop$data) %>% '['(1, 2:3) %>% as.numeric
    dimrow <- dimt[2]
    crop.offset.xy$y <- dimrow - round(input$img_ver[2]*dimrow/1000)
  })
  observeEvent(input$img_hor, {
    if (is.null(img.file.crop$data))
      return()
    dimt <- image_info(img.file.crop$data) %>% '['(1, 2:3) %>% as.numeric
    dimcol <- dimt[1]
    crop.offset.xy$x <- input$img_hor[1]*dimcol/100 %>% round
  })
  observeEvent(img.file.crop$data, {
    if (is.null(img.file.crop$data))
      return()
    dimt <- image_info(img.file.crop$data) %>% '['(1, 2:3) %>% as.numeric
    dimcol <- dimt[1]
    dimrow <- dimt[2]
    crop.offset.xy$x <- input$img_hor[1]*dimcol/100 %>% round
    crop.offset.xy$y <- dimrow - round(input$img_ver[2]*dimrow/1000)
  })
  
  # record mouse position to generate a preview path
  hover.xy <- reactiveValues(x = NULL, y = NULL)
  observeEvent(input$plot2_hover, {
    hover.xy$x <- input$plot2_hover$x
    hover.xy$y <- input$plot2_hover$y
  })
  
  # turn off ring width correction when switching to another mode
  observeEvent(input$hor_path, {
    updatePrettyCheckbox(
      session = session, inputId = "incline", 
      value = FALSE)
  })
  observeEvent(input$sel_sin_mul, {
    updatePrettyCheckbox(
      session = session, inputId = "incline", 
      value = FALSE)
  })
  
  ## create path with mouse clicks
  path.info <- reactiveValues(x = NULL, y = NULL, type = NULL, ID = NULL,
                              horizontal = NULL, incline = NULL, h = NULL, 
                              dpi = NULL, max = NULL, df = NULL)
  observeEvent(input$plot2_dblclick, 
  {
    if(input$sel_mode != "sel_path")
      return()
    if (is.null(img.file.crop$data)) {
      et <- 'Path creation fails because the image has not been plotted'
      sendSweetAlert(
        session = session, title = "Error", text = et, type = "error"
      )
      return()
    }
    dpi <- as.numeric(input$dpi)
    if (is.na(dpi)) {
      et <- 'Please enter the DPI of the image'
      sendSweetAlert(
        session = session, title = "Error", text = et, type = "error"
      )
      return()
    }
    seriesID <- input$tuid
    if (seriesID == '') {
      et <- 'Please enter a series ID'
      sendSweetAlert(
        session = session, title = "Error", text = et, type = "error"
      )
      return()
    }
    dimt <- image_info(img.file.crop$data) %>% '['(1, 2:3) %>% as.numeric
    dimrow <- dimt[2]
    dimcol <- dimt[1]
    if (!is.null(path.info$max)) {
      if(length(path.info$x) >= path.info$max) {
        et <- paste('You have already created a path')
        sendSweetAlert(
          session = session, title = "Error", text = et, type = "error"
        )
        return()
      }
    }
    if(length(path.info$x) >= 1) {
      cur.p.x <- round(input$plot2_dblclick$x + crop.offset.xy$x)
      last.point <- path.info$x[length(path.info$x)]
      if(last.point >= cur.p.x) {
        et <- paste('The x-position of the current point must be greater',
                    'than the x-position of the previous point')
        sendSweetAlert(
          session = session, title = "Error", text = et, type = "error"
        )
        return()
      }
    }
    px <- round(input$plot2_dblclick$x + crop.offset.xy$x)
    if (px <= 0 | px >= dimcol) {
      et <- paste('The X-coordinate of the endpoint is out of',
                  'range. Please click on the image.')
      sendSweetAlert(
        session = session, title = "Error", text = et, type = "error"
      )
      return()
    }
    crop.h <- round(diff(input$img_ver)*dimrow/1000)
    if (input$plot2_dblclick$y >= crop.h | input$plot2_dblclick$y <= 0) {
      et <- paste('The Y-coordinate of the endpoint is out of',
                        'range. Please click on the image.')
      sendSweetAlert(
        session = session, title = "Error", text = et, type = "error"
      )
      return() 
    }
    py <- round(input$plot2_dblclick$y + crop.offset.xy$y)
    dp <- dpi/25.4
    h.dis <- as.numeric(input$h.dis)
    d <- h.dis*dp/2
    incline <- input$incline
    hor <- input$hor_path
    if (hor & incline) {
      if(py + d >= dimrow) {
        et <- paste('The Y-coordinate of the upper path is out of range.',
                    'Please decrease the distance between paths')
        sendSweetAlert(
          session = session, title = "Error", text = et, type = "error"
        )
        return()
      }
      if(py - d <= 0) {
        et <- paste('The Y-coordinate of the lower path is out of range.',
                    'Please decrease the distance between paths')
        sendSweetAlert(
          session = session, title = "Error", text = et, type = "error"
        )
        return()
      }
    }
    if(length(path.info$x) >= 1) {
      if (path.info$horizontal) {
        py <- path.info$y[1]
      }
    }
    path.info$x <- c(path.info$x, px)
    path.info$y <- c(path.info$y, py)
    if(length(path.info$x) == 1) {
      rt <- paste('The beginning point of the path have been created.')
      sendSweetAlert(
        session = session, title = "Success", text = rt, type = "success"
      )
      # record path info only the first time you click
      path.info$type <- input$sel_sin_mul
      path.info$ID <- seriesID
      path.info$horizontal <- input$hor_path
      path.info$incline <- input$incline
      path.info$h <- as.numeric(input$h.dis)
      path.info$dpi <- dpi
      path.info$max <- input$num_seg + 1
      df.loc$ID <- input$tuid
    }
    # record xy-coordinates of the path
    if(length(path.info$x) == path.info$max) {
      rt <- paste('The ending point of the path have been created.',
                  'Please switch to another working mode.')
      sendSweetAlert(
        session = session, title = "Success", text = rt, type = "success"
      )
      px <- path.info$x
      py <- path.info$y
      path.df <- as.data.frame(matrix(ncol = 2, nrow = 0))
      colnames(path.df) <- c('x', 'y')
      len <- length(path.info$x) - 1
      for (i in 1:len) {
        p1 <- px[i]
        p2 <- px[i+1]
        lm1 <- lm(py[c(i, i + 1)] ~ c(p1, p2))
        cf1 <- coef(lm1)
        x1 <- p1:p2
        y1 <- cf1[1] + cf1[2] * x1
        c1 <- data.frame(x = x1, y = y1)
        path.df <- rbind(path.df, c1)
      }
      path.df$y <- round(path.df$y)
      path.info$df <- path.df
    }
  })
    
  ## Detect ring borders
  observeEvent(input$button_run_auto, { 
    if (is.null(path.info$df)) {
      et <- 'A path has not been created.'
      sendSweetAlert(
        session = session, title = "Error", text = et, type = "error"
      )
      return()
    }
    isrgb <- input$isrgb
    if (isrgb) {
      RGB <- c(0.299, 0.587, 0.114)
    } else {
      RGB <- strsplit(input$customRGB, ',')[[1]] %>% as.numeric
    }
    dpi <- path.info$dpi
    dp <- dpi/25.4
    incline <- path.info$incline
    h.dis <- path.info$h
    ph <- path.info$horizontal
    path.df <- path.info$df
    px <- path.info$x
    py <- path.info$y
    defaultse <- input$defaultse
    if (defaultse) {
      struc.ele1 <- NULL
      struc.ele2 <- NULL
    } else {
      struc.ele1 <- c(input$struc.ele1, input$struc.ele1) %>% as.numeric
      struc.ele2 <- c(input$struc.ele2, input$struc.ele2) %>% as.numeric
    }  
    img <- img.file.crop$data
    method <- input$method
    if(input$watershed.threshold == 'custom.waterthr'){
      watershed.threshold <- input$watershed.threshold2
    } else {
      watershed.threshold <- input$watershed.threshold
    }
    watershed.adjust <- input$watershed.adjust
    progressSweetAlert(
      session = session, id = "detect_progress",
      title = "Detection in progress",
      display_pct = F, value = 0
    )
    if (method == 'watershed') {
      df.loc$data <- automatic.det(
        img, incline, method, h.dis, dpi, RGB, px, py, ph, path.df,
        watershed.threshold, watershed.adjust, struc.ele1, struc.ele2
      )
    }
    if (method == "canny") {
      default.canny <- input$defaultcanny
      canny.t1 <- as.numeric(input$canny.t1)
      canny.t2 <- as.numeric(input$canny.t2)
      canny.adjust <- input$canny.adjust
      canny.smoothing <- input$canny.smoothing
      df.loc$data <- automatic.det(
        img, incline, method, h.dis, dpi, RGB, px, py, ph, path.df,
        watershed.threshold, watershed.adjust, struc.ele1, struc.ele2,
        default.canny, canny.t1, canny.t2, canny.adjust, canny.smoothing
      )
    }   
    if (method == "lineardetect") {
      if (incline | path.info$type == "Multi Segments" | !ph) {
        rt <- paste('The linear detection supports only Single Segment',
                    'mode (without ring width correction). Please recreate',
                    'a horizontal single-segment path.')
        sendSweetAlert(
          session = session, title = "ERROR", text = rt, type = "warning"
        )
        return()
      }
      origin <- as.numeric(input$origin)
      f.df.loc <- automatic.det(
        img, incline, method, h.dis, dpi, RGB, px, py, ph, path.df, 
        struc.ele1 = struc.ele1, struc.ele2 = struc.ele2, origin = origin
      )
      df.loc$data <- f.df.loc
    }
    number.border <- nrow(df.loc$data)
    if (number.border == 0) {
      rt <- 'Ring border was NOT detected'
      closeSweetAlert(session = session)
      sendSweetAlert(
        session = session, title = "Error", text = rt, type = "error"
      )
    } else {
      rt <- paste(number.border, 'borders were detected')
      closeSweetAlert(session = session)
      sendSweetAlert(
        session = session, title = "Finished", text = rt, type = "success"
      )
    }  
  })
  # Detect Early/late borders
  observeEvent(input$button_run_auto_early, { 
    if (is.null(path.info$df)) {
      et <- 'A path has not been created.'
      sendSweetAlert(
        session = session, title = "Error", text = et, type = "error"
      )
      return()
    }
    isrgb <- input$isrgb
    if (isrgb) {
      RGB <- c(0.299, 0.587, 0.114)
    } else {
      RGB <- strsplit(input$customRGB, ',')[[1]] %>% as.numeric
    }
    dpi <- path.info$dpi
    dp <- dpi/25.4
    incline <- path.info$incline
    h.dis <- path.info$h
    ph <- path.info$horizontal
    path.df <- path.info$df
    px <- path.info$x
    py <- path.info$y
    defaultse <- input$defaultse
    if (defaultse) {
      struc.ele1 <- NULL
      struc.ele2 <- NULL
    } else {
      struc.ele1 <- c(input$struc.ele1, input$struc.ele1) %>% as.numeric
      struc.ele2 <- c(input$struc.ele2, input$struc.ele2) %>% as.numeric
    }  
    img <- img.file.crop$data
    method <- input$method
    if(input$watershed.threshold == 'custom.waterthr'){
      watershed.threshold <- input$watershed.threshold2
    } else {
      watershed.threshold <- input$watershed.threshold
    }
    watershed.adjust <- input$watershed.adjust
    progressSweetAlert(
      session = session, id = "detect_progress",
      title = "Detection in progress",
      display_pct = F, value = 0
    )
    if (method == 'watershed') {
      el_wood_prev <- automatic.det(
        img, incline, method, h.dis, dpi, RGB, px, py, ph, path.df,
        watershed.threshold, watershed.adjust, struc.ele1, struc.ele2
      )
      el_wood$x <- el_wood_prev$x
      el_wood$y <- el_wood_prev$y
    }
    if (method == "canny") {
      default.canny <- input$defaultcanny
      canny.t1 <- as.numeric(input$canny.t1)
      canny.t2 <- as.numeric(input$canny.t2)
      canny.adjust <- input$canny.adjust
      canny.smoothing <- input$canny.smoothing
      el_wood_prev <- automatic.det(
        img, incline, method, h.dis, dpi, RGB, px, py, ph, path.df,
        watershed.threshold, watershed.adjust, struc.ele1, struc.ele2,
        default.canny, canny.t1, canny.t2, canny.adjust, canny.smoothing
      )
      el_wood$x <- el_wood_prev$x
      el_wood$y <- el_wood_prev$y
    }   
    if (method == "lineardetect") {
      if (incline | path.info$type == "Multi Segments" | !ph) {
        rt <- paste('The linear detection supports only Single Segment',
                    'mode (without ring width correction). Please recreate',
                    'a horizontal single-segment path.')
        sendSweetAlert(
          session = session, title = "ERROR", text = rt, type = "warning"
        )
        return()
      }
      origin <- as.numeric(input$origin)
      f.df.loc <- automatic.det(
        img, incline, method, h.dis, dpi, RGB, px, py, ph, path.df, 
        struc.ele1 = struc.ele1, struc.ele2 = struc.ele2, origin = origin
      )
      el_wood_prev <- f.df.loc
      el_wood$x <- el_wood_prev$x
      el_wood$y <- el_wood_prev$y
    }
    number.border <- length(el_wood$x)
    if (number.border == 0) {
      rt <- 'Ring border was NOT detected'
      closeSweetAlert(session = session)
      sendSweetAlert(
        session = session, title = "Error", text = rt, type = "error"
      )
    } else {
      rt <- paste(number.border, 'borders were detected')
      closeSweetAlert(session = session)
      sendSweetAlert(
        session = session, title = "Finished", text = rt, type = "success"
      )
    }  
  })
  # Detect rings for x-ray images
  observeEvent(input$button_run_auto_xray, { 
    if (is.null(path.info$df)) {
      et <- 'A path has not been created.'
      sendSweetAlert(
        session = session, title = "Error", text = et, type = "error"
      )
      return()
    }
    isrgb <- input$isrgb
    if (isrgb) {
      RGB <- c(0.299, 0.587, 0.114)
    } else {
      RGB <- strsplit(input$customRGB, ',')[[1]] %>% as.numeric
    }
    dpi <- path.info$dpi
    dp <- dpi/25.4
    incline <- path.info$incline
    h.dis <- path.info$h
    ph <- path.info$horizontal
    path.df <- path.info$df
    px <- path.info$x
    py <- path.info$y
    defaultse <- input$defaultse
    if (defaultse) {
      struc.ele1 <- NULL
      struc.ele2 <- NULL
    } else {
      struc.ele1 <- c(input$struc.ele1, input$struc.ele1) %>% as.numeric
      struc.ele2 <- c(input$struc.ele2, input$struc.ele2) %>% as.numeric
    }  
    img <- img.file.crop$data
    imgn <- image_negate(img)
    method <- input$method
    if(input$watershed.threshold == 'custom.waterthr'){
      watershed.threshold <- input$watershed.threshold2
    } else {
      watershed.threshold <- input$watershed.threshold
    }
    watershed.adjust <- input$watershed.adjust
    progressSweetAlert(
      session = session, id = "detect_progress",
      title = "Detection in progress",
      display_pct = F, value = 0
    )
    if (method == 'watershed') {
      df.loc$data <- automatic.det(
        imgn, incline, method, h.dis, dpi, RGB, px, py, ph, path.df,
        watershed.threshold, watershed.adjust, struc.ele1, struc.ele2
      )
    }
    if (method == "canny") {
      default.canny <- input$defaultcanny
      canny.t1 <- as.numeric(input$canny.t1)
      canny.t2 <- as.numeric(input$canny.t2)
      canny.adjust <- input$canny.adjust
      canny.smoothing <- input$canny.smoothing
      df.loc$data <- automatic.det(
        imgn, incline, method, h.dis, dpi, RGB, px, py, ph, path.df,
        watershed.threshold, watershed.adjust, struc.ele1, struc.ele2,
        default.canny, canny.t1, canny.t2, canny.adjust, canny.smoothing
      )
    }   
    if (method == "lineardetect") {
      if (incline | path.info$type == "Multi Segments" | !ph) {
        rt <- paste('The linear detection supports only Single Segment',
                    'mode (without ring width correction). Please recreate',
                    'a horizontal single-segment path.')
        sendSweetAlert(
          session = session, title = "ERROR", text = rt, type = "warning"
        )
        return()
      }
      origin <- as.numeric(input$origin)
      f.df.loc <- automatic.det(
        imgn, incline, method, h.dis, dpi, RGB, px, py, ph, path.df, 
        struc.ele1 = struc.ele1, struc.ele2 = struc.ele2, origin = origin
      )
      df.loc$data <- f.df.loc
    }
    number.border <- nrow(df.loc$data)
    if (number.border == 0) {
      rt <- 'Ring border was NOT detected'
      closeSweetAlert(session = session)
      sendSweetAlert(
        session = session, title = "Error", text = rt, type = "error"
      )
    } else {
      rt <- paste(number.border, 'borders were detected')
      closeSweetAlert(session = session)
      sendSweetAlert(
        session = session, title = "Finished", text = rt, type = "success"
      )
    }  
  })
  ## Ring editing mode
  observeEvent(input$plot2_dblclick, {
    if(input$sel_mode == "sel_det"){
      et <- paste('If you want to add new ring borders by double-clicking,',
                  'please switch to the "Ring Editing" mode')
      sendSweetAlert(
        session = session, title = "Error", text = et, type = "error"
      )
      return()
    }
  })
  # add a point by double clicking
  observeEvent(input$plot2_dblclick, {
    if(input$sel_mode != "sel_edit")
      return()
    if (is.null(img.file.crop$data)) {
      et <- paste('Adding new ring borders fails',
                  'because the image has not been plotted')
      sendSweetAlert(
        session = session, title = "Error", text = et, type = "error"
      )
      return()
    }
    if (is.null(path.info$df)) {
      et <- paste('Adding new ring borders fails',
                  'because a path has not been created')
      sendSweetAlert(
        session = session, title = "Error", text = et, type = "error"
      )
      return()
    }
    if (is.null(df.loc$data)) {
      bor.df <- matrix(nrow = 0, ncol = 3) %>% as.data.frame
      colnames(bor.df) <- c('x', 'y', 'z')
    } else {
      bor.df <- df.loc$data
    }
    dimt <- image_info(img.file.crop$data) %>% '['(1, 2:3) %>% as.numeric
    dimrow <- dimt[2]
    dimcol <- dimt[1]
    # mouse position info
    bor <- input$plot2_dblclick
    px <- round(bor$x + crop.offset.xy$x)
    y_cor <- round(bor$y + crop.offset.xy$y)
    if (px <= path.info$x[1] | px >= path.info$x[length(path.info$x)]) {
      et <- paste('The X-coordinate of the point you click is',
                  'out of range. Please click on the path')
      sendSweetAlert(
        session = session, title = "Error", text = et, type = "error"
      )
      return()
    }
    # check y-coordinates
    crop.h <- round(diff(input$img_ver)*dimrow/1000)
    if (input$plot2_dblclick$y >= crop.h | input$plot2_dblclick$y <= 0) {
      et <- paste('The Y-coordinate of the point you click is',
                  'out of range. Please click on the path')
      sendSweetAlert(
        session = session, title = "Error", text = et, type = "error"
      )
      return() 
    }
    path.df <- path.info$df
    if (path.info$horizontal & path.info$incline) {
      dpi <- path.info$dpi
      dp <- dpi/25.4
      h.dis <- path.info$h
      py <- path.df$y[path.df$x == px]
      py <- ifelse(y_cor > path.info$y[1], 
                   py + round(h.dis * dp / 2),
                   py - round(h.dis * dp / 2))
      pz <- ifelse(y_cor > path.info$y[1], 'u', 'l')
      temp.df <- data.frame(x = px, y = py, z = pz)
    } else {
      py <- path.df$y[path.df$x == px]
      temp.df <- data.frame(x = px, y = py, z = 'u')
    }
    if(input$edit_wood){
      el_wood$x <- c(el_wood$x, temp.df$x)
      el_wood$y <- c(el_wood$y, temp.df$y)
    }
    else{
      df.loc$data <- rbind(bor.df, temp.df)
    }
  })
  
  # delete points with a brush
  observeEvent(input$buttonzoomdel, {
    if (is.null(input$plot2_brush$xmin)) {
      err.text <- 'You have not selected ring borders with a brush'
      sendSweetAlert(
        session = session, title = "Error", text = err.text, type = "error"
      )
      return()
    } 
    if (is.null(path.info$df)) {
      err.text <- 'A path has not been created'
      sendSweetAlert(
        session = session, title = "Error", text = err.text, type = "error"
      )
      return()
    } 
    if(input$edit_wood){
      if (is.null(el_wood$x)) {
        remove.text <- 'Early/Late border was NOT found along the path'
        sendSweetAlert(
          session = session, title = "Error", text = remove.text, type = "error"
        )
        return()
      } 
    }
    else{
      if (is.null(df.loc$data)) {
        remove.text <- 'Ring border was NOT found along the path'
        sendSweetAlert(
          session = session, title = "Error", text = remove.text, type = "error"
        )
        return()
      } 
    }
    xmin <- round(input$plot2_brush$xmin + crop.offset.xy$x)
    xmax <- round(input$plot2_brush$xmax + crop.offset.xy$x)
    ymin <- round(input$plot2_brush$ymin + crop.offset.xy$y)
    ymax <- round(input$plot2_brush$ymax + crop.offset.xy$y)
    if(input$edit_wood){
      x.ranges <- el_wood$x
      delete.bor <- x.ranges >= xmin & x.ranges <= xmax
      y.ranges <- el_wood$y
      is.contain <- ymin <= y.ranges & ymax >= y.ranges
      delete.bor <- delete.bor & is.contain
      if (any(delete.bor)) {
        el_wood$x <- el_wood$x[!delete.bor]
        el_wood$y <- el_wood$y[!delete.bor]
      } else {
        err.text <- 'Early/Late border was NOT found in the area you selected'
        sendSweetAlert(
          session = session, title = "Error", text = err.text, type = "error"
        )
      }
    }
    else{
      x.ranges <- df.loc$data$x
      delete.bor <- x.ranges >= xmin & x.ranges <= xmax
      y.ranges <- df.loc$data$y
      is.contain <- ymin <= y.ranges & ymax >= y.ranges
      delete.bor <- delete.bor & is.contain
      if (any(delete.bor)) {
        df.loc$data <- df.loc$data[!delete.bor,]
      } else {
        err.text <- 'Ring border was NOT found in the area you selected'
        sendSweetAlert(
          session = session, title = "Error", text = err.text, type = "error"
        )
      }
    }
    
  })
  # Prints  the core with the borders (if any). img.file.crop$data contains the data of the img. df.loc$data contiene los puntos donde est los bordes.
  output$ring_edit <- renderPlot({
    if (is.null(img.file$data)) return()
    fig1 <- imgInput_crop(img.file.crop$data, input$img_ver, input$img_hor)
    sample_yr <- as.numeric(input$sample_yr)
    if (is.na(sample_yr)) return()
    pch <- as.numeric(input$pch)
    bor.color <- input$border.color
    lab.color <- input$label.color
    l.w <- as.numeric(input$linelwd)
    label.cex <- as.numeric(input$label.cex)*0.7
    plot.marker(path.info, hover.xy, sample_yr, l.w, pch,
                bor.color, lab.color, label.cex, el_wood)
  })
  
  output$profile_edit<- renderPlot({
    if(input$buttondensity && !is.null(calibration_profile$data)){ 
    dimrow<-nrow(data.frame(calibration_profile$data))
    par(mar = c(0, 0, 1, 0), xaxs='i')
    plot(calibration_profile$data, xlim=c(round(input$img_hor[1]*dimrow/100),round(input$img_hor[2]*dimrow/100)),ann=FALSE,xaxt='n',yaxt='n',type='l')
    abline(v=df.loc$data$x,col="blue")
    }
  })
  
  observeEvent(input$button_del, { 
    if (is.null(path.info$df)) {
      rt <- paste('You can not remove ring borders because',
                  'the path has not been created.')
      sendSweetAlert(
        session = session, title = "Error", text = rt, type = "error"
      )
      return()
    }
    if (is.null(df.loc$data)) {
      rt <- paste('Ring borders were not found')
      sendSweetAlert(
        session = session, title = "Error", text = rt, type = "error"
      )
      return()
    }
    incline <- path.info$incline
    bx <- df.loc$data$x
    by <- df.loc$data$y
    bz <- df.loc$data$z
    bz <- bz[order(bx)]
    by <- by[order(bx)]
    bx <- sort(bx)
    if (incline) {
      if (input$del.u == '' & input$del.l == '') {
        rt <- 'Please enter border numbers'
        sendSweetAlert(
          session = session, title = "Error", text = rt, type = "error"
        )
        return()
      }
      del.u <- input$del.u
      del.l <- input$del.l
      if (del.u != '')
        del.u <- strsplit(del.u, ",")[[1]] %>% as.numeric
      if(del.l != '')
        del.l <- strsplit(del.l, ",")[[1]] %>% as.numeric
      # ndf <- length(del.u) + length(del.l)
      up <- which(bz == 'u')
      lenup <- length(up)
      bx.u <- bx[up]
      by.u <- by[up]
      if (lenup >= 1 & input$del.u != '') {
        if (max(del.u) <= lenup) {
          bx.u <- bx.u[-del.u]
          by.u <- by.u[-del.u]
        } else {
          rt <- 'The border number you entered did not exist'
          sendSweetAlert(
            session = session, title = "Error", text = rt, type = "error"
          )
          return()
        }
      }
      lower <- which(bz == 'l')
      lenlo <- length(lower)
      bx.l <- bx[lower]
      by.l <- by[lower]
      if (lenlo >= 1 & input$del.l != '') {
        if (max(del.l) <= lenlo) {
          bx.l <- bx.l[-del.l]
          by.l <- by.l[-del.l]
        } else {
          rt <- 'The border number you entered did not exist'
          sendSweetAlert(
            session = session, title = "Error", text = rt, type = "error"
          )
          return()
        }
      }
      df.u <- data.frame(x = bx.u, y = by.u, z = 'u')
      df.l <- data.frame(x = bx.l, y = by.l, z = 'l')
      df.loc$data <- rbind(df.u, df.l)
      updateTextInput(session, "del.u",
        label = 'Border number in the upper portion',
        value = '')
      updateTextInput(session, "del.l",
        label = 'Border number in the lower portion',
        value = '')
    } else { 
      if (input$del == '') {
        rt <- 'You have not entered any border number'
        sendSweetAlert(
          session = session, title = "Error", text = rt, type = "error"
        )
        return()
      }
      del <- strsplit(input$del, ",")[[1]] %>% as.numeric
      if (max(del) <= length(bx)) {
        bx <- bx[-del]
        by <- by[-del]
        df.loc$data <- data.frame(x = bx, y = by, z = 'u')
        updateTextInput(session, "del", label = 'Border number', value = '')
      } else {
        rt <- 'The border number you entered did not exist'
        sendSweetAlert(
          session = session, title = "Error", text = rt, type = "error"
        )
        return()
      }
    }
  }) 
  
  rw.dataframe <- reactiveValues(data = NULL)
  observeEvent(input$button_results, {
    if (is.null(df.loc$data)) {
      error.text <- 'Ring border was not found along the path'
      sendSweetAlert(
        session = session, title = "Error", text = error.text, type = "error"
      )
      return()
    } 
    if (nrow(df.loc$data) <= 1) {
      error.text <- paste('A minimum of two ring borders on each path',
                          'was required to generate a ring-width series')
      sendSweetAlert(
        session = session, title = "Error", text = error.text, type = "error"
      )
      return()
    } 
    sample_yr <- as.numeric(input$sample_yr)
    if (is.na(sample_yr)) {
      error.text <- paste('Please check the argument \'Sampling year\' ')
      sendSweetAlert(
        session = session, title = "Error", text = error.text, type = "error"
      )
      return()
    }
    
    dpi <- path.info$dpi
    dp <- dpi/25.4
    incline <- path.info$incline
    h.dis <- path.info$h
    ph <- path.info$horizontal
    path.df <- path.info$df
    px <- path.info$x
    py <- path.info$y
    
    if (incline) {
      incline.cond <- df.loc$data$z %>% table %>% as.numeric
      if (length(incline.cond) == 1) {
        rt <- paste('A minimum of two ring borders on each path',
                    'was required to generate a ring-width series')
        sendSweetAlert(
          session = session, title = "Error", text = rt, type = "error"
        )
        return()
      }
      if (all(incline.cond >= 2) & incline.cond[1] == incline.cond[2]) {
        rw.dataframe$data <- f.rw(df.loc$data, sample_yr, 
                                  incline, dpi, h.dis)
      } else {
        if (any(incline.cond < 2)) {
          et <- paste('A minimum of two ring borders on each path',
                      'was required to generate a ring-width series')
          sendSweetAlert(
            session = session, title = "Error", text = et, type = "error"
          )
        } else {
          et <-  paste("If you tick the checkbox \"Inclined tree",
                       "rings\", the upper and lower paths should",
                       "have the same number of ring borders.")
          sendSweetAlert(
            session = session, title = "Error", text = et, type = "error"
          )
        }
      }   
    } else {
      rw.dataframe$data <- f.rw(df.loc$data, sample_yr, 
                                incline, dpi, h.dis)
    } 
  })
  output$results <- renderTable({
    if (is.null(rw.dataframe$data)) {
      return()
    } else {
      return(rw.dataframe$data)
    }
  })   
  observeEvent(input$button_hide, {
    if (is.null(rw.dataframe$data)) {
      rt <- 'The data frame to be deleted does not exist'
      sendSweetAlert(
        session = session, title = "Error", text = rt, type = "error"
      )
    } else {
      rw.dataframe$data <- NULL       
    }
  })
  output$RingWidth.csv <- downloadHandler(
    filename =  function() {
      if (is.null(img.file$data)) {
        img.name <- 'Download Fail'
        return(paste0(img.name, '.csv'))
      } else {
        img.name <- input$tuid
      }
      if (input$csv.name != '')
        img.name <- input$csv.name
      return(paste0(img.name, '.csv'))
    },
    content = function(filename) {
      if (is.null(df.loc$data)) {
        error.text <- 'Ring border was not found along the path'
        sendSweetAlert(
          session = session, title = "Error", text = error.text, type = "error"
        )
        return()
      } 
      if (nrow(df.loc$data) <= 1) {
        error.text <- paste('A minimum of two ring borders on each path',
                            'was required to generate a ring-width series')
        sendSweetAlert(
          session = session, title = "Error", text = error.text, type = "error"
        )
        return()
      } 
      sample_yr <- as.numeric(input$sample_yr)
      if (is.na(sample_yr)) {
        error.text <- paste('Please check the argument \'Sampling year\' ')
        sendSweetAlert(
          session = session, title = "Error", text = error.text, type = "error"
        )
        return()
      }
      
      dpi <- path.info$dpi
      dp <- dpi/25.4
      incline <- path.info$incline
      h.dis <- path.info$h

      if (incline) {
        incline.cond <- df.loc$data$z %>% table %>% as.numeric
        if (length(incline.cond) == 1) {
          rt <- paste('A minimum of two ring borders on each path',
                      'was required to generate a ring-width series')
          sendSweetAlert(
            session = session, title = "Error", text = rt, type = "error"
          )
          return()
        }
        if (all(incline.cond >= 2) & incline.cond[1] == incline.cond[2]) {
          df.rw <- f.rw(df.loc$data, sample_yr, incline, dpi, h.dis)
          write.csv(df.rw, filename, quote = FALSE, na = '--')
        } else {
          if (any(incline.cond < 2)) {
            rt <- paste('A minimum of two ring borders on each path ',
              'was required to generate a ring-width series')
            sendSweetAlert(
              session = session, title = "Error", text = rt, type = "error"
            )
            return()
          } else {
            rt <-  paste("If you tick the checkbox \"Inclined tree",
                         "rings\", the upper and lower paths should",
                         "have the same number of ring borders.")
            sendSweetAlert(
              session = session, title = "Error", text = rt, type = "error"
            )
            return()
          }
        }
      } else {
        df.rw <- f.rw(df.loc$data, sample_yr, incline, dpi, h.dis)
        write.csv(df.rw, filename, quote = FALSE, na = '--')
      } 
    },
    contentType = 'csv'
  )
  # Export image
  output$imageCore <- downloadHandler(
    filename =  function() {
      if (is.null(img.file$data)) {
        img.name <- 'Download Fail'
        return(paste0(img.name, '.png'))
      } else {
        img.name <- input$tuid
      }
      if (input$image.name != '')
        img.name <- input$image.name
      return(paste0(img.name, '.png'))
    },
    content = function(filename) {
      png(filename)
      plot(img.file.crop$data)
      dev.off()
      }
  )
  # Export Image from path
  output$imagePath <- downloadHandler(
    filename =  function() {
      if (is.null(img.file$data)) {
        img.name <- 'Download Fail'
        return(paste0(img.name, '.png'))
      } else {
        img.name <- input$tuid
      }
      if (input$imgpath.name != '')
        img.name <- input$imgpath.name
      return(paste0(img.name, '.png'))
    },
    content = function(filename) {
      png(filename)
      y <- path.info$y - crop.offset.xy$y
      par(mar = c(0, 0, 0, 0), xaxs='i', yaxs='i')
      plot(img.file.crop$data, ylim = c(y[1]-input$pixelspath, y[1]+input$pixelspath),
           main = '', xlab = "", ylab = "", cex.main = 1.2)
      dev.off()
    }
  )
    
    
  output$RingWidth.xlsx <- downloadHandler(
    filename =  function() {
      if (is.null(img.file$data)) {
        img.name <- 'Download Fail'
        return(paste0(img.name, '.xlsx'))
      } else {
        img.name <- input$tuid
      }
      if (input$excel.name != '')
        img.name <- input$excel.name
      return(paste0(img.name, '.xlsx'))
    },
    content = function(filename) {
      if (is.null(df.loc$data)) {
        error.text <- 'Ring border was not found along the path'
        sendSweetAlert(
          session = session, title = "Error", text = error.text, type = "error"
        )
        return()
      } 
      if (nrow(df.loc$data) <= 1) {
        error.text <- paste('A minimum of two ring borders on each path',
                            'was required to generate a ring-width series')
        sendSweetAlert(
          session = session, title = "Error", text = error.text, type = "error"
        )
        return()
      } 
      sample_yr <- as.numeric(input$sample_yr)
      sample_site <- input$sample_site
      sample_parcel <- input$sample_parcel
      sample_species <- input$sample_species
      tuid <- input$tuid
      dpi <- input$dpi
      if (is.na(sample_yr)) {
        error.text <- paste('Please check the argument \'Sampling year\' ')
        sendSweetAlert(
          session = session, title = "Error", text = error.text, type = "error"
        )
        return()
      }
      dpi <- path.info$dpi
      dp <- dpi/25.4
      incline <- path.info$incline
      h.dis <- path.info$h
      
      if (incline) {
        incline.cond <- df.loc$data$z %>% table %>% as.numeric
        if (length(incline.cond) == 1) {
          rt <- paste('A minimum of two ring borders on each path',
                      'was required to generate a ring-width series')
          sendSweetAlert(
            session = session, title = "Error", text = rt, type = "error"
          )
          return()
        }
        if (all(incline.cond >= 2) & incline.cond[1] == incline.cond[2]) {
          df.rw <- f.rw(df.loc$data, sample_yr, incline, dpi, h.dis)
          tree_info <- data.frame(tuid, dpi, sample_yr, sample_parcel, sample_site, sample_species)
          list_of_datasets <- list("RingData" = df.rw, "TreeInfo" = tree_info)
          write.xlsx(list_of_datasets, file = filename)
        } else {
          if (any(incline.cond < 2)) {
            rt <- paste('A minimum of two ring borders on each path ',
                        'was required to generate a ring-width series')
            sendSweetAlert(
              session = session, title = "Error", text = rt, type = "error"
            )
            return()
          } else {
            rt <-  paste("If you tick the checkbox \"Inclined tree",
                         "rings\", the upper and lower paths should",
                         "have the same number of ring borders.")
            sendSweetAlert(
              session = session, title = "Error", text = rt, type = "error"
            )
            return()
          }
        }
      } else {
        df.rw <- f.rw(df.loc$data, sample_yr, incline, dpi, h.dis)
        if(!is.null(calibration_profile$data)){
          prev = 0
          mean <- vector()
          max <- vector()
          min <- vector()
          std <- vector()
          for (i in df.loc$data$x){
            mean <- c(mean, round(mean(calibration_profile$data[prev:i]),digits=2))
            max <- c(max, round(max(calibration_profile$data[prev:i]),digits=2))
            min <- c(min, round(min(calibration_profile$data[prev:i]),digits=2))
            std <- c(std, round(sd(calibration_profile$data[prev:i]),digits=2))
            prev = i
          }
          df.rw$ring.meanDensity <- mean
          df.rw$ring.minDensity <- min
          df.rw$ring.maxDensity <- max
          df.rw$ring.std <- std
          if (!is.null(el_wood$x)){
            if(length(el_wood$x) != nrow(df.loc$data)){
              showNotification(paste("WARNING: Number or Ring borders is different of number of Early/Late borders, add or remove borders until they are the same length"), duration = 5)
            }
            df.rw$el_wood.x <- el_wood$x
            df.rw$el_wood.y <- el_wood$y
            early_density <- vector()
            late_density <- vector()
            max_early_density <- vector()
            min_early_density <- vector()
            std_early_density <- vector()
            max_late_density <- vector()
            min_late_density <- vector()
            std_late_density <- vector()
            for (i in 1:length(df.loc$data$x)){
              early_density <- c(early_density, round(mean(calibration_profile$data[el_wood$x[i]:df.loc$data$x[i]]),digits=2))
              max_early_density <- c(max_early_density, round(max(calibration_profile$data[el_wood$x[i]:df.loc$data$x[i]]),digits=2))
              min_early_density <- c(min_early_density, round(min(calibration_profile$data[el_wood$x[i]:df.loc$data$x[i]]),digits=2))
              std_early_density <- c(std_early_density, round(sd(calibration_profile$data[el_wood$x[i]:df.loc$data$x[i]]),digits=2))
              if (i != length(df.loc$data$x)){
                late_density <- c(late_density, round(mean(calibration_profile$data[df.loc$data$x[i]:el_wood$x[i+1]]),digits=2))
                max_late_density <- c(max_late_density, round(max(calibration_profile$data[df.loc$data$x[i]:el_wood$x[i+1]]),digits=2))
                min_late_density <- c(min_late_density, round(min(calibration_profile$data[df.loc$data$x[i]:el_wood$x[i+1]]),digits=2))
                std_late_density <- c(std_late_density, round(sd(calibration_profile$data[df.loc$data$x[i]:el_wood$x[i+1]]),digits=2))
              }
              else{
                late_density <- c(late_density, round(mean(calibration_profile$data[df.loc$data$x[i]:length(calibration_profile$data)]),digits=2))
                max_late_density <- c(max_late_density, round(max(calibration_profile$data[df.loc$data$x[i]:length(calibration_profile$data)]),digits=2))
                min_late_density <- c(min_late_density, round(min(calibration_profile$data[df.loc$data$x[i]:length(calibration_profile$data)]),digits=2))
                std_late_density <- c(std_late_density, round(sd(calibration_profile$data[df.loc$data$x[i]:length(calibration_profile$data)]),digits=2))
              }
            }
            df.rw$early_density <- early_density
            df.rw$late_density <- late_density
            df.rw$min_early_density <- min_early_density
            df.rw$min_late_density <- min_late_density
            df.rw$max_early_density <- max_early_density
            df.rw$max_late_density <- max_late_density
            df.rw$std_early_density <- std_early_density
            df.rw$std_late_density <- std_late_density
        }
        }
          tree_info <- data.frame(tuid, dpi, sample_yr, sample_parcel, sample_site, sample_species)
          list_of_datasets <- list("RingData" = df.rw, "TreeInfo" = tree_info, "DensityProfile" = calibration_profile$data)
          write.xlsx(list_of_datasets, file = filename)
      } 
    },
    contentType = 'excel'
  )
  observeEvent(input$reset.hdr,{
    updateTextInput(session, "tuhdr1",label = 'Site ID',value = '')
    updateTextInput(session, "tuhdr2",label = 'Site Name',value = '')
    updateTextInput(session, "tuhdr3",label = 'Species Code',value = '')
    updateTextInput(session, "tuhdr4",label = 'State or Country',value = '')
    updateTextInput(session, "tuhdr5",label = 'Species',value = '')
    updateTextInput(session, "tuhdr6",label = 'Elevation',value = '')
    updateTextInput(session, "tuhdr7",label = 'Latitude',value = '')
    updateTextInput(session, "tuhdr8",label = 'Longitude',value = '')
    updateTextInput(session, "tuhdr9",label = 'First Year',value = '')
    updateTextInput(session, "tuhdr10",label = 'Last Year',value = '')
    updateTextInput(session, "tuhdr11",label = 'Lead Investigator',value = '')
    updateTextInput(session, "tuhdr12",label = 'Completion Date',value = '')
  })
  output$RingWidth.rwl <- downloadHandler(
    filename = function() {
      if (is.null(df.loc$data)) {
        img.name <- 'Download Unavailable'
        return(paste0(img.name, '.rwl'))
      } else {
        img.name <- input$tuid
      }
      if (input$rwl.name != '')
        img.name <- input$rwl.name
      return(paste0(img.name, '.rwl'))
    }, 
    content = function(filename) {
      seriesID <- df.loc$ID
      miss.id1 <- seriesID == ''
      if (miss.id1) {
        rt <- 'Please enter a series ID'
        sendSweetAlert(
          session = session, title = "Error", text = rt, type = "error"
        )
        return()
      }
      if (is.null(df.loc$data)) {
        error.text <- 'Ring border was not found along the path'
        sendSweetAlert(
          session = session, title = "Error", text = error.text, type = "error"
        )
        return()
      } 
      if (nrow(df.loc$data) <= 1) {
        error.text <- paste('A minimum of two ring borders on each path',
                            'was required to generate a ring-width series')
        sendSweetAlert(
          session = session, title = "Error", text = error.text, type = "error"
        )
        return()
      } 
      sample_yr <- as.numeric(input$sample_yr)
      if (is.na(sample_yr)) {
        error.text <- paste('Please check the argument \'Sampling year\' ')
        sendSweetAlert(
          session = session, title = "Error", text = error.text, type = "error"
        )
        return()
      }
      
      dpi <- path.info$dpi
      dp <- dpi/25.4
      incline <- path.info$incline
      h.dis <- path.info$h
      df.rw <- NULL
      
      if (incline) {
        incline.cond <- df.loc$data$z %>% table %>% as.numeric
        if (length(incline.cond) == 1) {
          rt <- paste('A minimum of two ring borders on each path',
                      'was required to generate a ring-width series')
          sendSweetAlert(
            session = session, title = "Error", text = rt, type = "error"
          )
          return()
        }
        if (all(incline.cond >= 2) & incline.cond[1] == incline.cond[2]) {
          df.rw <- f.rw(df.loc$data, sample_yr, incline, dpi, h.dis)
        } else {
          if (any(incline.cond < 2)) {
            rt <- paste('A minimum of two ring borders on each path',
                        'was required to generate a ring-width series')
            sendSweetAlert(
              session = session, title = "Error", text = rt, type = "error"
            )
            return()
          } else {
            rt <- paste("If incline = TRUE, the upper and lower paths", 
                        "should have the same number of ring borders")
            sendSweetAlert(
              session = session, title = "Error", text = rt, type = "error"
            )
            return()
          }
        }
      } else {
        df.rw <- f.rw(df.loc$data, sample_yr, incline, dpi, h.dis)
      }
      df.rwl <- data.frame(df.rw$ring.width, row.names = df.rw$year)
      tuprec <- as.numeric(input$tuprec)
      tuheader <- input$tuheader
      tuhdr1 <- input$tuhdr1
      tuhdr2<- input$tuhdr2
      tuhdr3 <- input$tuhdr3
      tuhdr4 <- input$tuhdr4
      tuhdr5 <- input$tuhdr5
      tuhdr6 <- input$tuhdr6
      tuhdr7 <- input$tuhdr7
      tuhdr8 <- input$tuhdr8
      tuhdr9 <- input$tuhdr9
      tuhdr10 <- input$tuhdr10
      tuhdr11 <- input$tuhdr11
      tuhdr12 <- input$tuhdr12
      colnames(df.rwl) <- seriesID
      hdr.list<- NULL
      if (tuheader) {
        hdr <- c(tuhdr1, tuhdr2, tuhdr3, tuhdr4, tuhdr5, tuhdr6, 
          tuhdr7, tuhdr8, tuhdr9, tuhdr10, tuhdr11, tuhdr12)
        hdr.name <- c('site.id','site.name', 'spp.code', 'state.country', 
          'spp','elev', 'lat', 'long', 'first.yr', 'last.yr',
          'lead.invs', 'comp.date')
        which.not.empty <- hdr != ''
        if (any(which.not.empty)) {
          hdr.list <- lapply(hdr, function(x) x)
          names(hdr.list) <- hdr.name
        }
      }
      write.rwl(rwl.df = df.rwl, fname = filename,
        format = "tucson", header = hdr.list,
        append = FALSE, prec = tuprec)
    }, contentType = "rwl"
  )
}

shinyApp(ui = createUI(), server = createServer)

