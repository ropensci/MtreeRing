#' @import shiny
#' @import shinydashboard
#' @import shinyWidgets

createUI <- function()
{
shiny.title <- dashboardHeader(title = 'MtreeRing')
shiny.sider <- dashboardSidebar(
  sidebarMenu(
    menuItem('Image Loading',tabName = 'input_pre', 
             icon = icon('folder-open', lib = 'font-awesome')),
    menuItem('Measurement',tabName = 'mea_arg', 
             icon = icon('gear', lib = 'font-awesome'))
  )
)
page1 <- fluidRow(
  box(
    title = div(style = 'color:#FFFFFF;font-size:80%; 
                font-weight: bolder', 'Image Preview'),
    width = 12, status = 'primary', solidHeader = T, collapsible = T,
    plotOutput('pre.img',
               brush = brushOpts(
                 id = "plot1_brush",
                 opacity = 0.25,
                 resetOnNew = TRUE)
    )
  ), 
  box(
    title = div(style = 'color:#FFFFFF;font-size:80%;
                font-weight: bolder', 'Image Upload'),
    width = 4, status = 'primary', solidHeader = T, collapsible = T,
    conditionalPanel(
      condition = '!input.inmethod',
      fileInput('select.file', 'Choose an image file',
                buttonLabel = 'Browse...', width = '80%')
    ),
    prettySwitch(inputId = "magick.switch", label = "Magick ON",
                 value = TRUE, fill = TRUE, status = "success"),
    helpText('Image upload is limited to 150 MB per file. Supported',
             ' formats include png, jpg, tif and bmp.',
             style = 'color:#000000;font-size:90%'),
    prettyCheckbox(
      inputId = "inmethod", 
      label = div(style = 'color:#000000;font-weight: bolder;', 'Image Path'), 
      shape = "curve", value = F, status = "success"),
    conditionalPanel(
      condition = 'input.inmethod',
      textInput('enter.path', 'Enter file path', ''),
      helpText('For example: C:/Users/shiny/img01.png',
               style = 'color:#000000;font-size:90%'),
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
             "graphical window and the pith side at the right.",
             style = 'color:#000000;font-size:90%;text-align:justify;'),
    actionButton(
      'buttonrotate', 'Rotate',
      class = "btn btn-primary btn-md",
      icon = icon('repeat',"fa-1x"),
      style = 'color:#FFFFFF;text-align:center;
              font-weight: bolder;font-size:110%;')
  ),
  box(
    title = div(style = 'color:#FFFFFF;font-size:80%;
                font-weight: bolder', 'Image Cropping'),
    width = 3, status = 'primary', solidHeader = T, collapsible = T,
    helpText("To remove unwanted cores and irrelevant objects, ",
             "move the mouse to the core you wish to measure and",
             "create a rectangle by brushing, see details below.",
             style = 'color:#000000;font-size:90%;text-align:justify;'),
    prettyRadioButtons(inputId = "cropcondition", label = "",
                       choiceNames = 'UNCROPPED', choiceValues = list('a'),
                       status = "danger", shape = "square",
                       fill = FALSE, inline = FALSE),
    prettyCheckbox(
      inputId = "showcropp", 
      label = div(style = 'color:#000000;font-weight: bolder;', 'Show Help'),
      shape = "curve", value = F, status = "success"
    ),
    conditionalPanel(
      condition = 'input.showcropp',
      helpText(
        "The operation \"brush\" allows users to create a transparent ", 
        "rectangle on the image and drag it around. For cores scanned ", 
        "side by side, the user can choose a core of interest by brushing.", 
        style = 'color:#000000;text-align:justify;'),
      helpText(
        "After brushing, click on the button \"Crop\" to create a",
        " cropped area. The measurement will be performed within", 
        " this area, rather than the whole (uncropped) image.",
        style = 'color:#000000;text-align:justify;'),
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
  )
)
page2.1 <- fluidRow(
  box(
    title = div(style = 'color:#FFFFFF;font-size:80%;
                font-weight: bolder', 'Options'),
    width = 4, status = 'primary', solidHeader = T, collapsible = T,
    textInput('tuid', 'Series ID', '', width = '75%'),
    textInput('dpi', 'DPI', '', '75%'),
    textInput('sample.yr', 'Sampling year', '', '75%'),
    textInput('m.line', 'Y-coordinate of path', '', '75%'),
    prettyCheckbox(
      inputId = "incline", 
      label = div(
        style = 'color:#000000;font-weight: bolder;', 'Inclined tree rings'), 
      shape = "curve", value = F, status = "success"
    ),
    conditionalPanel(
      condition = 'input.incline',
      numericInput('h.dis', 'Distance between paths (mm)', 
                   1, 0.1, 30, 0.1, width = '75%')
    ),
    br(),
    radioGroupButtons(
      inputId = "measuremethod", 
      label = 'Measurement mode',
      status = "btn btn-primary btn-md",
      #individual = T,
      size = 'normal',
      selected = 'auto',
      choiceNames = list(
          div(style = 'color:#FFFFFF;font-weight: bolder;', 'Manual'), 
          div(style = 'color:#FFFFFF;font-weight: bolder;', 'Automation')),
      choiceValues = list('manual', 'auto'),
      width = '100%') 
  ),
  box(
    title = div(style = 'color:#FFFFFF;font-size:80%;
                font-weight: bolder', 'Options'),
    width = 4, status = 'primary', solidHeader = T, collapsible = T,
    sliderInput('linelwd', 'Path width', 
                0.2, 3, 1, 0.1, width = '80%'),
    sliderInput('label.cex', 'Magnification for labels',
                0.2, 3, 1, 0.1, width = '80%'),
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
  conditionalPanel(
    condition = 'input.measuremethod=="auto"',
    box(
      title = div(style = 'color:#FFFFFF;font-size:80%;
                  font-weight: bolder', 'Options'),
      width = 4, status = 'primary', solidHeader = T, collapsible = T,
      prettyCheckbox(
        inputId = "isrgb", 
        label = div(
          style = 'color:#000000;font-weight:bolder;', "Default RGB"), 
        shape = "curve", value = T, status = "success"
      ),
      conditionalPanel(
        condition = '!input.isrgb',
        textInput('customRGB', 'Custom RGB', '0.299,0.587,0.114'),
        helpText('Note:The three numbers correspond to',
                 'R, G and B components,respectively.',
                 style = 'color:#000000;font-weight: bolder'),
        hr()
      ),
      radioGroupButtons(
        inputId = "method",
        label = 'Ring detection method',
        status = "btn btn-primary btn-md",
        #individual = T,
        selected = 'canny',
        size = 'normal',
        choiceNames = list(
          div(style = 'color:#FFFFFF;font-weight: bolder;font-size:80%',
              'Watershed'),
          div(style = 'color:#FFFFFF;font-weight: bolder;font-size:80%',
              'Canny'),
          div(style = 'color:#FFFFFF;font-weight: bolder;font-size:80%',
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
          textInput('watershed.threshold2', 'Threshold value', ''),
          'A value of the form XX% (e.g. 98%)'
        )
      ),
      conditionalPanel(
        condition = 'input.method=="canny"',
        prettyCheckbox(
          inputId = "defaultcanny", 
          label = div(
            style = 'color:#000000;font-weight: bolder;',
            "Auto threshold (Recommanded)"), 
          shape = "curve", value = T, status = "success"),
        conditionalPanel(
          condition = 'input.defaultcanny',
          sliderInput('canny.adjust',
                      'Threshold adjusment factor',
                      0.8, 1.8, 1.4, 0.05, width = '75%')
        ),
        conditionalPanel(
          condition = '!input.defaultcanny',
          textInput('canny.t2', 'Threshold for strong edges', '', '75%'),
          textInput('canny.t1', 'Threshold for weak edges', '', '75%')
        ),
        numericInput('canny.smoothing',
                     'Degree of smoothing',
                     1, 0, 4, 1, width = '75%')
      ),
      conditionalPanel(
        condition = 'input.method!="lineardetect"',
        prettyCheckbox(inputId = "defaultse", 
                       label = div(
                         style = 'color:#000000;font-weight: bolder;',
                         "Default structuring elements"), 
                       shape = "curve", value = T, status = "success"),
        conditionalPanel(
          condition = '!input.defaultse',
          textInput('struc.ele1', 'First structuring element', '', '75%'),
          textInput('struc.ele2', 'Second structuring element', '', '75%')
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
      helpText('Automatic detection may take a few seconds (depending',
               ' on the image size and complexity of the sample).',
               #style = 'color:#000000;font-size:95%;text-align:justify;')
               style = 'color:#000000;font-size:90%;')
    )
  ),
  box(
    title = div(style = 'color:#FFFFFF;font-size:100%;
                font-weight: bolder', 'Tree Ring Detection'),
    width = 12, status = 'primary', solidHeader = T, collapsible = T,
    conditionalPanel(
      condition = 'input.measuremethod!="auto"',
      actionButton(
        'buttoncreatpath', 'Create Path',
        class = "btn btn-primary btn-md", icon = icon('plus'),
        style = 'color:#FFFFFF;text-align:center;font-weight: bolder'
      ),
      useSweetAlert(),
      actionButton(
        'buttonrcm', 'Remove All',
        class = "btn btn-danger btn-md", icon = icon('trash'),
        style = 'color:#FFFFFF;text-align:center;font-weight: bolder'
      ),
      useSweetAlert()
    ),
    conditionalPanel(
      condition = 'input.measuremethod=="auto"',
      actionButton(
        'buttoncreatpath2', 'Create Path',
        class = "btn btn-primary btn-md", icon = icon('plus'),
        style = 'color:#FFFFFF;text-align:center;font-weight: bolder'
      ),
      useSweetAlert(),
      actionButton(
        'buttonrcm2', 'Remove All',
        class = "btn btn-danger btn-md", icon = icon('trash'),
        style = 'color:#FFFFFF;text-align:center;font-weight: bolder'
      ),
      useSweetAlert(),
      actionButton(
        'button_run_auto', 'Run Detection',
        class = "btn btn-success btn-md", icon = icon('play'),
        style = 'color:#FFFFFF;text-align:center;font-weight: bolder'
      ),
      useSweetAlert()
    ),
    hr(),
    plotOutput('pre.img2',
               dblclick = "plot2_dblclick",
               brush = brushOpts(
                 id = "plot2_brush",
                 resetOnNew = TRUE
               )
    ),
    hr(),
    actionButton(
      'buttonsubimg', 'Create Sub-image',
      class = "btn btn-primary btn-md", icon = icon('search-plus'),
      style = 'color:#FFFFFF;text-align:center;font-weight: bolder'
    )
  ),
  box(
    title = div(style = 'color:#FFFFFF;font-size:100%;
                font-weight: bolder', 'Tree Ring Editing'),
    width = 12, status = 'primary', solidHeader = T, collapsible = T,
    actionButton(
      'buttonzoomdel', 'Delete Border',
      class = "btn btn-danger btn-md",
      icon = icon('eraser'),
      style = 'color:#FFFFFF;text-align:center;font-weight: bolder'
    ),
    useSweetAlert(),
    hr(),
    plotOutput('zoom.img',
               dblclick = dblclickOpts(id = "zoom_dblclick"),
               brush = brushOpts(
                 id = "zoom_brush",
                 resetOnNew = TRUE
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
          style = 'color:#000000;text-align:justify;'
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
        style = 'color:#000000;font-weight: bolder;',
        icon('cog', class = 'fa-spin', lib = 'font-awesome'), 'Output'),
      width = 6,
      tabPanel(
        div(style = 'color:#000000;font-weight: bolder;',
            icon('list-ol', 'fa-1x'), ' Results'),
        #HTML("<p style = 'color:#000000;'><b>Results</b></p>"),
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
        div(style = 'color:#000000;font-weight: bolder;',
            icon('arrow-down', 'fa-1x'), ' CSV'
        ),
        textInput('csv.name', 'Name of the csv file', '', width = '50%'),
        helpText(
          style = 'color:#000000;font-weight: normal;',
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
        #HTML("<p style = 'color:#000000;'><b>CSV</b></p>"),
        downloadButton(
          'RingWidth.csv', 'Download CSV',
          class = "btn btn-primary btn-md",
          style = 'color:#FFFFFF;text-align:center;font-weight:bolder;'
        )
      ),
      tabPanel(
        div(style = 'color:#000000;font-weight: bolder;',
            icon('arrow-down', 'fa-1x'), ' RWL'),
        textInput('rwl.name', 'Name of the rwl file', '', width = '50%'),
        helpText(style = 'color:#000000;font-weight: normal;',
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
        helpText(style = 'color:#000000;font-weight: normal;', 
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
        helpText(style = 'color:#000000;font-weight: normal;',
                 'For more details about the header, please', 
                 'read reference manual of the R package dplR.', 
                 'The output file is Tucson format.'),
        hr(),
        #HTML("<p style = 'color:#000000;'><b>RWL</b></p>"),
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