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

createServer <- function(input, output, session) 
{
  f.morphological <- function(seg.data, struc.ele1, struc.ele2, x.dpi) {
    if (is.null(struc.ele1)) {
      stru.1 <- x.dpi/400
      struc.ele1 <- c(stru.1, stru.1) %>% round
    }
    if (is.null(struc.ele2)) {
      stru.2 <- x.dpi/80
      struc.ele2 <- c(stru.2, stru.2) %>% round
    }
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
    watershed.seg <- watershed(as.cimg(water.seg), imgra, fill_lines = T)
    watershed.seg <- normalize(watershed.seg[, , 1, 1])
    return(watershed.seg)
  }
  r.det <- function(seg.data, py) {
    gray.values <- seg.data[py, ]
    diff.gray <- c(0, diff(gray.values, lag = 1))
    col.num <- which(diff.gray != 0)
    if (length(col.num) == 0) 
      stop("Ring border was not detected")
    return(col.num)
  }
  f.sort <- function(border.col, dp) {
    filter.col <- diff(border.col) >= dp/10
    selected.border <- c(border.col[1], border.col[-1][filter.col])
    return(selected.border)
  }
  f.border <- function(seg.data, py, dp) {
    border.col <- r.det(seg.data, py)
    border.col <- f.sort(border.col, dp)
    return(border.col)
  }
  plot.marker <- function(py, incline, dp, sample.yr, h.dis, l.w, 
    bor.color, lab.color, pch, label.cex, 
    df.loc, plot.year, img.name)
  {
    title(main = img.name)
    if (!is.null(py)) {
      abline(h = py, lty = 2, lwd = l.w, col = lab.color)
      if (incline) {
        abline(h = py, lty = 1, lwd = l.w, col = lab.color)
        number.of.pixels <- round((h.dis / 2) * dp)
        py.upper <- py + number.of.pixels
        abline(h = py.upper, lty = 2, lwd = l.w, col = lab.color)
        py.lower <- py - number.of.pixels
        abline(h = py.lower, lty = 2, lwd = l.w, col = lab.color)
      }
    } else {
      return()
    }
    if (nrow(df.loc ) >= 3) {
      bx <- df.loc$x[-c(1,2)]
      where.bx <- df.loc$z[-c(1,2)]
      where.bx <- where.bx[order(bx)]
      bx <- sort(bx)
      if (incline) {
        up <- which(where.bx > 0)
        lenup <- length(up)
        if (lenup >= 1) {
          by.up <- rep(py.upper, time = lenup)
          points(bx[up], by.up, col = bor.color, type = "p", 
            pch = pch, cex = label.cex * 0.75)
          if (plot.year) {
            year.u <- c(sample.yr:(sample.yr - lenup + 1))
            text(bx[up], by.up, year.u, adj = c(1.5, 0.5), 
              srt = 90, col = lab.color, cex = label.cex)
            border.num <- 1:lenup
            text(bx[up], by.up, border.num, adj = c(0.5, -1.25), 
              col = lab.color, cex = label.cex)
          }
        }
        lower <- which(where.bx < 0)
        lenlo <- length(lower)
        if (lenlo >= 1) {
          by.lower <- rep(py.lower, time = lenlo)
          points(bx[lower], by.lower, col = bor.color, type = "p", 
            pch = pch, cex = label.cex * 0.75)
          if (plot.year) {
            year.l <- c(sample.yr:(sample.yr - lenlo + 1))
            text(bx[lower], by.lower, year.l, adj = c(1.5, 0.5), 
              srt = 90, col = lab.color, cex = label.cex)
            border.num <- 1:lenlo
            text(bx[lower], by.lower, border.num, adj = c(0.5, -1.25), 
              col = lab.color, cex = label.cex)
          }
        }
      } else { 
        if (length(bx) >= 1) {
          lenbx <- length(bx)
          by <- rep(py, time = lenbx)
          points(bx, by, col = bor.color, type = "p", 
            pch = pch, cex = label.cex * 0.75)
          if (plot.year) {
            year.u <- c(sample.yr:(sample.yr - length(by) + 1))
            text(bx, by, year.u, adj = c(1.5, 0.5), 
              srt = 90, col = lab.color, cex = label.cex)
            border.num <- 1:lenbx
            text(bx, by, border.num, adj = c(0.5, -1.25), 
              col = lab.color, cex = label.cex)
          }
        }
      }
    }
  }
  f.rw <- function(outfile, sample.yr, incline, py, dpi, h.dis) {
    df.loc <- outfile
    bx <- df.loc$x[-c(1:2)]
    where.bx <- df.loc$z[-c(1:2)]
    where.bx <- where.bx[order(bx)]
    bx <- sort(bx)
    dp <- dpi/25.4
    if (!incline) {
      lenbx <- length(bx)
      diff.col.num <- c(NA, diff(bx))
      rw <- round(diff.col.num/dp, 2)
      years <- c(sample.yr:(sample.yr - lenbx + 1))
      df.rw <- data.frame(year = years, column.numbers = bx, ring.width = rw)
    } else { 
      up <- which(where.bx > 0)
      lenup <- length(up)
      if (lenup >= 1) {
        bx.up <- bx[up]
        diff.col.num.up <- c(NA, diff(bx.up))
        rw.up <- round(diff.col.num.up/dp, 2)
      }
      lower <- which(where.bx < 0)
      lenlo <- length(lower)
      if (lenlo >= 1) {
        bx.lower <- bx[lower]
        diff.col.num.lower <- c(NA, diff(bx.lower))
        rw.lower <- round(diff.col.num.lower/dp, 2)
      }
      years <- c(sample.yr:(sample.yr - lenup + 1))
      mean.bor <- (diff.col.num.lower[-1] + diff.col.num.up[-1])/2
      x.cor <- abs(bx.lower - bx.up)
      x.cor <- x.cor[-length(x.cor)]
      correct.rw <- mean.bor * cos(atan(x.cor/(dp * h.dis)))
      correct.rw <- c(NA, correct.rw)
      correct.rw <- round(correct.rw/dp, 2)
      df.rw <- data.frame(year = years, upper.cn = bx.up, upper.rw = rw.up, 
        lower.cn = bx.lower, lower.rw = rw.lower, 
        ring.width = correct.rw)
    }
    return(df.rw)
  }
  automatic.det <- function(img, incline, method, h.dis, dpi, m.line, RGB, 
    x1, x2, y1, y2, arghed, watershed.threshold, 
    watershed.adjust, struc.ele1, struc.ele2, 
    default.canny, canny.t1, canny.t2, canny.adjust, 
    canny.smoothing, origin) 
  {   
    dp <- dpi/25.4
    py <- round(m.line)
    if (incline) {
      number.of.pixels <- round((h.dis/2) * dp)
      py.upper <- py + number.of.pixels
      py.lower <- py - number.of.pixels
    }
    dim.img <- image_info(img) %>% '['(1,2:3) %>% as.numeric
    dimcol <- dim.img[1]
    dimrow <- dim.img[2]
    if (x1 <= 0) x1 <- 1
    if (y1 <= 0) y1 <- 0
    if (x2 >= dimcol) x2 <- dimcol
    if (y2 >= dimrow) y2 <- dimrow - 1
    img.range <- paste0(as.character(x2 - x1 + 1), 'x', 
      as.character(y2 - y1 + 1), '+',
      as.character(x1 - 1), '+', 
      as.character(dimrow - y2 - 1))
    img.crop <- image_crop(img, img.range)
    rd.martix <- img.crop[[1]]
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
    if (method == 'watershed') {
      seg.mor <- f.morphological(seg.data, struc.ele1, struc.ele2, dpi)
      black.hat <- hat(seg.mor, dpi, watershed.threshold, watershed.adjust)
      marker.img <- water.im(black.hat, T)
      seg.data <- watershed.im(marker.img, seg.mor)
    }  
    if (method == 'canny') {
      seg.mor <- f.morphological(seg.data, struc.ele1, struc.ele2, dpi)
      if (default.canny) {
        canny.seg <- cannyEdges(as.cimg(seg.mor), alpha = canny.adjust, 
          sigma = canny.smoothing)
      } else {
        canny.seg <- cannyEdges(as.cimg(seg.mor), t1=canny.t1, t2=canny.t2,
          alpha = canny.adjust, sigma = canny.smoothing)
      }
      seg.data <- canny.seg[, , 1, 1]
    } 
    if (method == 'lineardetect') {
      attributes(seg.data)['image'] <- 'img'
      smoothed <- graySmoothed(seg.data, ppi = dpi, rgb = RGB)
      borders <- linearDetect(smoothed, origin = origin)
      borders <- borders + x1 - 1
      py.ld <- round((y1 + y2)/2)
      df <- data.frame(x = borders, 
        y = rep(py.ld, time = length(borders)), 
        z = rep(0, time = length(borders)))
      df.loc <- rbind(arghed, df)
      return(df.loc)
    }
    if (incline) {
      bor.u <- f.border(seg.data, y2 - py.upper, dp) + x1 - 1
      bor.l <- f.border(seg.data, y2 - py.lower, dp) + x1 - 1
      if (method == 'watershed') {
        bor.u <- bor.u - 1
        bor.l <- bor.l - 1
      }
      df.u <- data.frame(x = bor.u, y = py.upper, z = 1)
      df.l <- data.frame(x = bor.l, y = py.lower, z = -1)
      df.loc <- rbind(arghed, df.u, df.l)
    } else {
      bor.col <- f.border(seg.data, y2 - py, dp) + x1 - 1
      if (method == 'watershed')
        bor.col <- bor.col - 1
      df <- data.frame(x = bor.col, y = py, z = 0)
      df.loc <- rbind(arghed, df)
    }
    return(df.loc)
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
    dimt <- attributes(tdata)$dimt
    img.name <- attributes(tdata)$img.name
    dimcol <- dimt[1]
    dimrow <- dimt[2]
    xleft <- 0
    ybottom <- 0
    xright <- dimcol
    ytop <- dimrow
    par(mar = c(2.5, 2, 2, 0))
    plot(x = c(xleft, xright), y = c(ybottom, ytop), 
      xlim = c(xleft, xright), ylim = c(ybottom, ytop), 
      main = img.name, xlab = "", ylab = "", 
      type = "n", axes = F, cex.main = 1.2)
    axis(1, col = "grey", cex.axis = 1)
    axis(2, col = "grey", cex.axis = 1)
    rasterImage(as.raster(tdata.copy), xleft, ybottom, 
      xright, ytop, interpolate = FALSE)
    if (!is.null(plot1_rangesx)) {
      xmin <- plot1_rangesx[1]
      xmax <- plot1_rangesx[2]
      ymin <- plot1_rangesy[1]
      ymax <- plot1_rangesy[2]
      x <- c(xmin, xmax, xmax, xmin, xmin)
      y <- c(ymin, ymin, ymax, ymax, ymin)
      points(x, y, type = 'l', lty = 2, lwd = 1.5)
    }
  }
  imgInput_crop <- function(tdata) {
    img.name <- attributes(tdata)$img.name
    dim.tdata <- dim(tdata)
    dimcol <- dim.tdata[2]
    dimrow <- dim.tdata[1]
    xleft <- 0
    ybottom <- 0
    xright <- dimcol
    ytop <- dimrow
    par(mar = c(2.5, 2, 2, 0))
    plot(x = c(xleft, xright), y = c(ybottom, ytop), 
      xlim = c(xleft, xright), ylim = c(ybottom, ytop), 
      main = img.name, xlab = "", ylab = "", 
      type = "n", axes = F, cex.main = 1.2)
    axis(1, col = "grey", cex.axis = 1)
    axis(2, col = "grey", cex.axis = 1)
    rasterImage(tdata, xleft, ybottom, 
      xright, ytop, interpolate = FALSE)
    return(tdata)  
  }
  rotateImg <- function(tdata, degree) {
    tdata <- image_rotate(tdata, degree)
    dim.tdata <- image_info(tdata) %>% '['(1,2:3) %>% as.numeric
    attributes(tdata) <- c(attributes(tdata), list(dimt = dim.tdata))
    return(tdata)
  }
  options(shiny.maxRequestSize = 150*(1024^2))
  img.file <- reactiveValues(data = NULL)
  img.file.crop <- reactiveValues(data = NULL)
  img.file.copy <- reactiveValues(data = NULL)
  observeEvent(input$inmethod, {
    img.file$data <- NULL
    img.file.copy$data <- NULL
    img.file.crop$data <- NULL
    img.file.zoom$data <- NULL
    df.loc$data <- NULL
    df.loc$ID <- NULL
    plot1_ranges$x <- NULL
    plot1_ranges$y <- NULL
    plot2_ranges$x <- NULL
    plot2_ranges$y <- NULL
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
  observeEvent(input$buttonrotate, {
    if (!input$inmethod)
      img <- input$select.file["datapath"] %>% as.character
    if (input$inmethod)
      img <- input$enter.path
    img.check1 <- ifelse(length(img) >= 1, TRUE, FALSE)
    img.check2 <- FALSE
    if (img.check1)
      img.check2 <- ifelse(nchar(img) > 1, TRUE, FALSE)
    if (any(!img.check1, !img.check2, is.null(img.file$data))) {
      err.text <- paste('The preview image has not been generated')
      sendSweetAlert(
        session = session, title = "Error", text = err.text, type = "error"
      )
      return()
    }
    degree <- input$rotatede %>% substring(7) %>% as.numeric
    img.file$data <- rotateImg(img.file$data, degree)
    img.file.crop$data <- img.file$data
    img.file.copy$data <- rotateImg(img.file.copy$data, degree)
    img.file.zoom$data <- NULL
    new.dimt <- attributes(img.file$data)[["dimt"]]
    attributes(img.file.copy$data)[["dimt"]] <- new.dimt
    plot1_ranges$x <- NULL
    plot1_ranges$y <- NULL
    plot2_ranges$x <- NULL
    plot2_ranges$y <- NULL
    df.loc$data <- NULL
    df.loc$ID <- NULL
    rw.dataframe$data <- NULL
    updateTextInput(session, "m.line", value = '',
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
  observeEvent(input$buttoninputimage, {
    magick.switch <- input$magick.switch
    if (!input$inmethod) {
      imgf <- input$select.file
      if (is.null(imgf)) {
        err.text <- paste('The image file has not been uploaded')
        sendSweetAlert(
          session = session, title = "Error", text = err.text, type = "error"
        )
        return()
      }
      img <- as.character(imgf["datapath"])
      img.name <- as.character(imgf["name"])
    }
    if (input$inmethod) {
      img <- input$enter.path
      if (img == '') {
        err.text <- paste('The file path has not been entered')
        sendSweetAlert(
          session = session, title = "Error", text = err.text, type = "error"
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
    rw.dataframe$data <- NULL
    #cur.time <- as.character(Sys.time())
    updateTextInput(session, "m.line", value = '',
      label = 'Y-coordinate of the path')
    updateTextInput(session, "tuid", value = '',
      label = 'Series ID')
    updateTextInput(session, "sample.yr", value = '',
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
    img.file.zoom$data <- NULL
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
      err.text <- paste('The preview image have not been generated')
      sendSweetAlert(
        session = session, title = "Error", text = err.text, type = "error"
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
    rw.dataframe$data <- NULL
    img.file.zoom$data <- NULL
    updateTextInput(session, "m.line", value = '',
      label = 'Y-coordinate of the path')
    updateTextInput(session, "tuid", value = '', label = 'Series ID')
    if (!is.null(plot1_brush)) {
      plot1_ranges$x <- c(round(plot1_brush$xmin), round(plot1_brush$xmax))
      plot1_ranges$y <- c(round(plot1_brush$ymin), round(plot1_brush$ymax))
      dimcol <- attributes(img.file$data)[["dimt"]][1]
      dimrow <- attributes(img.file$data)[["dimt"]][2]
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
  img.file.zoom <- reactiveValues(data = NULL)
  zoom.add <- reactiveValues(data = NULL)
  df.loc <- reactiveValues(data = NULL, ID = NULL)
  observeEvent(input$buttoncreatpath, {
    if (is.null(img.file.crop$data)) {
      err.text <- 'Path creation fails because the image has not been plotted'
      sendSweetAlert(
        session = session, title = "Error", text = err.text, type = "error"
      )
      return()
    }
    py <- as.numeric(input$m.line)
    if (is.na(py)) {
      err.text <- 'Please enter a valid y-coordinate of the path'
      sendSweetAlert(
        session = session, title = "Error", text = err.text, type = "error"
      )
      return()
    }
    dpi <- as.numeric(input$dpi)
    if (is.na(dpi)) {
      err.text <- 'Please enter the DPI of the image'
      sendSweetAlert(
        session = session, title = "Error", text = err.text, type = "error"
      )
      return()
    }
    seriesID <- input$tuid
    if (seriesID == '') {
      err.text <- 'Please enter the series ID'
      sendSweetAlert(
        session = session, title = "Error", text = err.text, type = "error"
      )
      return()
    }
    incline <- input$incline
    if(incline){
      h.dis <- as.numeric(input$h.dis)
      incline.cond <- 1
    } else {
      h.dis <- 0
      incline.cond <- 0
    }
    dim.tdata <- image_info(img.file.crop$data) %>% '['(1, 2:3) %>% as.numeric
    dimrow <- dim.tdata[2]
    if (py >= dimrow) {
      err.text <- paste('The Y-coordinate of the path is out of range.',
        'Please type a valid Y-coordinate')
      sendSweetAlert(
        session = session, title = "Error", text = err.text, type = "error"
      )
      return() 
    }
    f.df.loc <- c(dpi, incline.cond, 0, py, h.dis, 0) %>%
      matrix(byrow = T, nrow = 2) %>%
      as.data.frame(stringsAsFactors = F)
    colnames(f.df.loc) <- c('x', 'y', 'z')
    df.loc$data <- f.df.loc
    df.loc$ID <- seriesID
  })
  observeEvent(input$buttoncreatpath2, {
    if (is.null(img.file.crop$data)) {
      err.text <- 'Path creation fails because the image has not been plotted'
      sendSweetAlert(
        session = session, title = "Error", text = err.text, type = "error"
      )
      return()
    }
    py <- as.numeric(input$m.line)
    if (is.na(py)) {
      err.text <- 'Please enter a valid y-coordinate of the path'
      sendSweetAlert(
        session = session, title = "Error", text = err.text, type = "error"
      )
      return()
    }
    dpi <- as.numeric(input$dpi)
    if (is.na(dpi)) {
      err.text <- 'Please enter the DPI of the image'
      sendSweetAlert(
        session = session, title = "Error", text = err.text, type = "error"
      )
      return()
    }
    seriesID <- input$tuid
    if (seriesID == '') {
      err.text <- 'Please enter the series ID'
      sendSweetAlert(
        session = session, title = "Error", text = err.text, type = "error"
      )
      return()
    }
    incline <- input$incline
    if(incline){
      h.dis <- as.numeric(input$h.dis)
      incline.cond <- 1
    } else {
      h.dis <- 0
      incline.cond <- 0
    }
    dim.tdata <- image_info(img.file.crop$data) %>% '['(1, 2:3) %>% as.numeric
    dimrow <- dim.tdata[2]
    if (py >= dimrow) {
      err.text <- paste('The Y-coordinate of the path is out of range.',
        'Please type a valid Y-coordinate')
      sendSweetAlert(
        session = session, title = "Error", text = err.text, type = "error"
      )
      return() 
    }
    f.df.loc <- c(dpi, incline.cond, 0, py, h.dis, 0) %>%
      matrix(byrow = T, nrow = 2) %>%
      as.data.frame(stringsAsFactors = F)
    colnames(f.df.loc) <- c('x', 'y', 'z')
    df.loc$data <- f.df.loc
    df.loc$ID <- seriesID
  })
  observeEvent(input$buttonsubimg, {
    plot2_brush <- input$plot2_brush
    if (is.null(img.file.crop$data)) {
      err.text <- 'The tree ring image has not been plotted'
      sendSweetAlert(
        session = session, title = "Error", text = err.text, type = "error"
      )
      return()
    }
    if (!is.null(plot2_brush$xmin)) {
      plot2_ranges$x <- c(round(plot2_brush$xmin), round(plot2_brush$xmax))
      plot2_ranges$y <- c(round(plot2_brush$ymin), round(plot2_brush$ymax))
      dim.tdata <- image_info(img.file.crop$data) %>% '['(1,2:3) %>% as.numeric
      dimcol <- dim.tdata[1]
      dimrow <- dim.tdata[2]
      if (plot2_ranges$x[1] <= 0) plot2_ranges$x[1] <- 0
      if (plot2_ranges$y[1] <= 0) plot2_ranges$y[1] <- 0
      if (plot2_ranges$x[2] >= dimcol) plot2_ranges$x[2] <- dimcol
      if (plot2_ranges$y[2] >= dimrow) plot2_ranges$y[2] <- dimrow
      xmin <- plot2_ranges$x[1]
      ymin <- plot2_ranges$y[1]
      xmax <- plot2_ranges$x[2]
      ymax <- plot2_ranges$y[2]
      img.range <- paste0(as.character(xmax - xmin), 'x', 
        as.character(ymax - ymin), '+',
        as.character(xmin), '+',
        as.character(dimrow - ymax))
      img.file.zoom$data <- image_crop(img.file.crop$data, img.range)
    } else {
      plot2_ranges$x <- NULL
      plot2_ranges$y <- NULL
      img.file.zoom$data <- NULL
      err.text <- 'You have not selected a part of the image by brushing'
      sendSweetAlert(
        session = session, title = "Error", text = err.text, type = "error"
      )
      return()
    }
  })
  observeEvent(input$plot2_dblclick, {
    plot2_brush <- input$plot2_brush
    if (is.null(img.file.crop$data)) {
      err.text <- 'The tree ring image has not been plotted'
      sendSweetAlert(
        session = session, title = "Error", text = err.text, type = "error"
      )
      return()
    }
    if (!is.null(plot2_brush$xmin)) {
      plot2_ranges$x <- c(round(plot2_brush$xmin), round(plot2_brush$xmax))
      plot2_ranges$y <- c(round(plot2_brush$ymin), round(plot2_brush$ymax))
      dim.tdata <- image_info(img.file.crop$data) %>% '['(1,2:3) %>% as.numeric
      dimcol <- dim.tdata[1]
      dimrow <- dim.tdata[2]
      if (plot2_ranges$x[1] <= 0) plot2_ranges$x[1] <- 0
      if (plot2_ranges$y[1] <= 0) plot2_ranges$y[1] <- 0
      if (plot2_ranges$x[2] >= dimcol) plot2_ranges$x[2] <- dimcol
      if (plot2_ranges$y[2] >= dimrow) plot2_ranges$y[2] <- dimrow
      xmin <- plot2_ranges$x[1]
      ymin <- plot2_ranges$y[1]
      xmax <- plot2_ranges$x[2]
      ymax <- plot2_ranges$y[2]
      img.range <- paste0(as.character(xmax - xmin), 'x', 
        as.character(ymax - ymin), '+',
        as.character(xmin), '+',
        as.character(dimrow - ymax))
      img.file.zoom$data <- image_crop(img.file.crop$data, img.range)
    } else {
      plot2_ranges$x <- NULL
      plot2_ranges$y <- NULL
      img.file.zoom$data <- NULL
      err.text <- 'You have not selected a part of the image by brushing'
      sendSweetAlert(
        session = session, title = "Error", text = err.text, type = "error"
      )
      return()
    }
  })
  observeEvent(input$zoom_dblclick, {
    if (is.null(img.file.zoom$data)) {
      err.text <- 'A zoomed-in image has not been created'
      sendSweetAlert(
        session = session, title = "Error", text = err.text, type = "error"
      )
      return()
    } 
    if (is.null(df.loc$data)) {
      err.text <- paste('You can not add new ring borders',
        'because a path has not been created')
      sendSweetAlert(
        session = session, title = "Error", text = err.text, type = "error"
      )
      return()
    } 
    f.df.loc <- df.loc$data
    plot.arg <- f.df.loc[1:2,]
    incline <- ifelse(plot.arg[1, 2] == 0, FALSE, TRUE)
    py <- plot.arg[2, 1] - plot2_ranges$y[1]
    bor <- input$zoom_dblclick
    bor.x <- bor$x + plot2_ranges$x[1]
    bor.y <- bor$y
    if (!incline) 
      f.df.loc <- rbind(f.df.loc, list(bor.x, bor.y, 0))
    if (incline) {
      if (bor.y == py) {
        err.text <- 'Please click on the upper path or the lower path'
        sendSweetAlert(
          session = session, title = "Error", text = err.text, type = "error"
        )
        return()
      }
      if (bor.y > py) 
        f.df.loc <- rbind(f.df.loc, list(bor.x, bor.y, 1))
      if (bor.y < py) 
        f.df.loc <- rbind(f.df.loc, list(bor.x, bor.y, -1))
    }
    df.loc$data <- f.df.loc
  })
  # plot3_ranges <- reactiveValues(data = NULL)
  observeEvent(input$buttonzoomdel, {
    if (is.null(input$zoom_brush$xmin)) {
      err.text <- 'You have not selected a part of the image by brushing'
      sendSweetAlert(
        session = session, title = "Error", text = err.text, type = "error"
      )
      return()
    } 
    if (is.null(df.loc$data)) {
      err.text <- 'A path has not been created'
      sendSweetAlert(
        session = session, title = "Error", text = err.text, type = "error"
      )
      return()
    } 
    if (nrow(df.loc$data) <= 2) {
      remove.text <- 'Ring border was NOT found along the path'
      sendSweetAlert(
        session = session, title = "Error", text = remove.text, type = "error"
      )
      return()
    } 
    xmin <- input$zoom_brush$xmin
    xmax <- input$zoom_brush$xmax
    ymin <- input$zoom_brush$ymin
    ymax <- input$zoom_brush$ymax
    plot.arg <- df.loc$data[1:2,]
    dpi <- plot.arg[1, 1]
    dp <- dpi/25.4
    incline <- ifelse(plot.arg[1, 2] == 0, FALSE, TRUE)
    py <- plot.arg[2, 1]
    h.dis <- plot.arg[2, 2]
    x.ranges <- df.loc$data$x[-c(1:2)] - plot2_ranges$x[1]
    delete.bor <- which(x.ranges >= xmin & x.ranges <= xmax)
    if (length(delete.bor) == 0) {
      err.text <- 'Ring border was NOT found in the area you selected'
      sendSweetAlert(
        session = session, title = "Error", text = err.text, type = "error"
      )
      return()
    }
    if (incline) {
      number.of.pixels <- round((h.dis/2) * dp)
      py.upper <- py + number.of.pixels
      py.lower <- py - number.of.pixels
      which.line <- df.loc$data$z[-c(1:2)][delete.bor]
      y.value <- ifelse(which.line > 0, 
        py.upper - plot2_ranges$y[1],
        py.lower - plot2_ranges$y[1])
      is.contain <- ymin <= y.value & ymax >= y.value
      if (any(is.contain)) {
        delete.bor <- delete.bor[is.contain] + 2
        df.loc$data <- df.loc$data[-delete.bor,]
      } else {
        err.text <- 'Ring border was NOT found in the area you selected'
        sendSweetAlert(
          session = session, title = "Error", text = err.text, type = "error"
        )
      }
    } else {
      y.value <- py - plot2_ranges$y[1]
      is.contain <- ymin <= y.value & ymax >= y.value
      if (any(is.contain)) {
        delete.bor <- delete.bor[is.contain] + 2
        df.loc$data <- df.loc$data[-delete.bor,]
      } else {
        err.text <- 'Ring border was NOT found in the area you selected'
        sendSweetAlert(
          session = session, title = "Error", text = err.text, type = "error"
        )
      }
    }
  })
  output$pre.img2 <- renderPlot({
    if (is.null(img.file$data)) return()
    imgInput_crop(as.raster(img.file.crop$data))
    sample.yr <- as.numeric(input$sample.yr)
    if (is.na(sample.yr)) return()
    pch <- as.numeric(input$pch)
    bor.color <- input$border.color
    lab.color <- input$label.color
    l.w <- as.numeric(input$linelwd)
    label.cex <- as.numeric(input$label.cex)*0.7
    if (is.null(df.loc$data)) return()
    if (is.null(df.loc$ID)) return()
    f.df.loc <- df.loc$data
    plot.arg <- f.df.loc[1:2,]
    dpi <- plot.arg[1, 1]
    dp <- dpi/25.4
    incline <- ifelse(plot.arg[1, 2] == 0, FALSE, TRUE)
    py <- plot.arg[2, 1]
    h.dis <- plot.arg[2, 2]
    img.name <- paste('Series ID:', df.loc$ID)
    plot.marker(py, incline, dp, sample.yr, h.dis, l.w, bor.color, 
      lab.color, pch, label.cex, f.df.loc, T, img.name)
  })
  output$zoom.img <- renderPlot({
    if (is.null(plot2_ranges$x)) return()
    if (is.null(img.file.zoom$data)) return()
    # if (is.null(img.file.zoom.copy$data)) return()
    imgInput_crop(as.raster(img.file.zoom$data))
    sample.yr <- as.numeric(input$sample.yr)
    if(is.na(sample.yr)) return()
    pch <- as.numeric(input$pch)
    bor.color <- input$border.color
    lab.color <- input$label.color
    l.w <- as.numeric(input$linelwd)
    label.cex <- as.numeric(input$label.cex)
    if(is.null(df.loc$data)) return()
    if(is.null(df.loc$ID)) return()
    f.df.loc <- df.loc$data
    f.df.loc$x[-c(1, 2)] <- f.df.loc$x[-c(1, 2)] - plot2_ranges$x[1]
    plot.arg <- f.df.loc[1:2, ]
    dpi <- plot.arg[1, 1]
    dp <- dpi/25.4
    incline <- ifelse(plot.arg[1, 2] == 0, FALSE, TRUE)
    py <- plot.arg[2, 1] - plot2_ranges$y[1]
    h.dis <- plot.arg[2, 2]
    img.name <- paste('Series ID:', df.loc$ID)
    plot.marker(py, incline, dp, sample.yr, h.dis, l.w, bor.color, 
      lab.color, pch, label.cex, f.df.loc, T, img.name)
  })
  #autoresult <- reactiveValues(data = NULL, text = NULL)
  #icon.value <- reactiveValues(data = 0)
  observeEvent(input$button_run_auto, { 
    if (is.null(input$plot2_brush)) {
      brush.text <- paste('Please select a part of the image by brushing', 
        'before running the automatic measurement')
      sendSweetAlert(
        session = session, title = "Error", text = brush.text, type = "error"
      )
      return()
    }
    if (is.null(df.loc$data)) {
      err.text <- 'A path has not been created'
      sendSweetAlert(
        session = session, title = "Error", text = err.text, type = "error"
      )
      return()
    }
    f.df.loc <- df.loc$data
    brush <- input$plot2_brush
    x1 <- brush$xmin
    x2 <- brush$xmax
    y1 <- brush$ymin
    y2 <- brush$ymax
    isrgb <- input$isrgb
    if (isrgb) {
      RGB <- c(0.299, 0.587, 0.114)
    } else {
      RGB <- strsplit(input$customRGB, ',')[[1]] %>% as.numeric
    }
    plot.arg <- f.df.loc[1:2,]
    dpi <- plot.arg[1, 1]
    dp <- dpi/25.4
    incline <- ifelse(plot.arg[1, 2] == 0, FALSE, TRUE)
    m.line <- plot.arg[2, 1]
    h.dis <- plot.arg[2, 2]
    m.line.upper <- m.line
    m.line.lower <- m.line
    if (incline) {
      number.of.pixels <- round((h.dis/2) * dp)
      m.line.lower <- m.line + number.of.pixels
      m.line.upper <- m.line - number.of.pixels
    }
    img <- img.file.crop$data
    method <- input$method
    linear.warning <- FALSE
    if (m.line.upper <= y1 | m.line.lower >= y2) {
      result.text <- paste('The brushed area does not contain the',
        'path. Please re-brush on the image')
      sendSweetAlert(
        session = session, title = "Error", text = result.text, type = "error"
      )
      return()
    }
    defaultse <- input$defaultse
    if (defaultse) {
      struc.ele1 <- NULL
      struc.ele2 <- NULL
    } else {
      struc.ele1 <- input$struc.ele1
      struc.ele1 <- strsplit(struc.ele1, ',')[[1]] %>% as.numeric
      if (length(struc.ele1) >= 3) {
        err.t <- paste('The rectangular structuring element allows no more',
          'than two non-negative integers. If entering two',
          'integers, the first integer is the width of the',
          'structuring element and the second is height. Use',
          'a comma to separate integers. For example: 15,10')
        sendSweetAlert(
          session = session, title = "Error", text = err.t, type = "error"
        )
        return()
      }
      if (length(struc.ele1) == 0) {
        err.text <- paste('The size of the first structuring',
          'element has not been entered')
        sendSweetAlert(
          session = session, title = "Error", text = err.text, type = "error"
        )
        return()
      }
      if (as.numeric(struc.ele1) %>% is.na %>% any) {
        err.text <- paste('The size of the structuring element',
          'should be non-negative integers')
        sendSweetAlert(
          session = session, title = "Error", text = err.text, type = "error"
        )
        return()
      }
      if (length(struc.ele1) == 1)
        struc.ele1 <- c(struc.ele1, struc.ele1)
      struc.ele2 <- input$struc.ele2
      struc.ele2 <- strsplit(struc.ele2, ',')[[1]] %>% as.numeric
      if (length(struc.ele2) >= 3) {
        err.t <- paste('The rectangular structuring element allows no more',
          'than two non-negative integers. If entering two',
          'integers, the first integer is the width of the',
          'structuring element and the second is height. Use',
          'a comma to separate integers. For example: 15,10')
        sendSweetAlert(
          session = session, title = "Error", text = err.t, type = "error"
        )
        return()
      }
      if (length(struc.ele2) == 0) {
        err.text <- paste('The size of the second structuring',
          'element has not been entered')
        sendSweetAlert(
          session = session, title = "Error", text = err.text, type = "error"
        )
        return()
      }
      if (as.numeric(struc.ele2) %>% is.na %>% any) {
        err.text <- paste('The size of the structuring element',
          'should be non-negative integers')
        sendSweetAlert(
          session = session, title = "Error", text = err.text, type = "error"
        )
        return()
      }
      if(length(struc.ele2) == 1)
        struc.ele2 <- c(struc.ele2, struc.ele2)
    }
    if (method == 'watershed') {
      if(input$watershed.threshold == 'custom.waterthr'){
        watershed.threshold <- input$watershed.threshold2
      } else {
        watershed.threshold <- input$watershed.threshold
      }
      watershed.adjust <- input$watershed.adjust
      df.loc$data <- automatic.det(img, incline, method, h.dis, dpi, m.line, 
        RGB, x1, x2, y1, y2, plot.arg,
        watershed.threshold, watershed.adjust, 
        struc.ele1, struc.ele2)
    }
    if (method == "canny") {
      default.canny <- input$defaultcanny
      canny.t1 <- as.numeric(input$canny.t1)
      canny.t2 <- as.numeric(input$canny.t2)
      canny.adjust <- input$canny.adjust
      canny.smoothing <- input$canny.smoothing
      df.loc$data <- automatic.det(
        img, incline, method, h.dis, dpi, m.line, RGB, 
        x1, x2, y1, y2, plot.arg, watershed.threshold, 
        watershed.adjust, struc.ele1, struc.ele2, default.canny,
        canny.t1, canny.t2, canny.adjust, canny.smoothing)
    }   
    if (method == "lineardetect") {
      origin <- as.numeric(input$origin)
      py.ld <- round((y1 + y2)/2)
      updateTextInput(session, "m.line", value = as.character(round(py.ld)))
      f.df.loc <- automatic.det(img, incline, method, h.dis, dpi, m.line, 
        RGB, x1, x2, y1, y2, plot.arg, origin = origin)
      if(incline){
        updateCheckboxInput(session, 'incline', 'Inclined tree rings', F)
        f.df.loc[1, 2] <- FALSE
        linear.warning <- TRUE
      }
      f.df.loc[2, 1] <- py.ld 
      df.loc$data <- f.df.loc
    }
    number.border <- nrow(df.loc$data) - 2
    if (number.border == 0) {
      rt <- 'Ring border was NOT detected'
      sendSweetAlert(
        session = session, title = "Error", text = rt, type = "error"
      )
    } else {
      rt <- paste(number.border, 'boreders were detected')
      sendSweetAlert(
        session = session, title = "Finished", text = rt, type = "success"
      )
    }  
    if (linear.warning) {
      rt <- paste('If you use the linear detection, don\'t',
        'tick the checkbox "Inclined tree rings".',
        'This checkbox has been automatically corrected.')
      sendSweetAlert(
        session = session, title = "TIPS", text = rt, type = "warning"
      )
    }
  })
  observeEvent(input$buttonrcm, {
    # plot1_ranges$x <- NULL
    # plot1_ranges$y <- NULL
    plot2_ranges$x <- NULL
    plot2_ranges$y <- NULL
    df.loc$data <- NULL
    df.loc$ID <- NULL
    updateTextInput(session, "m.line", value = '',
      label = 'Y-coordinate of the path')
    rt <- paste('The existing path and borders have been',
      'removed. You need to re-create a path')
    sendSweetAlert(
      session = session, title = "Success", text = rt, type = "success"
    )
  })
  observeEvent(input$buttonrcm2, {
    plot2_ranges$x <- NULL
    plot2_ranges$y <- NULL
    df.loc$data <- NULL
    df.loc$ID <- NULL
    updateTextInput(session, "m.line", value = '',
      label = 'Y-coordinate of the path')
    rt <- paste('The existing path and borders have been',
      'removed. You need to re-create a path')
    sendSweetAlert(
      session = session, title = "Success", text = rt, type = "success"
    )
  })
  observeEvent(input$button_del, { 
    if (is.null(df.loc$data)) {
      rt <- paste('You can not remove ring borders because',
        'the path has not been created.')
      sendSweetAlert(
        session = session, title = "Error", text = rt, type = "error"
      )
      return()
    }
    f.df.loc <- df.loc$data
    plot.arg <- f.df.loc[1:2, ]
    dpi <- plot.arg[1, 1]
    dp <- dpi/25.4
    incline <- ifelse(plot.arg[1, 2] == 0, FALSE, TRUE)
    m.line <- plot.arg[2, 1]
    h.dis <- plot.arg[2, 2]
    if (nrow(f.df.loc) < 3) {
      rt <- 'Ring border was not found along the path'
      sendSweetAlert(
        session = session, title = "Error", text = rt, type = "error"
      )
      return()
    }
    bx <- f.df.loc$x[-c(1, 2)]
    where.bx <- f.df.loc$z[-c(1, 2)]
    where.bx <- where.bx[order(bx)]
    bx <- sort(bx)
    if (incline) {
      if (input$del.u == '' & input$del.l == '') {
        rt <- 'Please enter at least one border number'
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
      up <- which(where.bx > 0)
      lenup <- length(up)
      bx.u <- bx[up]
      if (lenup >= 1 & input$del.u != '') {
        if (max(del.u) <= lenup) {
          bx.u <- bx.u[-del.u]
        } else {
          rt <- 'The border number you entered did not exist'
          sendSweetAlert(
            session = session, title = "Error", text = rt, type = "error"
          )
          return()
        }
      }
      lower <- which(where.bx < 0)
      lenlo <- length(lower)
      bx.l <- bx[lower]
      if (lenlo >= 1 & input$del.l != '') {
        if (max(del.l) <= lenlo) {
          bx.l <- bx.l[-del.l]
        } else {
          rt <- 'The border number you entered did not exist'
          sendSweetAlert(
            session = session, title = "Error", text = rt, type = "error"
          )
          return()
        }
      }
      df.u <- data.frame(x = bx.u, y = m.line, z = 1) 
      df.l <- data.frame(x = bx.l, y = m.line, z = -1)
      df.loc$data <- rbind(plot.arg, df.u, df.l)
      updateTextInput(session, "del.u",
        label = 'Border number in the upper portion',
        value = '')
      updateTextInput(session, "del.l",
        label = 'Border number in the lower portion',
        value = '')
    } else { 
      if (input$del == '') {
        rt <- 'Please enter at least one border number'
        sendSweetAlert(
          session = session, title = "Error", text = rt, type = "error"
        )
        return()
      }
      del <- strsplit(input$del, ",")[[1]] %>% as.numeric
      if (max(del) <= length(bx)) {
        bx <- bx[-del]
        df <- data.frame(x = bx, y = m.line, z = 0)
        df.loc$data <- rbind(plot.arg, df)
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
    if (nrow(df.loc$data) <= 3) {
      error.text <- paste('A minimum of two ring borders on each path',
        'was required to generate a ring-width series')
      sendSweetAlert(
        session = session, title = "Error", text = error.text, type = "error"
      )
      return()
    } 
    sample.yr <- as.numeric(input$sample.yr)
    if (is.na(sample.yr)) {
      error.text <- paste('Please check the argument \'Sampling year\' ')
      sendSweetAlert(
        session = session, title = "Error", text = error.text, type = "error"
      )
      return()
    }
    plot.arg <- df.loc$data[1:2, ]
    dpi <- plot.arg[1, 1]
    dp <- dpi/25.4
    incline <- ifelse(plot.arg[1, 2] == 0, FALSE, TRUE)
    py <- plot.arg[2, 1]
    h.dis <- plot.arg[2, 2]
    if (incline) {
      incline.cond <- df.loc$data$z[-c(1:2)] %>% table %>% as.numeric
      if (length(incline.cond) == 1) {
        rt <- paste('A minimum of two ring borders on each path',
          'was required to generate a ring-width series')
        sendSweetAlert(
          session = session, title = "Error", text = rt, type = "error"
        )
        return()
      }
      if (all(incline.cond >= 2) & incline.cond[1] == incline.cond[2]) {
        rw.dataframe$data <- f.rw(df.loc$data, sample.yr, 
          incline, py, dpi, h.dis)
      } else {
        if (any(incline.cond < 2)) {
          err.text <- paste('A minimum of two ring borders on each path',
            'was required to generate a ring-width series')
          sendSweetAlert(
            session = session, title = "Error", text = err.text, type = "error"
          )
        } else {
          err.text <-  paste("If you tick the checkbox \"Inclined tree",
            "rings\", the upper and lower paths should",
            "have the same number of ring borders.")
          sendSweetAlert(
            session = session, title = "Error", text = err.text, type = "error"
          )
        }
      }   
    } else {
      rw.dataframe$data <- f.rw(df.loc$data, sample.yr, 
        incline, py, dpi, h.dis)
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
        img.name <- 'Download Unavailable'
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
        rt <- 'Make sure you have added ring borders to the image'
        sendSweetAlert(
          session = session, title = "Error", text = rt, type = "error"
        )
        return()
      } 
      sample.yr <- as.numeric(input$sample.yr)
      if (is.na(sample.yr)) {
        error.text <- 'Please check the argument \'Sampling year\''
        sendSweetAlert(
          session = session, title = "Error", text = error.text, type = "error"
        )
        return()
      }
      plot.arg <- df.loc$data[1:2, ]
      dpi <- plot.arg[1, 1]
      # dp <- dpi/25.4
      incline <- ifelse(plot.arg[1, 2] == 0, FALSE, TRUE)
      py <- plot.arg[2, 1]
      h.dis <- plot.arg[2, 2]
      if (nrow(df.loc$data) <= 3) {
        error.text <- paste('A minimum of two ring borders on each path',
          'was required to generate a ring-width series')
        sendSweetAlert(
          session = session, title = "Error", text = error.text, type = "error"
        )
        return()
      } 
      if (incline) {
        incline.cond <- df.loc$data$z[-c(1:2)] %>% table %>% as.numeric
        if (length(incline.cond) == 1) {
          rt <- paste('A minimum of two ring borders on each path',
            'was required to generate a ring-width series')
          sendSweetAlert(
            session = session, title = "Error", text = rt, type = "error"
          )
          return()
        }
        if (all(incline.cond >= 2) & incline.cond[1] == incline.cond[2]) {
          df.rw <- f.rw(df.loc$data, sample.yr, incline, py, dpi, h.dis)
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
            rt <- paste("If incline = TRUE, the upper and lower paths", 
              "should have the same number of ring borders")
            sendSweetAlert(
              session = session, title = "Error", text = rt, type = "error"
            )
            return()
          }
        }
      } else {
        df.rw <- f.rw(df.loc$data, sample.yr, incline, py, dpi, h.dis)
        write.csv(df.rw, filename, quote = FALSE, na = '--')
      } 
    },
    contentType = 'csv'
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
      if (is.null(img.file$data)) {
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
        rt <- 'The series ID has not been entered'
        sendSweetAlert(
          session = session, title = "Error", text = rt, type = "error"
        )
        return()
      }
      if (is.null(df.loc$data)) {
        error.text <- 'Make sure you have added ring borders to the image'
        sendSweetAlert(
          session = session, title = "Error", text = error.text, type = "error"
        )
        return()
      } 
      sample.yr <- as.numeric(input$sample.yr)
      if (is.na(sample.yr)) {
        error.text <- paste('Please check the argument \'Sampling year\'')
        sendSweetAlert(
          session = session, title = "Error", text = error.text, type = "error"
        )
        return()
      }
      if (nrow(df.loc$data) <= 3) {
        error.text <- paste('A minimum of two ring borders on each path ',
          'was required to generate a ring-width series')
        sendSweetAlert(
          session = session, title = "Error", text = error.text, type = "error"
        )
      } 
      plot.arg <- df.loc$data[1:2, ]
      dpi <- plot.arg[1, 1]
      # dp <- dpi/25.4
      incline <- ifelse(plot.arg[1, 2] == 0, FALSE, TRUE)
      py <- plot.arg[2, 1]
      h.dis <- plot.arg[2, 2]
      df.rw <- NULL
      if (incline) {
        incline.cond <- df.loc$data$z[-c(1:2)] %>% table %>% as.numeric
        if (length(incline.cond) == 1) {
          rt <- paste('A minimum of two ring borders on each path',
            'was required to generate a ring-width series')
          sendSweetAlert(
            session = session, title = "Error", text = rt, type = "error"
          )
          return()
        }
        if (all(incline.cond >= 2) & incline.cond[1] == incline.cond[2]) {
          df.rw <- f.rw(df.loc$data, sample.yr, incline, py, dpi, h.dis)
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
        df.rw <- f.rw(df.loc$data, sample.yr, incline, py, dpi, h.dis)
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


# Run the application
shinyApp(ui = createUI(), server = createServer)
