app <- ShinyDriver$new("../")
app$snapshotInit("run_detection2")

## create two paths in this test

app$setInputs(sidebarCollapsed = FALSE)
# Input 'plot2_brush' was set, but doesn't have an input binding.
# Input 'zoom_brush' was set, but doesn't have an input binding.
app$uploadFile(selectfile = system.file('001.png', package = 'MtreeRing'))
app$setInputs(buttoninputimage = "click")
app$setInputs(tuid = "940220")
app$setInputs(dpi = "1200")
app$setInputs(sample_yr = "2015")
app$setInputs(m_line = "90")
app$setInputs(method = 'watershed')
app$setInputs(incline = TRUE)
app$setInputs(buttoncreatpath2 = "click")
# Input 'plot2_brush' was set, but doesn't have an input binding.
app$setInputs(plot2_brush = list(xmin = 10, xmax = 2333, ymin = 40, ymax = 140),allowInputNoBinding_ = TRUE)
app$setInputs(button_run_auto = "click")
app$snapshot()
