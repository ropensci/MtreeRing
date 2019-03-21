app <- ShinyDriver$new("../")
app$snapshotInit("run_detection")

app$setInputs(sidebarCollapsed = FALSE)
# Input 'plot1_brush' was set, but doesn't have an input binding.
app$uploadFile(selectfile = system.file('001.png', package = 'MtreeRing')) 
app$setInputs(buttoninputimage = "click")
app$setInputs(tuid = "940220")
app$setInputs(dpi = "1")
app$setInputs(dpi = "1200")
app$setInputs(sample_yr = "2012")
app$setInputs(m_line = "100")
app$setInputs(buttoncreatpath2 = "click")
app$snapshot()
# Input 'plot2_brush' was set, but doesn't have an input binding.
app$snapshot()
# Input 'plot2_brush' was set, but doesn't have an input binding.
app$snapshot()
app$setInputs(button_run_auto = "click")
app$snapshot()
