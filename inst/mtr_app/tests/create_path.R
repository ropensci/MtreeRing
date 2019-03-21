app <- ShinyDriver$new("../")
app$snapshotInit("create_path")

app$setInputs(sidebarCollapsed = FALSE)
# Input 'plot2_brush' was set, but doesn't have an input binding.
# Input 'zoom_brush' was set, but doesn't have an input binding.
app$uploadFile(selectfile = system.file('001.png', package = 'MtreeRing'))
app$setInputs(buttoninputimage = "click")
app$snapshot()
app$setInputs(tuid = "940220")
app$setInputs(dpi = "1200")
app$setInputs(sample_yr = "2015")
app$setInputs(m_line = "201")
app$setInputs(m_line = "90")
app$snapshot()
app$setInputs(buttoncreatpath2 = "click")
# Input 'plot2_brush' was set, but doesn't have an input binding.
app$snapshot()
app$setInputs(button_run_auto = "click")
