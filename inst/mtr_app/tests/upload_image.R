app <- ShinyDriver$new("../")
app$snapshotInit("upload_image")

app$setInputs(sidebarCollapsed = FALSE)
# Input 'plot1_brush' was set, but doesn't have an input binding.
app$uploadFile(selectfile = system.file('001.png', package = 'MtreeRing'))
app$setInputs(buttoninputimage = "click")
app$snapshot()
