# To open your selected hyperlinks of a range at once, the following VBA code can help you, please do as this:

1. Hold down the ALT + F11 keys, and it opens the Microsoft Visual Basic for Applications window.

2. Click Insert > Module, and paste the following code in the Module Window.

Sub OpenHyperLinks()
'Update 20141124
    Dim xHyperlink As Hyperlink
    Dim WorkRng As Range
    On Error Resume Next
    xTitleId = "KutoolsforExcel"
    Set WorkRng = Application.Selection
    Set WorkRng = Application.InputBox("Range", xTitleId, WorkRng.Address, Type:=8)
    For Each xHyperlink In WorkRng.Hyperlinks
        xHyperlink.Follow
    Next
End Sub

3. Then press F5 key to run this code, and a prompt box will pop out to remind you to select a range including hyperlinks that you want to open at once time
