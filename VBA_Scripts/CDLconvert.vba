Function landuse(CDL_value)
    If CDL_value = 1 Then
    landuse = "C"
    ElseIf CDL_value = 2 Or CDL_value = 238 Then
    landuse = "Co"
    ElseIf CDL_value = 3 Then
    landuse = "R"
    ElseIf CDL_value = 5 Then
    landuse = "S"
    ElseIf CDL_value = 24 Then
    landuse = "Ww"
    ElseIf CDL_value = 26 Then
    landuse = "Wws"
    ElseIf CDL_value = 61 Or CDL_value = 36 Or CDL_value = 37 Or CDL_value = 152 Or CDL_value = 176 Then
    landuse = "Crp"
    ElseIf CDL_value = 63 Or CDL_value = 190 Then
    landuse = "F"
    ElseIf CDL_value = 81 Or CDL_value = 82 Or CDL_value = 121 Or CDL_value = 122 Or CDL_value = 123 Or CDL_value = 124 Then
    landuse = "D"
    ElseIf CDL_value = 4 Or CDL_value = 6 Or CDL_value = 10 Or CDL_value = 21 Or CDL_value = 22 Or CDL_value = 23 Or CDL_value = 25 Or CDL_value = 27 Or CDL_value = 28 Or CDL_value = 29 Or CDL_value = 31 Or CDL_value = 41 Or CDL_value = 42 Or CDL_value = 43 Or CDL_value = 44 Or CDL_value = 59 Or CDL_value = 70 Or CDL_value = 71 Or CDL_value = 74 Or CDL_value = 236 Or CDL_value = 241 Then
    landuse = "Oc"
    ElseIf CDL_value = 83 Or CDL_value = 87 Or CDL_value = 92 Or CDL_value = 111 Or CDL_value = 195 Then
    landuse = "W"
    End If
End Function
