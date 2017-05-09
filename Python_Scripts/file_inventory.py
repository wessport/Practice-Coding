# Script for gathering landsat metadata

# C:\Python27\ArcGISx6410.3\python.exe file_inventory.py

in_file = open("files.txt",'r')

out_file = open("file_inventory.csv",'w')

headers = 'Landsat Mission' + ',' + 'Sensor' + ',' + 'Path' + ',' + 'Row' + ',' + 'Year' + ',' + 'Day of Year' +'\n'

out_file.write(headers)

for line in in_file:
    s = str(line)
    landsat = s[0] + s[2]
    sensor = s[1]
    path = s[3:6]
    row = s[6:9]
    year = s[9:13]
    DoY = s[13:16]

    out_string = str(landsat) + ',' + str(sensor) + ',' + str(path) + ',' + str(row) + ',' + str(year) + ',' + str(DoY) + '\n'
    out_file.write(out_string)

in_file.close()
out_file.close()
