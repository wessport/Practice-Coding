import numpy as np
import sys
import math as ma

def writeASCII(nc,nr,cs,nv,xll,yll,grid,filename):
	fg = open(filename,'w')
	s2w = "ncols         " + str(nc) + ' \n'
	fg.write(s2w)
	s2w = 'nrows         ' + str(nr) + ' \n'
	fg.write(s2w)
	s2w = 'xllcorner     ' + str(xll) + ' \n'
	fg.write(s2w)
	s2w = 'yllcorner     ' + str(yll) + ' \n'
	fg.write(s2w)
	s2w = 'cellsize      ' + str(cs) + ' \n'
	fg.write(s2w)
	s2w = 'NODATA_value  ' + str(nv) + ' \n'
	fg.write(s2w)
	for r in range(nr):
		for c in range(nc):
			s2w = str(grid[r,c]) + ' '
			fg.write(s2w)
		fg.write('\n')
	fg.close()

def readASCII(fileName,flag):
	f = open(fileName,'r')
	strtmp = f.readline()  # number of columns
	strtmp2 = strtmp.strip()
	lsttmp = strtmp2.split()
	ncols = int(lsttmp[1])

	strtmp = f.readline()  # number of rows
	strtmp2 = strtmp.strip()
	lsttmp = strtmp2.split()
	nrows = int(lsttmp[1])

	strtmp = f.readline()  # xllcorner
	strtmp2 = strtmp.strip()
	lsttmp = strtmp2.split()
	xllcorner = float(lsttmp[1])

	strtmp = f.readline()  # yllcorner
	strtmp2 = strtmp.strip()
	lsttmp = strtmp2.split()
	yllcorner = float(lsttmp[1])

	strtmp = f.readline()  # number of rows
	strtmp2 = strtmp.strip()
	lsttmp = strtmp2.split()
	cellSize = float(lsttmp[1])

	strtmp = f.readline()  # number of rows
	strtmp2 = strtmp.strip()
	lsttmp = strtmp2.split()
	nullVal= float(lsttmp[1])

	f.close()
	if (flag ==1):
		data = np.genfromtxt(fileName, dtype=np.int,delimiter=" ", skip_header=6)
	if (flag ==2):
		data = np.genfromtxt(fileName, dtype=np.float,delimiter=" ", skip_header=6)
	else:
		data = np.genfromtxt(fileName, dtype=np.int,delimiter=" ", skip_header=6)

	return ncols,nrows,cellSize,nullVal, xllcorner, yllcorner, data

# get inputfiles

demFile = sys.argv[1]
linFile = sys.argv[2]
ptsFile = sys.argv[3]
mdmFile = sys.argv[4]

sys.stdout.write('Original DEM file             : ' + demFile + '\n')
sys.stdout.write('Culvert line file             : ' + linFile + '\n')
sys.stdout.write('Culvert point file            : ' + ptsFile + '\n')
sys.stdout.write('Modified DEM file             : ' + mdmFile + '\n')

sys.stdout.write('Reading DEM file              : ')
ncols,nrows,cellSize,nullVal, xllcorner, yllcorner, demArray = readASCII(demFile,2)
sys.stdout.write('DONE\n')

sys.stdout.write('Reading culvert line file     : ')
ncols,nrows,cellSize,nullVal, xllcorner, yllcorner, culArray = readASCII(linFile,1)
sys.stdout.write('DONE\n')

sys.stdout.write('Reading culvert point file    : ')
ncols,nrows,cellSize,nullVal, xllcorner, yllcorner, cptArray = readASCII(ptsFile,1)
sys.stdout.write('DONE\n')

# go over the data
modArray = np.copy(demArray)
edges = {}

for r in range(nrows):
	for c in range(ncols):
		val = cptArray[r,c]
		if (val > 0):
			if val in edges:
				u = edges[val]
				u.append(r)
				u.append(c)
				edges[val] = u
				u = None
			else:
				edges.update({val:[r,c]})
#print edges

for r in range(nrows):
	for c in range(ncols):
		val = culArray[r,c]

		if (val > 0):
			locVal = edges[val]
			if (len(locVal) == 4):
				ri = locVal[0]
				ci = locVal[1]
				rf = locVal[2]
				cf = locVal[3]
				d = ma.sqrt((ri-rf)**2 + (ci-cf)**2)
				d2 = ma.sqrt((ri-r)**2 + (ci-c)**2)
				if (d2 > 0.001):
					e = [0,d]
					f = [demArray[ri,ci],demArray[rf,cf]]
					v = np.interp(d2,e,f)
					modArray[r,c] = v
#					if (val == 82):
#						print d,d2,e,f,v,ri,r,ci,c,rf,cf,demArray[rf,cf]
#print edges
# write the results
writeASCII(ncols,nrows,cellSize,nullVal,xllcorner,yllcorner,modArray,mdmFile)


# clean memory
demArray = None
culArray = None
cptArray = None
tmpArray = None
modArray = None
