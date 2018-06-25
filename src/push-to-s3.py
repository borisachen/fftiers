

foo = 'aws s3 cp ~/projects/fftiers/out/current/png/weekly-QB.png s3://fftiers/out/weekly-QB.png'
aws1 = 'aws s3 cp '
aws2 = ' s3://fftiers/out/'

import os

def absoluteFilePaths(directory):
	ans=[]
	for dirpath,_,filenames in os.walk(directory):
		for f in filenames:
			ans.append(os.path.abspath(os.path.join(dirpath, f)))
	return ans


def FilesPaths(directory):
	ans=[]
	for dirpath,_,filenames in os.walk(directory):
		for f in filenames:
			ans.append(f)
	return ans

mypath = '/home/ubuntu/projects/fftiers/out/'
mypath = '/Users/borischen/projects/fftiers/out/current/'
mypath = '/Users/bchen/projects/fftiers/out/current/'
files_to_push = absoluteFilePaths(mypath)
file_list = FilesPaths(mypath)
"""
files_to_push2 = []
file_list2 = []
for f in files_to_push:
	if not ('csv' in f and 'ALL' not in f):
		files_to_push2.append(f)

for f in file_list:
	if not ('csv' in f and 'ALL' not in f):
		file_list2.append(f)
"""

for i in range(0,len(files_to_push)):
	exeline = aws1 + files_to_push[i] + aws2 + file_list[i]
	os.system(exeline)

