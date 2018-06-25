

foo = 'aws s3 cp ~/projects/fftiers/out/current/png/weekly-QB.png s3://fftiers/out/weekly-QB.png'
aws1 = 'aws s3 cp '
aws2 = ' s3://fftiers/out/'

awssync = 'aws s3 sync '

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

def set_path():
	user = os.popen('whoami').read()[:-1]
	if user == 'bchen':
		mypath = '/Users/bchen/projects/fftiers/out/current/'
	elif user == 'borischen':
		mypath = '/Users/borischen/projects/fftiers/out/current/'
	else:	
		mypath = '/home/ubuntu/projects/fftiers/out/'
	return mypath

mypath = set_path()
files_to_push = absoluteFilePaths(mypath)
file_list = FilesPaths(mypath)

for i in range(0,len(files_to_push)):
	exeline = aws1 + files_to_push[i] + aws2 + file_list[i]
	os.system(exeline)
