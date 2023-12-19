import os 

def set_path():
	user = os.popen('whoami').read()[:-1]
	if user == 'bchen':
		mypath = '/Users/bchen/projects/fftiers'
	elif user == 'borischen':
		mypath = '/Users/borischen/projects/fftiers'
	else:	
		mypath = '/home/ubuntu/projects/fftiers'
	return mypath

head_path = set_path()

cmd1 = 'Rscript %s/src/main.R t' % head_path
os.system(cmd1)

# cmd2 = 'python %s/src/push-to-s3.py' % head_path
# os.system(cmd2)

cmd3 = 'python3 %s/src/push-to-s3.py' % head_path
os.system(cmd3)

