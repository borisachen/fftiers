import os

def set_path():
	return os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

head_path = set_path()

cmd1 = 'Rscript %s/src/main.R t' % head_path
os.system(cmd1)

# cmd2 = 'python %s/src/push-to-s3.py' % head_path
# os.system(cmd2)

cmd3 = 'python3 %s/src/push-to-s3.py' % head_path
os.system(cmd3)

