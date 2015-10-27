import os 

cmd1 = 'Rscript /home/ubuntu/projects/fftiers/src/main.R t'
os.system(cmd1)

cmd2 = 'python /home/ubuntu/projects/fftiers/src/push-to-s3.py'
os.system(cmd2)

