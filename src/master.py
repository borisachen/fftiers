import argparse
import os
import subprocess
import sys


def repo_root():
	return os.path.dirname(os.path.dirname(os.path.abspath(__file__)))


def main():
	parser = argparse.ArgumentParser(description='Run fftiers generate then S3 push')
	parser.add_argument('--dry-run', action='store_true', help='Pass --dry-run to push-to-s3.py')
	args = parser.parse_args()

	root = repo_root()
	subprocess.check_call(['Rscript', os.path.join(root, 'src', 'main.R'), 't'], cwd=root)

	push_cmd = [sys.executable, os.path.join(root, 'src', 'push-to-s3.py')]
	if args.dry_run:
		push_cmd.append('--dry-run')
	subprocess.check_call(push_cmd, cwd=root)


if __name__ == '__main__':
	main()
