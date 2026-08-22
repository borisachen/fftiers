
import argparse
import os
import subprocess

S3_PREFIX = 's3://fftiers/out/'
DEFAULT_PROFILE = 'personal'
UPLOAD_SUBDIRS = ('png', 'csv', 'txt')


def repo_root():
	return os.path.dirname(os.path.dirname(os.path.abspath(__file__)))


def current_out_dir():
	return os.path.join(repo_root(), 'out', 'current')


def profile_exists(profile_name):
	credentials = os.path.expanduser('~/.aws/credentials')
	config = os.path.expanduser('~/.aws/config')
	profile_headers = {'[%s]' % profile_name, '[profile %s]' % profile_name}
	for path in (credentials, config):
		if not os.path.isfile(path):
			continue
		with open(path, 'r') as handle:
			for line in handle:
				if line.strip() in profile_headers:
					return True
	return False


def configure_aws_profile():
	if os.environ.get('AWS_PROFILE'):
		return os.environ['AWS_PROFILE']
	if profile_exists(DEFAULT_PROFILE):
		os.environ['AWS_PROFILE'] = DEFAULT_PROFILE
		return DEFAULT_PROFILE
	return None


def collect_uploads(base_dir):
	uploads = []
	for subdir in UPLOAD_SUBDIRS:
		dir_path = os.path.join(base_dir, subdir)
		if not os.path.isdir(dir_path):
			raise SystemExit('Missing upload directory: %s' % dir_path)
		files = [
			name for name in os.listdir(dir_path)
			if os.path.isfile(os.path.join(dir_path, name)) and not name.startswith('.')
		]
		if not files:
			raise SystemExit('No files to upload in: %s' % dir_path)
		for name in sorted(files):
			uploads.append((
				os.path.abspath(os.path.join(dir_path, name)),
				S3_PREFIX + name,
			))
	return uploads


def main():
	parser = argparse.ArgumentParser(description='Upload fftiers out/current assets to S3')
	parser.add_argument('--dry-run', action='store_true', help='Print uploads without calling aws s3 cp')
	args = parser.parse_args()

	profile = configure_aws_profile()
	if profile:
		print('Using AWS profile: %s' % profile, flush=True)
	else:
		print('Using default Workbench AWS credentials', flush=True)

	uploads = collect_uploads(current_out_dir())
	for local_path, dest in uploads:
		if args.dry_run:
			print('DRY RUN: %s -> %s' % (local_path, dest))
		else:
			print('Uploading %s -> %s' % (local_path, dest))
			subprocess.check_call(['aws', 's3', 'cp', local_path, dest])

	if args.dry_run:
		print('DRY RUN: %d file(s) would upload' % len(uploads), flush=True)


if __name__ == '__main__':
	main()
