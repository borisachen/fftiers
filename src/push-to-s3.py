
import os
import subprocess

S3_PREFIX = 's3://fftiers/out/'
DEFAULT_PROFILE = 'personal'


def set_path():
	return os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), 'out', 'current')


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


def absolute_file_paths(directory):
	paths = []
	for dirpath, _, filenames in os.walk(directory):
		for filename in filenames:
			paths.append(os.path.abspath(os.path.join(dirpath, filename)))
	return paths


def file_names(directory):
	names = []
	for dirpath, _, filenames in os.walk(directory):
		for filename in filenames:
			names.append(filename)
	return names


def main():
	profile = configure_aws_profile()
	if profile:
		print('Using AWS profile: %s' % profile, flush=True)
	else:
		print('Using default Workbench AWS credentials', flush=True)

	mypath = set_path()
	files_to_push = absolute_file_paths(mypath)
	file_list = file_names(mypath)

	for i in range(len(files_to_push)):
		dest = S3_PREFIX + file_list[i]
		print('Uploading %s -> %s' % (files_to_push[i], dest))
		subprocess.check_call(['aws', 's3', 'cp', files_to_push[i], dest])


if __name__ == '__main__':
	main()
