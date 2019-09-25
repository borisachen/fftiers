import os
import json
import argparse
import datetime

def perform_session_download(year, position, week, scoring, json_path):
	"""
	year = '2019'
	position = 'RB'
	week = '9'
	scoring = 'STD'
	api_key = '7B7tSChOlX7qQ10DjsRX44Dvah6S1HwL5bWU0mbp'
	json_path = '/Users/borischen/projects/fftiers/test_rb.json'
	"""
	api_key = getAPIKey()
	filters = '64:113:120:125:127:317:406:534'
	day_of_week = datetime.datetime.today().weekday()
	make_path()

	if False:
		curl_str = """
		curl "https://api.fantasypros.com/public/v2/json/nfl/{}/consensus-rankings?position={}&week={}&scoring={}&filters={}" -H "x-api-key: {}" > {}	
		""".format(year, position, week, scoring, filters, api_key, json_path)
	
	if True:
	#if (day_of_week == 1) or (day_of_week == 2) or (week == 0):
		curl_str = """
		curl "https://api.fantasypros.com/public/v2/json/nfl/{}/consensus-rankings?position={}&week={}&scoring={}" -H "x-api-key: {}" > {}	
		""".format(year, position, week, scoring, api_key, json_path)
	
	print(curl_str)
	os.system(curl_str)

def make_path():
	try:
		os.system('mkdir -p /Users/bchen/projects/fftiers/dat/2019/')
	except:
		pass

def playerToRow(player, week):
	try:
		p = player
		r = []
		r = r + [str(p['rank_ecr'])]
		r = r + [p['player_name']]
		if week == 0:
			r = r + [p['player_positions']]
		if week >= 1:
			r = r + [p['player_opponent']]
		r = r + [p['rank_min']]
		r = r + [p['rank_max']]
		r = r + [p['rank_ave']]
		r = r + [p['rank_std']]
		res = ",".join(r)
		return res
	except:
		return ''


def convertJsonToCsv(json_path, out_csv, week):
	with open(json_path) as data_file:    
		data = json.load(data_file)
	rows = []
	if week == 0:
		rows.append("Rank,Player,Position,Best,Worst,Avg,Std Dev")
	else:
		rows.append("Rank,Player,Opp,Best,Worst,Avg,Std Dev")
	for player in data['players']:
		pr = playerToRow(player, week)
		if pr != '':
			rows.append(pr)
	fout = open(out_csv, 'w')
	for item in rows:
		fout.write("%s\n" % item)
	fout.close()


def getAPIKey():
	with open('api_key.py', 'r') as f:
		lines = f.readlines()
	key = lines[0][:-1]
	return key


if __name__ == "__main__":
	"""
	python /Users/borischen/projects/fftiers/src/fp_dl.py
	python /home/ubuntu/projects/fftiers/src/fp_dl.py 
	"""
	parser = argparse.ArgumentParser("FantasyPros clustering program")
	parser.add_argument('-j', dest='json_path', help="JSON Destination", required=True)
	parser.add_argument('-c', dest='csv_file_name', help="CSV Destination", required=True)
	parser.add_argument('-y', dest='year', help="year", required=True)
	parser.add_argument('-p', dest='position', help="position", required=True)
	parser.add_argument('-w', dest='week', help="week", required=True)
	parser.add_argument('-s', dest='scoring', help="scoring", required=True)
	args = parser.parse_args()
	perform_session_download(args.year, args.position, args.week, args.scoring, args.json_path)
	convertJsonToCsv(args.json_path, args.csv_file_name, int(args.week))
	#convertJsonToCsv('/Users/bchen/projects/fftiers/dat/2019/week-0-all-raw.txt', 'foo.csv', 0)
