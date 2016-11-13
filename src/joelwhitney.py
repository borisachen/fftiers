__author__ = 'joelwhitney'
'''
A clustering program that uses FantasyPros data inspired by Boris Chen (http://www.borischen.co/)
This program utilizes unsupervised machine learning by flat clustering with KMeans -- a simple way
to uncover like tiers within the player data mined from FantasyPros (http://www.fantasypros.com/)

To Do's
-Comment/clean code
-Improve plot output
-Add logging and improved cmd line stuff
-Add sms alert when graph updated (pass/fail)

Big picture
-Make the script run continuously once a day from Raspberry Pi
  -See main() method
  -Add local v Pi run option (save locations will differ)
  -Upload plots to site root folder
-Make this program work with NHL data for Fantasy Hockey
'''
import argparse
import requests
from lxml import html
import traceback
import os
import logging
import logging.handlers
import datetime
import sys
import csv
from threading import Timer
import numpy as np
from sklearn.cluster import KMeans
import matplotlib.pyplot as plt
from matplotlib.pyplot import cm
from matplotlib import style
style.use("ggplot")



def initialize_logging(logFile):
    """
    setup the root logger to print to the console and log to file
    :param logFile: string log file to write to
    """
    formatter = logging.Formatter("[%(asctime)s] [%(filename)30s:%(lineno)4s - %(funcName)30s()]\
         [%(threadName)5s] [%(name)10.10s] [%(levelname)8s] %(message)s")  # The format for the logs
    logger = logging.getLogger()  # Grab the root logger
    logger.setLevel(logging.DEBUG)  # Set the root logger logging level (DEBUG, INFO, WARNING, ERROR, CRITICAL)
    # Create a handler to print to the console
    sh = logging.StreamHandler(sys.stdout)
    sh.setFormatter(formatter)
    sh.setLevel(logging.DEBUG)
    # Create a handler to log to the specified file
    rh = logging.handlers.RotatingFileHandler(logFile, mode='a', maxBytes=10485760)
    rh.setFormatter(formatter)
    rh.setLevel(logging.DEBUG)
    # Add the handlers to the root logger
    logger.addHandler(sh)
    logger.addHandler(rh)


def verify_file_path(filePath):
    """
    verify file exists
    :param filePath: the joined directory and file name
    :return: Boolean: True if file exists
    """
    logger = logging.getLogger()
    ospath = os.path.abspath(filePath)
    ospath = ospath.replace("\\","\\\\")
    logger.debug("Evaluating if {} exists...".format(ospath))
    if not os.path.isfile(str(ospath)):
        logger.info("File not found: {}".format(filePath))
        return False
    else:
        return True


def csv_from_excel(full_file_name):
    """
    converts old xls to csv using this roundabout method
    :param full_file_name: downloaded xls file name
    """
    logger = logging.getLogger()
    logger.debug("Opening xls and csv files for conversion...")
    try:
        if verify_file_path(full_file_name):
            with open(full_file_name, 'r') as xlsfile, open(full_file_name[:-4] + '.csv', 'w', newline="\n", encoding="utf-8") as csv_file:
                xls_reader = csv.reader(xlsfile, delimiter='\t')
                csv_writer = csv.writer(csv_file)
                # skip first five lines
                for i in range(5):
                    next(xls_reader, None)
                # write subsequent rows to csv file
                for row in xls_reader:
                    csv_writer.writerow(row)
                logger.debug("Conversion succeeded...")
        else:
            logger.info("XLS file not found for: {} Skipping file...".format(full_file_name))
    except Exception as e:
        logger.info("Conversion failed with: {}".format(e))


def get_nfl_week(start_week_date):
    """
    get the nfl_week
    :param start_week_date: date object for Tuesday before 1st Thursday game
    :return: week: integer
    """
    week = 0
    today_date = datetime.datetime.now().date()
    if today_date >= start_week_date:
        # get days passed start date as date object
        difference_days = today_date - start_week_date
        # calculate week
        week = int((difference_days.days / 7) + 1)
        return week
    else:
        return week


def perform_session_download(args, url, full_file_name):
    """
    creates a session that allows the user to log in to FantasyPros and use the tokens
    :param args: list of parameters can be used to get data directories
    :param url: string of the export xls url
    :param full_file_name: string of the full file path and name of file to be saved
    """
    logger = logging.getLogger()
    try:
        # get payload values from command line parameters
        username, password, token = args.username, args.password, args.token
        payload = {"username": username,
                   "password": password,
                   "csrfmiddlewaretoken": token}
        # start session
        logger.debug("Starting download session...")
        session_requests = requests.session()
        login_url = "https://secure.fantasypros.com/accounts/login/?"
        result = session_requests.get(login_url)
        # refresh token on new request
        tree = html.fromstring(result.text)
        logger.debug("Updating token...")
        authenticity_token = list(set(tree.xpath("//input[@name='csrfmiddlewaretoken']/@value")))[0]
        payload["csrfmiddlewaretoken"] = authenticity_token
        session_requests.post(login_url,
                              data=payload,
                              headers=dict(referer=login_url))
        # prepare to write data to file
        logger.debug("Opening xls file to write data...")
        with open(full_file_name, 'wb') as handle:
            response = session_requests.get(url)
            if not response.ok:
                logger.info("Writing to xls failed...")
            for block in response.iter_content(1024):
                handle.write(block)
            logger.info("Writing to xls succeeded...")
    except Exception as e:
        logger.info("Session download failed with: {}".format(e))


def download_nfl_data(args, week, position_list):
    """
    download xls file from fantasy pros to the data_directory specified above
    :param args: list of parameters can be used to get data directories
    :param week: integer week to be used when building file names
    :param position_list: list of positions to download, also used to build file names
    """
    logger = logging.getLogger()
    try:
        download_data = args.download_data
        if download_data == "True":
            # get data directory from command line parameters
            data_directory = args.data_directory
            # if preseason
            if week == 0:
                preseason_rankings = ['https://www.fantasypros.com/nfl/rankings/consensus-cheatsheets.php?export=xls',
                                      'https://www.fantasypros.com/nfl/rankings/qb-cheatsheets.php?export=xls',
                                      'https://www.fantasypros.com/nfl/rankings/rb-cheatsheets.php?export=xls',
                                      'https://www.fantasypros.com/nfl/rankings/wr-cheatsheets.php?export=xls',
                                      'https://www.fantasypros.com/nfl/rankings/te-cheatsheets.php?export=xls',
                                      'https://www.fantasypros.com/nfl/rankings/k-cheatsheets.php?export=xls',
                                      'https://www.fantasypros.com/nfl/rankings/dst-cheatsheets.php?export=xls']
                preseason_rankings_names = ['week-0-preseason-overall-raw.xls',
                                            'week-0-preseason-qb-raw.xls', 'week-0-preseason-rb-raw.xls',
                                            'week-0-preseason-wr-raw.xls', 'week-0-preseason-te-raw.xls',
                                            'week-0-preseason-k-raw.xls', 'week-0-preseason-dst-raw.xls']
                # download each link separately
                for item_position in range(len(preseason_rankings)):
                    # prepare link and path/filename
                    full_file_name = os.path.join(data_directory, preseason_rankings_names[item_position])
                    url = preseason_rankings[item_position]
                    # download using sessions
                    logger.debug("Starting session download...")
                    perform_session_download(args, url, full_file_name)
                    # convert the xls to csv
                    logger.debug("Starting xls conversion...")
                    csv_from_excel(full_file_name)
            # if not preseason
            else:
                # download each position from the position list
                for position in position_list:
                    # prepare link and path/filename
                    filename = 'week-' + str(week) + '-' + position + '-raw.xls'
                    full_file_name = os.path.join(data_directory, filename)
                    url = 'http://www.fantasypros.com/nfl/rankings/' + position + '.php?export=xls'
                    # download using sessions
                    logger.debug("Starting session download...")
                    perform_session_download(args, url, full_file_name)
                    # convert the xls to csv
                    logger.debug("Starting xls conversion...")
                    csv_from_excel(full_file_name)
    except Exception as e:
        logger.info("Generic download and conversion failed with: {}".format(e))


def get_position_setting(position, settings):
    """
    returns the max number of players to show and the k-value for clusters
    TODO's: comment, build in preseason stuff here (see plot() TODO)
    :param position: string position of setting you want
    :param settings: list of dictionaries of settings
    :returns: max_num, k_val: positional settings for plotting
    """
    logger = logging.getLogger()
    # iterate over dictionaries until dictionary for position is found
    for dict in settings:
        if str(dict.get('pos')).lower() == str(position).lower():
            max_num = dict.get('max_num')
            k_val = dict.get('k_val')
    return max_num, k_val


def lists_from_csv(position, week, data_directory):
    """
    builds lists from the csv to be used in the graphing
    :param position: string position used for building csv name
    :param week: integer week used for building csv name
    :param data_directory: string data directory used for building csv name
    :returns: rank_list, name_list, position_list, average_rank_list, standard_deviation_list: lists of data
    """
    logger = logging.getLogger()
    try:
        # set up empty lists for data storing
        rank_list = []
        name_list = []
        position_list = []
        average_rank_list = []
        standard_deviation_list = []
        # build path/filename for csv file
        filename = 'week-' + str(week) + '-' + position + '-raw.csv'
        full_file_name = os.path.join(data_directory, filename)
        logger.debug("Trying to find csv file: {}...".format(full_file_name))
        # verify can find file before trying to process data
        if verify_file_path(full_file_name):
            # set up csv file to read
            with open(full_file_name, 'r') as csv_file:
                csv_reader = csv.reader(csv_file)
                # iterate over each row adding column to appropriate list
                for row in csv_reader:
                    rank_list.append(int(row[0]))
                    name_list.append(str(row[1]))
                    # preseason-overall includes position column, this accounts for it
                    if position == 'preseason-overall':
                        position_list.append(str(row[2]))
                        average_rank_list.append(float(row[7]))
                        standard_deviation_list.append(float(row[8]))
                    # all other positions will use this
                    else:
                        position_list.append(str(position))
                        average_rank_list.append(float(row[6]))
                        standard_deviation_list.append(float(row[7]))
            return rank_list, name_list, position_list, average_rank_list, standard_deviation_list
        else:
            logger.info("CSV file not found for: {} - Week {}. Skipping position...".format(position, week))
    except Exception as e:
        logger.info("Building lists from csv failed with: {}".format(e))


def get_cluster_settings(week):
    """
    helper function for getting the parameters needed for plotting
    TODO's: comment, rethink this piece (maybe just return based on position instead of whole list
    :param week: int week used for getting right settings
    :returns: type_cluster_settings and ros_settings: list of dictionaries with the appropiate settings
    """
    logger = logging.getLogger()
    # preseason clustering settings
    preseason_cluster_settings = [{'pos': 'preseason-overall', 'plot1': 70, 'k_val_1': 10, 'plot2': 70, 'k_val_2': 8, 'plot3': 70, 'k_val_3': 8},
                                  {'pos': 'preseason-qb', 'max_num': 24, 'k_val': 8},
                                  {'pos': 'preseason-rb', 'max_num': 40, 'k_val': 9},
                                  {'pos': 'preseason-wr', 'max_num': 60, 'k_val': 12},
                                  {'pos': 'preseason-te', 'max_num': 24, 'k_val': 8},
                                  {'pos': 'preseason-flex', 'max_num': 80, 'k_val': 14},
                                  {'pos': 'preseason-k', 'max_num': 24, 'k_val': 5},
                                  {'pos': 'preseason-dst', 'max_num': 24, 'k_val': 6}]
    # positional clustering settings
    weekly_pos_cluster_settings = [{'pos': 'qb', 'max_num': 24, 'k_val': 8},
                                   {'pos': 'rb', 'max_num': 40, 'k_val': 9},
                                   {'pos': 'wr', 'max_num': 60, 'k_val': 12},
                                   {'pos': 'te', 'max_num': 24, 'k_val': 8},
                                   {'pos': 'flex', 'max_num': 80, 'k_val': 14},
                                   {'pos': 'k', 'max_num': 24, 'k_val': 5},
                                   {'pos': 'dst', 'max_num': 24, 'k_val': 6}]
    # rest of season clustering settings
    ros_pos_cluster_settings = [{'pos': 'ros-qb', 'max_num': 32, 'k_val': 7},
                                {'pos': 'ros-rb', 'max_num': 50, 'k_val': 12},
                                {'pos': 'ros-wr', 'max_num': 64, 'k_val': 65/5},
                                {'pos': 'ros-te', 'max_num': 30, 'k_val': 7},
                                {'pos': 'ros-k', 'max_num': 20, 'k_val': 5},
                                {'pos': 'ros-dst', 'max_num': 25, 'k_val': 5}]
    # get cluster settings
    if week == 0:
        type_cluster_settings = preseason_cluster_settings
        ros_settings = ros_pos_cluster_settings
    else:
        type_cluster_settings = weekly_pos_cluster_settings
        ros_settings = ros_pos_cluster_settings
    return type_cluster_settings, ros_settings


def plot(position, week, args):
    """
    the first stage of the plotting that prepares the data to then be cluster_and_plot'ed
    TODO's: comment, utilize get_position_settings for preseason data
    :param position: string position used for getting data and position settings for the plotting
    :param week: integer week used for getting data
    :param args: list of parameters can be used to get data and plot directories
    """
    logger = logging.getLogger()
    filename = 'week-' + str(week) + '-' + position + '-raw.png'
    plots_directory = args.plots_directory
    plot_full_file_name = os.path.join(plots_directory, filename)
    data_directory = args.data_directory
    # get the cluster settings
    type_cluster_settings, ros_cluster_settings = get_cluster_settings(week)
    # get preseason settings
    if week == 0:
        rank_list, name_list, position_list, average_rank_list, standard_deviation_list = lists_from_csv(position, week=week, data_directory=data_directory)
        # split lists for pos == overall
        if position == 'preseason-overall':
            for dict in type_cluster_settings:
                if dict.get('pos') == 'preseason-overall':
                    start1, stop1 = 0, 0 + dict.get('plot1')
                    start2 = stop1 + 1
                    stop2 = start2 + dict.get('plot2')
                    start3 = stop2 + 1
                    stop3 = start3 + dict.get('plot3')
                    rank_list_1, name_list_1, position_list1, average_rank_list_1, standard_deviation_list_1 = rank_list[start1: stop1], \
                                                                                                               name_list[start1: stop1], \
                                                                                                               position_list[start1: stop1], \
                                                                                                               average_rank_list[start1: stop1], \
                                                                                                               standard_deviation_list[start1: stop1]
                    rank_list_2, name_list_2, position_list2, average_rank_list_2, standard_deviation_list_2 = rank_list[start2: stop2], \
                                                                                                               name_list[start2: stop2], \
                                                                                                               position_list[start2: stop2], \
                                                                                                               average_rank_list[start2: stop2], \
                                                                                                               standard_deviation_list[start2: stop2]
                    rank_list_3, name_list_3, position_list3, average_rank_list_3, standard_deviation_list_3 = rank_list[start3: stop3], \
                                                                                                               name_list[start3: stop3], \
                                                                                                               position_list[start3: stop3], \
                                                                                                               average_rank_list[start3: stop3], \
                                                                                                               standard_deviation_list[start3: stop3]
                    k_value_1, k_value_2, k_value_3 = dict.get('k_val_1'), \
                                                      dict.get('k_val_2'), \
                                                      dict.get('k_val_3')
                    list_of_lists1 = [[rank_list_1, name_list_1, position_list1, average_rank_list_1, standard_deviation_list_1, k_value_1],
                                     [rank_list_2, name_list_2, position_list2, average_rank_list_2, standard_deviation_list_2, k_value_2],
                                     [rank_list_3, name_list_3, position_list3, average_rank_list_3, standard_deviation_list_3, k_value_3]]
                    cluster_and_plot(list_of_lists1, plot_full_file_name)
        else:
            max_number, k_value = get_position_setting(position, type_cluster_settings)
            rank_list, name_list, position_list, average_rank_list, standard_deviation_list = rank_list[0:max_number], \
                                                                                              name_list[0:max_number], \
                                                                                              position_list[0:max_number], \
                                                                                              average_rank_list[0:max_number], \
                                                                                              standard_deviation_list[0:max_number]
            list_of_lists2 = [[rank_list, name_list, position_list, average_rank_list, standard_deviation_list, k_value]]
            cluster_and_plot(list_of_lists2, plot_full_file_name)
    else:
        # get settings for weekly plots
        max_number, k_value = get_position_setting(position, type_cluster_settings)
        rank_list, name_list, position_list, average_rank_list, standard_deviation_list = lists_from_csv(position, week=week, data_directory=data_directory)
        list_of_lists = [[rank_list, name_list, position_list, average_rank_list, standard_deviation_list, k_value]]
        cluster_and_plot(list_of_lists, plot_full_file_name)


def cluster_and_plot(list_of_lists, plot_full_file_name):
    """
    the second stage of the plotting that clusters and plots the data
    TODO's: format graph
    :param list_of_lists: list of lists that has the pertinent plotting data
    :param plot_full_file_name: the file name of the plot to be saved
    """
    logger = logging.getLogger()
    try:
        list_count = 1  # count for appending to file names (necessary for split plots)
        plot_file_name = plot_full_file_name[:-4]  # strip .png off file name so adjustments can be made
        # iterate over lists -- needed if plot is split into multiple
        for list in list_of_lists:
            # add count for split plots
            plot_full_file_name = plot_file_name + '-{}.png'.format(list_count)
            rank_list, name_list, position_list, average_rank_list, standard_deviation_list, k_value = list[0], list[1], list[2], list[3], list[4], list[5]
            # empty list that will be converted into array
            average_rank_array = []
            for n in range(len(average_rank_list)):
                # build list from item and append the list to the list of lists
                item_list = [average_rank_list[n]]
                average_rank_array.append(item_list)
            # convert the list of lists to an array
            X = np.array(average_rank_array)
            # initialize KMeans and fit over the array
            kmeans = KMeans(n_clusters=k_value)
            kmeans.fit(X)
            centroids = kmeans.cluster_centers_  # not used here
            # array of labels where a cluster value is assigned to each item
            labels = kmeans.labels_
            # color list that will automatically generate based on number of clusters
            colors = []
            color_cycle = iter(cm.rainbow(np.linspace(0, 5, len(labels))))
            for i in range(len(labels)):
                c = next(color_cycle)
                colors.append(c)
            # iterate over array and plot values, standard deviation, and color by clusters
            for i in range(len(X)):
                plt.errorbar(X[i][0], rank_list[i], xerr=standard_deviation_list[i], marker='.', markersize=4, color=colors[labels[i]], ecolor=colors[labels[i]])
                position = position_list[i][10:].upper() if len(position_list[i]) > 10 else position_list[i].upper()
                plt.text(X[i][0] + standard_deviation_list[i] + 1, rank_list[i], "{} {} ({})".format(name_list[i], position, rank_list[i]), size=6, color=colors[labels[i]],
                         ha="left", va="center")
            plt.gca().invert_yaxis()  # top-left of graph should start at 1
            # plt.show()
            plt.savefig(plot_full_file_name, bbox_inches='tight')  # save the png file
            plt.clf()  # clear plot after use otherwise subsequent iterations
            list_count += 1
    except Exception as e:
        logger.info("Clustering and plotting failed with: {}".format(e))


def clustering_program(args, start_week_date, position_list):
    """
    adjusts the position list based on if preseason or not then runs program
    :param args: list of parameters can be used to get data and plot directories
    :param start_week_date: date object for start of season
    :param position_list: list of positions to be used
    """
    week = get_nfl_week(start_week_date)
    adjust_position_list = position_list
    if week == 0:
        adjust_position_list.remove('flex')
        adjust_position_list.insert(0, 'overall')
        download_nfl_data(args, week, position_list)
        for pos in position_list:
            preseason_pos = 'preseason-{}'.format(pos)
            plot(preseason_pos, week, args)
    else:
        download_nfl_data(args, week, position_list)
        for pos in position_list:
            plot(pos, week, args)


def main(args):
    logger = logging.getLogger()
    # downloading settings
    position_list = ['qb', 'rb', 'wr', 'te', 'flex', 'k', 'dst']
    start_week_date = datetime.date(2016, 9, 6)
    injured_player_list = []
    clustering_program(args, start_week_date, position_list)

    # # start loop
    # while True:
    #     # get timer components
    #     x = datetime.datetime.now()
    #     y = x.replace(day=x.day + 1, hour=23, minute=40, second=0, microsecond=0)
    #     delta_t = y - x
    #     secs = delta_t.seconds + 1
    #     print(secs)
    #     # after secs is up run the program
    #     t = Timer(secs, clustering_program(args, start_week_date, position_list))
    #     t.start()


if __name__ == "__main__":    # get all of the commandline arguments
    parser = argparse.ArgumentParser("FantasyPros clustering program")
    # required parameters
    parser.add_argument('-u', dest='username', help="FantasyPros username", required=True)
    parser.add_argument('-p', dest='password', help="FantasyPros password", required=True)
    parser.add_argument('-t', dest='token', help="FantasyPros token", required=True)
    # optional parameters
    parser.add_argument('-down', dest='download_data', help="Boolean for if script should download data", default="True")
    parser.add_argument('-dat', dest='data_directory', help="The directory where the data is downloaded", default="data/fftiers/2016/")
    parser.add_argument('-plot', dest='plots_directory', help="The directory where the plots are saved", default="plots/fftiers/2016/")
    # required for logging
    parser.add_argument('-logFile', dest='logFile', help='The log file to use', default="log.txt")
    args = parser.parse_args()
    initialize_logging(args.logFile)
    try:
        main(args)
    except Exception as e:
        logging.getLogger().critical("Exception detected, script exiting")
        print(e)
        logging.getLogger().critical(e)
        logging.getLogger().critical(traceback.format_exc().replace("\n"," | "))