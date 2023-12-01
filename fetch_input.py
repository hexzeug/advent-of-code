from argparse import ArgumentParser
from datetime import date
from dotenv import load_dotenv
import urllib.request as request
import os

if __name__ == "__main__":
    load_dotenv('.env.local')
    today = date.today()
    argparser = ArgumentParser()
    argparser.add_argument('-d', '--day', type=int, choices=range(1, 26), default=today.day)
    argparser.add_argument('-y', '--year', type=int, default=today.year)
    argparser.add_argument('-o', '-f', '--filename', default='input.txt', required=False)
    argparser.add_argument('--url', required=False)
    args = argparser.parse_args()
    dirpath = f"{args.year}/{str(args.day).rjust(2, '0')}/"
    url = args.url if args.url is not None else f"https://adventofcode.com/{args.year}/day/{args.day}/input"
    os.makedirs(dirpath, exist_ok=True)
    print('fetching input file from:', url)
    req = request.Request(url, headers={'Cookie': f"session={os.environ['SESSION']}"})
    with open(dirpath + args.filename, 'wb') as f:
        f.write(request.urlopen(req).read())