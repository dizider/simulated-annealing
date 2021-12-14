"""Plotter

Usage:
  plotter.py (-i | --input) <input-file>
  plotter.py (-h | --help)

Options:
  -h --help     Show this screen.
  --drifting    Drifting mine.

"""
from docopt import docopt
import pandas as pd
import matplotlib.pyplot as plt
import csv

def main(args):
    x = []
    y = []
    best = []


    with open(args['<input-file>'],'r') as csvfile:
        plots = csv.reader(csvfile, delimiter = ' ')

        for row in plots:
            x.append(float(row[0]))
            y.append(float(row[1]))
            best.append(float(row[2]))

    plt.plot(x, y, color = 'g', linestyle = 'dashed', marker = 'o',label = "Value")
    plt.plot(x, best, color = 'b', linestyle = 'dashed', marker = 'o',label = "Best value")
    plt.xlabel('Optimalizacani kriterium')
    plt.ylabel('Pocet iteraci')
    plt.title('Simulovane ochlazovani')
    plt.legend()
    plt.show()

if __name__ == '__main__':
    arguments = docopt(__doc__, version='Plotter 1.0')
    main(arguments)
