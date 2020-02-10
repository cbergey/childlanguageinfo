from __future__ import absolute_import, division, print_function, unicode_literals
import io
import os
from gensim import utils
import gensim.models
import gensim.models.word2vec
from gensim.test.utils import datapath
import logging
logging.basicConfig(format='%(asctime)s : %(levelname)s : %(message)s', level=logging.INFO)
from sklearn.decomposition import IncrementalPCA    
from sklearn.manifold import TSNE                   
import numpy as np   
import csv

person = 'adult'

model = gensim.models.Word2Vec.load("childes_" + person + "_word2vec.model")

pairs = [   
    ('car', 'bicycle'),  
    ('car', 'airplane'),  
    ('car', 'pear'),   
    ('king', 'queen'),
    ('chicken','beef'),
    ('chicken','pig'),
]

for w1, w2 in pairs:
  print('%r\t%r\t%.2f' % (w1, w2, model.wv.similarity(w1, w2)))

model.wv.evaluate_word_pairs(datapath('wordsim353.tsv'))

# functions below adapted from gensim dim reduction tutorial
def reduce_dimensions(model):
    num_dimensions = 2  # final num dimensions (2D, 3D, etc)

    vectors = [] # positions in vector space
    labels = [] # keep track of words to label our data again later
    for word in model.wv.vocab:
        vectors.append(model.wv[word])
        labels.append(word)

    # convert both lists into numpy vectors for reduction
    vectors = np.asarray(vectors)
    labels = np.asarray(labels)

    # reduce using t-SNE
    vectors = np.asarray(vectors)
    tsne = TSNE(n_components=num_dimensions, random_state=0)
    vectors = tsne.fit_transform(vectors)

    x_vals = [v[0] for v in vectors]
    y_vals = [v[1] for v in vectors]
    return x_vals, y_vals, labels


def writecsv(name, neighbors, targetword):
    with open(name, 'w') as csvfile:
        writer = csv.writer(csvfile, delimiter=' ',
            quotechar='|', quoting=csv.QUOTE_MINIMAL)
        writer.writerow(['word, distance, targetword'])
        for w in neighbors:
            row = [w[0] + ',' + str(w[1]) + ',' + targetword]
            writer.writerow(row)

soonneighbors = model.wv.most_similar(positive = 'soon', topn = 20)

for w in soonneighbors:
    print(w[0],w[1])

def getneighbors(word, n):
    neighbors = model.wv.most_similar(positive = word, topn = n)
    writecsv('childes_' + person + '_' + word + 'neighbors.csv', neighbors, word)

wordstogetneighbors = [
    ('long', 500),
    ('short', 500),
    ('soon', 100),
    ('wide', 100),
]

for word, numneighbors in wordstogetneighbors:
    getneighbors(word, numneighbors)

'''    
longneighbors = model.wv.most_similar(positive = 'long', topn = 500)

writecsv('cocalongneighbors.csv', longneighbors, 'long')

shortneighbors = model.wv.most_similar(positive = 'short', topn = 500)

writecsv('cocashortneighbors.csv', shortneighbors, 'short')

soonneighbors = model.wv.most_similar(positive = 'soon', topn = 100)

writecsv('cocasoonneighbors.csv', soonneighbors, 'soon')

wideneighbors = model.wv.most_similar(positive = 'wide', topn = 100)

writecsv('cocawideneighbors.csv', wideneighbors, 'soon')
'''

'''
x_vals, y_vals, labels = reduce_dimensions(model)

def plot_with_plotly(x_vals, y_vals, labels, plot_in_notebook=True):
    from plotly.offline import init_notebook_mode, iplot, plot
    import plotly.graph_objs as go

    trace = go.Scatter(x=x_vals, y=y_vals, mode='text', text=labels)
    data = [trace]

    if plot_in_notebook:
        init_notebook_mode(connected=True)
        iplot(data, filename='word-embedding-plot')
    else:
        plot(data, filename='word-embedding-plot.html')


def plot_with_matplotlib(x_vals, y_vals, labels):
    import matplotlib.pyplot as plt
    import random

    random.seed(0)

    plt.figure(figsize=(12, 12))
    plt.scatter(x_vals, y_vals)

    indices = list(range(len(labels)))
    selected_indices = random.sample(indices, 50)
    for i in selected_indices:
        plt.annotate(labels[i], (x_vals[i], y_vals[i]))
    plt.show()
    plt.savefig('COCA.png')

try:
    get_ipython()
except Exception:
    plot_function = plot_with_matplotlib
else:
    plot_function = plot_with_plotly

plot_function(x_vals, y_vals, labels)
'''
