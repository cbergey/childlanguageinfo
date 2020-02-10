from __future__ import absolute_import, division, print_function, unicode_literals
import io
import os
from gensim import utils
import gensim.models
import gensim.models.word2vec
from gensim.test.utils import datapath
from gensim.models import KeyedVectors
import logging
logging.basicConfig(format='%(asctime)s : %(levelname)s : %(message)s', level=logging.INFO)             
import numpy as np   
import csv
from matplotlib import pyplot
from numpy import cov
import seaborn

model = gensim.models.Word2Vec.load("ldp_adult_word2vec.model")

sim_judgments = {}
with open('../data/judgments_session.csv', mode='r') as csv_file:
    readCSV = csv.DictReader(csv_file, delimiter=',')
    for row in readCSV:
        if row['adj'] not in sim_judgments:
            sim_judgments[row['adj']] = {}
        sim_judgments[row['adj']][row['noun']] = row['mean_typ']

pairs = [   
    ('car', 'bicycle'),  
    ('car', 'airplane'),  
    ('car', 'pear'),   
    ('chicken','beef'),
    ('chicken','pig'),
    ('banana','yellow'),
    ('banana','green'),
    ('banana','brown'),
    ('apple','green'),
    ('apple','red'),
    ('chocolate','white'),
    ('chocolate','brown'),
]

for w1, w2 in pairs:
  print('%r\t%r\t%.2f' % (w1, w2, model.wv.similarity(w1, w2)))

model.wv.evaluate_word_pairs(datapath('wordsim353.tsv'))

def reduce_dimensions(model): # this function adapted from gensim dim reduction tutorial
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

human_judgments = []
word2vec_judgments = []

for adj in sim_judgments:
    for noun in sim_judgments[adj]:
        if adj in model.wv.vocab and noun in model.wv.vocab:
            human_judgments.append(float(sim_judgments[adj][noun]))
            word2vec_judgments.append(float(model.wv.similarity(adj, noun)))


seaborn.stripplot(human_judgments, word2vec_judgments, s = 2, jitter = 3)
pyplot.show()
covariance = np.corrcoef(human_judgments, word2vec_judgments)
print(covariance)


model.wv.evaluate_word_pairs(datapath('simlex999.txt'))


