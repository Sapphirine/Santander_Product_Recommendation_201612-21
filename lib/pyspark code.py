# -*- coding: utf-8 -*-
"""
Created on Thu Dec 22 12:25:39 2016

@author: Andy
"""

%pyspark

# load in the data

from pyspark.sql import SQLContext

# create SQLContext instance
sqlContext = SQLContext(sc)

person = sc.textFile("hdfs://sandbox.hortonworks.com/user/spark/personfinalized.csv").map(lambda line: line.split(','))\
         .filter(lambda line: len(line) > 1)
item = sc.textFile("hdfs://sandbox.hortonworks.com/user/spark/itemfinalized.csv").map(lambda line: line.split(','))\
         .filter(lambda line: len(line) > 1)
         
# have a quick view into the data
print person.collect()[10]
print item.collect()[10]

# calculate Pearson coefficient as the similarity 

from pyspark.mllib.stat import Statistics

person_matrix = Statistics.corr(person, method="pearson")
item_matrix = Statistics.corr(item, method="pearson")

# based on test on small dataset, get the optimal alpha
alpha = 0.1

weighted = person_matrix*alpha + (1-alpha)*item_matrix

# do k-means based on the similarity matrix
# because in Pyspark, there is no K-medoid based method, and the similarity here is defined by ourself
# so we considered about using Power iteration clustering (PIC)

from pyspark.mllib.clustering import PowerIterationClustering, PowerIterationClusteringModel
piccluster = PowerIterationClustering.train(weighted,5, 10)
piccluster.assignments().foreach(lambda x: print(str(x.id) + " -> " + str(x.cluster)))

#save the result
model.save(sc, "hdfs://sandbox.hortonworks.com/user/spark/picresult")

# load the result after fixing it and merge it into the item csv

used = sc.textFile("hdfs://sandbox.hortonworks.com/user/spark/itemfinalized.csv").map(lambda line: line.split(','))\
         .filter(lambda line: len(line) > 1)

# check the data
print used.collect()[10]

