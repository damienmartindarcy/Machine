from sklearn import *
import pandas as pd
import numpy as np

from sklearn.decomposition import PCA
from sklearn.cluster import KMeans

 

def print_full(x):
    pd.set_option('display.max_rows', len(x))
    print(x)
    pd.reset_option('display.max_rows')

data = pd.read_csv("C:/Udemy/Pro Data Science in Python/Countries_Kaggle/Kaggle.csv")
ids  = pd.DataFrame(data['Id'])
del data['Id']
min_max_scaler     =  preprocessing.MinMaxScaler()
original_dataset   =  min_max_scaler.fit_transform(data)



##########################################################################################################
#we can cluster the countries based on the original attributes
ds                    = KMeans(n_clusters=3).fit(original_dataset)
clustered             = pd.DataFrame(ids)
clustered["clusters"] = ds.labels_[:, np.newaxis]
print_full(clustered)


##########################################################################################################
#we can run PCA and extract one component and then cluster
pca = PCA(n_components=1)
pca.fit(original_dataset)
transf_dataset = pca.transform(original_dataset)
print(pca.explained_variance_ratio_)

Q = transf_dataset

clustered["PCA_1"] = Q
ds                        = KMeans(n_clusters=3).fit(Q)
clustered["clusters_PCA"] = ds.labels_
#print_full(clustered)

##########################################################################################################

siho = metrics.silhouette_samples(data,ds.labels_)
print("Silhouette Coefficient: %0.3f"% metrics.silhouette_score(Q, clustered["clusters_PCA"]))
print("Silhouette Coefficient: %0.3f"% metrics.silhouette_score(original_dataset, clustered["clusters"]))
#Both silhoutes are very similar, so that's good

##########################################################################################################
#analyzing the results, I will use the PCA clustering results

reporting_table = pd.concat((data,clustered),axis=1)
grouped = reporting_table.groupby("clusters_PCA").mean()
print(grouped)

pd.set_option('display.max_columns', 500)
pd.set_option('display.width', 1000)
print_full(grouped.transpose())





