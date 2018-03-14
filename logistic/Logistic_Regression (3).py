import pandas
from sklearn import tree
import numpy as np
from sklearn import grid_search
from sklearn import preprocessing
from sklearn import cross_validation
from sklearn import svm
from sklearn import linear_model
from sklearn import cross_validation

###############################################################


d = pandas.read_csv("C:/Udemy/Practical Data Science in Python/adult.csv",encoding = "ISO-8859-1")
d = d[d["workclass"] != "?"]
d = d[d["education"] != "?"]
d = d[d["marital.status"] != "?"]
d = d[d["occupation"] != "?"]
d = d[d["race"] != "?"]
d = d[d["sex"] != "?"]
d = d[d["relationship"] != "?"]
d = d[d["native.country"] != "?"]

x10 = pandas.get_dummies(d["workclass"]).values
x11 = pandas.get_dummies(d["education"]).values
x12 = pandas.get_dummies(d["marital.status"]).values
x13 = pandas.get_dummies(d["occupation"]).values
x14 = pandas.get_dummies(d["relationship"]).values
x15 = pandas.get_dummies(d["race"]).values
x16 = pandas.get_dummies(d["sex"]).values
x17 = pandas.get_dummies(d["native.country"]).values

d['income_gt_50k'] = np.where(d['income'] == ">50K" , 1, 0)
x = d[["hours.per.week","age"]].values
x = np.concatenate((x,x10,x11,x12,x13,x14,x15,x16,x17),axis=1)

################################################################

logreg = linear_model.LogisticRegression(C=0.010,penalty="l1")
logreg.fit(x, d['income_gt_50k'])

print(logreg.score(x, d['income_gt_50k']))
print(np.mean(cross_validation.cross_val_score(logreg, x, d['income_gt_50k'], cv=5)))
print(logreg.coef_)

#############################################################
#We can do some variable selection using cross validation, we basically get the same error
#but with much less variables

x10 = pandas.get_dummies(d["workclass"]).values
x11 = pandas.get_dummies(d["education"]).values
x12 = pandas.get_dummies(d["marital.status"]).values

d['income_gt_50k'] = np.where(d['income'] == ">50K" , 1, 0)
xx = d[["hours.per.week","age"]].values
xx = np.concatenate((xx,x10,x11,x12),axis=1)

logreg = linear_model.LogisticRegression(C=0.010,penalty="l1")
logreg.fit(xx, d['income_gt_50k'])

print(logreg.score(xx, d['income_gt_50k']))
print(np.mean(cross_validation.cross_val_score(logreg, xx, d['income_gt_50k'], cv=5)))
print(logreg.coef_)
coef = np.transpose(logreg.coef_)

name = list()
name.append("hours.per.week")
name.append("age")
for q in (pandas.get_dummies(d["workclass"]).columns):
    name.append(q)
for q in (pandas.get_dummies(d["education"]).columns):
    name.append(q)
for q in (pandas.get_dummies(d["marital.status"]).columns):
    name.append(q)

final_coeffs = pandas.DataFrame(data=name)
coef         = pandas.DataFrame(data=coef)
final_coeffs = pandas.concat((final_coeffs,coef),axis=1)

