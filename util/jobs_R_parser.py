# -*- coding: utf-8 -*-
"""jobs parser"""
from xlrd import open_workbook
import csv
from os import listdir

lines = []
with open('all.jobs.mini.2') as f:
    lines = f.readlines()

datasets = []
labels = ["max pairs", "calculated support", "gamma", "completeness", "accuracy", "def.rank", "rules"]
dataset = []
for line in lines:
    if any(label in line for label in labels):
        value = line.split(":")[1].replace("\n", "").strip()
        if "support" in line:
            value = "0.0" + value
        dataset.append(value)
    elif "baseline" in line:
        tempset = []
        tempset.append(dataset[2])
        tempset.append(dataset[1])
        tempset.append(dataset[3])
        tempset.append(dataset[4])
        tempset.append(dataset[5])
        tempset.append(dataset[6])
        tempset.append(dataset[0])
        datasets.append(tempset)
        dataset = []



index = 0
for ds in datasets:
    if index == 0:
        print("gamma=c("+','.join(ds)+")")
    elif index == 2:
        print("completeness=c("+','.join(ds)+")")
    elif index == 3:
        print("accuracy=c("+','.join(ds)+")")
    index += 1
