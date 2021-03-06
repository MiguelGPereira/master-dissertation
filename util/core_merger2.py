# -*- coding: utf-8 -*-
"""core merger"""
from xlrd import open_workbook
import csv
from os import listdir
import string

wb = open_workbook('base.xlsx')
sheet = wb.sheets()[0]
number_of_rows = sheet.nrows
number_of_columns = sheet.ncols

partidos = []
partidos_row = 10
partidos_column = 2
partido = sheet.cell(partidos_row, partidos_column).value
while(partido != ""):    
    partidos.append(partido)
    partidos_column = partidos_column + 1
    partido = sheet.cell(partidos_row, partidos_column).value

municipios = []
municipios_data = []
municipios_data_rows = []

#RANK
rank_letters = list(string.ascii_lowercase)
rank_letters_index = 0
ranks = []

for row in range(1, number_of_rows):
    value = sheet.cell(row, 0).value
    if value == "Município":
        value = sheet.cell(row, 1).value
        municipios.append(value)
        data_column = 2
        rank_letters_index = 0
        municipio_data = []
        data = sheet.cell(row, data_column).value
        while(data != ""):
            #RANK
            if data_column>2 and data_column<8:
                partido = partidos[data_column-2]
                votos = data
                print(data)
                letter = rank_letters[rank_letters_index]
                voting_data = dict()
                voting_data["partido"] = partido
                voting_data["votos"] = votos
                voting_data["letra"] = letter
                ranks.append(voting_data)
                rank_letters_index = rank_letters_index + 1
            municipio_data.append(data)
            data_column = data_column + 1
            data = sheet.cell(row, data_column).value
        #RANK
        sortedrank = sorted(ranks, key=lambda k: k['votos'], reverse=True)
        rank_by_letters = [d['letra'] for d in sortedrank]
        rankFinal = ''.join(e+'>' for e in rank_by_letters)[:-1]
        municipio_data.append(rankFinal)
        ranks = []
        municipios_data.append(municipio_data)
        municipios_data_rows.append(row)

#print(ranks[3])
#newlist = sorted(ranks, key=lambda k: k['votos'])
#print("#############")
#print(newlist[3])

feature_files = listdir('features')

features = []
features_row = partidos_row
#features_column = partidos_column
features_data = []
#data_column = partidos_column

for file in feature_files:
    wb = open_workbook('features/'+file)
    sheet = wb.sheets()[0]

    features_column = partidos_column
    #data_column = partidos_column

    feature = sheet.cell(features_row, features_column).value
    while(feature != ""):    
        features.append(feature)
        #print(feature)
        features_column = features_column + 1
        feature = sheet.cell(features_row, features_column).value

    features_data_temp = []
    for row in municipios_data_rows:
        data_column = partidos_column
        feature_data = []
        data = sheet.cell(row, data_column).value
        #print("#", row, data_column)
        while(data != ""):
            feature_data.append(data)
            data_column = data_column + 1
            data = sheet.cell(row, data_column).value
        #print(feature_data)
        features_data_temp.append(feature_data)

    for row in range(0, len(features_data_temp)):
        #features_data[row] =  features_data[row] + feature_data if features_data[row] != NULL else feature_data
        try:
            features_data[row] =  features_data[row] + features_data_temp[row]
        except IndexError:
            features_data.append(features_data_temp[row])

with open('out.csv', 'w', encoding='latin-1') as csvfile:
    writer = csv.writer(csvfile, delimiter=';', quotechar='|', quoting=csv.QUOTE_MINIMAL)
    #writer.writerow(['Spam'] * 5 + ['Baked Beans'])
    writer.writerow([""] + partidos + features)
    for row in range(0, len(municipios)):
        writer.writerow([municipios[row]] + municipios_data[row] + features_data[row])


print("0", len(municipios))
print("1", len(municipios_data))
print("2", len(features_data))
print("exit 0")