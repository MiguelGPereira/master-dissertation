# -*- coding: utf-8 -*-
"""core merger"""
from xlrd import open_workbook
import csv

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

for row in range(1, number_of_rows):
    value = sheet.cell(row, 0).value
    if value == "Município":
        value = sheet.cell(row, 1).value
        municipios.append(value)
        data_column = 2
        municipio_data = []
        data = sheet.cell(row, data_column).value
        while(data != ""):
            municipio_data.append(data)
            data_column = data_column + 1
            data = sheet.cell(row, data_column).value
        municipios_data.append(municipio_data)
        municipios_data_rows.append(row)

wb = open_workbook('features.xlsx')
sheet = wb.sheets()[0]

features = []
features_row = partidos_row
features_column = partidos_column
feature = sheet.cell(features_row, features_column).value
while(feature != ""):    
    features.append(feature)
    features_column = features_column + 1
    feature = sheet.cell(features_row, features_column).value

features_data = []
for row in municipios_data_rows:
    data_column = partidos_column
    feature_data = []
    data = sheet.cell(row, data_column).value
    while(data != ""):
        feature_data.append(data)
        data_column = data_column + 1
        data = sheet.cell(row, data_column).value
    features_data.append(feature_data)

with open('out.csv', 'w', encoding='latin-1') as csvfile:
    writer = csv.writer(csvfile, delimiter=';', quotechar='|', quoting=csv.QUOTE_MINIMAL)
    #writer.writerow(['Spam'] * 5 + ['Baked Beans'])
    writer.writerow([""] + partidos + features)
    for row in range(0, len(municipios)):
        writer.writerow([municipios[row]] + municipios_data[row] + features_data[row])


print("exit 0")