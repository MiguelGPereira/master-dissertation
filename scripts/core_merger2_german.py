# -*- coding: utf-8 -*-
"""core merger"""
from xlrd import open_workbook
import csv
from os import listdir
import string

wb = open_workbook('german2009_excel.xlsx')
sheet = wb.sheets()[0]
number_of_rows = sheet.nrows
number_of_columns = sheet.ncols

print(number_of_rows)
print(number_of_columns)

heads = []
heads_row = 0
heads_column = 0
head = sheet.cell(heads_row, heads_column).value
while(heads_column < number_of_columns - 1):
    heads.append(head)
    heads_column = heads_column + 1
    print(heads_column)
    head = sheet.cell(heads_row, heads_column).value
heads.append('a')
heads.append('b')
heads.append('c')
heads.append('d')
heads.append('e')

#ranking_column_sample = sheet.cell(number_of_rows-1, number_of_columns-1).value
#print(ranking_column_sample)
#parties = ranking_column_sample.split('>')

data = []

for row in range(1, number_of_rows):
    row_data = []
    temp_column = 0
    temp = sheet.cell(row, temp_column).value
    while(temp_column < number_of_columns-1):
        row_data.append(temp)
        temp_column = temp_column + 1
        temp = sheet.cell(row, temp_column).value

    ranking_temp = sheet.cell(row, number_of_columns-1).value.split('>')
    ranking = []
    for value in ranking_temp:
        if len(value) == 1:
            ranking.append(value)
        else:
            parties = list(value)
            for p in parties:
                ranking.append(p)

    partyA = ranking.index('a') + 1
    partyB = ranking.index('b') + 1
    partyC = ranking.index('c') + 1
    partyD = ranking.index('d') + 1
    partyE = ranking.index('e') + 1
    row_data.append(partyA)
    row_data.append(partyB)
    row_data.append(partyC)
    row_data.append(partyD)
    row_data.append(partyE)

    data.append(row_data)

with open('out_german.csv', 'w', encoding='latin-1') as csvfile:
    writer = csv.writer(csvfile, delimiter=';', quotechar='|', quoting=csv.QUOTE_MINIMAL)
    writer.writerow(heads)
    for row in range(0, len(data)):
        writer.writerow(data[row])

print("exit 0")
