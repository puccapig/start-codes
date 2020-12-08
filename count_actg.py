# count_actg.py
# reference analyze_2fold.py
# To count how many a and c and t and g in a sequence
# Rachel Yuan Nong 2018-March-20 Uppsala Sweden; modified on 2018-April-12, 2018-May-03, 2018-May-17

import string
#import numpy as np
#import scipy.stats as stats


x = raw_input("input your sequence :")
y = raw_input("is this sequence read from exsiting articles (Yes/No)? :")

def count_actg(x):
    
    count_G = int(0)
    count_T = int(0)
    count_C = int(0)
    count_A = int(0)
    count_N = int(0)
    count_U = int(0)
    count_any_letter = int(0)

    for i in range(0, len(x)):
        if x[i] == 'G':
            count_G = count_G + 1
        if x[i] == 'T':
            count_T = count_T + 1
        if x[i] == 'C':
            count_C = count_C + 1
        if x[i] == 'A':
            count_A = count_A + 1
        if x[i] == 'N':
            count_N = count_N + 1
        if x[i] == 'U':
            count_U = count_U + 1
        if x[i] == 'B' or x[i] == 'D' or x[i] == 'H' or x[i] == 'V' or x[i] == 'W' or x[i] == 'K' or x[i] == 'S' or x[i] == 'R' or x[i] == 'Y' or x[i] == 'M':
            count_any_letter = count_any_letter + 1

# N = A or C or G or T (any)
# B = C or G or T (not A)
# D = A or G or T (not C)
# H = A or C or T (not G)
# V = A or C or G (not T)
# W = A or T
# S = C or G
# R = A or G
# Y = C or T
# M = A or C
# K = G or T

    total_length = len(x)
    proportion_G = float(count_G)/float(total_length)*100
    proportion_T = float(count_T)/float(total_length)*100
    proportion_C = float(count_C)/float(total_length)*100
    proportion_A = float(count_A)/float(total_length)*100
    proportion_N = float(count_N)/float(total_length)*100
    proportion_U = float(count_U)/float(total_length)*100
    
    print("[]total A bases : " + str(count_A) + " []proportion to entire sequence : " + str(proportion_A) + " %")
    print("[]total T bases : " + str(count_T) + " []proportion to entire sequence : " + str(proportion_T) + " %")
    print("[]total G bases : " + str(count_G) + " []proportion to entire sequence : " + str(proportion_G) + " %")
    print("[]total C bases : " + str(count_C) + " []proportion to entire sequence : " + str(proportion_C) + " %")
    print("\n")
    print("[]total N bases : " + str(count_N) + " []proportion to entire sequence : " + str(proportion_N) + " %")
    print("[]total U bases : " + str(count_U) + " []proportion to entire sequence : " + str(proportion_U) + " %")
    
    print("[]standard fixed-random base: " + str(count_any_letter) + " nt" )
    print("[]sum length is : " + str(total_length) + " nt")

to_count_actg = count_actg(x)

temp_save_note = "#temporately saved input sequence, mannually typing in reference information required /Rachel"

temp_owner = raw_input("who is designer or owner of this input sequences? \n")

temp_save_file = open('/Users/rachelnong/Desktop/RYN2018_log_work/temperatory_saved.txt', 'w')

temp_save_file.write(temp_save_note)
temp_save_file.write('\n\n')
temp_save_file.write(temp_owner)
temp_save_file.write('\n\n')
temp_save_file.write(x)
temp_save_file.write('\n\n')

temp_save_file.close()
