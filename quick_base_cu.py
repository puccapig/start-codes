#To change sequence base U to base T.
#Rachel

print("#### we would like to change base 'U' to base 'T' ####") 

import string

x = raw_input("#### your sequence is: ")

def clean_up(x):
    z = ""
    for i in range(0, len(x)):
        if x[i] is not 'A' and x[i] is not 'T' and x[i] is not 'G' and x[i] is not 'C' and x[i] is not 'U':
            z += ''
        else: z += x[i]                                                                                         
    print ("####     new sequence: " + z)
    return z

def change_U_to_T(z):
    y = ""
    for i in range(0, len(z)):
        if z[i] is not 'U':
            y += z[i]
        else: y += 'T'
    print ("#### base U changed to T: " + y)
    return y

def count_nucleotides(z):
    total_nt = str(len(z))
    print("Length of your input sequence is "+ total_nt + " nt.")
    return(total_nt)

cleaned_sequence = clean_up(x)
change_U_to_T(cleaned_sequence)
count_nucleotides(cleaned_sequence)

#2018-02-15 add scripts to write input sequence to a text file

tempsave_note = "#temporately saved input sequence, no reference information - required to mannually typed in /Rachel"

temp_owner = raw_input("who is designer or owner of this input sequences? \n")

tempsave_file = open('temp_saved_sequence_justnu.txt', 'w')

tempsave_file.write(tempsave_note)
tempsave_file.write('\n\n')
tempsave_file.write(temp_owner)
tempsave_file.write('\n\n')
tempsave_file.write(x)
tempsave_file.write('\n\n')

tempsave_file.close()
