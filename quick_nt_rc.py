#To write revese complementary strand of input sequences
#Rachel
#Read through on 2019-OKT-19

##note that when using python3 input is used as raw_input for reading screen.

import string

x = raw_input("input nt(s) to reverse:")
#x = input("input nt(s) to reverse:")

def reverse_complementary(x):
    rc = string.maketrans('UAGTCagtc', 'ATCAGtcag')
    z = x.translate(rc)[::-1]
    print("reverse complementary sequence:" + z)

reverse = reverse_complementary(x)
