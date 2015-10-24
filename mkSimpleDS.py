import sys

ans = open("data/ans.txt").readlines()
eq = open("data/eq.txt").readlines()
q = open("data/q.txt").readlines()
idx = open("HighFunctioningData.txt")

for line in idx:
    i = line.split(".")[0]
    i = int(i)
    print(q[i].strip())
    print(eq[i].strip())
    print(ans[i].strip())
