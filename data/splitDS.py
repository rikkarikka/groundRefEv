import sys

with open(sys.argv[1]) as f:
    ff = f.readlines()
    with open("q.txt",'w') as q:
        with open("a.txt",'w') as a:
            with open("eq.txt",'w') as eq:
                i = 0
                while i < len(ff):
                    q.write(ff[i])
                    i+=1
                    eq.write(ff[i])
                    i+=1 
                    a.write(ff[i])
                    i+=1

