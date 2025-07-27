def normalise(markov: dict) -> dict:
    ret = dict()
    for (key,entry) in markov.items():
        ret[key] = {}
        total = sum(entry.values())
        for (nextkey,nextval) in entry.items():
            prob = int((nextval/total)*100)
            ret[key][nextkey] = prob
    return ret
def gettable(markov: dict) -> str:
    ret = f'table[{len(markov.keys())+1}] \n'
    for word in markov.keys():
        ret += f'"{word}",'
    ret += f'"UNK";'
    return ret
def canonise(word: str) -> str: 
    return "".join(["r"] + [ hex(ord(x))[2:] for x in word ])

with open("lyrics.txt", "r") as f:
    file = str(f.read())
    tokens = file.split()
    markov = dict()

    for i in range(len(tokens)):
        token = tokens[i]
        nextt = tokens[i+1] if (i+1)<len(tokens) else None
        if token not in markov:
            markov[token] = {}
        if nextt != None:
            if nextt in markov[token]:
                markov[token][nextt] += 1
            else:
                markov[token][nextt] = 1
        prev = token
    markov = normalise(markov)
    table = gettable(markov)

    # Now, build a transition function
    for key in markov.keys():
        seuil = 0
        fn = f"trans_{canonise(key)}()" + '{\n\textrn rand;' 'auto n; n = rand() % 100;\n'
        for (nextt,prob) in markov[key].items():
            seuil += prob


    print(table)
