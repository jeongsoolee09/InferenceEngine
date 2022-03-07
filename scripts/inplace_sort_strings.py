def get_class(sig):
    return sig.split(".")[0].split(" ")[-1]


with open("../solutions/sagan_api.txt", "r") as f:
    lines = f.readlines()
    lines.sort(key=get_class)


with open("../solutions/sagan_api.txt", "w") as f:
    f.writelines(lines)
