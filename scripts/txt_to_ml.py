import os
os.chdir("/Users/jslee/Dropbox/InferenceEngine")

with open("UDFs.txt", "r") as udf_file:
    udf_lines = udf_file.readlines()
    udf_lines = list(map(lambda string: string.rstrip(), udf_lines))
    udf_lines = list(filter(lambda string: "<init>" not in string and\
                            "<clinit>" not in string and\
                            "Tests" not in string and\
                            "lambda" not in string and\
                            "Lambda" not in string, udf_lines))
    udf_lines = list(map(lambda string: f"(\"{string}\", []);\n", udf_lines))

with open("APIs.txt", "r") as api_file:
    api_lines = api_file.readlines()
    api_lines = list(map(lambda string: string.rstrip(), api_lines))
    api_lines = list(filter(lambda string: "<init>" not in string and "__" not in string, api_lines))
    api_lines = list(map(lambda string: f"(\"{string}\", []);\n", api_lines))

lines = ["let sagan_solution = ["] + udf_lines + api_lines + ["]\n"]

with open("sagan_solution.ml", "w") as sagan_solution:
    sagan_solution.writelines(lines)
