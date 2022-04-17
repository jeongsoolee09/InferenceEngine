import os

os.chdir("/Users/jslee/spechunter-benchs/yorubaname-website")

with open("UDFs.txt", "r") as udf_file:
    udf_lines = udf_file.readlines()
    udf_lines = list(map(lambda string: string.rstrip(), udf_lines))
    udf_lines = list(
        filter(
            lambda string: "<init>" not in string
            and "<clinit>" not in string
            and "Tests" not in string
            and "lambda" not in string
            and "Lambda" not in string,
            udf_lines,
        )
    )
    udf_lines = list(map(lambda string: f'("{string}", []);\n', udf_lines))

with open("APIs.txt", "r") as api_file:
    api_lines = api_file.readlines()
    api_lines = list(map(lambda string: string.rstrip(), api_lines))
    api_lines = list(
        filter(lambda string: "<init>" not in string and "__" not in string, api_lines)
    )
    api_lines = list(map(lambda string: f'("{string}", []);\n', api_lines))

udf_lines = (
    ["let yorubaname_udf_solution : solution array = [|"]
    + sorted(udf_lines, key=lambda str: (str.split(".")[0]).split(" ")[-1])
    + ["|]\n"]
)
api_lines = (
    ["let yorubaname_api_solution : solution array = [|"]
    + sorted(api_lines, key=lambda str: (str.split(".")[0]).split(" ")[-1])
    + ["|]\n"]
)

with open(
    "/Users/jslee/Dropbox/InferenceEngine/lib/yorubaname_solution.ml", "w"
) as yorubaname_solution:
    yorubaname_solution.write("type solution = Method.t * string list\n\n")
    yorubaname_solution.writelines(udf_lines)
    yorubaname_solution.write("\n\n")
    yorubaname_solution.writelines(api_lines)
