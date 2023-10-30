from LISP.Functional import TestFile, lispFileList
from sys import argv
try:
    from termcolor import colored
except ModuleNotFoundError:
    def colored(string, *args, **kwargs):
        return string


class GladosFile(TestFile):
    def __init__(self, filename, expected_code=0, output="", expected_in_error=[]) -> None:
        super().__init__(filename, expected_code, output,
                         expected_in_error, folder="test_files/glados/")
        self.options = ["--run"]


basic_print_output = """0
1
-1
1.0
-1.0
0.0
True
False
nil
h
Hello, world!
["This","is","a","string","list"]"""

loops_output = ["\n".join(str(x) for x in range(10))]

fileList = [
    GladosFile("okay/basic_prints.gld", output=basic_print_output),
    GladosFile("okay/for.gld", output=loops_output),
    GladosFile("okay/while.gld", output=loops_output),
    GladosFile("okay/while.gld", output=loops_output),
    GladosFile("okay/function.gld", output="10"),
    GladosFile("okay/if.gld", output="0\n1\n2"),
    GladosFile("okay/no_return_function.gld", output="nil"),
    GladosFile("okay/pre_processor.gld", output="3.14"),
    GladosFile("okay/priority.gld", output="14\n20"),

    GladosFile("okay/operators/add.gld", output="3"),
    GladosFile("okay/operators/and.gld", output="False"),
    GladosFile("okay/operators/div.gld", output="0.5"),
    GladosFile("okay/operators/equal.gld", output="True"),
    GladosFile("okay/operators/greater_equal.gld", output="True"),
    GladosFile("okay/operators/greater.gld", output="False"),
    GladosFile("okay/operators/lower_equal.gld", output="True"),
    GladosFile("okay/operators/lower.gld", output="True"),
    GladosFile("okay/operators/mod.gld", output="3"),
    GladosFile("okay/operators/mul.gld", output="12"),
    GladosFile("okay/operators/negative.gld", output="-12"),
    GladosFile("okay/operators/not_eq.gld", output="True"),
    GladosFile("okay/operators/not.gld", output="False"),
    GladosFile("okay/operators/or.gld", output="True"),
    GladosFile("okay/operators/sub.gld", output="-1"),
    GladosFile("okay/operators/ternary.gld", output="1\n2")


] + lispFileList


def main():
    skipGood = "--skip-good" in argv
    success_count = 0
    for file in fileList:
        file.run()
        if (file.checkResult(skipGood)):
            success_count += 1
    if (success_count == len(fileList)):
        print(
            colored(f"{success_count}/{success_count} test passed", "light_green"))
        return 0
    else:
        print(
            colored(f"{success_count}/{len(fileList)} test passed", "light_red"))
        return 84


if __name__ == "__main__":
    main()
