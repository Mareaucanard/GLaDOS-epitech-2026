from LISP.Functional import TestFile, lispFileList
from sys import argv
from multiprocessing import Pool
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
'h'
Hello, world!
["This", "is", "a", "string", "list"]"""

typeof_res = """string
char
list
integer
float
nil"""

loops_output = "\n".join(str(x) for x in range(10))

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
    GladosFile("okay/operators/div.gld", output="0\n0.5"),
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
    GladosFile("okay/operators/ternary.gld", output="1\n2"),

    GladosFile("okay/factorial.gld", output="3628800"),
    GladosFile("okay/factorial_ternary.gld", output="3628800"),
    GladosFile("okay/sort.gld", output="[1, 3, 4, 5, 12]"),

    GladosFile("okay/builtin/acos.gld", output="0.0"),
    GladosFile("okay/builtin/all.gld", output="[False, False, True, True]"),
    GladosFile("okay/builtin/any.gld", output="[True, False, True, False]"),
    GladosFile("okay/builtin/append.gld", output="[0, 1, 2, 3]"),
    GladosFile("okay/builtin/asin.gld", output="0.0"),
    GladosFile("okay/builtin/atan.gld", output="0.0"),
    GladosFile("okay/builtin/ceil.gld", output="2\n2"),
    GladosFile("okay/builtin/concat.gld", output="Hello World"),
    GladosFile("okay/builtin/cos.gld", output="1.0"),
    GladosFile("okay/builtin/exp.gld", output="1.0"),
    GladosFile("okay/builtin/float.gld", output="0.5"),
    GladosFile("okay/builtin/floor.gld", output="1\n1"),
    GladosFile("okay/builtin/head.gld", output="0"),
    GladosFile("okay/builtin/int.gld", output="2"),
    GladosFile("okay/builtin/join.gld", output="Hello World"),
    GladosFile("okay/builtin/len.gld", output="10"),
    GladosFile("okay/builtin/log.gld", output="1.0"),
    GladosFile("okay/builtin/map.gld", output="[0, 1, 4, 9, 16]"),
    GladosFile("okay/builtin/nils.gld", output="[nil, nil, nil]"),
    GladosFile("okay/builtin/prepend.gld", output="[-1, 0, 1, 2]"),
    GladosFile("okay/builtin/rand.gld", output="5"),
    GladosFile("okay/builtin/randrange.gld", output="5"),
    GladosFile("okay/builtin/range.gld", output="[0, 1, 2]"),
    GladosFile("okay/builtin/reverse.gld", output="dlrow olleH"),
    GladosFile("okay/builtin/round.gld", output="1\n2"),
    GladosFile("okay/builtin/sin.gld", output="0.0"),
    GladosFile("okay/builtin/split.gld", output="[\"Hello,\", \"world!\"]"),
    GladosFile("okay/builtin/sqrt.gld", output="4.0"),
    GladosFile("okay/builtin/string.gld", output="11"),
    GladosFile("okay/builtin/tail.gld", output="[1, 2, 3, 4]"),
    GladosFile("okay/builtin/tan.gld", output="0.0"),
    GladosFile("okay/builtin/throw.gld",
               expected_in_error=["Throw test"], expected_code=1),
    GladosFile("okay/builtin/typeof.gld", output=typeof_res),
    GladosFile("okay/builtin/uniform.gld", output="5"),
    GladosFile("okay/builtin/vargs.gld", output="[]"),

] + lispFileList


skipGood = "--skip-good" in argv


def handle_file(file):
    file.run()
    return file.checkResult(skipGood)

def main():
    with Pool() as p:
        success_count = sum(p.map(handle_file, fileList))
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
