import subprocess
from termcolor import colored

class TestFile:
    folder = "test_files/"
    def __init__(self, filename, expected_code = 0, expected_output = "", expected_error = "") -> None:
        self.filename = filename
        self.timed_out = False
        self.expected_code = expected_code
        self.expected_error = expected_error
        self.expected_output = expected_output
        self.actual = ""
        self.error_channel = ""
        self.exit_code = 0
        self.has_ran = False

    def run(self):
        self.has_ran = True
        try:
            res = subprocess.run(["./glados", self.folder + self.filename], capture_output=True, timeout=2, stdin=subprocess.DEVNULL)
        except subprocess.TimeoutExpired as e:
            self.timed_out = True
            return
        self.actual = res.stdout.decode("utf-8")
        self.error_channel = res.stderr.decode("utf-8")
        self.exit_code = res.returncode

    def checkResult(self) -> bool:
        if (not self.has_ran):
            print(f"File {self.filename} has not been ran yet; skipping\n")
            return False
        if self.timed_out:
            print(colored(f"File {self.filename} has timed out\n", "light_red"))
            return False
        elif self.actual != self.expected_output or self.error_channel != self.expected_error or self.exit_code != self.exit_code:
            s = f"File {self.filename}: KO"
            if self.exit_code != self.exit_code:
                s = s + f"\nExpected exit code {self.exit_code} but got {self.expected_code}"
            if self.actual != self.expected_output:
                s = s + f"\nExpected output:\n'{self.expected_output}'\nBut got:\n'{self.actual}'"
            if self.error_channel != self.expected_error:
                s = s + f"\nExpected error:\n'{self.expected_error}'\nBut got\n'{self.error_channel}'"
            print(colored(s + "\n", "light_red"))
            return False
        else:
            print(colored(f"File {self.filename}: OK\n", "light_green"))
            return True

fileList = [
    TestFile("okay/factorial.txt", expected_output="120"),
    TestFile("okay/foo.txt", expected_output="42"),
    TestFile("okay/lazy_vars.txt", expected_output="1"),
    TestFile("okay/var_attribution.txt", expected_output=""),

    TestFile("okay/basic_operations/add.txt", expected_output="5"),
    TestFile("okay/basic_operations/sub.txt", expected_output="-1"),
    TestFile("okay/basic_operations/mul.txt", expected_output="6"),
    TestFile("okay/basic_operations/div.txt", expected_output="0"),
    TestFile("okay/basic_operations/mod.txt", expected_output="2"),
    TestFile("okay/basic_operations/pow.txt", expected_output="8"),

    TestFile("error/div-zero.txt", expected_code=84, expected_error="Error: Division by zero"),
    TestFile("error/unknown_var.txt", expected_code=84, expected_error="Error: Symbol 'foo' is not defined"),
    TestFile("error/missing.txt", expected_code=84, expected_error="Error: Symbol can not contain spaces"),
    TestFile("error/unmatched.txt", expected_code=84, expected_error="Error: Unmatched parenthesis"),
]

def main():
    success_count = 0
    for file in fileList:
        file.run()
        if (file.checkResult()):
            success_count += 1
    if (success_count == len(fileList)):
        print(colored(f"{success_count}/{success_count} test passed", "light_green"))
    else:
        print(colored(f"{success_count}/{len(fileList)} test passed", "light_red"))

if __name__ == "__main__":
    main()
