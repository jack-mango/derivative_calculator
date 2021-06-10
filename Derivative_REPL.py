import sys
from BNF_Derivatives import derivative_calculate


def read_eval_print():
    while True:
        try:
            line = input("derivative? ")
            if line.lower() == 'exit' or line.lower() == 'quit':
                return
            print(derivative_calculate(line))
        except EOFError:
            sys.exit(0)
        except:
            print(f"Bad input: {line}", file=sys.stderr)

if __name__ == "__main__":
    read_eval_print()