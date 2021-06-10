from lark import Lark, Transformer
import numpy as np
import sys

grammar = """
    start: add_sub_expr

    ?add_sub_expr: (exp_expr | mul_expr | div_expr | sub_expression) (ADD_SUB add_sub_expr)?

    mul_expr: (div_expr | sub_expression) "*" add_sub_expr | ("(" add_sub_expr ")" sub_expression) | (sub_expression ("(" add_sub_expr ")")+) | (SIGNED_NUMBER func_expr)

    div_expr: (div_expr | sub_expression) "/" sub_expression 

    exp_expr: sub_expression "^" (exp_expr | sub_expression)

    sub_expression: "(" add_sub_expr ")" | exp_expr | func_expr | SIGNED_NUMBER

    func_expr: FUNC sub_expression

    FUNC: "exp" | "ln" | "sin" | "cos" | "tan" | "csc" | "sec" | "cot" | "arcsin" | "arccos" | "arctan" | "sqrt"

    ADD_SUB: "+" | "-"


    %ignore /\s+/
    %import common.SIGNED_NUMBER
    """

parser = Lark(grammar)

class Eval(Transformer):
    def start(self, args):
        return args[0]
    
    def add_sub_expr(self, args):
        op = args[1]
        operand1, operand2 = args[0], args[2]
        if op == '+':
            return operand1 + operand2
        elif op == '-':
            return operand1 - operand2

    def mul_expr(self, args):
        return np.prod(args)
        
    def div_expr(self, args):
        return args[0] / args[1] 
    
    def exp_expr(self, args):
        return args[0] ** args[1]

    def sub_expression(self, args):
        return args[0]

    def func_expr(self, args):
        return args[0](args[1])
            
    def SIGNED_NUMBER(self, num):
        return float(num)

    def ADD_SUB(self, operator):
        return operator
    
    def FUNC(self, func):
        functions = {'exp': np.exp, 'ln': np.log, 'sin': np.sin, 'cos': np.cos, 'tan': np.tan, 
                'csc': lambda x : 1 / np.sin(x), 'sec' : lambda x : 1 / np.cos(x), 'cot': lambda x : 1 / np.tan(x), 
                'arcsin': np.arcsin, 'arccos': np.arccos, 'arctan': np.arctan, 'sqrt': np.sqrt}
        return functions[func]

evaluator = Eval()

def read_eval_print():
    while True:
        try:
            line = input("calc> ")
            if line.lower() == 'exit' or line.lower() == 'quit':
                return
            tree = parser.parse(line)
            print(evaluator.transform(tree))
        except EOFError:
            sys.exit(0)
        except:
            print(f"Bad input: {line}", file=sys.stderr)

if __name__ == "__main__":
    read_eval_print()





