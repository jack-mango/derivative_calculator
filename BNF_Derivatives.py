from lark import Lark, Transformer
import numpy as np

class Add_Sub:
    def __init__(self, operator, op1, op2):
        self.operator = operator
        self.op1 = op1
        self.op2 = op2

    def __str__(self):
        return f'{str(self.op1)} {self.operator} {str(self.op2)}'

    def __eq__(self, other):
        return type(other) == Add_Sub and self.operator == other.operator and self.op1 == other.op1 and self.op2 == other.op2

    def simplify(self):
        if type(self.op1.simplify()) == Number and type(self.op2.simplify()) == Number:
            return Number(eval(f'{self.op1.simplify()} {self.operator} {self.op2.simplify()}'))
        elif self.op1.simplify() == Number(0):
            return self.op2.simplify()
        elif self.op2.simplify() == Number(0):
            return self.op1.simplify()
        else:
            return Add_Sub(self.operator, self.op1.simplify(), self.op2.simplify())

    def derivative(self):
        return Add_Sub(self.operator, self.op1.derivative(), self.op2.derivative()).simplify()

class Product:
    def __init__(self, op1, op2):
        self.op1 = op1
        self.op2 = op2

    def __str__(self):
        if type(self.op1) == Number and type(self.op2) == Variable:
            return f'{self.op1}{self.op2}'
        elif type(self.op1) == Number or type(self.op1) == Product:
            return f'{self.op1}({self.op2})'
        else:
            return f'({str(self.op1)})({str(self.op2)})'

    def __eq__(self, other):
        return type(other) == Product and self.op1 == other.op1 and self.op2 == other.op2

    def simplify(self):
        if type(self.op1.simplify()) == Number and type(self.op2.simplify()) == Number:
            return Number(eval(f'{self.op1.simplify()} * {self.op2.simplify()}'))
        elif self.op1.simplify() == Number(1):
            return self.op2.simplify()
        elif self.op2.simplify() == Number(1):
            return self.op1.simplify()
        elif self.op1.simplify() == Number(0) or self.op2.simplify() == Number(0):
            return Number(0)
        else:
            return Product(self.op1.simplify(), self.op2.simplify())

    def derivative(self):
        return Add_Sub('+', Product(self.op1.derivative(), self.op2), Product(self.op2.derivative(), self.op1)).simplify()

class Quotient:
    def __init__(self, op1, op2):
        self.op1 = op1
        self.op2 = op2  

    def __str__(self):
        if (type(self.op1) == Variable or type(self.op1) == Number) and (type(self.op1) == Number or type(self.op2) == Variable):
            return f'{self.op1} / {self.op2}' 
        elif type(self.op1) == Number or type(self.op1) == Variable:
            return f'{self.op1} / ({self.op2})'
        elif type(self.op2) == Number or type(self.op2) == Variable:
            return f'({self.op1}) / {self.op2}'
        else:
            return f'({str(self.op1)}) / ({str(self.op2)})'

    def __eq__(self, other):
        return type(other) == Quotient and self.op1 == other.op1 and self.op2 == other.op2

    def simplify(self):
        if type(self.op1.simplify()) == Number and type(self.op2.simplify()) == Number:
            return Number(eval(f'{self.op1.simplify()} / {self.op2.simplify()}'))
        elif self.op2.simplify() == Number(1):
            return self.op1.simplify()
        elif self.op1.simplify() == Number(0):
            return Number(0)
        else:
            return Quotient(self.op1.simplify(), self.op2.simplify())
        
    def derivative(self):
        return Quotient(Add_Sub('-', Product(self.op1.derivative(), self.op2), Product(self.op2.derivative(), self.op1)), Exponential(self.op2, 2)).simplify()

class Exponential:
    def __init__(self, base, exponent):
        self.base = base
        self.exponent = exponent

    def __str__(self):
        if (type(self.base) == Number and type (self.exponent) == Variable) or (type(self.base) == Variable and type(self.exponent) == Number):
            return f'{self.base} ^ {self.exponent}'
        elif type(self.base) == Number or type(self.base) == Variable:
            return f'{self.base} ^ ({self.exponent})'
        elif type(self.exponent) == Number or type(self.exponent) == Variable:
            return f'({self.base}) ^ {self.exponent}'
        else:
            return f'({str(self.base)}) ^ ({str(self.exponent)})'

    def __eq__(self, other):
        return type(other) == Exponential and self.base == other.base and self.exponent == other.exponent

    def simplify(self):
        if type(self.base.simplify()) == Number and type(self.exponent.simplify()) == Number:
            return Number(eval(f'{self.base.simplify()} ** {self.exponent.simplify()}'))
        elif self.exponent.simplify() == Number(0) or self.base.simplify() == Number(1):
            return Number(1)
        elif self.exponent.simplify() == Number(1):
            return self.base
        else:
            return Exponential(self.base.simplify(), self.exponent.simplify())

    def derivative(self):
        if 'x' in str(self.exponent):
            print('test')
            return Product(Product(Function('ln', self.base), self), self.exponent.derivative()).simplify()
        else:
            return Product(Product(self.exponent, Exponential(self.base, Add_Sub('-', self.exponent, Number(1)))), self.base.derivative()).simplify()

class Function:
    def __init__(self, func, expr):
        self.func = func
        self.expr = expr

    def __str__(self):
        return f'{str(self.func)}({str(self.expr)})'

    def __eq__(self, other):
        return type(other) == Function and self.func == other.func and self.expr == other.expr

    def simplify(self):
        functions = {'exp': np.exp, 'ln': np.log, 'sin': np.sin, 'cos': np.cos, 'tan': np.tan, 
                    'csc': lambda x : 1 / np.sin(x), 'sec' : lambda x : 1 / np.cos(x), 'cot': lambda x : 1 / np.tan(x),
                    'arcsin': np.arcsin, 'arccos': np.arccos, 'arctan': np.arctan, 'sqrt': np.sqrt}
        return Function(self.func, self.expr.simplify())

    def derivative(self):
        func_derivatives = {'exp': Function('exp', self.expr), 'ln': Quotient(Number(1), self.expr), 'sin': Function('cos', self.expr),
                            'cos': Product(Number(-1), Function('sin', self.expr)), 'tan': Exponential(Function('sec', self.expr), Number(2)), 
                            'csc': Product(Number(-1), Product(self, Function('cot', self.expr))), 'sec': Product(self, Function('tan', self.expr)),
                            'cot': Product(Number(-1), Exponential(Function('csc', self.expr), Number(2))), 'arcsin': Quotient(1, Function('sqrt', (Add_Sub('-', Number(1), Exponential(self.expr, Number(2)))))),
                            'arccos': Product(Number(-1), Quotient(Number(1), Function('sqrt', (Add_Sub('-', Number(1), Exponential(self.expr, Number(2))))))),
                            'arctan': Quotient(Number(1), (Add_Sub('+', Number(1), Exponential(self.expr, Number(2))))), 'sqrt': Quotient(Number(2), Product(Number(2), Function('sqrt', self.expr)))}

        return Product(func_derivatives[self.func], self.expr.derivative()).simplify()

class Sub_expression:
    def __init__(self, expr):
        self.expr = expr

    def __str__(self):
        return f'({str(self.expr)})'

    def __eq__(self, other):
        return type(other) == Sub_expression and self.expr == other.expr

    def simplify(self):
        if type(self.expr) == Number or type(self.expr) == Variable:
            return self.expr
        else:
            return self.expr.simplify()

    def derivative(self):
        return Sub_expression(self.expr.derivative()).simplify()

class Variable:
    def __init__(self, var):
        self.var = var

    def __str__(self):
        return self.var

    def __eq__(self, other):
        return type(other) == Variable and self.var == other.var

    def simplify(self):
        return self
    
    def derivative(self):
        return Number(1)

class Number:
    def __init__(self, num):
        self.num = num

    def __str__(self):
        return f'{self.num}'

    def __eq__(self, other):
        return type(other) == Number and self.num == other.num

    def simplify(self):
        return self

    def derivative(self):
        return Number(0)



grammar = """
        start: add_sub_expr

        ?add_sub_expr: (exp_expr | mul_expr | div_expr | sub_expression) (ADD_SUB add_sub_expr)?

        mul_expr: ((div_expr | sub_expression) "*" add_sub_expr) | ("(" add_sub_expr ")" sub_expression) | (sub_expression ("(" add_sub_expr ")")+ ) | ((SIGNED_NUMBER | VAR) func_expr) | SIGNED_NUMBER VAR

        div_expr: (div_expr | sub_expression) "/" sub_expression 

        exp_expr: sub_expression "^" (exp_expr | sub_expression)

        sub_expression: "(" add_sub_expr ")" | exp_expr | func_expr | SIGNED_NUMBER | VAR

        func_expr: FUNC sub_expression

        FUNC: "exp" | "ln" | "sin" | "cos" | "tan" | "csc" | "sec" | "cot" | "arcsin" | "arccos" | "arctan" | "sqrt"

        VAR: /[a-z]/

        ADD_SUB: "+" | "-"

        %ignore /\s+/
        %import common.SIGNED_NUMBER
        """


parser = Lark(grammar)

class Derivative(Transformer):
    def start(self, args):
        return args[0]
    
    def add_sub_expr(self, args):
        return Add_Sub(args[1], args[0], args[2])
    
    def mul_expr(self, args):
        return Product(args[0], args[1])

    def div_expr(self, args):
        return Quotient(args[0], args[1])
    
    def exp_expr(self, args):
        return Exponential(args[0], args[1])

    def sub_expression(self, args):
        return Sub_expression(args[0])

    def func_expr(self, args):
        return Function(args[0], args[1])
    
    def FUNC(self, function):
        return function
    
    def VAR(self, var):
        return Variable(var)

    def ADD_SUB(self, op):
        return op
    
    def SIGNED_NUMBER(self, num):
        return Number(num)


def derivative_calculate(expression):
    unsimplified = Derivative().transform(parser.parse(expression)).derivative()
    simplified = unsimplified.simplify()
    while not (simplified == unsimplified):
        unsimplified = simplified
        simplified = simplified.simplify()
    return simplified






