import sys
import argparse
from enum import Enum
from abc import ABC, abstractmethod
from collections import OrderedDict
import yaml
import ply.lex as lex
import ply.yacc as yacc

# Class Definitions

class Exit(Enum):
    SUCCESS = 0
    NOT_IMPLEMENTED = 1
    LEXING_ERROR = 2
    PARSING_ERROR = 3

    def _is_error(self):
        if self in [self.SUCCESS, self.NOT_IMPLEMENTED]: return False
        else: return True

    def _print(self, *args):
        print('error:', end='\t')
        if self is self.LEXING_ERROR:
            if args[0]:
                print('Illegal Character {} in Line {}'.format(args[0].value[0], args[0].lexer.lineno))
            else:
                print('Illegal Character at End of File')

        elif self is self.PARSING_ERROR:
            if args[0]:
                print('Syntax Error {} in Line {}'.format(args[0].value[0], args[0].lexer.lineno))
            else:
                print('Syntax Error at End of File')



    def __call__(self, *args):
        if self._is_error():
            self._print(*args)

        sys.exit(self.value)


class Node(ABC):
    @abstractmethod
    def items(self):
        return [('name', self.name)]
    def __init__(self, lineno):
        self.lineno = lineno

class Nodelist(Node):
    def __init__(self, lineno, node):
        super().__init__(lineno)
        self.nodelist = [node]
    def insert(self, node):
        self.nodelist = [node] + self.nodelist
        return self
    def items(self):
        return super().items() + [(self.listname, self.nodelist)]

class Prog(Node):
    name = 'prog'
    def __init__(self, lineno, externs, funcs):
        super().__init__(lineno)
        self.externs = externs
        self.funcs = funcs
    def items(self):
        items = super().items()
        if self.externs: items.append(('externs', self.externs))
        items.append(('funcs', self.funcs))
        return items

class Externs(Nodelist):
    name = 'externs'
    listname = name

class Extern(Node):
    name = 'extern'
    def __init__(self, lineno, ret_type, globid, tdecls):
        super().__init__(lineno)
        self.ret_type = ret_type
        self.globid = globid
        self.tdecls = tdecls
    def items(self):
        return super().items() + [('ret_type', self.ret_type),
                                  ('globid', self.globid),
                                  ('tdecls', self.tdecls)]

class Funcs(Nodelist):
    name = 'funcs'
    listname = name

class Func(Node):
    name = 'func'
    def __init__(self, lineno, ret_type, globid, vdecls, blk):
        super().__init__(lineno)
        self.ret_type = ret_type
        self.globid = globid
        self.vdecls = vdecls
        self.blk = blk
    def items(self):
        items = super().items()
        items.append(('ret_type', self.ret_type))
        items.append(('globid', self.globid))
        if self.vdecls: items.append(('vdecls', self.vdecls))
        items.append(('blk', self.blk))
        return items

class Blk(Node):
    name = 'blk'
    def __init__(self, lineno, contents):
        super().__init__(lineno)
        self.contents = contents
    def items(self):
        return super().items() + [('contents', self.contents)]

class Stmts(Nodelist):
    name = 'stmts'
    listname = name

class Ret(Node):
    name = 'ret'
    def __init__(self, lineno, exp):
        super().__init__(lineno)
        self.exp = exp
    def items(self):
        items = super().items()
        if self.exp: items.append(('exp', self.exp))
        return items

class Vardeclstmt(Node):
    name = 'vardeclstmt'
    def __init__(self, lineno, vdecl, exp):
        super().__init__(lineno)
        self.vdecl = vdecl
        self.exp = exp
    def items(self):
        return super().items() + [('vdecl', self.vdecl),
                                  ('exp', self.exp)]

class Expstmt(Node):
    name = 'expstmt'
    def __init__(self, lineno, exp):
        super().__init__(lineno)
        self.exp = exp
    def items(self):
        return super().items() + [('exp', self.exp)]

class While(Node):
    name = 'while'
    def __init__(self, lineno, cond, stmt):
        super().__init__(lineno)
        self.cond = cond
        self.stmt = stmt
    def items(self):
        return super().items() + [('cond', self.cond),
                                  ('stmt', self.stmt)]

class If(Node):
    name = 'if'
    def __init__(self, lineno, cond, stmt, else_stmt):
        super().__init__(lineno)
        self.cond = cond
        self.stmt = stmt
        self.else_stmt = else_stmt
    def items(self):
        items = super().items()
        items.append(('cond', self.cond))
        items.append(('stmt', self.stmt))
        if self.else_stmt: items.append(('else_stmt', self.else_stmt))
        return items

class Print(Node):
    name = 'print'
    def __init__(self, lineno, exp):
        super().__init__(lineno)
        self.exp = exp
    def items(self):
        return super().items() + [('exp', self.exp)]

class Printslit(Node):
    name = 'printslit'
    def __init__(self, lineno, string):
        super().__init__(lineno)
        self.string = string
    def items(self):
        return super().items() + [('string', self.string)]

class Exps(Nodelist):
    name = 'exps'
    listname = name

class Funccall(Node):
    name = 'funccall'
    def __init__(self, lineno, globid, params):
        super().__init__(lineno)
        self.globid = globid
        self.params = params
    def items(self):
        items = super().items()
        items.append(('globid', self.globid))
        if self.params: items.append(('params', self.params))
        return items

class Assign(Node):
    name = 'assign'
    def __init__(self, lineno, var, exp):
        super().__init__(lineno)
        self.var = var
        self.exp = exp
    def items(self):
        return super().items() + [('var', self.var),
                                  ('exp', self.exp)]

class Caststmt(Node):
    name = 'caststmt'
    def __init__(self, lineno, type, exp):
        super().__init__(lineno)
        self.type = type
        self.exp = exp
    def items(self):
        return super().items() + [('type', self.type),
                                  ('exp', self.exp)]

class Binop(Node):
    name = 'binop'
    def __init__(self, lineno, op, lhs, rhs):
        super().__init__(lineno)
        self.op = op
        self.lhs = lhs
        self.rhs = rhs
    def items(self):
        return super().items() + [('op', self.op),
                                  ('lhs', self.lhs),
                                  ('rhs', self.rhs)]

class Uop(Node):
    name = 'uop'
    def __init__(self, lineno, op, exp):
        super().__init__(lineno)
        self.op = op
        self.exp = exp
    def items(self):
        return super().items() + [('op', self.op),
                                  ('exp', self.exp)]

class Litnode(Node):
    def __init__(self, lineno, value):
        super().__init__(lineno)
        self.value = value
    def items(self):
        return super().items() + [('value', self.value)]

class Lit(Litnode):
    name = 'lit'

class Flit(Litnode):
    name = 'flit'

class Blit(Litnode):
    name = 'blit'

class Varval(Node):
    name = 'varval'
    def __init__(self, lineno, var):
        super().__init__(lineno)
        self.var = var
    def items(self):
        return super().items() + [('var', self.var)]

class Vdecls(Nodelist):
    name = 'vdecls'
    listname = 'vars'

class Tdecls(Nodelist):
    name = 'tdecls'
    listname = 'types'

class Vdecl(Node):
    name = 'vdecl'
    def __init__(self, lineno, type, var):
        super().__init__(lineno)
        self.type = type
        self.var = var
    def items(self):
        #return super().items() + [('type', self.type),
        #                          ('var', self.var)]
        return [('node', self.name)] + [('type', self.type),
                                        ('var', self.var)]



# Lexing Rules

reserved = {
    'extern': 'EXTERN',
    'def': 'DEF',
    'return': 'RETURN',
    'while': 'WHILE',
    'if': 'IF',
    'else': 'ELSE',
    'print': 'PRINT',
    'true': 'TRUE',
    'false': 'FALSE',
    'int': 'TYPEINT',
    'cint': 'TYPECINT',
    'float': 'TYPEFLOAT',
    'bool': 'TYPEBOOL',
    'void': 'TYPEVOID',
    'noalias': 'TYPENOALIAS',
    'ref': 'TYPEREF'
}

tokens = [ 'NAME', 'DOLLARNAME', 'SEMICOLON', 'EQUALS', 'COMMA',
           'LPAREN', 'RPAREN', 'LBRACE', 'RBRACE', 'LBRACK', 'RBRACK',
           'TIMES', 'DIVIDE', 'PLUS', 'MINUS', 'EQUAL', 'LESS', 'GREATER',
           'BITAND', 'BITOR', 'BITNOT',
           'NUMBER', 'NUMBERFLOAT',
           'STRING' ] + list(reserved.values())


t_ignore = ' \t\r'
t_ignore_COMMENT = r'\#.*'

t_DOLLARNAME = r'\$[a-zA-Z_][a-zA-Z_0-9]*'

t_SEMICOLON = r'\;'
t_EQUALS = r'\='
t_COMMA = r'\,'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LBRACE = r'\{'
t_RBRACE = r'\}'
t_LBRACK = r'\['
t_RBRACK = r'\]'
t_TIMES = r'\*'
t_DIVIDE = r'\/'
t_PLUS = r'\+'
t_MINUS = r'\-'
t_EQUAL = r'\=\='
t_LESS = r'\<'
t_GREATER = r'\>'
t_BITAND = r'\&\&'
t_BITOR = r'\|\|'
t_BITNOT = r'\!'


def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

def t_NAME(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    if t.value in reserved:
        t.type = reserved[t.value]
    return t

def t_NUMBER(t):
    r'[0-9]+(?:\.[0-9]+)?'
    if '.' not in t.value:
        t.value = int(t.value)
    else:
        t.value = float(t.value)
        t.type = 'NUMBERFLOAT'
    return t

def t_STRING(t):
    r'\"[^\"\n\r]*\"'
    t.value = t.value[1:-1]
    return t


def t_error(t):
    Exit.LEXING_ERROR(t)


lexer = lex.lex()



# Parsing rules
precedence = (
    ('nonassoc', 'IFX'),
    ('nonassoc', 'ELSE'),
    ('right', 'EQUALS'),
    ('left', 'BITOR'),
    ('left', 'BITAND'),
    ('left', 'EQUAL'),
    ('left', 'LESS', 'GREATER'),
    ('left', 'PLUS', 'MINUS'),
    ('left', 'TIMES', 'DIVIDE'),
    ('right', 'UMINUS', 'BITNOT', 'TYPECAST'),
    )

binopmap = {
    t_TIMES.replace('\\',''): 'mul',
    t_DIVIDE.replace('\\',''): 'div',
    t_PLUS.replace('\\',''): 'add',
    t_MINUS.replace('\\',''): 'sub',
    t_EQUAL.replace('\\',''): 'eq',
    t_LESS.replace('\\',''): 'lt',
    t_GREATER.replace('\\',''): 'gt',
    t_BITAND.replace('\\',''): 'and',
    t_BITOR.replace('\\',''): 'or',
}
uopmap = {
    t_MINUS.replace('\\',''): 'minus',
    t_BITNOT.replace('\\',''): 'not',
}

def p_prog(p):
    '''prog : externs funcs
            | funcs'''
    if len(p) == 3:
        p[0] = Prog(lineno=p.lexer.lineno, externs=p[1], funcs=p[2])
    else:
        p[0] = Prog(lineno=p.lexer.lineno, externs=None, funcs=p[1])


def p_externs(p):
    '''externs : extern externs
               | extern'''
    if len(p) == 2:
        p[0] = Externs(lineno=p.lexer.lineno, node=p[1])
    else:
        p[0] = p[2].insert(p[1])

def p_extern(p):
    '''extern : EXTERN type globid LPAREN tdecls RPAREN SEMICOLON
              | EXTERN type globid LPAREN RPAREN SEMICOLON'''
    if len(p) == 8:
        p[0] = Extern(lineno=p.lexer.lineno, ret_type=p[2], globid=p[3], tdecls=p[5])
    else:
        p[0] = Extern(lineno=p.lexer.lineno, ret_type=p[2], globid=p[3], tdecls=None)


def p_funcs(p):
    '''funcs : func funcs
             | func'''
    if len(p) == 2:
        p[0] = Funcs(lineno=p.lexer.lineno, node=p[1])
    else:
        p[0] = p[2].insert(p[1])

def p_func(p):
    '''func : DEF type globid LPAREN vdecls RPAREN blk
            | DEF type globid LPAREN RPAREN blk'''
    if len(p) == 8:
        p[0] = Func(lineno=p.lexer.lineno, ret_type=p[2], globid=p[3], vdecls=p[5], blk=p[7])
    else:
        p[0] = Func(lineno=p.lexer.lineno, ret_type=p[2], globid=p[3], vdecls=None, blk=p[6])

def p_blk(p):
    'blk : LBRACE stmts RBRACE'
    p[0] = Blk(lineno=p.lexer.lineno, contents=p[2])

def p_stmts(p):
    '''stmts : stmt stmts
             | stmt'''
    if len(p) == 2:
        p[0] = Stmts(lineno=p.lexer.lineno, node=p[1])
    else:
        p[0] = p[2].insert(p[1])

def p_stmt_blk(p):
    'stmt : blk'
    p[0] = p[1]
def p_stmt_ret(p):
    '''stmt : RETURN exp SEMICOLON
            | RETURN SEMICOLON'''
    if len(p) == 4:
        p[0] = Ret(lineno=p.lexer.lineno, exp=p[2])
    else:
        p[0] = Ret(lineno=p.lexer.lineno, exp=None)
def p_stmt_vardeclstmt(p):
    'stmt : vdecl EQUALS exp SEMICOLON'
    p[0] = Vardeclstmt(lineno=p.lexer.lineno, vdecl=p[1], exp=p[3])
def p_stmt_expstmt(p):
    'stmt : exp SEMICOLON'
    p[0] = Expstmt(lineno=p.lexer.lineno, exp=p[1])
def p_stmt_while(p):
    'stmt : WHILE LPAREN exp RPAREN stmt'
    p[0] = While(lineno=p.lexer.lineno, cond=p[3], stmt=p[5])
def p_stmt_if(p):
    '''stmt : IF LPAREN exp RPAREN stmt ELSE stmt
            | IF LPAREN exp RPAREN stmt %prec IFX'''
    if len(p) == 8:
        p[0] = If(lineno=p.lexer.lineno, cond=p[3], stmt=p[5], else_stmt=p[7])
    else:
        p[0] = If(lineno=p.lexer.lineno, cond=p[3], stmt=p[5], else_stmt=None)
def p_stmt_print(p):
    'stmt : PRINT exp SEMICOLON'
    p[0] = Print(lineno=p.lexer.lineno, exp=p[2])
def p_stmt_printslit(p):
    'stmt : PRINT slit SEMICOLON'
    p[0] = Printslit(lineno=p.lexer.lineno, string=p[2])

def p_exps(p):
    '''exps : exp COMMA exps
            | exp'''
    if len(p) == 2:
        p[0] = Exps(lineno=p.lexer.lineno, node=p[1])
    else:
        p[0] = p[3].insert(p[1])

def p_exp_exp(p):
    'exp : LPAREN exp RPAREN'
    p[0] = p[2]
def p_exp_funccall(p):
    '''exp : globid LPAREN exps RPAREN
           | globid LPAREN RPAREN'''
    if len(p) == 5:
        p[0] = Funccall(lineno=p.lexer.lineno, globid=p[1], params=p[3])
    else:
        p[0] = Funccall(lineno=p.lexer.lineno, globid=p[1], params=None)
def p_exp(p):
    '''exp : binop
           | uop
           | lit
           | varid'''
    p[0] = p[1]

def p_binop_assign(p):
    'binop : DOLLARNAME EQUALS exp'
    p[0] = Assign(lineno=p.lexer.lineno, var=p[1], exp=p[3])
def p_binop_caststmt(p):
    'binop : LBRACK type RBRACK exp %prec TYPECAST'
    p[0] = Caststmt(lineno=p.lexer.lineno, type=p[2], exp=p[4])
def p_binop(p):
    '''binop : arithop
             | logicop'''
    p[0] = p[1]

def p_arithop(p):
    '''arithop : exp TIMES exp
               | exp DIVIDE exp
               | exp PLUS exp
               | exp MINUS exp'''
    p[0] = Binop(lineno=p.lexer.lineno, op=binopmap[p[2]], lhs=p[1], rhs=p[3])
def p_logicop(p):
    '''logicop : exp EQUAL exp
               | exp LESS exp
               | exp GREATER exp
               | exp BITAND exp
               | exp BITOR exp'''
    p[0] = Binop(lineno=p.lexer.lineno, op=binopmap[p[2]], lhs=p[1], rhs=p[3])

def p_uop(p):
    '''uop : MINUS exp %prec UMINUS
           | BITNOT exp'''
    p[0] = Uop(lineno=p.lexer.lineno, op=uopmap[p[1]], exp=p[2])

def p_lit_lit(p):
    'lit : NUMBER'
    p[0] = Lit(lineno=p.lexer.lineno, value=p[1])
def p_lit_flit(p):
    'lit : NUMBERFLOAT'
    p[0] = Flit(lineno=p.lexer.lineno, value=p[1])
def p_lit_blit(p):
    '''lit : TRUE
           | FALSE'''
    p[0] = Blit(lineno=p.lexer.lineno, value=p[1])
def p_slit(p):
    'slit : STRING'
    p[0] = p[1]

def p_ident(p):
    'ident : NAME'
    p[0] = p[1]
def p_varid(p):
    'varid : DOLLARNAME'
    p[0] = Varval(lineno=p.lexer.lineno, var=p[1])
def p_globid(p):
    'globid : ident'
    p[0] = p[1]

def p_type_ref(p):
    '''type : TYPENOALIAS TYPEREF type
            | TYPEREF type'''
    if len(p) == 4: p[0] = ' '.join([p[1], p[2], p[3]])
    else: p[0] = ' '.join([p[1], p[2]])
def p_type(p):
    '''type : TYPEINT
            | TYPECINT
            | TYPEFLOAT
            | TYPEBOOL
            | TYPEVOID'''
    p[0] = p[1]

def p_vdecls(p):
    '''vdecls : vdecl COMMA vdecls
              | vdecl'''
    if len(p) == 2:
        p[0] = Vdecls(lineno=p.lexer.lineno, node=p[1])
    else:
        p[0] = p[3].insert(p[1])

def p_tdecls(p):
    '''tdecls : type COMMA tdecls
              | type'''
    if len(p) == 2:
        p[0] = Tdecls(lineno=p.lexer.lineno, node=p[1])
    else:
        p[0] = p[3].insert(p[1])

def p_vdecl(p):
    'vdecl : type DOLLARNAME'
    p[0] = Vdecl(lineno=p.lexer.lineno, type=p[1], var=p[2])

def p_error(p):
    Exit.PARSING_ERROR(p)


yacc_parser = yacc.yacc(debug=False)



def main(input_args=None):
    parser = argparse.ArgumentParser(description='The Extended-Kaleidoscope Language Compiler',
                                     add_help=False,
                                     usage='%(prog)s [-h|-?] [-v] [-O] [-emit-ast|-emit-llvm] -o <output-file> <input-file>',
                                     epilog='Author: Naga Nithin Manne')

    parser.add_argument('-h', '-?', action='help', default=argparse.SUPPRESS,
                            help='Show this help message and exit')

    parser.add_argument('-v', action='store_true', help='Enable Verbose mode')

    parser.add_argument('-O', action='store_true', help='Enable Optimization')

    emit_group = parser.add_mutually_exclusive_group()
    emit_group.add_argument('-emit-ast', action='store_true', help='Dump AST in YAML Format to the Output File')
    emit_group.add_argument('-emit-llvm', action='store_true', help='Dump LLVM IR to the Output File')

    parser.add_argument('-o', metavar='output-file', help='Output File to emit AST or LLVM IR')

    parser.add_argument('input', metavar='input-file', help='Input .ek File to Compile')

    args = parser.parse_args(args=input_args)

    with open(args.input) as input_file:
        ast = yacc_parser.parse(input_file.read())

    if args.emit_ast:
        yaml.representer.Representer.add_multi_representer(Node, yaml.representer.Representer.represent_dict)
        if args.o:
            with open(args.o, 'w') as ast_output_file:
                yaml.dump(ast, ast_output_file, indent=2, sort_keys=False, explicit_start=True, explicit_end=True)
        else:
            print(yaml.dump(ast, indent=2, sort_keys=False, explicit_start=True, explicit_end=True))
    elif args.emit_llvm:
        raise NotImplementedError('Not Implemented LLVM IR Dump')

    raise NotImplementedError('Not Implemented Compiling after AST Generation')



if __name__=='__main__':
    main()
