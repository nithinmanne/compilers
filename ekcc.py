import sys
import argparse
from enum import Enum
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
    p[0] = OrderedDict()
    p[0]['name'] = 'prog'
    if len(p) == 3:
        p[0]['externs'] = p[1]
        p[0]['funcs'] = p[2]
    else: p[0]['funcs'] = p[1]


def p_externs(p):
    '''externs : extern externs
               | extern'''
    if len(p) == 2:
        p[0] = OrderedDict()
        p[0]['name'] = str(p.slice[0])
        p[0][p[0]['name']] = [p[1]]
    else:
        p[0] = p[2]
        p[0][p[0]['name']].insert(0, p[1])

def p_extern(p):
    '''extern : EXTERN type globid LPAREN tdecls RPAREN SEMICOLON
              | EXTERN type globid LPAREN RPAREN SEMICOLON'''
    p[0] = OrderedDict()
    p[0]['name'] = str(p.slice[0])
    p[0]['ret_type'] = p[2]
    p[0]['globid'] = p[3]
    if len(p) == 8: p[0]['tdecls'] = p[5]


def p_funcs(p):
    '''funcs : func funcs
             | func'''
    if len(p) == 2:
        p[0] = OrderedDict()
        p[0]['name'] = str(p.slice[0])
        p[0][p[0]['name']] = [p[1]]
    else:
        p[0] = p[2]
        p[0][p[0]['name']].insert(0, p[1])

def p_func(p):
    '''func : DEF type globid LPAREN vdecls RPAREN blk
            | DEF type globid LPAREN RPAREN blk'''
    p[0] = OrderedDict()
    p[0]['name'] = str(p.slice[0])
    p[0]['ret_type'] = p[2]
    p[0]['globid'] = p[3]
    if len(p) == 8:
        p[0]['vdecls'] = p[5]
        p[0]['blk'] = p[7]
    else:
        p[0]['blk'] = p[6]

def p_blk(p):
    'blk : LBRACE stmts RBRACE'
    p[0] = OrderedDict()
    p[0]['name'] = str(p.slice[0])
    p[0]['contents'] = p[2]

def p_stmts(p):
    '''stmts : stmt stmts
             | stmt'''
    if len(p) == 2:
        p[0] = OrderedDict()
        p[0]['name'] = str(p.slice[0])
        p[0][p[0]['name']] = [p[1]]
    else:
        p[0] = p[2]
        p[0][p[0]['name']].insert(0, p[1])

def p_stmt_blk(p):
    'stmt : blk'
    p[0] = p[1]
def p_stmt_ret(p):
    '''stmt : RETURN exp SEMICOLON
            | RETURN SEMICOLON'''
    p[0] = OrderedDict()
    p[0]['name'] = 'ret'
    if len(p) == 4: p[0]['exp'] = p[2]
def p_stmt_vardeclstmt(p):
    'stmt : vdecl EQUALS exp SEMICOLON'
    p[0] = OrderedDict()
    p[0]['name'] = 'vardeclstmt'
    p[0]['vdecl'] = p[1]
    p[0]['exp'] = p[3]
def p_stmt_expstmt(p):
    'stmt : exp SEMICOLON'
    p[0] = OrderedDict()
    p[0]['name'] = 'expstmt'
    p[0]['exp'] = p[1]
def p_stmt_while(p):
    'stmt : WHILE LPAREN exp RPAREN stmt'
    p[0] = OrderedDict()
    p[0]['name'] = 'while'
    p[0]['cond'] = p[3]
    p[0]['stmt'] = p[5]
def p_stmt_if(p):
    '''stmt : IF LPAREN exp RPAREN stmt ELSE stmt
            | IF LPAREN exp RPAREN stmt %prec IFX'''
    p[0] = OrderedDict()
    p[0]['name'] = 'if'
    p[0]['cond'] = p[3]
    p[0]['stmt'] = p[5]
    if len(p) == 8: p[0]['else_stmt'] = p[7]
def p_stmt_print(p):
    'stmt : PRINT exp SEMICOLON'
    p[0] = OrderedDict()
    p[0]['name'] = 'print'
    p[0]['exp'] = p[2]
def p_stmt_printslit(p):
    'stmt : PRINT slit SEMICOLON'
    p[0] = OrderedDict()
    p[0]['name'] = 'printslit'
    p[0]['string'] = p[2]

def p_exps(p):
    '''exps : exp COMMA exps
            | exp'''
    if len(p) == 2:
        p[0] = OrderedDict()
        p[0]['name'] = str(p.slice[0])
        p[0][p[0]['name']] = [p[1]]
    else:
        p[0] = p[3]
        p[0][p[0]['name']].insert(0, p[1])

def p_exp_exp(p):
    'exp : LPAREN exp RPAREN'
    p[0] = p[2]
def p_exp_funccall(p):
    '''exp : globid LPAREN exps RPAREN
           | globid LPAREN RPAREN'''
    p[0] = OrderedDict()
    p[0]['name'] = 'funccall'
    p[0]['globid'] = p[1]
    if len(p) == 5: p[0]['params'] = p[3]
def p_exp(p):
    '''exp : binop
           | uop
           | lit
           | varid'''
    p[0] = p[1]

def p_binop_assign(p):
    'binop : DOLLARNAME EQUALS exp'
    p[0] = OrderedDict()
    p[0]['name'] = 'assign'
    p[0]['var'] = p[1]
    p[0]['exp'] = p[3]
def p_binop_caststmt(p):
    'binop : LBRACK type RBRACK exp %prec TYPECAST'
    p[0] = OrderedDict()
    p[0]['name'] = 'caststmt'
    p[0]['type'] = p[2]
    p[0]['exp'] = p[4]
def p_binop(p):
    '''binop : arithop
             | logicop'''
    p[0] = p[1]

def p_arithop(p):
    '''arithop : exp TIMES exp
               | exp DIVIDE exp
               | exp PLUS exp
               | exp MINUS exp'''
    p[0] = OrderedDict()
    p[0]['name'] = 'binop'
    p[0]['op'] = binopmap[p[2]]
    p[0]['lhs'] = p[1]
    p[0]['rhs'] = p[3]
def p_logicop(p):
    '''logicop : exp EQUAL exp
               | exp LESS exp
               | exp GREATER exp
               | exp BITAND exp
               | exp BITOR exp'''
    p[0] = OrderedDict()
    p[0]['name'] = 'binop'
    p[0]['op'] = binopmap[p[2]]
    p[0]['lhs'] = p[1]
    p[0]['rhs'] = p[3]

def p_uop(p):
    '''uop : MINUS exp %prec UMINUS
           | BITNOT exp'''
    p[0] = OrderedDict()
    p[0]['name'] = 'uop'
    p[0]['op'] = uopmap[p[1]]
    p[0]['exp'] = p[2]

def p_lit_lit(p):
    'lit : NUMBER'
    p[0] = OrderedDict()
    p[0]['name'] = 'lit'
    p[0]['value'] = p[1]
def p_lit_flit(p):
    'lit : NUMBERFLOAT'
    p[0] = OrderedDict()
    p[0]['name'] = 'flit'
    p[0]['value'] = p[1]
def p_lit_blit(p):
    '''lit : TRUE
           | FALSE'''
    p[0] = OrderedDict()
    p[0]['name'] = 'blit'
    p[0]['value'] = p[1]
def p_slit(p):
    'slit : STRING'
    p[0] = p[1]

def p_ident(p):
    'ident : NAME'
    p[0] = p[1]
def p_varid(p):
    'varid : DOLLARNAME'
    p[0] = OrderedDict()
    p[0]['name'] = 'varval'
    p[0]['var'] = p[1]
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
        p[0] = OrderedDict()
        p[0]['name'] = str(p.slice[0])
        p[0]['vars'] = [p[1]]
    else:
        p[0] = p[3]
        p[0]['vars'].insert(0, p[1])

def p_tdecls(p):
    '''tdecls : type COMMA tdecls
              | type'''
    if len(p) == 2:
        p[0] = OrderedDict()
        p[0]['name'] = str(p.slice[0])
        p[0]['types'] = [p[1]]
    else:
        p[0] = p[3]
        p[0]['types'].insert(0, p[1])

def p_vdecl(p):
    'vdecl : type DOLLARNAME'
    p[0] = OrderedDict()
    p[0]['node'] = 'vdecl'
    p[0]['type'] = p[1]
    p[0]['var'] = p[2]

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
        yaml.representer.Representer.add_representer(OrderedDict, yaml.representer.Representer.represent_dict)
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
