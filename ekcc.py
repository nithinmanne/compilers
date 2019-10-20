import sys, glob
import ply.lex as lex

def find_column(input, token):
    line_start = input.rfind('\n', 0, token.lexpos) + 1
    return (token.lexpos - line_start) + 1

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


tokens = [ 'NAME', 'SEMICOLON', 'EQUALS', 'COMMA',
           'LPAREN', 'RPAREN', 'LBRACE', 'RBRACE', 'LBRACK', 'RBRACK',
           'TIMES', 'DIVIDE', 'PLUS', 'MINUS', 'EQUAL', 'LESS', 'GREATER',
           'BITAND', 'BITOR', 'BITNOT', 'DOLLAR',
           'NUMBER', 'NUMBERINT', 'NUMBERFLOAT',
           'STRING' ] + list(reserved.values())

# Tokens

t_ignore = ' \t'
t_ignore_COMMENT = r'\#.*'

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
t_DOLLAR = r'\$'
t_STRING = r'\"[^\"\n\r]*\"'



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
        t.type = 'NUMBERINT'
    else:
        t.value = float(t.value)
        t.type = 'NUMBERFLOAT'
    return t


def t_error(t):
    print('Illegal Character "{}" at Line {} and Column {}'.format(t.value[0],
                                                                   t.lexer.lineno,
                                                                   find_column(t.lexer.lexdata, t)))
    t.lexer.skip(1)




lexer = lex.lex()
if __name__=='__main__':
    if len(sys.argv) == 1:
        while True:
            lexer.input(input('lex >> '))
            for lexed in lexer:
                print(lexed)
    else:
        for fileg in sys.argv[1:]:
            for file in glob.glob(fileg):
                with open(file) as filep:
                    print(file, '\n')
                    lexer.input(filep.read())
                    for lexed in lexer:
                        print(lexed)
                    print('\n')
