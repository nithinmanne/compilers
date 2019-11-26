import os
import sys
import argparse
import subprocess
from enum import Enum
from abc import ABC, abstractmethod
from collections import OrderedDict
import yaml
import ply.lex as lex
import ply.yacc as yacc
from llvmlite import ir
import llvmlite.binding as llvm

# Class Definitions

class Exit(Enum):
    SUCCESS = 0
    NOT_IMPLEMENTED = 1
    LEXING_ERROR = 2
    PARSING_ERROR = 3
    INVALID_VAR_TYPE_ERROR = 4
    TYPE_CAST_ERROR = 5
    IMPLICIT_TYPE_CAST_ERROR = 6
    UNDECLARED_USAGE_ERROR = 7
    REDECLARED_NAME_ERROR = 8
    RUN_FUNCTION_MISSING = 9
    INVALID_OPERATION_TYPE = 10
    INVALID_EXTERN_BUILTIN = 11

    def _is_error(self):
        if self is self.SUCCESS: return False
        else: return True

    def _print(self, *args):
        print('error:', end='\t')
        if self is self.NOT_IMPLEMENTED:
            print('Not Implemented Operation ({})'.format(args[0]))

        elif self is self.LEXING_ERROR:
            if args[0]:
                print('Illegal Character {} in Line {}'.format(args[0].value[0], args[0].lexer.lineno))
            else:
                print('Illegal Character at End of File')

        elif self is self.PARSING_ERROR:
            if args[0]:
                print('Parsing Syntax Error {} in Line {}'.format(args[0].value, args[0].lexer.lineno))
            else:
                print('Parsing Syntax Error at End of File')

        elif self is self.INVALID_VAR_TYPE_ERROR:
            print('Invalid Type for {} in Line {}'.format(args[1], args[0]))

        elif self is self.TYPE_CAST_ERROR:
            print('Invalid Type Cast from {} to {} in Line {}'.format(args[2], args[1], args[0]))

        elif self is self.IMPLICIT_TYPE_CAST_ERROR:
            print('Implicit Type Cast between {} and {} in Line {}'.format(args[1], args[2], args[0]))

        elif self is self.UNDECLARED_USAGE_ERROR:
            print('Undeclared/Invalid Usage of {} in Line {}'.format(args[1], args[0]))

        elif self is self.REDECLARED_NAME_ERROR:
            print('Redeclaration of {} in Line {}'.format(args[1], args[0]))

        elif self is self.RUN_FUNCTION_MISSING:
            print('Invalid/Missing int run() Function')

        elif self is self.INVALID_OPERATION_TYPE:
            print('Invalid Type for {} in Line {}'.format(args[1], args[0]))

        elif self is self.INVALID_EXTERN_BUILTIN:
            print('Invalid Declaration for builtin {}'.format(args[0]))




    def __call__(self, *args):
        if self._is_error():
            self._print(*args)

        sys.exit(self.value)


class Node(ABC):
    def __init__(self, lineno):
        self.lineno = lineno
    def _set_scope(self, scope):
        self.scope = scope
    def _set_ir_builder(self, builder):
        self.builder = builder
    def walk_ast(self, scope, builder):
        self._set_scope(scope.copy())
        self._set_ir_builder(builder)
    @property
    def ir_type(self):
        '''self.ir_type -> IR Type based on self.type'''
        return {'void': ir.VoidType(),
                'char': ir.IntType(8),
                'char*': ir.IntType(8).as_pointer(),
                'char**': ir.IntType(8).as_pointer().as_pointer(),
                'int': ir.IntType(32),
                'cint': ir.IntType(32),
                'float': ir.FloatType(),
                'double': ir.DoubleType(),
                'bool': ir.IntType(1)}[self.type]
    @abstractmethod
    def items(self):
        return [('name', self.name)]

class Stmt(Node):
    '''Common Code and Enforced Properties for Statements'''
    pass

class Exp(Node):
    '''Common Code and Enforced Properties for Expressions'''
    @abstractmethod
    def _set_type(self):
        pass
    def walk_ast(self, scope, builder):
        super().walk_ast(scope.copy(), builder)
        self._set_type()
    def items(self):
        return super().items() + [('type', self.type)]

class Decl(Node):
    '''Common Code to initlaize types'''
    def __init__(self, lineno, noalias, ref, type):
        super().__init__(lineno)
        self.noalias = noalias
        self.ref = ref
        self.type = type
    @property
    def ir_type(self):
        if hasattr(self, 'ref') and self.ref: return super().ir_type.as_pointer()
        else: return super().ir_type
    def items(self):
        return super().items() + [('type', 'noalias '*self.noalias +\
                                   'ref '*self.ref + self.type)]

class Callable(Node):
    '''Common Code between an Extern and a Fucntion'''
    def __init__(self, lineno, ret_type, globid, decls, var_arg=False):
        super().__init__(lineno)
        self.ret_type = ret_type
        self.globid = globid
        self.decls = decls
        self.var_arg = var_arg
    def walk_ast(self, scope, builder):
        try:
            scope.add_callable(self)
        except ScopeException:
            Exit.REDECLARED_NAME_ERROR(self.lineno, self.globid)
        self.type = self.ret_type
        if self.decls:
            self.func_type = ir.FunctionType(self.ir_type, [decl.ir_type for decl in self.decls], var_arg=self.var_arg)
        else:
            self.func_type = ir.FunctionType(self.ir_type, [], var_arg=self.var_arg)
        self.ptr = ir.Function(builder, self.func_type, name=self.globid)
        scope.add_ptr(self.globid, self.ptr)
        if self.decls:
            self.decls.walk_ast(scope.copy(), builder)
        super().walk_ast(scope.copy(), builder)
    def items(self):
        return super().items() + [('ret_type', self.ret_type),
                                  ('globid', self.globid),
                                  ('decls', self.decls)]

class Nodelist(Node):
    '''Container for sharing common code between sequence of nodes, like stmts, funcs, externs'''
    def __init__(self, lineno, node):
        super().__init__(lineno)
        self.nodelist = [node]
    def __add__(self, node):
        self.nodelist = [node] + self.nodelist
        return self
    def __len__(self):
        return len(self.nodelist)
    def __iter__(self):
        return iter(self.nodelist)
    def walk_ast(self, scope, builder):
        for node in self:
            node.walk_ast(scope.copy(), builder)
            scope = node.scope
        super().walk_ast(scope.copy(), builder)
    def items(self):
        return super().items() + [(self.listname, self.nodelist)]


class Prog(Node):
    name = 'prog'
    def __init__(self, lineno, externs, funcs):
        super().__init__(lineno)
        self.externs = externs
        self.funcs = funcs
    def walk_ast(self, scope=None, builder='main', jit=None):
        if not scope:
            scope = Scope()
        self.module = ir.Module(name=builder)
        scope.module = self.module
        printf = Extern(lineno=-1, ret_type='int', globid='printf',
                        decls=Tdecls(lineno=-1, node=Tdecl(lineno=-1, noalias=False,
                                                           ref=False, type='char*')),
                        var_arg=True)
        if self.externs:
            self.externs = self.externs + printf
        else:
            self.externs = Externs(lineno=-1, node=argf) + printf
        self.externs.walk_ast(scope.copy(), self.module)
        scope = self.externs.scope
        if 'run' in scope:
            Exit.RUN_FUNCTION_MISSING()
        self.funcs.walk_ast(scope.copy(), self.module)
        scope = self.funcs.scope
        try:
            if scope('run', None) != 'int':
                raise ScopeException
        except ScopeException:
            Exit.RUN_FUNCTION_MISSING()
        super().walk_ast(scope.copy(), builder)
        self.make_arg_externs(scope, jit)
    def make_arg_externs(self, scope, jit):
        '''Create arg and argf Funtions in case of JIT, and also main when compiling'''
        if jit is None:
            self.type = 'char**'
            global_argv_constant = ir.Constant(self.ir_type, None)
            global_argv = ir.GlobalVariable(scope.module, self.ir_type, 'global_argv')
            global_argv.initializer = global_argv_constant
            self.type = 'int'
            global_argc_constant = ir.Constant(self.ir_type, None)
            global_argc = ir.GlobalVariable(scope.module, self.ir_type, 'global_argc')
            global_argc.initializer = global_argc_constant
            self.type = 'char**'
            main_func_type = ir.FunctionType(ir.IntType(32), [ir.IntType(32), self.ir_type])
            main_func = ir.Function(scope.module, main_func_type, name='main')
            argc, argv = main_func.args
            main_block = main_func.append_basic_block()
            main_builder = ir.IRBuilder(main_block)
            main_builder.store(argv, global_argv)
            main_builder.store(argc, global_argc)
            run_ret = main_builder.call(scope.get_ptr('run'), [])
            main_builder.ret(run_ret)
            self.type = 'char*'
            atoi_type = ir.FunctionType(ir.IntType(32), [self.ir_type])
            atoi = ir.Function(scope.module, atoi_type, name='atoi')
            atof_type = ir.FunctionType(ir.DoubleType(), [self.ir_type])
            atof = ir.Function(scope.module, atof_type, name='atof')
            for name, type, type_str in zip(['arg', 'argf'], [int, float], ['int', 'float']):
                try:
                    func = scope.get_ptr(name)
                except:
                    continue
                try:
                    if scope(name, [Decl(lineno=-1, noalias=False, ref=False, type='int')]) != type_str:
                        raise ScopeException
                except ScopeException:
                    Exit.INVALID_EXTERN_BUILTIN(name)
                arg, = func.args
                entry_block = func.append_basic_block()
                func_builder = ir.IRBuilder(entry_block)
                argv = func_builder.load(global_argv)
                argc = func_builder.load(global_argc)
                index = func_builder.add(arg, ir.Constant(ir.IntType(32), 1))
                argc_check = func_builder.icmp_signed('<', index, argc)
                with func_builder.if_then(argc_check):
                    argv_index = func_builder.gep(argv, [index])
                    argv_n_str = func_builder.load(argv_index)
                    if type_str in ['int']:
                        atoival = func_builder.call(atoi, [argv_n_str])
                        func_builder.ret(atoival)
                    elif type_str in ['float']:
                        atofval = func_builder.call(atof, [argv_n_str])
                        self.type = 'float'
                        fval = func_builder.fptrunc(atofval, self.ir_type)
                        func_builder.ret(fval)
                self.type = type_str
                func_builder.ret(self.ir_type(type(0)))
        else:
            for name, type, type_str in zip(['arg', 'argf'], [int, float], ['int', 'float']):
                try:
                    func = scope.get_ptr(name)
                except:
                    continue
                try:
                    if scope(name, [Decl(lineno=-1, noalias=False, ref=False, type='int')]) != type_str:
                        raise ScopeException
                except ScopeException:
                    Exit.INVALID_EXTERN_BUILTIN(name)
                arg, = func.args
                entry_block = func.append_basic_block()
                func_builder = ir.IRBuilder(entry_block)
                default_block = func_builder.append_basic_block()
                self.type = type_str
                with func_builder.goto_block(default_block):
                    func_builder.ret(self.ir_type(0))
                switch = func_builder.switch(arg, default_block)
                for i, val in enumerate(jit):
                    switch_block = func_builder.append_basic_block()
                    with func_builder.goto_block(switch_block):
                        try: func_builder.ret(self.ir_type(type(val)))
                        except: func_builder.ret(self.ir_type(type(0)))
                    switch.add_case(i, switch_block)
    def items(self):
        return super().items() + [('externs', self.externs),
                                  ('funcs', self.funcs)]

class Externs(Nodelist):
    name = 'externs'
    listname = name

class Extern(Callable):
    name = 'extern'

class Funcs(Nodelist):
    name = 'funcs'
    listname = name

class Func(Callable):
    name = 'func'
    def __init__(self, lineno, ret_type, globid, decls, blk):
        super().__init__(lineno, ret_type, globid, decls)
        self.blk = blk
    def walk_ast(self, scope, builder):
        super().walk_ast(scope.copy(), builder)
        self.entry_block = self.ptr.append_basic_block()
        self.block_builder = ir.IRBuilder(self.entry_block)
        if self.decls:
            blk_scope = self.decls.scope
            for vdecl, arg in zip(self.decls, self.ptr.args):
                vdecl.scope = blk_scope
                vdecl.builder = self.block_builder
                vdecl.set_exp(arg)
        else:
            blk_scope = self.scope
        self.blk.walk_ast(blk_scope.copy(), self.block_builder)
        if self.ret_type in ['void']:
            self.block_builder.ret_void()
        else:
            self.block_builder.ret(self.ir_type(0))
    def items(self):
        return super().items() + [('blk', self.blk)]

class Blk(Stmt):
    name = 'blk'
    def __init__(self, lineno, contents):
        super().__init__(lineno)
        self.contents = contents
    def walk_ast(self, scope, builder):
        if self.contents: self.contents.walk_ast(scope.copy(), builder)
        super().walk_ast(scope.copy(), builder)
    def items(self):
        return super().items() + [('contents', self.contents)]

class Stmts(Nodelist):
    name = 'stmts'
    listname = name

class Ret(Stmt):
    name = 'ret'
    def __init__(self, lineno, exp):
        super().__init__(lineno)
        self.exp = exp
    def walk_ast(self, scope, builder):
        if self.exp:
            self.exp.walk_ast(scope.copy(), builder)
        super().walk_ast(scope.copy(), builder)
        if self.exp:
            builder.ret(self.exp.ir)
        else:
            builder.ret_void()
        self.block = builder.append_basic_block()
        builder.position_at_start(self.block)
    def items(self):
        return super().items() + [('exp', self.exp)]

class Vardeclstmt(Stmt):
    name = 'vardeclstmt'
    def __init__(self, lineno, vdecl, exp):
        super().__init__(lineno)
        self.vdecl = vdecl
        self.exp = exp
    def walk_ast(self, scope, builder):
        self.exp.walk_ast(scope.copy(), builder)
        self.vdecl.walk_ast(scope.copy(), builder)
        if (self.vdecl.ref and not isinstance(self.exp, Varval)) or self.vdecl.type != self.exp.type:
            Exit.INVALID_VAR_TYPE_ERROR(self.lineno, self.vdecl.var)
        self.vdecl.set_exp(self.exp)
        scope = self.vdecl.scope
        super().walk_ast(scope.copy(), builder)
    def items(self):
        return super().items() + [('vdecl', self.vdecl),
                                  ('exp', self.exp)]

class Expstmt(Stmt):
    name = 'expstmt'
    def __init__(self, lineno, exp):
        super().__init__(lineno)
        self.exp = exp
    def walk_ast(self, scope, builder):
        self.exp.walk_ast(scope.copy(), builder)
        super().walk_ast(scope.copy(), builder)
    def items(self):
        return super().items() + [('exp', self.exp)]

class While(Stmt):
    name = 'while'
    def __init__(self, lineno, cond, stmt):
        super().__init__(lineno)
        self.cond = cond
        self.stmt = stmt
    def walk_ast(self, scope, builder):
        self.cond_block = builder.append_basic_block()
        self.loop_block = builder.append_basic_block()
        self.endloop_block = builder.append_basic_block()
        builder.branch(self.cond_block)
        with builder.goto_block(self.cond_block):
            self.cond.walk_ast(scope.copy(), builder)
            if self.cond.type not in ['bool']:
                Exit.INVALID_OPERATION_TYPE(self.lineno, self.name)
            builder.cbranch(self.cond.ir, self.loop_block, self.endloop_block)
        with builder.goto_block(self.loop_block):
            self.stmt.walk_ast(scope.copy(), builder)
            builder.branch(self.cond_block)
        super().walk_ast(scope.copy(), builder)
        builder.position_at_start(self.endloop_block)
    def items(self):
        return super().items() + [('cond', self.cond),
                                  ('stmt', self.stmt)]

class If(Stmt):
    name = 'if'
    def __init__(self, lineno, cond, stmt, else_stmt):
        super().__init__(lineno)
        self.cond = cond
        self.stmt = stmt
        self.else_stmt = else_stmt
    def walk_ast(self, scope, builder):
        self.cond.walk_ast(scope.copy(), builder)
        if self.cond.type not in ['bool']:
            Exit.INVALID_OPERATION_TYPE(self.lineno, self.name)
        if self.else_stmt:
            with builder.if_else(self.cond.ir) as (if_block, else_block):
                with if_block:
                    self.stmt.walk_ast(scope.copy(), builder)
                with else_block:
                    self.else_stmt.walk_ast(scope.copy(), builder)
        else:
            with builder.if_then(self.cond.ir):
                self.stmt.walk_ast(scope.copy(), builder)
        super().walk_ast(scope.copy(), builder)
    def items(self):
        return super().items() + [('cond', self.cond),
                                  ('stmt', self.stmt),
                                  ('else_stmt', self.else_stmt)]

class Print(Stmt):
    name = 'print'
    def __init__(self, lineno, exp):
        super().__init__(lineno)
        self.exp = exp
    def walk_ast(self, scope, builder):
        self.exp.walk_ast(scope.copy(), builder)
        super().walk_ast(scope.copy(), builder)
        if self.exp.type in ['int', 'cint', 'bool']: fmt = '%i\n\0'
        elif self.exp.type in ['float']: fmt = '%f\n\0'
        self.type = 'char'
        fmt_ir_type = ir.ArrayType(self.ir_type, len(fmt))
        fmt_constant = ir.Constant(fmt_ir_type, bytearray(fmt.encode()))
        global_str = ir.GlobalVariable(self.scope.module, fmt_ir_type, self.scope.module.get_unique_name('string'))
        global_str.initializer = fmt_constant
        self.type = 'char*'
        fmt_arg = builder.bitcast(global_str, self.ir_type)
        if self.exp.type in ['int', 'cint']:
            self.exp_ir = self.exp.ir
        elif self.exp.type in ['float']:
            self.type = 'double'
            self.exp_ir = builder.fpext(self.exp.ir, self.ir_type)
        elif self.exp.type in ['bool']:
            self.type = 'bool'
            self.exp_ir = builder.zext(self.exp.ir, self.ir_type)
        printf = self.scope.get_ptr('printf')
        builder.call(printf, [fmt_arg, self.exp_ir])
    def items(self):
        return super().items() + [('exp', self.exp)]

class Printslit(Stmt):
    name = 'printslit'
    def __init__(self, lineno, string):
        super().__init__(lineno)
        self.string = string + '\n\0'
    def walk_ast(self, scope, builder):
        super().walk_ast(scope.copy(), builder)
        self.type = 'char'
        self.string_ir_type = ir.ArrayType(self.ir_type, len(self.string))
        self.string_constant = ir.Constant(self.string_ir_type, bytearray(self.string.encode()))
        self.global_str = ir.GlobalVariable(self.scope.module, self.string_ir_type, self.scope.module.get_unique_name('string'))
        self.global_str.initializer = self.string_constant
        self.type = 'char*'
        printf = self.scope.get_ptr('printf')
        printf_arg = builder.bitcast(self.global_str, self.ir_type)
        builder.call(printf, [printf_arg])
    def items(self):
        return super().items() + [('string', self.string)]

class Exps(Nodelist):
    name = 'exps'
    listname = name

class Funccall(Exp):
    name = 'funccall'
    def __init__(self, lineno, globid, params):
        super().__init__(lineno)
        self.globid = globid
        self.params = params
    def _set_type(self):
        try:
            self.type = self.scope(self.globid, self.params)
        except ScopeException:
            Exit.UNDECLARED_USAGE_ERROR(self.lineno, self.globid)
    def walk_ast(self, scope, builder):
        if self.params:
            self.params.walk_ast(scope.copy(), builder)
        super().walk_ast(scope.copy(), builder)
        self.ir = builder.call(self.scope.get_ptr(self.globid), self.scope.get_args(self.globid, self.params))
    def items(self):
        return super().items() + [('globid', self.globid),
                                  ('params', self.params)]

class Assign(Exp):
    name = 'assign'
    def __init__(self, lineno, var, exp):
        super().__init__(lineno)
        self.var = var
        self.exp = exp
    def _set_type(self):
        try:
            self.type = self.scope[self.var]
        except ScopeException:
            Exit.UNDECLARED_USAGE_ERROR(self.lineno, self.var)
        if self.type != self.exp.type:
            Exit.IMPLICIT_TYPE_CAST_ERROR(self.lineno, self.exp.type, self.type)
    def walk_ast(self, scope, builder):
        self.exp.walk_ast(scope.copy(), builder)
        super().walk_ast(scope.copy(), builder)
        builder.store(self.exp.ir, self.scope.get_ptr(self.var))
        self.ir = self.exp.ir
    def items(self):
        return super().items() + [('var', self.var),
                                  ('exp', self.exp)]

class Caststmt(Exp):
    name = 'caststmt'
    def __init__(self, lineno, type, exp):
        super().__init__(lineno)
        self.type = type
        self.exp = exp
    def _set_type(self):
        cast_type = {
            'int': ['int', 'cint', 'float'],
            'cint': ['int', 'cint', 'float'],
            'float': ['int', 'cint', 'float'],
            'bool': ['bool']
        }
        if self.type not in cast_type[self.exp.type]:
            Exit.TYPE_CAST_ERROR(self.lineno, self.type, self.exp.type)
    def walk_ast(self, scope, builder):
        self.exp.walk_ast(scope.copy(), builder)
        super().walk_ast(scope.copy(), builder)
        if self.type == self.exp.type:
            self.ir = self.exp.ir
        else:
            if self.type in ['int', 'cint']:
                self.ir = builder.fptosi(self.exp.ir, self.ir_type)
            elif self.type in ['float']:
                self.ir = builder.sitofp(self.exp.ir, self.ir_type)
    def items(self):
        return super().items() + [('exp', self.exp)]

class Binop(Exp):
    name = 'binop'
    def __init__(self, lineno, op, lhs, rhs):
        super().__init__(lineno)
        self.op = op
        self.lhs = lhs
        self.rhs = rhs
    def _set_type(self):
        if self.lhs.type == self.rhs.type:
            try:
                if self.op in ['*', '/', '+', '-']:
                    if self.lhs.type not in ['int', 'cint', 'float']:
                        raise TypeError
                    self.type = self.lhs.type
                elif self.op in ['==']:
                    self.type = 'bool'
                elif self.op in ['<', '>']:
                    if self.lhs.type not in ['int', 'cint', 'float']:
                        raise TypeError
                    self.type = 'bool'
                elif self.op in ['&&', '||']:
                    if self.lhs.type not in ['bool']:
                        raise TypeError
                    self.type = 'bool'
            except TypeError:
                Exit.INVALID_OPERATION_TYPE(self.lineno, self.op)
        else:
            Exit.IMPLICIT_TYPE_CAST_ERROR(self.lineno, self.lhs.type, self.rhs.type)
    def walk_ast(self, scope, builder):
        self.lhs.walk_ast(scope.copy(), builder)
        self.rhs.walk_ast(scope.copy(), builder)
        super().walk_ast(scope.copy(), builder)
        if self.op in ['*']:
            if self.type in ['int', 'cint']:
                self.overflow_ir = builder.smul_with_overflow(self.lhs.ir, self.rhs.ir)
                self.ir = builder.extract_value(self.overflow_ir, 0)
            elif self.type in ['float']:
                self.ir = builder.fmul(self.lhs.ir, self.rhs.ir)
        elif self.op in ['/']:
            if self.type in ['int', 'cint']:
                self.ir = builder.sdiv(self.lhs.ir, self.rhs.ir)
            elif self.type in ['float']:
                self.ir = builder.fdiv(self.lhs.ir, self.rhs.ir)
        elif self.op in ['+']:
            if self.type in ['int', 'cint']:
                self.overflow_ir = builder.sadd_with_overflow(self.lhs.ir, self.rhs.ir)
                self.ir = builder.extract_value(self.overflow_ir, 0)
            elif self.type in ['float']:
                self.ir = builder.fadd(self.lhs.ir, self.rhs.ir)
        elif self.op in ['-']:
            if self.type in ['int', 'cint']:
                self.overflow_ir = builder.ssub_with_overflow(self.lhs.ir, self.rhs.ir)
                self.ir = builder.extract_value(self.overflow_ir, 0)
            elif self.type in ['float']:
                self.ir = builder.fsub(self.lhs.ir, self.rhs.ir)
        elif self.op in ['==', '<', '>']:
            if self.lhs.type in ['int', 'cint']:
                self.ir = builder.icmp_signed(self.op, self.lhs.ir, self.rhs.ir)
            elif self.lhs.type in ['float']:
                self.ir = builder.fcmp_ordered(self.op, self.lhs.ir, self.rhs.ir)
        elif self.op in ['&&']:
            self.ir = builder.and_(self.lhs.ir, self.rhs.ir)
        elif self.op in ['||']:
            self.ir = builder.or_(self.lhs.ir, self.rhs.ir)
    def items(self):
        return super().items() + [('op', self.op),
                                  ('lhs', self.lhs),
                                  ('rhs', self.rhs)]

class Uop(Exp):
    name = 'uop'
    def __init__(self, lineno, op, exp):
        super().__init__(lineno)
        self.op = op
        self.exp = exp
    def _set_type(self):
        try:
            if self.op in ['-']:
                if self.exp.type not in ['int', 'cint', 'float']:
                    raise TypeError
            elif self.op in ['!']:
                if self.exp.type not in ['bool']:
                    raise TypeError
            self.type = self.exp.type
        except TypeError:
            Exit.INVALID_OPERATION_TYPE(self.lineno, self.op)
    def walk_ast(self, scope, builder):
        self.exp.walk_ast(scope.copy(), builder)
        super().walk_ast(scope.copy(), builder)
        if self.op in ['-']:
            self.ir = builder.neg(self.exp.ir)
        elif self.op in ['!']:
            self.ir = builder.not_(self.exp.ir)
    def items(self):
        return super().items() + [('op', self.op),
                                  ('exp', self.exp)]

class Litnode(Exp):
    def __init__(self, lineno, value):
        super().__init__(lineno)
        self.value = value
    def walk_ast(self, scope, builder):
        super().walk_ast(scope.copy(), builder)
        self.ir = ir.Constant(self.ir_type, self.value)
    def items(self):
        return super().items() + [('value', self.value)]

class Lit(Litnode):
    name = 'lit'
    def _set_type(self):
        self.type = 'int'

class Flit(Litnode):
    name = 'flit'
    def _set_type(self):
        self.type = 'float'

class Blit(Litnode):
    name = 'blit'
    def _set_type(self):
        self.type = 'bool'

class Varval(Exp):
    name = 'varval'
    def __init__(self, lineno, var):
        super().__init__(lineno)
        self.var = var
    def _set_type(self):
        try:
            self.type = self.scope[self.var]
        except ScopeException:
            Exit.UNDECLARED_USAGE_ERROR(self.lineno, self.var)
    def walk_ast(self, scope, builder):
        super().walk_ast(scope.copy(), builder)
        self.ptr = self.scope.get_ptr(self.var)
        self.ir = builder.load(self.ptr)
    def items(self):
        return super().items() + [('var', self.var)]

class Vdecls(Nodelist):
    name = 'vdecls'
    listname = name

class Tdecls(Nodelist):
    name = 'tdecls'
    listname = name

class Vdecl(Decl):
    name = 'vdecl'
    def __init__(self, lineno, noalias, ref, type, var):
        super().__init__(lineno, noalias, ref, type)
        self.var = var
    def walk_ast(self, scope, builder):
        try:
            scope.add_variable(self)
        except ScopeException:
            Exit.REDECLARED_NAME_ERROR(self.lineno, self.var)
        super().walk_ast(scope.copy(), builder)
    def set_exp(self, exp):
        if isinstance(exp, ir.Argument):
            if self.ref:
                self.ptr = exp
                if self.noalias:
                    exp.add_attribute('noalias')
            else:
                self.ptr = self.builder.alloca(self.ir_type, name=self.var)
                self.builder.store(exp, self.ptr)
        else:
            if self.ref:
                self.ptr = self.scope.get_ptr(exp.var)
            else:
                self.ptr = self.builder.alloca(self.ir_type, name=self.var)
                self.builder.store(exp.ir, self.ptr)
        self.scope.add_ptr(self.var, self.ptr)
    def items(self):
        return super().items() + [('var', self.var)]

class Tdecl(Decl):
    name = 'tdecl'


class Scope:
    '''Store Types, Pointers and other Information of variables in current scope'''
    def __init__(self, callables=None, variables=None, pointers=None):
        if callables: self.callables = callables.copy()
        else: self.callables = {}
        if variables: self.variables = variables.copy()
        else: self.variables = {}
        if pointers: self.pointers = pointers.copy()
        else: self.pointers = {}
    def __contains__(self, value):
        return value in self.callables or value in self.variables
    def __call__(self, name, args):
        if name not in self.callables:
            raise ScopeException
        call = self.callables[name]
        if args is None and call.decls is None:
            return call.ret_type
        elif args is None or call.decls is None or len(args) != len(call.decls):
            raise ScopeException
        else:
            for decl, arg in zip(call.decls, args):
                if (decl.type != arg.type) or (decl.ref and not isinstance(arg, Varval)):
                    raise ScopeException
            return call.ret_type
    def add_callable(self, callable):
        if callable.globid in self:
            raise ScopeException
        self.callables[callable.globid] = callable
    def __getitem__(self, name):
        if name not in self.variables:
            raise ScopeException
        return self.variables[name].type
    def add_variable(self, variable):
        if variable.var in self:
            raise ScopeException
        self.variables[variable.var] = variable
    def copy(self):
        new_scope = self.__class__(callables=self.callables, variables=self.variables, pointers=self.pointers)
        new_scope.module = self.module
        return new_scope
    def add_ptr(self, name, ptr): self.pointers[name] = ptr
    def get_ptr(self, name): return self.pointers[name]
    def get_args(self, name, args):
        call = self.callables[name]
        if args is None: return []
        return [arg.ptr if decl.ref else arg.ir for decl, arg in zip(call.decls, args)]

class ScopeException(Exception):
    '''Exception for Out of Scope'''
    pass


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


lex.lex()


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
        p[0] = p[2] + p[1]

def p_extern(p):
    '''extern : EXTERN type globid LPAREN tdecls RPAREN SEMICOLON
              | EXTERN TYPEVOID globid LPAREN tdecls RPAREN SEMICOLON
              | EXTERN type globid LPAREN RPAREN SEMICOLON
              | EXTERN TYPEVOID globid LPAREN RPAREN SEMICOLON'''
    if len(p) == 8:
        p[0] = Extern(lineno=p.lexer.lineno, ret_type=p[2], globid=p[3], decls=p[5])
    else:
        p[0] = Extern(lineno=p.lexer.lineno, ret_type=p[2], globid=p[3], decls=None)


def p_funcs(p):
    '''funcs : func funcs
             | func'''
    if len(p) == 2:
        p[0] = Funcs(lineno=p.lexer.lineno, node=p[1])
    else:
        p[0] = p[2] + p[1]

def p_func(p):
    '''func : DEF type globid LPAREN vdecls RPAREN blk
            | DEF TYPEVOID globid LPAREN vdecls RPAREN blk
            | DEF type globid LPAREN RPAREN blk
            | DEF TYPEVOID globid LPAREN RPAREN blk'''
    if len(p) == 8:
        p[0] = Func(lineno=p.lexer.lineno, ret_type=p[2], globid=p[3], decls=p[5], blk=p[7])
    else:
        p[0] = Func(lineno=p.lexer.lineno, ret_type=p[2], globid=p[3], decls=None, blk=p[6])

def p_blk(p):
    '''blk : LBRACE stmts RBRACE
           | LBRACE RBRACE'''
    if len(p) == 4:
        p[0] = Blk(lineno=p.lexer.lineno, contents=p[2])
    else:
        p[0] = Blk(lineno=p.lexer.lineno, contents=None)

def p_stmts(p):
    '''stmts : stmt stmts
             | stmt'''
    if len(p) == 2:
        p[0] = Stmts(lineno=p.lexer.lineno, node=p[1])
    else:
        p[0] = p[2] + p[1]

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
        p[0] = p[3] + p[1]

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
    p[0] = Binop(lineno=p.lexer.lineno, op=p[2], lhs=p[1], rhs=p[3])
def p_logicop(p):
    '''logicop : exp EQUAL exp
               | exp LESS exp
               | exp GREATER exp
               | exp BITAND exp
               | exp BITOR exp'''
    p[0] = Binop(lineno=p.lexer.lineno, op=p[2], lhs=p[1], rhs=p[3])

def p_uop(p):
    '''uop : MINUS exp %prec UMINUS
           | BITNOT exp'''
    p[0] = Uop(lineno=p.lexer.lineno, op=p[1], exp=p[2])

def p_lit_lit(p):
    'lit : NUMBER'
    p[0] = Lit(lineno=p.lexer.lineno, value=p[1])
def p_lit_flit(p):
    'lit : NUMBERFLOAT'
    p[0] = Flit(lineno=p.lexer.lineno, value=p[1])
def p_lit_blit(p):
    '''lit : TRUE
           | FALSE'''
    p[0] = Blit(lineno=p.lexer.lineno, value={'false': 0, 'true': 1}[p[1]])
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

def p_type(p):
    '''type : TYPEINT
            | TYPECINT
            | TYPEFLOAT
            | TYPEBOOL'''
    p[0] = p[1]

def p_vdecls(p):
    '''vdecls : vdecl COMMA vdecls
              | vdecl'''
    if len(p) == 2:
        p[0] = Vdecls(lineno=p.lexer.lineno, node=p[1])
    else:
        p[0] = p[3] + p[1]

def p_tdecls(p):
    '''tdecls : tdecl COMMA tdecls
              | tdecl'''
    if len(p) == 2:
        p[0] = Tdecls(lineno=p.lexer.lineno, node=p[1])
    else:
        p[0] = p[3] + p[1]

def p_vdecl(p):
    '''vdecl : TYPENOALIAS TYPEREF type DOLLARNAME
             | TYPEREF type DOLLARNAME
             | type DOLLARNAME'''
    if len(p) == 5:
        p[0] = Vdecl(lineno=p.lexer.lineno, noalias=True, ref=True, type=p[3], var=p[4])
    elif len(p) == 4:
        p[0] = Vdecl(lineno=p.lexer.lineno, noalias=False, ref=True, type=p[2], var=p[3])
    else:
        p[0] = Vdecl(lineno=p.lexer.lineno, noalias=False, ref=False, type=p[1], var=p[2])
def p_tdecl(p):
    '''tdecl : TYPENOALIAS TYPEREF type
             | TYPEREF type
             | type'''
    if len(p) == 4:
        p[0] = Tdecl(lineno=p.lexer.lineno, noalias=True, ref=True, type=p[3])
    elif len(p) == 3:
        p[0] = Tdecl(lineno=p.lexer.lineno, noalias=False, ref=True, type=p[2])
    else:
        p[0] = Tdecl(lineno=p.lexer.lineno, noalias=False, ref=False, type=p[1])


def p_error(p):
    Exit.PARSING_ERROR(p)


yacc_parser = yacc.yacc(debug=False)



def main(input_args=None):
    parser = argparse.ArgumentParser(description='The Extended-Kaleidoscope Language Compiler',
                                     add_help=False,
                                     usage='%(prog)s [-h|-?] [-v] [-O] [-emit-ast|-emit-llvm] [-jit] [-o <output-file>] <input-file> [args]',
                                     epilog='Authors: Naga Nithin Manne & Dipti Sengupta')

    parser.add_argument('-h', '-?', action='help', default=argparse.SUPPRESS,
                            help='Show this help message and exit')

    parser.add_argument('-v', action='store_true', help='Enable Verbose mode')

    parser.add_argument('-O', action='store_true', help='Enable Optimization')

    emit_group = parser.add_mutually_exclusive_group()
    emit_group.add_argument('-emit-ast', action='store_true', help='Dump AST in YAML Format to the Output File')
    emit_group.add_argument('-emit-llvm', action='store_true', help='Dump LLVM IR to the Output File')

    parser.add_argument('-jit', action='store_true', help='Run the compiled Code')

    parser.add_argument('-o', metavar='output-file', help='Output File to emit AST or LLVM IR')

    parser.add_argument('input', metavar='input-file', help='Input .ek File to Compile')

    parser.add_argument('args', metavar='args', nargs='*', default=None, help='Optional arguments when performing JIT and execution')

    args = parser.parse_args(args=input_args)

    with open(args.input) as input_file:
        ast = yacc_parser.parse(input_file.read())

    if args.jit: ast.walk_ast(builder=os.path.basename(args.input), jit=args.args)
    else: ast.walk_ast(builder=os.path.basename(args.input), jit=None)

    llvm.initialize()
    llvm.initialize_native_target()
    llvm.initialize_native_asmprinter()
    llvm_ir = str(ast.module)

    # Create a target machine representing the host
    target = llvm.Target.from_default_triple()
    target_machine = target.create_target_machine()
    # And an execution engine with an empty backing module
    backing_mod = llvm.parse_assembly("")
    engine = llvm.create_mcjit_compiler(backing_mod, target_machine)


    # Create a LLVM module object from the IR
    mod = llvm.parse_assembly(llvm_ir)
    mod.verify()


    #add optimization pipeline
    if args.O:
        pmb = llvm.create_pass_manager_builder()
        pmb.opt_level = 3

        fpm = llvm.create_function_pass_manager(mod)
        pmb.populate(fpm)

        pm = llvm.create_module_pass_manager()
        pmb.populate(pm)

        pm.run(mod)

    if args.emit_ast:
        yaml.representer.Representer.add_multi_representer(Node, yaml.representer.Representer.represent_dict)
        if args.o:
            with open(args.o, 'w') as ast_output_file:
                yaml.dump(ast, ast_output_file, indent=2, sort_keys=False, explicit_start=True, explicit_end=True)
        else:
            print(yaml.dump(ast, indent=2, sort_keys=False, explicit_start=True, explicit_end=True))
    elif args.emit_llvm:
        if args.o:
            with open(args.o, 'w') as ast_output_file:
                print(ast.module, file=ast_output_file)
        else:
            print(mod)

    # Now add the module and make sure it is ready for execution
    engine.add_module(mod)
    engine.finalize_object()
    engine.run_static_constructors()

    if args.jit:
        func_ptr = engine.get_function_address('run')
        from ctypes import CFUNCTYPE, c_int32
        cfunc = CFUNCTYPE(c_int32)(func_ptr)
        res = cfunc()
        sys.exit(res)
    else:
        if args.o:
            object_file = args.o + '.exe'
        else:
            object_file = args.input + '.exe'
        proc = subprocess.Popen(['gcc', '-o', object_file, '-x', 'assembler', '-'], stdin=subprocess.PIPE)
        proc.communicate(target_machine.emit_assembly(mod).encode())

if __name__=='__main__':
    main()
