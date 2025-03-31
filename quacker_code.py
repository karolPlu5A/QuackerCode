"""a programming language based of basic implemented in python"""
import string
import math
import random
import os
CYFRY = '0123456789' #cyfry do lexer'a
LITERY = string.ascii_letters
LITERY_CYFRY = LITERY + CYFRY
TT_IDENTIFIER = 'IDENTIFIER'
TT_KEYWORD = 'KEYWORD'
TT_EQ = 'EQ'
TT_INT = 'INT'
TT_FLOAT = 'FLOAT'
TT_STRING = 'STRING'
TT_PLUS = 'PLUS'
TT_MINUS = 'MINUS'
TT_MUL = 'MUL'
TT_DIV = 'DIV'
TT_POW = 'POW'
TT_LPAREN = 'LPAREN'
TT_RPAREN = 'RPAREN'
TT_LSQUARE = 'LSQUARE'
TT_RSQUARE = 'RSQUARE'
TT_EE = 'EE' #==
TT_NE = 'NE' #!=
TT_LT = 'LT' #<
TT_GT = 'GT' #>
TT_LTE = 'LTE' #<=
TT_GTE = 'GTE' #>=
TT_COMMA = 'COMMA'
TT_ARROW = 'ARROW'
TT_EOF = 'EOF'
KEYWORDS = [
    'VAR',
    'AND',
    'OR',
    'NOT',
    'IF',
    'ELIF',
    'ELSE',
    'THEN',
    'FOR',
    'TO',
    'STEP',
    'WHILE',
    'FUNC'
]
class ERROR:
    def __init__(self, pos_start, pos_end, errorname, details):
        self.pos_start = pos_start
        self.pos_end = pos_end
        self.error_name = errorname
        self.details = details
    def Err_string(self):
        error = f'In file: {self.pos_start.fn}, line {self.pos_start.line + 1}:\n\t{self.error_name}: {self.details}'
        return error
class IllegalCharERROR(ERROR):
    def __init__(self, pos_start, pos_end,details):
        super().__init__(pos_start,pos_end,"Illegal character", details)
class InvalidSyntaxERROR(ERROR):
    def __init__(self, pos_start, pos_end,details):
        super().__init__(pos_start,pos_end,"Invalid Syntax", details)
class RunTimeERROR(ERROR):
    def __init__(self, pos_start, pos_end,details, context):
        super().__init__(pos_start,pos_end,"Runtime Error", details)
        self.context = context
    def Err_string(self):
        error = self.generate_traceback()
        error += f'{self.error_name}: {self.details}'
        return error
    def generate_traceback(self):
        result = ''
        pos = self.pos_start
        ctx = self.context
        while ctx:
            result = f'In file: {self.pos_start.fn}, line {str(pos.line + 1)}, in {ctx.display_name}\n\t' + result
            pos = ctx.parent_entry_pos
            ctx = ctx.parent
        return 'Traceback (most recent call last):\n' + result
class ExpectedCharERROR(ERROR):
    def __init__(self, pos_start, pos_end, details):
        super().__init__(pos_start, pos_end, "Expected: ", details)
class Token:
    def __init__(self, type_,value=None, pos_start=None, pos_end=None):
        self.type = type_
        self.value = value
        if pos_start:
            self.pos_start = pos_start.copy()
            self.pos_end = pos_start.copy()
            self.pos_end.advance()
        if pos_end:
            self.pos_end = pos_end
    def matches(self, type_, value):
        return self.type == type_ and self.value == value

    def __repr__(self):
        if self.value: return f'{self.type}:{self.value}'
        return f'{self.type}'
###########
#POZYCJA BŁEDU
###########
class Position:
    def __init__(self, index, line, col, filename, filetext):
        self.index = index
        self.line = line
        self.col = col
        self.fn = filename
        self.ftxt = filetext
    def advance(self, curr_char=None):
        self.index += 1
        self.col += 1
        if curr_char == '\n':
            self.line += 1
            self.col = 0
        return self
    def copy(self):
        return Position(self.index, self.line, self.col, self.fn, self.ftxt)
###########
#LEXER
###########

class Lexer:
    def __init__(self, filename, text):
        self.file = filename
        self.text = text
        self.pos = Position(-1, 0, -1,filename, text)
        self.curr_char = None
        self.advance()
    def advance(self):
        self.pos.advance(self.curr_char)
        self.curr_char = self.text[self.pos.index] if self.pos.index < len(self.text) else None
    def make_tokens(self):
        tokens = []

        while self.curr_char != None:
            if self.curr_char in ' \t':
                self.advance()
            elif self.curr_char in CYFRY:
                tokens.append(self.make_num())
            elif self.curr_char in LITERY:
                tokens.append(self.make_identifier())
            elif self.curr_char == '"':
                tokens.append(self.make_string())
            elif self.curr_char == '+':
                tokens.append(Token(TT_PLUS, pos_start=self.pos))
                self.advance()
            elif self.curr_char == '-':
                tokens.append(self.minus_or_arrow())
            elif self.curr_char == '*':
                tokens.append(Token(TT_MUL, pos_start=self.pos))
                self.advance()
            elif self.curr_char == '/':
                tokens.append(Token(TT_DIV, pos_start=self.pos))
                self.advance()
            elif self.curr_char == '^':
                tokens.append(Token(TT_POW, pos_start=self.pos))
                self.advance()
            elif self.curr_char == '[':
                tokens.append(Token(TT_LSQUARE, pos_start=self.pos))
                self.advance()
            elif self.curr_char == ']':
                tokens.append(Token(TT_RSQUARE, pos_start=self.pos))
                self.advance()
            elif self.curr_char == '(':
                tokens.append(Token(TT_LPAREN, pos_start=self.pos))
                self.advance()
            elif self.curr_char == ')':
                tokens.append(Token(TT_RPAREN, pos_start=self.pos))
                self.advance()
            elif self.curr_char == '!':
                tok, error = self.make_not_equals()
                if error: return [], error
                tokens.append(tok)
            elif self.curr_char == '=':
                tokens.append(self.make_equals())
            elif self.curr_char == '>':
                tokens.append(self.make_greater_than())
            elif self.curr_char == '<':
                tokens.append(self.make_less_than())
            elif self.curr_char == ',':
                tokens.append(Token(TT_COMMA, pos_start=self.pos))
                self.advance()
            else:
                start_err = self.pos.copy()
                char = self.curr_char
                self.advance()
                return [], IllegalCharERROR(start_err, self.pos,"'" + char + "'")
        tokens.append(Token(TT_EOF, pos_start=self.pos))    
        return tokens, None
    def make_string(self):
        str_str = ""
        pos_start = self.pos.copy()
        esc_char = False
        esc_chars = {
            "n":"\n",
            "t":"\t"
            }
        self.advance()
        while self.curr_char != None and (self.curr_char != '"' or esc_char):
            if esc_char:
                str_str += esc_chars.get(self.curr_char, self.curr_char)
                esc_char = False
            else:
                if self.curr_char == "\\":
                    esc_char = True
                else:
                    str_str += str(self.curr_char)
                    esc_char = False
            self.advance()
        self.advance()
        return Token(TT_STRING, str_str, pos_start, self.pos)
    def minus_or_arrow(self):
        tok_type = TT_MINUS
        pos_start = self.pos.copy()
        self.advance()
        if self.curr_char == '>':
            self.advance()
            tok_type = TT_ARROW
        return Token(tok_type, pos_start=pos_start,pos_end=self.pos)
    def make_num(self):
        num_str = ''
        dot_count = 0
        pos_start = self.pos.copy()

        while self.curr_char != None and self.curr_char in CYFRY + '.':
            if self.curr_char == '.':
                if dot_count == 1: break
                dot_count+=1
                num_str += '.'
            else:
                num_str += self.curr_char
            self.advance()
        if dot_count == 0:
            return Token(TT_INT, value=int(num_str),pos_start=pos_start, pos_end=self.pos)
        else:
            return Token(TT_FLOAT, float(num_str), pos_start, self.pos)
    def make_identifier(self):
        id_str = ''
        pos_start = self.pos.copy()
        while self.curr_char != None and self.curr_char in LITERY_CYFRY + '_':
            id_str += self.curr_char
            self.advance()
        tok_type = TT_KEYWORD if id_str in KEYWORDS else TT_IDENTIFIER
        return Token(tok_type, id_str, pos_start, self.pos)
    def make_equals(self):
        tok_type = TT_EQ
        pos_start = self.pos.copy()
        self.advance()
        if self.curr_char == '=':
            self.advance()
            tok_type = TT_EE
        return Token(tok_type, pos_start=pos_start,pos_end=self.pos)
    def make_not_equals(self):
        pos_start = self.pos.copy()
        neq_string = '!'
        self.advance()
        if self.curr_char == '=':
            return Token(TT_NE,pos_start=pos_start,pos_end=self.pos), None
        neq_string += str(self.curr_char)
        if neq_string == '!None':
            return None, InvalidSyntaxERROR(pos_start,self.pos,"char: '!' in wrong position")
        return None, ExpectedCharERROR(pos_start,self.pos,f" '!='  not {neq_string} ")
    def make_less_than(self):
        tok_type = TT_LT
        pos_start = self.pos.copy()
        self.advance()
        if self.curr_char == '=':
            self.advance()
            tok_type = TT_LTE
        return Token(tok_type, pos_start=pos_start,pos_end=self.pos)
    def make_greater_than(self):
        tok_type = TT_GT
        pos_start = self.pos.copy()
        self.advance()
        if self.curr_char == '=':
            self.advance()
            tok_type = TT_GTE
        return Token(tok_type, pos_start=pos_start,pos_end=self.pos)

###########
# NODY
###########        
class NumberNode:
    def __init__(self, tok):
        self.tok = tok
        self.pos_start = self.tok.pos_start
        self.pos_end = self.tok.pos_end
    def __repr__(self):
        return f'{self.tok}'
class BinOpNode:
    def __init__(self, left_node, op_tok, right_node):
        self.left_node = left_node
        self.op_tok = op_tok
        self.right_node = right_node
        self.pos_start = self.left_node.pos_start
        self.pos_end = self.right_node.pos_end
    def __repr__(self):
        return f'({self.left_node}, {self.op_tok}, {self.right_node})'
class UnaryOpNode:
    def __init__(self, op_tok, node):
        self.op_tok = op_tok
        self.node = node
        self.pos_start = self.op_tok.pos_start
        self.pos_end = self.node.pos_end
    def __repr__(self):
        return f'{self.op_tok}, {self.node}'
class VarAccessNode:
    def __init__(self, var_name_tok):
        self.var_name_tok = var_name_tok
        self.pos_start = var_name_tok.pos_start
        self.pos_end = var_name_tok.pos_end
class VarAssignNode:
    def __init__(self, var_name_tok, value_node):
        self.var_name_tok = var_name_tok
        self.value_node = value_node
        self.pos_start = var_name_tok.pos_start
        self.pos_end = self.value_node.pos_end
class IfNode:
    def __init__(self, cases, else_case):
        self.cases = cases
        self.else_case = else_case
        self.pos_start = self.cases[0][0].pos_start
        self.pos_end = (self.else_case or self.cases[-1][0]).pos_end
class ForNode():
    def __init__(self, var_name_tok, start_value_node, end_value_node, step_value_node, body_node):
        self.var_name_tok = var_name_tok
        self.start_value_node = start_value_node
        self.end_value_node= end_value_node
        self.step_value_node = step_value_node
        self.body_node = body_node
        self.pos_start = self.var_name_tok.pos_start
        self.pos_end = self.body_node.pos_end
class WhileNode():
    def __init__(self, condition_node, body_node):
        self.condition_node = condition_node
        self.body_node = body_node
        self.pos_start = condition_node.pos_start
        self.pos_end = self.body_node.pos_end
class FuncDefNode():
    def __init__(self, var_name_tok, arg_name_toks, body_node):
        self.var_name_tok = var_name_tok
        self.arg_name_toks = arg_name_toks
        self.body_node = body_node
        if self.var_name_tok:
            self.pos_start = self.var_name_tok.pos_start
        elif len(self.arg_name_toks) > 0:
            self.pos_start = self.arg_name_toks[0].pos_start
        else:
            self.pos_start = self.body_node.pos_start
        self.pos_end = self.body_node.pos_end
class CallNode():
    def __init__(self, node_to_call, arg_nodes):
        self.node_to_call = node_to_call
        self.arg_nodes = arg_nodes
        self.pos_start = self.node_to_call.pos_start
        if len(self.arg_nodes) > 0:
            self.pos_end = self.arg_nodes[-1].pos_end
        else:
            self.pos_end = self.node_to_call.pos_end
class StringNode():
    def __init__(self, tok):
        self.tok = tok
        self.pos_start = self.tok.pos_start
        self.pos_end = self.tok.pos_end
    def __repr__(self):
        return f'{self.tok}'
class ListNode():
    def __init__(self, element_nodes, pos_start, pos_end):
        self.element_nodes = element_nodes
        self.pos_start = pos_start
        self.pos_end = pos_end
###########
#WYNIKI
###########
class ParseResult:
    def __init__(self):
        self.error = None
        self.node = None
        self.advance_count = 0
    def register_advancement(self):
        self.advance_count += 1
    def register(self,res):
        self.advance_count += res.advance_count
        if res.error: self.error = res.error
        return res.node
    def success(self, node):
        self.node = node
        return self
    def failure(self, error): #just like u:)
        if not self.error or self.advance_count == -0:
            self.error = error
        return self
    def matches(self, node, value):
        return self.node == node
    
class RTresult:
    def __init__(self):
        self.value = None
        self.error = None
    def register(self, res):
        if res.error: self.error = res.error
        return res.value 
    def success(self, value):
        self.value = value
        return self
    def failure(self, error):
        self.error = error
        return self
###########
#PARSER
###########
class Parser:
    def __init__(self,tokens):
        self.tokens = tokens
        self.tok_index = -1
        self.advance()
    def advance(self):
        self.tok_index += 1
        if self.tok_index < len(self.tokens):
            self.current_tok = self.tokens[self.tok_index]
        return self.current_tok
    def parse(self):
        res = self.expr()
        if not res.error and self.current_tok.type != TT_EOF:
            return res.failure(InvalidSyntaxERROR(
                self.current_tok.pos_start, self.current_tok.pos_end,
                "Expected: '+', '-', '*', '/', '^'"
            ))
        return res
    def call(self):
        res = ParseResult()
        atom = res.register(self.atom())
        if res.error: 
            return res
        if self.current_tok.type == TT_LPAREN:
            res.register_advancement()
            self.advance()  # consume '('
            arg_nodes = []
            if self.current_tok.type == TT_RPAREN:
                res.register_advancement()
                self.advance()  # consume ')'
            else:
                arg_nodes.append(res.register(self.expr()))
                if res.error: 
                    return res.failure(InvalidSyntaxERROR(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    "Expected an expression"
                ))
                while self.current_tok.type == TT_COMMA:
                    res.register_advancement()
                    self.advance()  # consume comma
                    arg_nodes.append(res.register(self.expr()))
                    if res.error: return res
            # *** New: Check for and consume the closing parenthesis ***
                if self.current_tok.type != TT_RPAREN:
                    return res.failure(InvalidSyntaxERROR(
                        self.current_tok.pos_start, self.current_tok.pos_end,
                        "Expected: ')'"
                    ))
                res.register_advancement()
                self.advance()  # consume ')'
            return res.success(CallNode(atom, arg_nodes))
        return res.success(atom)

    def atom(self):
        res= ParseResult()
        tok = self.current_tok
        if tok.type in (TT_INT, TT_FLOAT):
            res.register_advancement()
            self.advance()
            return res.success(NumberNode(tok))
        elif tok.type == TT_STRING:
            res.register_advancement()
            self.advance()
            return res.success(StringNode(tok))
        elif tok.type == TT_IDENTIFIER:
            res.register_advancement()
            self.advance()
            return res.success(VarAccessNode(tok))
        elif tok.type == TT_LPAREN:  
            res.register_advancement()
            self.advance()  
            expr = res.register(self.expr())  
            if res.error: return res
            if self.current_tok.type != TT_RPAREN:  
                return res.failure(InvalidSyntaxERROR(
                self.current_tok.pos_start, self.current_tok.pos_end,
                "Expected: ')'"
                ))
            else:
                res.register_advancement()
                self.advance()
                return res.success(expr)
        elif tok.type == TT_LSQUARE: 
            list_expr = res.register(self.list_expr())  
            if res.error: return res
            return res.success(list_expr)
        elif self.current_tok.matches(TT_KEYWORD, 'IF'):
            if_expr = res.register(self.if_expr())
            if res.error: return res
            return res.success(if_expr)
        elif self.current_tok.matches(TT_KEYWORD, 'FOR'):
                for_expr = res.register(self.for_expr())
                if res.error: return res
                return res.success(for_expr)
        elif self.current_tok.matches(TT_KEYWORD, 'WHILE'):
            while_expr = res.register(self.while_expr())
            if res.error: return res
            return res.success(while_expr)
        elif self.current_tok.matches(TT_KEYWORD, 'FUNC'):
            func_def = res.register(self.func_def())
            if res.error: return res
            return res.success(func_def)
        return res.failure(InvalidSyntaxERROR(
            self.current_tok.pos_start, self.current_tok.pos_end,
            "Expected: INT, FLOAT, identifier, '+', '-',  IF, FOR, WHILE, FUNC '[' or '('"
        ))
    def power(self):
        return self.bin_op(self.call, (TT_POW,), self.factor)
    def factor(self):
        res = ParseResult()
        tok = self.current_tok
        if tok.type in (TT_PLUS, TT_MINUS):
            res.register_advancement()
            self.advance()
            factor = res.register(self.factor())
            if res.error: return res
            return res.success(UnaryOpNode(tok, factor))    
        return self.power()
    def term(self):
        return self.bin_op(self.factor, (TT_MUL, TT_DIV))
    def arith_expr(self):
        return self.bin_op(self.term, (TT_PLUS, TT_MINUS))
    def comp_expr(self):
        res = ParseResult()
        if self.current_tok.matches(TT_KEYWORD, "NOT"):
            op_tok = self.current_tok
            res.register_advancement()
            self.advance()
            node = res.register(self.comp_expr())
            if res.error: return res
            return res.success(UnaryOpNode(op_tok, node))
        node = res.register(self.bin_op(self.arith_expr, (TT_EE, TT_NE, TT_GT, TT_GTE, TT_LT, TT_LTE)))
        if res.error: 
            return res.failure(InvalidSyntaxERROR( self.current_tok.pos_start, self.current_tok.pos_end,
                "Expected 'NOT', INT, FLOAT, identifier, '+', '-', '[' or '('"
            ))
        return res.success(node)
    def expr(self):
        res = ParseResult()
        if self.current_tok.matches(TT_KEYWORD, 'VAR'):
            res.register_advancement()
            self.advance()
            if self.current_tok.type != TT_IDENTIFIER:
                return res.failure(InvalidSyntaxERROR(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    "Excpected: identifier"
                ))
            var_name = self.current_tok
            res.register_advancement()
            self.advance()
            if self.current_tok.type != TT_EQ:
                return res.failure(InvalidSyntaxERROR(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    "Excpected: '='"
            ))
            res.register_advancement()
            self.advance()
            expr = res.register(self.expr())
            if res.error: return res
            return res.success(VarAssignNode(var_name, expr))
        if self.current_tok.matches(TT_KEYWORD, 'IF'):
            node = res.register(self.if_expr())
            if res.error: return res
            return res.success(node)
        if self.current_tok.matches(TT_KEYWORD, 'FOR'):
            node = res.register(self.for_expr())
            if res.error: return res
            return res.success(node)
        if self.current_tok.matches(TT_KEYWORD, 'WHILE'):
            node = res.register(self.while_expr())
            if res.error: return res
            return res.success(node)
        if self.current_tok.matches(TT_KEYWORD, 'FUNC'):
            node = res.register(self.func_def())
            if res.error: return res
            return res.success(node)
        node = res.register(self.bin_op(self.comp_expr, ((TT_KEYWORD,"AND"), (TT_KEYWORD,"OR"))))
        if res.error: 
            return res.failure(InvalidSyntaxERROR(
                self.current_tok.pos_start, self.current_tok.pos_end,
                "Excpected: 'NOT','VAR', INT, FLOAT, identifier, '+', '-',  IF, FOR, WHILE, FUNC, '[' or '('"
            ))
        return res.success(node)
    def list_expr(self):
        res = ParseResult()
        element_nodes = []
        pos_start = self.current_tok.pos_start.copy()
        if self.current_tok.type != TT_LSQUARE:
            return res.failure(InvalidSyntaxERROR(
                        self.current_tok.pos_start, self.current_tok.pos_end,
                        "Expected: '['"
                    ))
        res.register_advancement()
        self.advance()
        if self.current_tok.type == TT_RSQUARE:
            res.register_advancement()
            self.advance()
        else:
            element_nodes.append(res.register(self.expr()))
            if res.error: 
                return res.failure(InvalidSyntaxERROR(
                self.current_tok.pos_start, self.current_tok.pos_end,
                "Expected an expression"
            ))
            while self.current_tok.type == TT_COMMA:
                res.register_advancement()
                self.advance()  # consume comma
                element_nodes.append(res.register(self.expr()))
                if res.error: return res
            if self.current_tok.type != TT_RSQUARE:
                element_nodes.append(res.register(self.expr()))
                if res.error:
                    return res.failure(InvalidSyntaxERROR(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    "Expected: ']'"
                    ))
        res.register_advancement()
        self.advance() 
        return res.success(ListNode(element_nodes, pos_start, self.current_tok.pos_end.copy()))
    def if_expr(self):
        res = ParseResult()
        cases = []
        else_case = None
        if not self.current_tok.matches(TT_KEYWORD, 'IF'):
            return res.failure(InvalidSyntaxERROR(
                self.current_tok.pos_start, self.current_tok.pos_end,
                f"Expected: 'IF'"
            ))
        res.register_advancement()
        self.advance()
        condition = res.register(self.expr())
        if res.error: return res
        if not self.current_tok.matches(TT_KEYWORD, 'THEN'):
            return res.failure(InvalidSyntaxERROR(
                self.current_tok.pos_start, self.current_tok.pos_end,
                f"Expected: 'THEN'"
            ))
        res.register_advancement()
        self.advance()
        expr = res.register(self.expr())
        if res.error: return res
        cases.append((condition, expr))
        while self.current_tok.matches(TT_KEYWORD, 'ELIF'):
            res.register_advancement()
            self.advance()
            condition = res.register(self.expr())
            if res.error: return res
            if not self.current_tok.matches(TT_KEYWORD, 'THEN'):
                return res.failure(InvalidSyntaxERROR(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    f"Expected: 'THEN'"
            ))
            res.register_advancement()
            self.advance()
            expr = res.register(self.expr())
            if res.error: return res
            cases.append((condition,expr))
        if self.current_tok.matches(TT_KEYWORD, 'ELSE'):
            res.register_advancement()
            self.advance()
            else_case = res.register(self.expr())
            if res.error: return res
        return res.success(IfNode(cases, else_case))
    def for_expr(self):
        res = ParseResult()
        if not self.current_tok.matches(TT_KEYWORD, 'FOR'):
           return res.failure(InvalidSyntaxERROR(
                self.current_tok.pos_start, self.current_tok.pos_end,
                f"Expected: 'FOR'"
            ))
        res.register_advancement()
        self.advance()
        if self.current_tok.type != TT_IDENTIFIER:
            return res.failure(InvalidSyntaxERROR(
                self.current_tok.pos_start, self.current_tok.pos_end,
                f"Expected: identifier"
            ))
        var_name = self.current_tok
        res.register_advancement()
        self.advance()
        if self.current_tok.type != TT_EQ:
            return res.failure(InvalidSyntaxERROR(
                self.current_tok.pos_start, self.current_tok.pos_end,
                f"Expected: '='"
            ))
        res.register_advancement()
        self.advance()
        start_value = res.register(self.expr())
        if res.error: return res
        if not self.current_tok.matches(TT_KEYWORD, 'TO'):
            return res.failure(InvalidSyntaxERROR(
                self.current_tok.pos_start, self.current_tok.pos_end,
                f"Expected: 'TO'"
            ))
        res.register_advancement()
        self.advance()
        end_value = res.register(self.expr())
        if res.error: return res
        if self.current_tok.matches(TT_KEYWORD, 'STEP'):
            res.register_advancement()
            self.advance()
            step_value = res.register(self.expr())
            if res.error: return res
        else:
            step_value = None
        if not self.current_tok.matches(TT_KEYWORD, 'THEN'):
            return res.failure(InvalidSyntaxERROR(
                self.current_tok.pos_start, self.current_tok.pos_end,
                f"Expected: 'THEN'"
            ))
        res.register_advancement()
        self.advance()
        body = res.register(self.expr())
        if res.error: return res
        return res.success(ForNode(var_name, start_value, end_value, step_value, body))
    def while_expr(self):
        res = ParseResult()
        if not self.current_tok.matches(TT_KEYWORD, 'WHILE'):
            return res.failure(InvalidSyntaxERROR(
                self.current_tok.pos_start, self.current_tok.pos_end,
                f"Expected: 'WHILE'"
            ))
        res.register_advancement()
        self.advance()
        condition = res.register(self.expr())
        if res.error: return res
        if not self.current_tok.matches(TT_KEYWORD, 'THEN'):
            return res.failure(InvalidSyntaxERROR(
                self.current_tok.pos_start, self.current_tok.pos_end,
                f"Expected: 'THEN'"
            ))
        res.register_advancement()
        self.advance()
        body = res.register(self.expr())
        if res.error: return res
        return res.success(WhileNode(condition, body))
    def func_def(self):
        res = ParseResult()
        if not self.current_tok.matches(TT_KEYWORD, 'FUNC'):
            return res.failure(InvalidSyntaxERROR(
                self.current_tok.pos_start, self.current_tok.pos_end,
                "Expected: 'FUNC'"
        ))
        res.register_advancement()
        self.advance()

    
        var_name_tok = None
        if self.current_tok.type == TT_IDENTIFIER:
            var_name_tok = self.current_tok
            res.register_advancement()
            self.advance()

    # Next token must be '('.
        if self.current_tok.type != TT_LPAREN:
            return res.failure(InvalidSyntaxERROR(
                self.current_tok.pos_start, self.current_tok.pos_end,
                "Expected: '('"
            ))
        res.register_advancement()
        self.advance()  # consume '('

        arg_name_toks = []
        # Read parameters if present.
        if self.current_tok.type == TT_IDENTIFIER:
            arg_name_toks.append(self.current_tok)
            res.register_advancement()
            self.advance()
            while self.current_tok.type == TT_COMMA:
                res.register_advancement()
                self.advance()  # consume comma
                if self.current_tok.type != TT_IDENTIFIER:
                    return res.failure(InvalidSyntaxERROR(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    "Expected: identifier"
                    ))
                arg_name_toks.append(self.current_tok)
                res.register_advancement()
                self.advance()
        if self.current_tok.type != TT_RPAREN:
            return res.failure(InvalidSyntaxERROR(
            self.current_tok.pos_start, self.current_tok.pos_end,
            "Expected: ')'"
            ))
        res.register_advancement()
        self.advance()  # consume ')'

        # Now expect the arrow token.
        if self.current_tok.type != TT_ARROW:
            return res.failure(InvalidSyntaxERROR(
            self.current_tok.pos_start, self.current_tok.pos_end,
            "Expected: '->'"
            ))
        res.register_advancement()
        self.advance()  # consume '->'

    # Parse the function body (the expression to return)
        node_to_return = res.register(self.expr())
        if res.error: return res

        return res.success(FuncDefNode(var_name_tok, arg_name_toks, node_to_return))

    def bin_op(self, func_a, ops, func_b=None):
        if func_b is None:
            func_b = func_a
        res = ParseResult()
        left = res.register(func_a())
        if res.error:
            return res
        while self.current_tok.type in ops or (self.current_tok.type, self.current_tok.value) in ops:
            op_tok = self.current_tok
            res.register_advancement()
            self.advance()
            right = res.register(func_b())
            if res.error:
                return res
            left = BinOpNode(left, op_tok, right)
        return res.success(left)
############
# VALUE
############
class Value:
    def __init__(self):
        self.set_pos()
        self.set_context()
    def set_pos(self, pos_start=None, pos_end=None):
        self.pos_start = pos_start
        self.pos_end = pos_end
        return self
    def set_context(self, context=None):
        self.context = context
        return self
    def added_to(self, other):
        return None, self.illegal_operation(other)
    def subbed_by(self, other):
        return None, self.illegal_operation(other)
    def multed_by(self, other):
        return None, self.illegal_operation(other)
    def divided_by(self, other):
        return None, self.illegal_operation(other)
    def powed_by(self, other):
        return None, self.illegal_operation(other)
    def get_comparison_eq(self,other):
        return None, self.illegal_operation(other)
    def get_comparison_ne(self,other):
            return None, self.illegal_operation(other)
    def get_comparison_gt(self,other):
        return None, self.illegal_operation(other)
    def get_comparison_lt(self,other):
        return None, self.illegal_operation(other)
    def get_comparison_gte(self,other):
        return None, self.illegal_operation(other)
    def get_comparison_lte(self,other):
        return None, self.illegal_operation(other)
    def if_and(self,other):
        return None, self.illegal_operation(other)
    def if_or(self,other):
        return None, self.illegal_operation(other)
    def if_not(self):
        return None, self.illegal_operation()
    def copy(self):
        raise Exception("No copy method defined")
    def is_true(self):
        return False
    def execute(self):
        return None, self.illegal_operation()
    def illegal_operation(self, other=None):
        if not other: other = self
        return RunTimeERROR(
            self.pos_start, other.pos_end,
            "Illegal operation",
            self.context)
############
#CYFRY
############
class Number(Value):
    """A number used in Quacker."""
    def __init__(self, value):
        super().__init__()
        self.value = value
    def added_to(self, other):
        if isinstance(other, Number):
            return Number(self.value + other.value).set_context(self.context), None
        return None, Value.illegal_operation(self.pos_start, other.pos_end)
    def subbed_by(self, other):
        if isinstance(other, Number):
            return Number(self.value - other.value).set_context(self.context), None
        return None, Value.illegal_operation(self.pos_start, other.pos_end)
    def multed_by(self, other):
        if isinstance(other, Number):
            return Number(self.value * other.value).set_context(self.context), None
        return None, Value.illegal_operation(self.pos_start, other.pos_end)
    def divided_by(self, other):
        if isinstance(other, Number):
            if other.value == 0: 
                return None, RunTimeERROR(other.pos_start, other.pos_end,
                "Division By Zero",
                self.context
                )
            return Number(self.value / other.value).set_context(self.context), None
        return None, Value.illegal_operation(self.pos_start, other.pos_end)
    def powed_by(self, other):
        if isinstance(other, Number):
            return Number(self.value ** other.value).set_context(self.context), None
        return None, Value.illegal_operation(self.pos_start, other.pos_end)
    def get_comparison_eq(self,other):
        if isinstance(other, Number):
            return Number(int(self.value == other.value)).set_context(self.context), None
        return None, Value.illegal_operation(self.pos_start, other.pos_end)
    def get_comparison_ne(self,other):
        if isinstance(other, Number):
            return Number(int(self.value != other.value)).set_context(self.context), None
        return None, Value.illegal_operation(self.pos_start, other.pos_end)
    def get_comparison_gt(self,other):
        if isinstance(other, Number):
            return Number(int(self.value > other.value)).set_context(self.context), None
        return None, Value.illegal_operation(self.pos_start, other.pos_end)
    def get_comparison_lt(self,other):
        if isinstance(other, Number):
            return Number(int(self.value < other.value)).set_context(self.context), None
        return None, Value.illegal_operation(self.pos_start, other.pos_end)
    def get_comparison_gte(self,other):
        if isinstance(other, Number):
            return Number(int(self.value >= other.value)).set_context(self.context), None
        return None, Value.illegal_operation(self.pos_start, other.pos_end)
    def get_comparison_lte(self,other):
        if isinstance(other, Number):
            return Number(int(self.value <= other.value)).set_context(self.context), None
        return None, Value.illegal_operation(self.pos_start, other.pos_end)
    def if_and(self,other):
        if isinstance(other, Number):
            return Number(int(self.value and other.value)).set_context(self.context), None
        return None, Value.illegal_operation(self.pos_start, other.pos_end)
    def if_or(self,other):
        if isinstance(other, Number):
            return Number(int(self.value or other.value)).set_context(self.context), None
        return None, Value.illegal_operation(self.pos_start, other.pos_end)
    def if_not(self):
        return Number(1 if self.value == 0 else 0).set_context(self.context), None
    def is_true(self):
        return self.value != 0
    def copy(self):
        copy = Number(self.value)
        copy.set_pos(self.pos_start,self.pos_end)
        copy.set_context(self.context)
        return copy
    def __repr__(self):
        return str(self.value)
Number.null = Number(0)
Number.true = Number(1)
Number.false = Number(0)
class String(Value):
    """A string used in Quacker."""
    def __init__(self, value):
        super().__init__()
        self.value = value
    def added_to(self, other):
        if isinstance(other, String):
            return String(self.value + other.value).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)
    def multed_by(self, other):
        if isinstance(other, Number):
            return String(str(self.value)*other.value).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)
    def is_true(self):
        return len(self.value) > 0
    def copy(self):
        copy = String(self.value)
        copy.set_pos(self.pos_start,self.pos_end)
        copy.set_context(self.context)
        return copy
    def __repr__(self):
        return f'"{self.value}"'
class List(Value):
    def __init__(self, elements):
        super().__init__()
        self.elements = elements
    def added_to(self, other):
        if not isinstance(other, List):
            new_list = self.copy()
            new_list.elements.append(other)
            return new_list, None
        else:
            return None, InvalidSyntaxERROR(self.pos_start,self.pos_end,
                details="Use '*' to concatenate two lists together"
            )
    def subbed_by(self, other):
        if isinstance(other, Number):
            new_list = self.copy()
            try:
                new_list.elements.pop(other.value)
                return new_list, None
            except IndexError:
                return None, RunTimeERROR(
                    other.pos_start, other.pos_end,
                    "List index out of bounds", self.context
                )
        else:
            return None, Value.illegal_operation(self, other)
    def multed_by(self, other):
        if isinstance(other, List):
            new_list = self.copy()
            new_list.elements.extend(other.elements)
            return new_list, None
        else:
            return None, Value.illegal_operation(self, other)
    def divided_by(self, other):
        if isinstance(other, Number):
            try:
                return self.elements[other.value], None
            except IndexError:
                return None, RunTimeERROR(
                    other.pos_start, other.pos_end,
                    "List index out of bounds", self.context
                )
        else:
            return None, Value.illegal_operation(self, other)
    def copy(self):
        copy = List(self.elements)
        copy.set_pos(self.pos_start,self.pos_end)
        copy.set_context(self.context)
        return copy
    def __repr__(self):
        return f"[{', '.join(repr(el) for el in self.elements)}]"
class BaseFunction(Value):
    """The basics of a function used in Quacker."""
    def __init__(self, name):
        super().__init__()
        self.name = name or '<anonymous>'
    def generate_new_context(self):
        new_context = Context(self.name, self.context, self.pos_start)
        new_context.symbol_table = SymbolTable(new_context.parent.symbol_table)
        return new_context
    def check_args(self, arg_names, args):
        res = RTresult()
        if len(args) > len(arg_names):
            return res.failure(RunTimeERROR(
                self.pos_start, self.pos_end,
                f"You passed too many args into {self.name}, supposed to get {len(arg_names)}, got {len(args)}"
            ))
        if len(args) < len(arg_names):
            return res.failure(RunTimeERROR(
                self.pos_start, self.pos_end,
                f"You passed too few args into {self.name}, supposed to get {len(arg_names)}, got {len(args)}"
            ))
        return res.success(None)
    def populate_args(self, arg_names, args, exec_ctx):
        for i in range(len(args)):
            arg_name = arg_names[i]
            arg_value = args[i]
            arg_value.set_context(exec_ctx)
            exec_ctx.symbol_table.set(arg_name, arg_value, exec_ctx)
    def check_and_populate_args(self, arg_names, args, exec_ctx):
        res = RTresult()
        res.register(self.check_args(arg_names,args))
        if res.error: return res
        self.populate_args(arg_names,args,exec_ctx)
        return res.success(None)
class Function(BaseFunction):
    def __init__(self, name, body_node, arg_names):
        super().__init__(name)
        self.body_node = body_node
        self.arg_names = arg_names
    def execute(self, args):
        res = RTresult()
        interpreter = Interpreter()
        exec_context = self.generate_new_context()
        res.register(self.check_and_populate_args(self.arg_names,args, exec_context))
        if res.error: return res
        value = res.register(interpreter.visit(self.body_node, exec_context))
        if res.error: return res
        return res.success(value)
    def copy(self):
        copy = Function(self.name, self.body_node, self.arg_names)
        copy.set_context(self.context)
        copy.set_pos(self.pos_start, self.pos_end)
        return copy
    def __repr__(self):
        return f"<function {self.name}>"
class BuiltInFunc(BaseFunction):
    """An extension of BaseFunction, for pre-defined methods. The methods are: abs(), sqrt()"""
    def __init__(self, name):
        super().__init__(name)
    def execute(self, args):
        res = RTresult()
        exec_ctx = self.generate_new_context()
        method_name = f'execute_{self.name}'
        method = getattr(self,method_name, self.no_visit_method)
        res.register(self.check_and_populate_args(method.arg_names, args, exec_ctx))
        return_value = res.register(method(exec_ctx))
        if res.error: return res
        return res.success(return_value)
    def no_visit_method(self,node,context):
        raise Exception(f'No execute_{self.name} method defined')
    def copy(self):
        copy = Function(self.name, self.body_node, self.arg_names)
        copy.set_context(self.context)
        copy.set_pos(self.pos_start, self.pos_end)
        return copy
    def execute_printf(self, exec_ctx):
        print(str(exec_ctx.symbol_table.get('value')))
        return RTresult().success(Number.null)
    execute_printf.arg_names = ["value"]
    def execute_print_ret(self, exec_ctx):
        return RTresult().success(String(str(exec_ctx.symbol_table.get('value'))))
    execute_print_ret.arg_names = ["value"]
    def execute_input(self, exec_ctx):
        text = input()
        return RTresult().success(String(text))
    execute_input.arg_names = []
    def execute_input_int(self, exec_ctx):
        text = input()
        try:
            number = int(text)
        except:
            return RTresult().failure(RunTimeERROR(
                self.pos_start, self.pos_end,
                "Use input() to input non-ints", self.context
            ))
        return RTresult().success(Number(number))
    execute_input_int.arg_names = []
    def execute_clear(self):
        os.system('cls' if os.name == 'nt' else 'clear')
        return RTresult().success(Number.null)
    execute_clear.arg_names = []
    def execute_is_number(self,exec_ctx):
        is_number = isinstance(exec_ctx.symbol_table.get('value'), Number)
        return RTresult().success(Number.true if is_number else Number.false)
    execute_is_number.arg_names = ['value']
    def execute_is_string(self,exec_ctx):
        is_string = isinstance(exec_ctx.symbol_table.get('value'), String)
        return RTresult().success(Number.true if is_string else Number.false)
    execute_is_string.arg_names = ['value']
    def execute_is_list(self,exec_ctx):
        is_list = isinstance(exec_ctx.symbol_table.get('value'), List)
        return RTresult().success(Number.true if is_list else Number.false)
    execute_is_list.arg_names = ['value']
    def execute_is_func(self,exec_ctx):
        is_func = isinstance(exec_ctx.symbol_table.get('value'), BaseFunction)
        return RTresult().success(Number.true if is_func else Number.false)
    execute_is_func.arg_names = ['value']
    def execute_append(self, exec_ctx):
        list_ = exec_ctx.symbol_table.get("list")
        value = exec_ctx.symbol_table.get("value")
        if not isinstance(list_, List):
            return RTresult().failure(RunTimeERROR(
                self.pos_start, self.pos_end,
                f"First argument must be list, not {list_.type}"
            ))
        list_.elements.append(value)
        return RTresult().success(Number.null)
    execute_append.arg_names = ['list', 'value']
    def execute_pop(self, exec_ctx):
        list_ = exec_ctx.symbol_table.get("list")
        index = exec_ctx.symbol_table.get("index")
        if not isinstance(list_, List):
            return RTresult().failure(RunTimeERROR(
                self.pos_start, self.pos_end,
                f"First argument must be list, not {list_.type}"
            ))
        if not isinstance(index, Number):
            return RTresult().failure(RunTimeERROR(
                self.pos_start, self.pos_end,
                f"Second argument must be number, not {index.type}"
            ))
        try:
            element = list_.elements.pop(index.value)
        except ValueError:
            return RTresult().failure(RunTimeERROR(
                    self.pos_start, self.pos_end,
                    "List index out of bounds", exec_ctx
                ))
        return RTresult().success(element)
    execute_pop.arg_names = ['list', 'index']
    def execute_extend(self, exec_cxt):
        listA = exec_cxt.symbol_table.get('listA')
        listB = exec_cxt.symbol_table.get('listB')
        if not isinstance(listA, List):
            return RTresult().failure(RunTimeERROR(
                self.pos_start, self.pos_end,
                f"First argument must be list, not {listA.type}"
            ))
        if not isinstance(listB, List):
            return RTresult().failure(RunTimeERROR(
                self.pos_start, self.pos_end,
                f"First argument must be list, not {listB.type}"
            ))
        listA.elements.extend(listB.elements)
        return RTresult().success(Number.null)
    execute_extend.arg_names = ['listA', 'listB']
    def execute_abs(self, args):
        res = RTresult()
        if len(args) != 1:
            return res.failure(RunTimeERROR(
                self.pos_start, self.pos_end,
                f"func abs() excpected 1 argument got {len(args)}", self.context
            ))
        arg = args[0]
        if isinstance(arg, Number):
            return res.success(Number(abs(arg.value)).set_context(self.context))
        else:
            return res.failure(RunTimeERROR(
                self.pos_start, self.pos_end,
                "Argument to abs() must be a number", self.context
            ))
    def execute_sqrt(self, args):
        res = RTresult()
        
        if len(args) != 1:
            return res.failure(RunTimeERROR(
                self.pos_start, self.pos_end,
                f"sqrt() expected 1 argument, got {len(args)}", self.context
            ))
        
        arg = args[0]
        if not isinstance(arg, Number):
            return res.failure(RunTimeERROR(
                self.pos_start, self.pos_end,
                "Argument to sqrt() must be a number", self.context
            ))
        

        if arg.value < 0:
            return res.failure(RunTimeERROR(
                self.pos_start, self.pos_end,
                "Cannot compute sqrt of a negative number", self.context
            ))
        
        result = Number(math.sqrt(arg.value)).set_context(self.context)
        return res.success(result)
    def execute_len(self, args):
        res = RTresult()
        if len(args) != 1:
            return res.error(RunTimeERROR(
                self.pos_start, self.pos_end,
            f" len() expected exactly 1 argument, got {len(args)}",
            self.context
            ))
        arg = args[0]
        if isinstance(arg, String):
            return res.success(Number(len(arg.value)).set_context(self.context))
        elif isinstance(arg, List):
            return res.success(Number(len(arg.elements)).set_context(self.context))
        else: return res.failure(RunTimeERROR(self.pos_start, self.pos_end,
            "Argument to len() must be a string or list",
            self.context
            ))
    def execute_random(self,args):
        res = RTresult()
        if len(args) != 2:
            return res.failure(RunTimeERROR(
                self.pos_start, self.pos_end,
                f"random() takes exactly 2 arguments, got {len(args)}", self.context
            ))
        start_num = args[0]
        end_num = args[1]
        if isinstance(start_num, Number) and isinstance(end_num, Number):
            return res.success(Number(random.randint(start_num.value, end_num.value)).set_context(self.context))
        else:
            return res.failure(RunTimeERROR(
                self.pos_start, self.pos_end, 
                "Both arguments to random() must be numbers", self.context
            ))
    def __repr__(self):
        return f"<built-in function {self.name}"

#############
# CONTEXT
#############
class Context:
    def __init__(self, display_name, parent=None, parent_entry_pos=None):
        self.display_name = display_name    
        self.parent = parent
        self.parent_entry_pos = parent_entry_pos
        self.symbol_table = None
#############
# SYMBOL TABLE
#############
class SymbolTable:
    def __init__(self, parent=None):
        self.symbols = {}
        self.parent = parent
    def get(self, name):
        value = self.symbols.get(name, None)
        if value == None and self.parent:
            self.parent.get(name)
        return value
    def set(self, name, value):
        self.symbols[name] = value
    def remove(self, name):
        del self.symbols[name]
#############
# INTERPRETER
#############
class Interpreter:
    def __init__(self):
        pass
    def visit(self,node, context):
        method_name = f'visit_{type(node).__name__}'
        method = getattr(self, method_name, self.no_visit_method)
        return method(node, context)  
    def no_visit_method(self, node, context):
        raise Exception(f"No visit_{type(node).__name__} method defined")
    def visit_StringNode(self, node, context):
        return RTresult().success(
            String(node.tok.value).set_context(context).set_pos(node.pos_start, node.pos_end)
        )
    def visit_ListNode(self, node, context):
        res = RTresult()
        elements = []
        for element_node in node.element_nodes:
            elements.append(res.register(self.visit(element_node,context)))
            if res.error: return res
        return res.success(
            List(elements).set_context(context).set_pos(node.pos_start, node.pos_end)
        )
    def visit_IfNode(self, node, context):
        res = RTresult()
        for condition, expr in node.cases:
            condition_value = res.register(self.visit(condition,context))
            if res.error: return res
            if condition_value.is_true():
                expr_value = res.register(self.visit(expr, context))
                if res.error: return res
                return res.success(expr_value)
        if node.else_case:
            else_value = res.register(self.visit(node.else_case, context))
            if res.error: return res
            return res.success(else_value)
        return res.success(None)
    def visit_ForNode(self, node, context):
        res = RTresult()
        elements = []
        start_value = res.register(self.visit(node.start_value_node, context))
        if res.error: return res
        end_value = res.register(self.visit(node.end_value_node, context))
        if res.error: return res
        if node.step_value_node:
            step_value = res.register(self.visit(node.step_value_node, context))
            if res.error: return res
        else:
            step_value = Number(1)
        i = start_value.value
        if step_value.value >= 0:
            condition = lambda: i < end_value.value
        else:
            condition = lambda: i > end_value.value
        while condition():
            context.symbol_table.set(node.var_name_tok.value, Number(i))
            i += step_value.value
            elements.append(res.register(self.visit(node.body_node, context)))
            if res.error: return res
        return res.success(
            List(elements).set_context(context).set_pos(node.pos_start, node.pos_end)
        )
    def visit_WhileNode(self, node, context):
        res = RTresult()
        elements = []
        while True:
            condition = res.register(self.visit(node.condition_node, context))
            if res.error: return res
            if not condition.is_true(): break
            elements.append(res.register(self.visit(node.body_node, context)))
            if res.error: return res
        return res.success(
            List(elements).set_context(context).set_pos(node.pos_start, node.pos_end)
        )
    def visit_VarAccessNode(self, node, context):
        res = RTresult()
        var_name = node.var_name_tok.value
        value = context.symbol_table.get(var_name)
        if not value: 
            return res.failure(RunTimeERROR(
                node.pos_start, node.pos_end,
                f"'{var_name}' is not defined", context
            ))
        value = value.copy().set_pos(node.pos_start, node.pos_end).set_context(context)
        return res.success(value)
    def visit_VarAssignNode(self, node, context):
        res = RTresult()
        var_name = node.var_name_tok.value
        value = res.register(self.visit(node.value_node, context))
        if res.error: return res
        context.symbol_table.set(var_name, value)
        return res.success(value)
    def visit_NumberNode(self,node, context):
        return RTresult().success(
            Number(node.tok.value).set_context(context).set_pos(node.pos_start, node.pos_end)
        )
    def visit_BinOpNode(self,node, context):
        res = RTresult()
        left = res.register(self.visit(node.left_node, context))
        if res.error: return res
        right = res.register(self.visit(node.right_node, context))
        if res.error: return res
        if node.op_tok.type == TT_PLUS:
            result, error = left.added_to(right)
        elif node.op_tok.type == TT_MINUS:
            result, error = left.subbed_by(right)
        elif node.op_tok.type == TT_MUL:
            result, error = left.multed_by(right)
        elif node.op_tok.type == TT_DIV:
            result, error = left.divided_by(right)
        elif node.op_tok.type == TT_POW:
            result, error = left.powed_by(right)
        if node.op_tok.type == TT_EE:
            result, error = left.get_comparison_eq(right)
        elif node.op_tok.type == TT_NE:
            result, error = left.get_comparison_ne(right)
        elif node.op_tok.type == TT_GT:
            result, error = left.get_comparison_gt(right)
        elif node.op_tok.type == TT_LT:
            result, error = left.get_comparison_lt(right)
        elif node.op_tok.type == TT_GTE:
            result, error = left.get_comparison_gte(right)
        elif node.op_tok.type == TT_LTE:
            result, error = left.get_comparison_lte(right)
        elif node.op_tok.matches(TT_KEYWORD, 'AND'):
            result, error = left.if_and(right)
        elif node.op_tok.matches(TT_KEYWORD, 'OR'):
            result, error = left.if_or(right)
        if error: return res.failure(error)
        else:
            return res.success(result.set_pos(node.pos_start, node.pos_end))
    def visit_UnaryOpNode(self,node, context):
        res = RTresult()
        number = res.register(self.visit(node.node, context))
        if res.error: return res
        error = None
        if node.op_tok.type == TT_MINUS:
            number, error = number.multed_by(Number(-1))
        elif node.op_tok.matches(TT_KEYWORD, 'NOT'):
            number, error = number.if_not()
        if error: return res.failure(error)
        return res.success(number.set_pos(node.pos_start, node.pos_end))
    def visit_FuncDefNode(self, node, context):
        res = RTresult()
        func_name = node.var_name_tok.value if node.var_name_tok else None
        body_node = node.body_node
        arg_names = [arg_name.value for arg_name in node.arg_name_toks]
        func_value = Function(func_name, body_node, arg_names).set_context(context).set_pos(node.pos_start, node.pos_end)
        if node.var_name_tok:
            context.symbol_table.set(func_name, func_value)
        return res.success(func_value)
    def visit_CallNode(self, node, context):
        res = RTresult()
        args = []
        value_to_call = res.register(self.visit(node.node_to_call, context))
        if res.error: return res
        value_to_call = value_to_call.copy().set_pos(node.pos_start, node.pos_end)
        for arg_node in node.arg_nodes:
            args.append(res.register(self.visit(arg_node, context)))
            if res.error: return res
        return_value = res.register(value_to_call.execute(args))
        if res.error: return res
        return res.success(return_value)
#FAJNE RZECZY :)
#############
# WBUDOWANE ZMIENNE
#############
global_symbol_table = SymbolTable()
global_symbol_table.set("NULL", Number.null)
global_symbol_table.set("TRUE", Number.true)
global_symbol_table.set("FALSE", Number.false)
global_symbol_table.set("MATH_PI", Number(math.pi))
############
# WBUDOWANE FUNKCJE
############
global_symbol_table.set("sqrt", BuiltInFunc("sqrt"))
global_symbol_table.set("abs", BuiltInFunc("abs"))
global_symbol_table.set("len", BuiltInFunc("len"))
global_symbol_table.set("random", BuiltInFunc("random"))
global_symbol_table.set("printf", BuiltInFunc("printf"))
global_symbol_table.set("print_ret", BuiltInFunc("print_ret"))
global_symbol_table.set("input", BuiltInFunc("input"))
global_symbol_table.set("input_int", BuiltInFunc("input_int"))
global_symbol_table.set("clear", BuiltInFunc("clear"))
global_symbol_table.set("is_number", BuiltInFunc("is_number"))
global_symbol_table.set("is_string", BuiltInFunc("is_string"))
global_symbol_table.set("is_list", BuiltInFunc("is_list"))
global_symbol_table.set("is_function", BuiltInFunc("is_function"))
global_symbol_table.set("append", BuiltInFunc("append"))
global_symbol_table.set("pop", BuiltInFunc("pop"))
global_symbol_table.set("extend", BuiltInFunc("extend"))
def run(fn, text):
    lexer = Lexer(fn, text)
    tokens , error = lexer.make_tokens()
    if error: return None, error
    parser = Parser(tokens)
    ast = parser.parse()
    if ast.error: return None, ast.error
    interpreter = Interpreter()
    context = Context('<program>')
    context.symbol_table = global_symbol_table
    result = interpreter.visit(ast.node, context)
    return result.value, result.error