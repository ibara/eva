import std.stdio;
import std.file;
import std.conv;
import std.exception;
import std.uni;
import std.range;

/+
 + Globals.
 +/

enum {
    TOK_DO = 1,
    TOK_IF,
    TOK_IS,
    TOK_END,
    TOK_SUB,
    TOK_VAR,
    TOK_CALL,
    TOK_ELSE,
    TOK_GOTO,
    TOK_THEN,
    TOK_CONST,
    TOK_IDENT,
    TOK_LABEL,
    TOK_WHILE,
    TOK_ASSIGN,
    TOK_EXTERN,
    TOK_NUMBER,
    TOK_STRING,
    TOK_INTEGER,
    TOK_LITERAL
}

struct token {
    int type;
    string tokstr;
};

token[] tokens;

string source, tokstr;
size_t i, ind, line;
int depth, sub, type;

/+
 + Misc. functions.
 +/

private void error(string msg)
{
    writeln("evac: error: " ~ to!string(line) ~ ": " ~ msg);
    enforce(0);
}

/+
 + Lexer.
 +/

private void
comment()
{
    int ch;

    while ((ch = source[i++]) != '}') {
        if (ch == '\0')
            error("unterminated comment");

        if (ch == '\n')
            ++line;
    }
}

private int
ident()
{
    size_t j = i;

    while (isAlpha(source[i]) || source[i] == '_')
        ++i;

    tokstr = source[j .. i];

    switch (tokstr) {
    case "const":
        return TOK_CONST;
    case "var":
        return TOK_VAR;
    case "extern":
        return TOK_EXTERN;
    case "sub":
        return TOK_SUB;
    case "is":
        return TOK_IS;
    case "call":
        return TOK_CALL;
    case "end":
        return TOK_END;
    case "if":
        return TOK_IF;
    case "then":
        return TOK_THEN;
    case "else":
        return TOK_ELSE;
    case "while":
        return TOK_WHILE;
    case "do":
        return TOK_DO;
    case "label":
        return TOK_LABEL;
    case "goto":
        return TOK_GOTO;
    case "integer":
        return TOK_INTEGER;
    case "string":
        return TOK_STRING;
    default:
        return TOK_IDENT;
    }
}

private int
number()
{
    size_t j = i;

    while (isNumber(source[i]))
        ++i;

    tokstr = source[j .. i];

    enforce(to!int(tokstr) >= int.min && to!int(tokstr) <= int.max);

    return TOK_NUMBER;
}

private int
get_string()
{
    size_t j = i;

    while (source[++i] != '"') {
        if (source[i] == '\n' || source[i] == '\0')
            error("unterminated string");
        if (source[i] == '\\') {
            ++i;
            if (source[i] == '\n' || source[i] == '\0')
                error("unterminated string");
            if (source[i] == '"' || source[i] == '\\') {}
        }
    }

    tokstr = source[j .. ++i];

    return TOK_LITERAL;
}

private int lex()
{
again:
    while (source[i] == ' ' || source[i] == '\t' || source[i] == '\n') {
        if (source[i++] == '\n')
            ++line;
    }

    if (isAlpha(source[i]) || source[i] == '_')
        return ident();

    if (isNumber(source[i]))
        return number();

    switch (source[i]) {
    case '{':
        comment();
        goto again;
    case '=':
    case ',':
    case ';':
    case '#':
    case '+':
    case '-':
    case '*':
    case '/':
    case '(':
    case ')':
    case '[':
    case ']':
    case '<':
    case '>':
        tokstr = source[i .. i + 1];
        return source[i++];
    case ':':
        if (source[++i] == '=') {
            ++i;
            tokstr = source[i - 2 .. i];

            return TOK_ASSIGN;
        }

        tokstr = source[i - 1 .. i];

        return source[i - 1];
    case '"':
        return get_string();
    case '\0':
        tokstr = null;
        return 0;
    default:
        error("unknown token: '" ~ source[i] ~ "'");
    }

    assert(0);
}

private void
expect(int match)
{
    if (ind == tokens.length)
        error("incomplete statement");

    if (match != tokens[ind].type)
        error("syntax error at: '" ~ to!string(tokens[ind].tokstr) ~ "' got: " ~ to!string(tokens[ind].type) ~ " expected: " ~ to!string(match));

    if (ind + 1 == tokens.length) {
	if (tokens[ind].type != ';' && tokens[ind].type != TOK_IS &&
            tokens[ind].type != TOK_END && tokens[ind].type != TOK_THEN &&
            tokens[ind].type != TOK_ELSE && tokens[ind].type != TOK_DO &&
            tokens[ind].type != 0) {
            error("incomplete statement");
        }
    }

    ++ind;
}

private void get_statement()
{
    type = -1;
    tokens = null;
    while (type != ';' && type != TOK_IS && type != TOK_END &&
           type != TOK_THEN && type != TOK_ELSE && type != TOK_DO &&
           type != 0) {
        type = lex();
        tokens ~= token(type, dup(tokstr));
    }
}

private void complete_statement()
{
    if (ind != tokens.length)
        error("extra tokens at end of statement");
    ind = 0;
}

/+
 + Code generator.
 +/

/+
private void aout(string msg)
{
    writeln(msg);
}

private void cg_init()
{
    aout("#include <limits.h>");
    aout("#include <stdio.h>");
    aout("#include <stdlib.h>");
    aout("#include <string.h>");
}

private void cg_write(T...)(T t)
{
    foreach (a; t) {
        switch (a.type)
        case TOK_INTEGER:
            aout("printf(\"%d\", t.tokstr);");
            break;
        case TOK_CHAR:
            aout("printf(\"%c\", t.tokstr);");
            break;
        case TOK_STRING:
            aout("printf(\"%s\", t.tokstr);");
            break;
        default:
            error("invalid write parameter: " ~ a.tokstr);
    }
}

private void cg_writeln(T...)(T t)
{
    foreach (a; t)
        cg_write(a);

    aout("printf(\"\\n\");");
}
+/

/+
 + Parser.
 +/

private void factor()
{
    switch(tokens[ind].type) {
    case TOK_IDENT:
        expect(TOK_IDENT);
        break;
    case TOK_NUMBER:
        expect(TOK_NUMBER);
        break;
    case '(':
        expect('(');
        expression();
        expect(')');
        break;
    default:
        error("invalid factor: " ~ tokens[ind].tokstr);
    }
}

private void term()
{
    factor();

    while (tokens[ind].type == '*' || tokens[ind].type == '/' ||
           tokens[ind].type == '%') {
        //cg_symbol
        if (tokens[ind].type == '*')
            expect('*');
        else if (tokens[ind].type == '/')
            expect('/');
        else
            expect('%');

        factor();
    }
}

private void expression()
{
    if (tokens[ind].type == '+') {
        //cg_symbol
        expect('+');
    } else if (tokens[ind].type == '-') {
        //cg_symbol
        expect('-');
    }

    term();

    while (tokens[ind].type == '+' || tokens[ind].type == '-') {
        //cg_symbol
        if (tokens[ind].type == '+')
            expect('+');
        else
            expect('-');

        term();
    }
}

private void comparator()
{
    switch (tokens[ind].type) {
    case '=':
        expect('=');
        break;
    case '#':
        expect('#');
        break;
    case '<':
        expect('<');
        break;
    case '>':
        expect('>');
        break;
    default:
        error("invalid comparator: " ~ tokens[ind].tokstr);
    }
}

private bool is_comparator()
{
    if (tokens[ind].type == '=' || tokens[ind].type == '#' ||
        tokens[ind].type == '<' || tokens[ind].type == '>') {
        return true;
    }

    return false;
}

private void condition()
{
    expression();
    if (is_comparator()) {
        comparator();
        expression();
    }
}

private bool in_statement()
{
    if (tokens[ind].type == TOK_IDENT || tokens[ind].type == TOK_CALL ||
        tokens[ind].type == TOK_IF || tokens[ind].type == TOK_WHILE ||
        tokens[ind].type == TOK_NUMBER || tokens[ind].type == ';') {
        return true;
    }

    return false;
}

private void statement()
{
    switch (tokens[ind].type) {
    case TOK_IDENT:
        expect(TOK_IDENT);
        if (tokens[ind].type == '[') {
            expect('[');
            expression();
            expect(']');
        }
        expect(TOK_ASSIGN);
        if (tokens[ind].type == TOK_CALL) {
            goto call;
        } else {
            expression();
        }
        break;
        expect(';');
        complete_statement();
        get_statement();
        break;
    case TOK_CALL:
call:
        expect(TOK_CALL);
        expect(TOK_IDENT);
        expect('(');
        if (tokens[ind].type == TOK_IDENT || tokens[ind].type == TOK_NUMBER ||
            tokens[ind].type == '+' || tokens[ind].type == '-' ||
            tokens[ind].type == '*' || tokens[ind].type == '/' ||
            tokens[ind].type == '(') {
            expression();
            while (tokens[ind].type == ',') {
                expect(',');
                expression();
            }
        }
        expect(')');
        expect(';');
        complete_statement();
        get_statement();
        break;
    case TOK_NUMBER:
        expect(TOK_NUMBER);
        expect(':');
        statement();
        break;
    case TOK_GOTO:
        expect(TOK_GOTO);
        expect(TOK_NUMBER);
        expect(';');
        complete_statement();
        get_statement();
        break;
    case TOK_IF:
        expect(TOK_IF);
        condition();
        expect(TOK_THEN);
        complete_statement();
        get_statement();
        statement();
        while (in_statement())
            statement();
        if (tokens[ind].type == TOK_ELSE) {
            expect(TOK_ELSE);
            complete_statement();
            get_statement();
            statement();
            while (in_statement())
                statement();
        }
        expect(TOK_END);
        complete_statement();
        get_statement();
        expect(TOK_IF);
        expect(';');
        complete_statement();
        get_statement();
        break;
    case TOK_WHILE:
        expect(TOK_WHILE);
        condition();
        expect(TOK_DO);
        complete_statement();
        get_statement();
        statement();
        while (in_statement())
            statement();
        expect(TOK_END);
        complete_statement();
        get_statement();
        expect(TOK_WHILE);
        expect(';');
        complete_statement();
        get_statement();
        break;
    default:
        /+ Empty statement.  +/
        expect(';');
        complete_statement();
        get_statement();
        break;
    }
}

private void block()
{
    get_statement();

    if (depth++ > 1)
        error("cannot have nested procedures");

    if (tokens[ind].type == TOK_LABEL) {
        expect(TOK_LABEL);
        if (tokens[ind].type == TOK_NUMBER) {
        }
        expect(TOK_NUMBER);
        while (tokens[ind].type == ',') {
            expect(',');
            if (tokens[ind].type == TOK_NUMBER) {
            }
            expect(TOK_NUMBER);
        }
        expect(';');
        complete_statement();
        get_statement();
    }

    if (tokens[ind].type == TOK_CONST) {
        expect(TOK_CONST);
        if (tokens[ind].type == TOK_IDENT) {
            //addsymbol
            //cg_const
        }
        expect(TOK_IDENT);
        expect(TOK_ASSIGN);
        if (tokens[ind].type == TOK_NUMBER) {
            //cg_symbol
            //cg_semicolon
        }
        if (tokens[ind].type == TOK_NUMBER)
            expect(TOK_NUMBER);
        else if (tokens[ind].type == TOK_LITERAL)
            expect(TOK_LITERAL);
        else
            error("constants must be numbers of string literals");
        while (tokens[ind].type == ',') {
            expect(',');
            if (tokens[ind].type == TOK_IDENT) {
                //addsymbol
                //cg_const
            }
            expect(TOK_IDENT);
            expect('=');
            if (tokens[ind].type == TOK_NUMBER) {
                //cg_symbol
                //cg_semicolon
            }
            expect(TOK_NUMBER);
        }
        expect(';');
        complete_statement();
        get_statement();
    }

    if (tokens[ind].type == TOK_VAR) {
        expect(TOK_VAR);
vardecl:
        if (tokens[ind].type == TOK_IDENT) {
            //addsymbol
            //cg_var
        }
        expect(TOK_IDENT);
        if (tokens[ind].type == '[') {
            expect('[');
            expression();
            expect(']');
        }
        while (tokens[ind].type == ',') {
            expect(',');
            if (tokens[ind].type == TOK_IDENT) {
                //addsymbol
                //cg_var
            }
            expect(TOK_IDENT);
            if (tokens[ind].type == '[') {
                expect('[');
                expression();
                expect(']');
            }
        }
        expect(':');
        if (tokens[ind].type == TOK_INTEGER) {
            expect(TOK_INTEGER);
        } else if (tokens[ind].type == TOK_STRING) {
            expect(TOK_STRING);
        } else {
            error("invalid type: " ~ tokens[ind].tokstr);
        }
        expect(';');
        complete_statement();
        get_statement();
        if (tokens[ind].type != TOK_SUB && tokens[ind].type != TOK_NUMBER) {
            foreach (i, token; tokens) {
                if (token.type == ':')
                    goto vardecl;
            }
        }
        //cg_crlf
    }

    while (tokens[ind].type == TOK_EXTERN) {
        expect(TOK_EXTERN);
        if (tokens[ind].type == TOK_IDENT) {
            //addsymbol
            //cg_forward
        }
        expect(TOK_IDENT);
        expect(';');
        complete_statement();
        get_statement();
    }

    while (tokens[ind].type == TOK_SUB) {
        sub = 1;
        expect(TOK_SUB);
        if (type == TOK_IDENT) {
            //addsymbol
            //cg_sub
        }
        expect(TOK_IDENT);
        expect('(');
        if (tokens[ind].type == TOK_IDENT) {
varlist:
            expect(TOK_IDENT);
            if (tokens[ind].type == '[') {
                expect('[');
                expect(']');
            }
            expect(':');
            if (tokens[ind].type == TOK_INTEGER) {
                expect(TOK_INTEGER);
            } else if (tokens[ind].type == TOK_STRING) {
                expect(TOK_STRING);
            } else {
                error("invalid type: " ~ tokens[ind].tokstr);
            }
            while (tokens[ind].type == ',') {
                expect(',');
                goto varlist;
            }
        }
        expect(')');
        if (tokens[ind].type == ':') {
            expect(':');
            if (tokens[ind].type == TOK_INTEGER) {
                expect(TOK_INTEGER);
            } else if (tokens[ind].type == TOK_STRING) {
                expect(TOK_STRING);
            } else {
                error("invalid type: " ~ tokens[ind].tokstr);
            }
        }
        expect(TOK_IS);
        complete_statement();
        block();
        expect(TOK_END);
        complete_statement();
        get_statement();
        expect(TOK_SUB);
        expect(';');
        complete_statement();
        get_statement();
        sub = 0;
        //destroysymbols
    }

    if (sub == 0) {
        if (type == 0)
            goto done;
        //cg_procedure
    }

    statement();
    while (in_statement()) {
        if (type == -1)
            break;
        statement();
    }

    //cg_epilogue

done:
    if (--depth < 0)
        error("depth underflow");
}

private int program()
{
    //cg_init();
    block();

    if (lex() != 0)
        error("extra tokens at EOF");

    return 0;
}

/+
 + Driver.
 +/

int main(string[] args)
{
    if (args.length != 2) {
        stderr.writeln("usage: evac file.eva");
        return 1;
    }

    source = cast(string)read(args[1]);
    source ~= '\0';

    line = 1;

    return program();
}
