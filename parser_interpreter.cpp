#include <chrono>
#include <Tokenizer.h>

u32 StringHash(const char* str , u32 c) {
    u32 hash = 7;
    for(u32 i = 0; i < c ; i++) {
        hash = hash * 31 + str[i];
    }
    return hash;
}

template<typename T> T min(T t0 , T t1) {
    return t0 > t1 ? t1 : t0;
}
template<typename T> T max(T t0 , T t1) {
    return t0 < t1 ? t1 : t0;
}

enum ValueTypes { 
    VALUE_NULL_LITERAL = TOKEN_NULL_LITERAL,
    VALUE_STRING_LITERAL = TOKEN_STRING_LITERAL,
    VALUE_BOOL_LITERAL = TOKEN_BOOL_LITERAL,
    VALUE_NUMBER_LITERAL = TOKEN_NUMBER_LITERAL,

    VALUE_VARIABLE,
    VALUE_NATIVE_FUNCTION,
    VALUE_NON_NATIVE_FUNCTION,
};

struct Value {
    ValueTypes type;
    byte mem[8]{0};
};
template<typename T> T& Cast(void* mem) {
    return *((T*)mem);
}
enum ExprType {
    EXPRESSION_NULL,

    EXPRESSION_BINARY,
    EXPRESSION_UNARY,
    EXPRESSION_LITERAL,
    EXPRESSION_GROUPING,
    EXPRESSION_VARIABLE,
    EXPRESSION_ASSIGNMENT,
    EXPRESSION_CALL,

    EXPRESSION_COUNT,
};

void PrintValue(Value v) {
    switch (v.type) {
        case TOKEN_STRING_LITERAL:
        
            PrintToken( *Cast<Token*>(v.mem) );
            break;
        case TOKEN_BOOL_LITERAL:
            std::cout << (Cast<bool>(v.mem) ? "true" : "false");
            break;
        case TOKEN_NUMBER_LITERAL:
            std::cout << Cast<double>(v.mem);
            break;
        case TOKEN_NULL_LITERAL:
            std::cout << "null";
            break;
        
        default:
            break;
    }

    std::cout << std::endl;
}

struct Expr {
    u32 index;
};
struct BinaryExpr : Expr {
    Token opr;
    Expr left;
    Expr right;
};
struct UnaryExpr : Expr {
    Token opr;
    Expr right;
};
struct LiteralExpr : Expr {
    Token literal;
};
struct GroupingExpr : Expr {
    Expr expr;
};
struct VariableExpr : Expr {
    Token name;
};
struct AssignExpr : Expr {
    Expr value;
    Token name;
};
struct CallExpr : Expr {
    Expr callee;
    Expr args;
};


enum StatementType {
    STATEMENT_NULL,

    STATEMENT_PRINT,
    STATEMENT_EXPRESSION,
    STATEMENT_VAR_DECLARATION,
    STATEMENT_FN_DECLARATION,
    STATEMENT_SCOPE_ENTER,
    STATEMENT_SCOPE_EXIT,
    STATEMENT_IF,
    STATEMENT_JMP,
    STATEMENT_VAR_PUSH,
    STATEMENT_VAR_POP,

    STATEMENT_COUNT,
};

struct Stmt {
    u32 index;
};
struct ExprStmt : Stmt {
    Expr expr;
};
struct PrintStmt : Stmt {
    Expr expr;
};
struct ScopeStmt : Stmt {
    Stmt statements;
};
struct DeclVarStmt : Stmt {
    Token name;
    Expr expr;
};
struct DeclFnStmt : Stmt {
    Token name;
    u32 paramIndex;
    Stmt body;
};
struct JmpStmt : Stmt {
    Stmt Target;
};
struct BranchStmt : Stmt {
    Expr condition;
    Stmt thenBranch;
    Stmt elseBranch;
};
struct DeclFnStmtReversed {
    Token name;
    Stmt body;
    u32 paramIndex;
    StatementType type;
};
struct StackVarStmt : Stmt {
    Token name;
};

struct Parser {
    Tokenizer tokenizer;
    DynamicBufferSimple<Token> tokenBuffer;
    char* source = nullptr;

    byte* mem = nullptr;
    u32 memCap = 512 * KILO_BYTES;
    u32 exprAllocator = 64;
    u32 stmtAllocator = 256 * KILO_BYTES;
};


typedef Value(*native_function_t)(Value* args);



struct InterpreterState {
    HashTable<u32,Value> symbolTable;
    byte* stack = nullptr;
    byte* stackPtr;
    byte* returnAddress;
    Stmt program;
};

template<typename T> void Push(InterpreterState* interpreter, T t) {
    Cast<T>(interpreter->stackPtr) = t;
    interpreter->stackPtr += sizeof(T);
}
template<typename T> T Pop(InterpreterState* interpreter) {
    interpreter->stackPtr -= sizeof(T);
    return Cast<T>(interpreter->stackPtr);
}

Token PreviousToken(Parser* parser) {
    return parser->tokenBuffer.mem[parser->tokenBuffer.size - 1];
}
Token NextToken(Parser* parser) {
    u32 i = parser->tokenBuffer.PushBack( GetToken(&parser->tokenizer) );
    return parser->tokenBuffer[i];
}

bool Check(Parser* parser , TokenType type) {
    if(!parser->tokenizer.at[0]) return false;

    Tokenizer peek = parser->tokenizer;
    Token nextToken = GetToken(&peek);
    return (nextToken.type == type);
}

bool Match(Parser* parser , TokenType* types , u32 count) {
    for(u32 i = 0; i < count ;i++) {
        if(Check(parser , types[i])) {
            NextToken(parser);
            return true;
        }
    }
    return false;
}

void ExpectToken(Parser* parser , TokenType e) {

    Token token = NextToken(parser);

    if(token.type != e) {
        std::cout << "ERROR: exptected : ";
        PrintTokenType(e);
        std::cout << " got : ";
        PrintToken(token);
        std::cout << " " << token.lenght << std::endl;
    }
}



Expr Expression(Parser* parser);
Expr Primary(Parser* parser) {

    TokenType m[4]{TOKEN_BOOL_LITERAL , TOKEN_NULL_LITERAL , TOKEN_NUMBER_LITERAL , TOKEN_STRING_LITERAL };
    TokenType identifier = TOKEN_IDENTIFIER;
    TokenType openParen = TOKEN_OPEN_PAREN;

    if(Match(parser , &openParen, 1)) {
        Expr expr = Expression(parser);

        ExpectToken(parser , TOKEN_CLOSE_PAREN);

        u32 index = parser->exprAllocator;
        parser->exprAllocator += sizeof(GroupingExpr);
        ((GroupingExpr*)(parser->mem + index))->index = EXPRESSION_GROUPING;
        ((GroupingExpr*)(parser->mem + index))->expr = expr;

        return Expr{index};
    }
    else if(Match(parser , m , 4)) {
        u32 index = parser->exprAllocator;
        parser->exprAllocator += sizeof(LiteralExpr);
        ((LiteralExpr*)(parser->mem + index))->index = EXPRESSION_LITERAL;
        ((LiteralExpr*)(parser->mem + index))->literal = PreviousToken(parser);
        return Expr{index};
    }
    else if(Match(parser , &identifier, 1)) {
        u32 index = parser->exprAllocator;
        parser->exprAllocator += sizeof(VariableExpr);
        VariableExpr* var = &Cast<VariableExpr>(parser->mem + index);
        var->index = EXPRESSION_VARIABLE;
        var->name = PreviousToken(parser);

        return Expr{index};
    }

    ASSERT(false);

    return Expr{0};
}

Expr RestOfCall(Parser* parser, Expr callee) {

    u32 index = parser->exprAllocator;
    CallExpr& call = Cast<CallExpr>(parser->mem + index);
    parser->exprAllocator += sizeof(CallExpr);

    call.index = EXPRESSION_CALL;
    call.callee = callee;

    u32 argCount = 0;
    Tokenizer peek = parser->tokenizer;
    if(!Check(parser , TOKEN_CLOSE_PAREN)) {
        do {
            argCount++;
            GetToken(&peek);
        } while(GetToken(&peek).type == TOKEN_COMMA);
    }

    Expr args{parser->exprAllocator};
    parser->exprAllocator += sizeof(Expr) * argCount;
    Cast<Expr>(parser->mem + parser->exprAllocator).index = EXPRESSION_NULL;
    parser->exprAllocator += sizeof(Expr);

    
    u32 i = args.index;
    if(!Check(parser , TOKEN_CLOSE_PAREN)) {
        do {
            Cast<Expr>(parser->mem + i) = Expression(parser);
            i += sizeof(Expr);
        } while(NextToken(parser).type == TOKEN_COMMA);

        TokenType t = parser->tokenBuffer.Back().type;
        if( t != TOKEN_CLOSE_PAREN) {
            std::cout << "ERROR: expected ) at line: " << parser->tokenizer.line << std::endl;
        }
    }
    else {
        NextToken(parser);
    }

    call.args = args;
    return Expr{index};
}
Expr Call(Parser* parser) {

    Expr expr = Primary(parser);

    for(;;) {
        
        if(Check(parser,TOKEN_OPEN_PAREN)) {
            NextToken(parser);
            expr = RestOfCall(parser, expr);
        }
        else {
            break;
        }
    }
    return expr;
}

Expr Unary(Parser* parser) {

    TokenType tokens[2]{TOKEN_EXLAMATION_MARK,TOKEN_MINUS};
    while(Match(parser , tokens , 2)) {
        Token opr = PreviousToken(parser);
        Expr right = Unary(parser);

        u32 index = parser->exprAllocator;
        parser->exprAllocator += sizeof(UnaryExpr);
        ((UnaryExpr*)(parser->mem + index))->index = EXPRESSION_UNARY;
        ((UnaryExpr*)(parser->mem + index))->opr = opr;
        ((UnaryExpr*)(parser->mem + index))->right = right;

        return Expr{index};
    }

    //return Primary(parser);
    return Call(parser);
}
Expr Factor(Parser* parser) {

    Expr expr = Unary(parser);

    TokenType tokens[2]{TOKEN_ASTERISK , TOKEN_SLASH};
    while(Match(parser, tokens , 2)) {
        Token opr = PreviousToken(parser);
        Expr right = Unary(parser);

        u32 index = parser->exprAllocator;
        parser->exprAllocator += sizeof(BinaryExpr);
        ((BinaryExpr*)(parser->mem + index))->index = EXPRESSION_BINARY;
        ((BinaryExpr*)(parser->mem + index))->left = expr;
        ((BinaryExpr*)(parser->mem + index))->opr = opr;
        ((BinaryExpr*)(parser->mem + index))->right = right;

        expr.index = index;
    }

    return expr;
}
Expr Term(Parser* parser) {
    Expr expr = Factor(parser);

    TokenType tokens[2]{TOKEN_PLUS,TOKEN_MINUS};

    while(Match(parser, tokens , 2)) {
        Token opr = PreviousToken(parser);
        Expr right = Factor(parser);

        u32 index = parser->exprAllocator;
        parser->exprAllocator += sizeof(BinaryExpr);
        ((BinaryExpr*)(parser->mem + index))->index = EXPRESSION_BINARY;
        ((BinaryExpr*)(parser->mem + index))->left = expr;
        ((BinaryExpr*)(parser->mem + index))->opr = opr;
        ((BinaryExpr*)(parser->mem + index))->right = right;

        expr.index = index;
    }

    return expr;
}
Expr Comparison(Parser* parser) {

    Expr expr = Term(parser);

    TokenType tokens[4]{TOKEN_RSHIFT_EQUALS , TOKEN_LSHIFT_EQUALS , TOKEN_LSHIFT , TOKEN_RSHIFT};

    while(Match(parser, tokens , 4)) {
        Token opr = PreviousToken(parser);
        Expr right = Term(parser);

        u32 index = parser->exprAllocator;
        parser->exprAllocator += sizeof(BinaryExpr);
        ((BinaryExpr*)(parser->mem + index))->index = EXPRESSION_BINARY;
        ((BinaryExpr*)(parser->mem + index))->left = expr;
        ((BinaryExpr*)(parser->mem + index))->opr = opr;
        ((BinaryExpr*)(parser->mem + index))->right = right;

        expr.index = index;
    }

    return expr;
}
Expr Equality(Parser* parser) {

    Expr expr = Comparison(parser);

    TokenType tokens[2]{TOKEN_EXCLAMATION_EQUALS,TOKEN_EQUALS_EQUALS};
    while(Match(parser, tokens , 2)) {
        Token opr = PreviousToken(parser);
        Expr right = Comparison(parser);

        u32 index = parser->exprAllocator;
        parser->exprAllocator += sizeof(BinaryExpr);
        ((BinaryExpr*)(parser->mem + index))->index = EXPRESSION_BINARY;
        ((BinaryExpr*)(parser->mem + index))->left = expr;
        ((BinaryExpr*)(parser->mem + index))->opr = opr;
        ((BinaryExpr*)(parser->mem + index))->right = right;

        expr.index = index;
    }

    return expr;
}
Expr Assignment(Parser* parser) {

    Expr expr = Equality(parser);

    TokenType e = TOKEN_EQUAL_SIGN;
    if(Match(parser , &e , 1)) {

        Expr value = Assignment(parser);
        ExprType expresionType = Cast<ExprType>(parser->mem + expr.index);

        if(expresionType == EXPRESSION_VARIABLE) {
            Token name = Cast<VariableExpr>(parser->mem + expr.index).name;

            u32 index = parser->exprAllocator;
            parser->exprAllocator += sizeof(AssignExpr);
            AssignExpr* assign = &Cast<AssignExpr>(parser->mem + index);
            assign->index = EXPRESSION_ASSIGNMENT;
            assign->name = name;
            assign->value = value;

            return Expr{index};
        }

        std::cout << "ERROR: expression must be a modifiable l-value at line: " << parser->tokenizer.line << std::endl;
    }

    return expr;
}
Expr Expression(Parser* parser) {
    return Assignment(parser);
}

Stmt Statement(Parser* parser) {

    bool expect = true;
    Stmt ret{0};

    if(Check(parser,TOKEN_KEYWORD_PRINT )) {

        NextToken(parser);

        u32 index = parser->stmtAllocator;
        PrintStmt* stmt = (PrintStmt*)(parser->mem + index);
        parser->stmtAllocator += sizeof(PrintStmt);

        stmt->index = STATEMENT_PRINT;
        stmt->expr = Expression(parser);

        ret.index = index;
    }
    else if(Check(parser , TOKEN_KEYWORD_VAR)) {
        NextToken(parser);

        u32 index = parser->stmtAllocator;
        DeclVarStmt& declaration = Cast<DeclVarStmt>(parser->mem + index);
        parser->stmtAllocator += sizeof(DeclVarStmt);

        declaration.index = STATEMENT_VAR_DECLARATION;
        ExpectToken(parser , TOKEN_IDENTIFIER);
        declaration.name = parser->tokenBuffer.Back();
        declaration.expr.index = 0;
        if(!Check(parser , TOKEN_SEMICOLON)) {
            ExpectToken(parser , TOKEN_EQUAL_SIGN);
            declaration.expr = Expression(parser);
        }

        ret.index = index;
    } 
    else if(Check(parser , TOKEN_KEYWORD_FN)) {
        NextToken(parser);

        u32 index = parser->stmtAllocator;
        DeclFnStmt& declaration = Cast<DeclFnStmt>(parser->mem + index);
        parser->stmtAllocator += sizeof(DeclFnStmt);


        declaration.index = STATEMENT_FN_DECLARATION;
        ExpectToken(parser , TOKEN_IDENTIFIER);
        declaration.name = parser->tokenBuffer.Back();
        ExpectToken(parser , TOKEN_OPEN_PAREN);

        TokenType match = TOKEN_COMMA;
        declaration.paramIndex = parser->stmtAllocator;
        do {
            Cast<Token>(parser->mem + parser->stmtAllocator) = NextToken(parser);
            parser->stmtAllocator += sizeof(Token);
        } while(Match(parser, &match , 1));

        Token null;
        null.type = TOKEN_EOF;
        Cast<Token>(parser->mem + parser->stmtAllocator) = null;
        parser->stmtAllocator += sizeof(Token);

        u32 jmpIndex = parser->stmtAllocator;
        parser->stmtAllocator += sizeof(JmpStmt);
        Cast<JmpStmt>(parser->mem + jmpIndex).index = STATEMENT_JMP;

        ExpectToken(parser , TOKEN_CLOSE_PAREN);
        declaration.body = Statement(parser);

        expect = (Cast<StatementType>(parser->mem + declaration.body.index) != STATEMENT_SCOPE_ENTER);

        Cast<JmpStmt>(parser->mem + jmpIndex).Target = Stmt{parser->stmtAllocator};

        ret.index = index;
    } 
    else if(Check(parser , TOKEN_OPEN_BRACES)) {
        NextToken(parser);
        expect = false;
        
        u32 index = parser->stmtAllocator;
        ScopeStmt& scope = Cast<ScopeStmt>(parser->mem + index);
        parser->stmtAllocator += sizeof(ScopeStmt);

        scope.index = STATEMENT_SCOPE_ENTER;
        scope.statements = Stmt{parser->stmtAllocator};

        while( Cast<StatementType>(parser->mem + Statement(parser).index) != STATEMENT_SCOPE_EXIT);

        ret.index = index;
    }
    else if(Check(parser , TOKEN_CLOSE_BRACES)) {
        NextToken(parser);
        expect = false;

        u32 index = parser->stmtAllocator;
        Stmt& scopeEnd = Cast<ScopeStmt>(parser->mem + index);
        parser->stmtAllocator += sizeof(ScopeStmt);

        scopeEnd.index = STATEMENT_SCOPE_EXIT;

        ret.index = index;
    }
    else if(Check(parser , TOKEN_KEYWORD_IF)) {

        NextToken(parser);
        u32 index = parser->stmtAllocator;
        BranchStmt& branch = Cast<BranchStmt>(parser->mem + index);
        parser->stmtAllocator += sizeof(BranchStmt);

        ExpectToken(parser , TOKEN_OPEN_PAREN);
        branch.condition = Expression(parser);
        ExpectToken(parser , TOKEN_CLOSE_PAREN);

        branch.index = STATEMENT_IF;
        branch.thenBranch = Statement(parser);
        branch.elseBranch = Stmt{parser->stmtAllocator};

        if(Check(parser , TOKEN_KEYWORD_ELSE)) {
            NextToken(parser);
            
            expect = (Cast<StatementType>(parser->mem + branch.elseBranch.index ) != STATEMENT_SCOPE_ENTER);

            u32 index = parser->stmtAllocator;
            JmpStmt& jmp = Cast<JmpStmt>(parser->mem + index);
            parser->stmtAllocator += sizeof(JmpStmt);

            branch.elseBranch = Statement(parser);

            jmp.index = STATEMENT_JMP;
            jmp.Target = Stmt{parser->stmtAllocator};
        }
        else {
            expect = (Cast<StatementType>(parser->mem + branch.thenBranch.index ) != STATEMENT_SCOPE_ENTER);
        }
        ret.index = index;
    }
    else if(Check(parser , TOKEN_KEYWORD_FOR)) {
        NextToken(parser);
        ExpectToken(parser , TOKEN_OPEN_PAREN);

        u32 index = parser->stmtAllocator;
        ScopeStmt& scopeOuterBegin = Cast<ScopeStmt>(parser->mem + index);
        parser->stmtAllocator += sizeof(ScopeStmt);
        scopeOuterBegin.index = STATEMENT_SCOPE_ENTER;

        Stmt init = Statement(parser);
        StatementType initT = Cast<StatementType>(parser->mem + init.index);
        if( !(initT == STATEMENT_EXPRESSION || initT == STATEMENT_VAR_DECLARATION) ) {
            std::cout << "ERROR: for loop initalizer must be either an expression statement or a declaration at line: " << parser->tokenizer.line << std::endl;
        }

        u32 branchIndex = parser->stmtAllocator;
        BranchStmt& branch = Cast<BranchStmt>(parser->mem + branchIndex);
        parser->stmtAllocator += sizeof(BranchStmt);
        branch.index = STATEMENT_IF;
        branch.condition = Expression(parser);
        ExpectToken(parser, TOKEN_SEMICOLON);
        
        Stmt incSmtm = Stmt{parser->stmtAllocator};
        Cast<ExprStmt>(parser->mem + incSmtm.index).index = STATEMENT_EXPRESSION;
        Cast<ExprStmt>(parser->mem + incSmtm.index).expr = Expression(parser);

        if(Cast<StatementType>(parser->mem + incSmtm.index) != STATEMENT_EXPRESSION) {
            std::cout << "ERROR: for loop increment must be an expression statement at line: " << parser->tokenizer.line << std::endl;
        }
        ExprStmt incStmt = Cast<ExprStmt>(parser->mem + incSmtm.index);

        ExpectToken(parser, TOKEN_CLOSE_PAREN);

        Stmt body;
        if(Check(parser , TOKEN_OPEN_BRACES)) {
            body = Statement(parser);

            expect = false;
        }
        else {
            expect = Check(parser , TOKEN_SEMICOLON);

            ScopeStmt& scopeInnerBegin = Cast<ScopeStmt>(parser->mem + parser->stmtAllocator);
            parser->stmtAllocator += sizeof(ScopeStmt);
            scopeInnerBegin.index = STATEMENT_SCOPE_ENTER;

            body = Statement(parser);

            ScopeStmt& scopeInnerEnd = Cast<ScopeStmt>(parser->mem + parser->stmtAllocator);
            parser->stmtAllocator += sizeof(ScopeStmt);
            scopeInnerEnd.index = STATEMENT_SCOPE_EXIT;
        }
        branch.thenBranch = body;

        Cast<ExprStmt>(parser->mem + parser->stmtAllocator) = incStmt;
        parser->stmtAllocator += sizeof(ExprStmt);
       
        JmpStmt& jmp = Cast<JmpStmt>(parser->mem + parser->stmtAllocator);
        parser->stmtAllocator += sizeof(JmpStmt);
        jmp.index = STATEMENT_JMP;
        jmp.Target = Stmt{branchIndex};

        branch.elseBranch = Stmt{parser->stmtAllocator};
        ScopeStmt& scopeOuterEnd = Cast<ScopeStmt>(parser->mem + parser->stmtAllocator);
        parser->stmtAllocator += sizeof(ScopeStmt);
        scopeOuterEnd.index = STATEMENT_SCOPE_EXIT;
        

        ret.index = index;
    }
    else {
        u32 index = parser->stmtAllocator;
        ExprStmt* exprStmt = (ExprStmt*)(parser->mem + index);
        parser->stmtAllocator += sizeof(ExprStmt);

        exprStmt->index = STATEMENT_EXPRESSION;
        exprStmt->expr = Expression(parser);

        ret.index = index;
    }

    if(expect) {
        ExpectToken(parser , TOKEN_SEMICOLON);
    }

    return ret;
}

Stmt Parse(Parser* parser) {


    Stmt program;
    program.index = parser->stmtAllocator;

    for(;;) {

        Tokenizer peek = parser->tokenizer;
        if( GetToken(&peek).type != TOKEN_EOF ) {
            Statement(parser);
        }
        else {
            break;
        }
    }

    u32 index = parser->stmtAllocator;
    Stmt* end = (Stmt*)(parser->mem + index);
    end->index = STATEMENT_NULL;

    return program;
}


double GetNumber(LiteralExpr l) {    
    char str[l.literal.lenght+1]{0};
    for(u32 i = 0; i < l.literal.lenght ; i++) {
        str[i] = l.literal.text[i];
    }
    return strtod(str,nullptr);
}
bool GetBool(LiteralExpr l) {
    return TokenEquals(l.literal , "true");
}



void UnwindStack(InterpreterState* interpreter) {
    while(interpreter->stackPtr != interpreter->returnAddress ) {

        StatementType type = Cast<StatementType>(interpreter->stackPtr-8);

        switch (type) {
            case STATEMENT_VAR_DECLARATION:
                {
                    DeclVarStmt stmt = Pop<DeclVarStmt>(interpreter);
                    u32 hash = StringHash(stmt.name.text , stmt.name.lenght);
                    interpreter->symbolTable.Delete(hash);
                }
                break;
            case STATEMENT_FN_DECLARATION:
                {
                    DeclFnStmt stmt = Pop<DeclFnStmt>(interpreter);
                    u32 hash = StringHash(stmt.name.text , stmt.name.lenght);
                    interpreter->symbolTable.Delete(hash);
                }
                break;

            default:
                return;
                break;
        }
    }

    byte* returnAddress = Pop<byte*>(interpreter);
    interpreter->returnAddress = returnAddress;
}

void DefineVar(InterpreterState* interpreter, Value v , Token name) {

    u32 hash = StringHash(name.text,name.lenght);
    u32 index = interpreter->symbolTable.Find(hash);
    if(index == ~(u32(0))) {
        interpreter->symbolTable.Insert(hash, v);
    }
}
void UnDefineVar(InterpreterState* interpreter, Token name) {

    u32 hash = StringHash(name.text,name.lenght);
    u32 index = interpreter->symbolTable.Find(hash);
    if(index != ~(u32(0))) {
        interpreter->symbolTable.Delete(hash);
    }
}
Value Evaluate(InterpreterState* interpreter, Parser* parser, Expr expr);

void Execute(InterpreterState* interpreter ,Parser* parser, Stmt program) {

    interpreter->program = program;

    bool running = true;
    while(running) {
        switch ( Cast<StatementType>( parser->mem + interpreter->program.index ) ) {
            case STATEMENT_EXPRESSION:
                {
                    ExprStmt& stmt = Cast<ExprStmt>( parser->mem + interpreter->program.index );
                    Evaluate(interpreter, parser , stmt.expr);
                    interpreter->program.index += sizeof(ExprStmt);
                }
                break;
            case STATEMENT_PRINT:
                {
                    PrintStmt& stmt = Cast<PrintStmt>( parser->mem + interpreter->program.index );
                    Value val = Evaluate(interpreter ,parser , stmt.expr);
                    interpreter->program.index += sizeof(PrintStmt);
                    PrintValue(val);
                }
                break;
            case STATEMENT_VAR_DECLARATION:
                {
                    DeclVarStmt& stmt = Cast<DeclVarStmt>( parser->mem + interpreter->program.index );
                    Value v;
                    Cast<u64>(v.mem) = 0;
                    v.type = VALUE_NULL_LITERAL;
                    if( stmt.expr.index != 0 ) {
                        v = Evaluate(interpreter ,parser , stmt.expr);
                    }
                    u32 hash = StringHash(stmt.name.text , stmt.name.lenght);
                    u32 index = interpreter->symbolTable.Find(hash);
                    if(index != ~(u32(0))) {
                        std::cout << "ERROR: variable redecleration ";
                        PrintToken(stmt.name);
                        std::cout << " at line: " << GetLineNumber(parser->source , stmt.name.text) << std::endl;
                    }
                    else {
                        interpreter->symbolTable.Insert(hash, v );
                        DeclVarStmt s = stmt;
                        s.expr.index = STATEMENT_VAR_DECLARATION;
                        Push<DeclVarStmt>(interpreter, s);
                    }

                    interpreter->program.index += sizeof(DeclVarStmt);
                }
                break;
            case STATEMENT_FN_DECLARATION:
                {
                    DeclFnStmt& decl = Cast<DeclFnStmt>(parser->mem + interpreter->program.index);

                    u32 hash = StringHash(decl.name.text, decl.name.lenght);
                    u32 index = interpreter->symbolTable.Find(hash);
                    if(index != ~(u32(0))) {
                        std::cout << "ERROR: function redecleration ";
                        PrintToken(decl.name);
                        std::cout << " at line: " << parser->tokenizer.line << std::endl;
                    }
                    else {

                        Value function;
                        function.type = VALUE_NON_NATIVE_FUNCTION;
                        Cast<Stmt>(function.mem) = interpreter->program;

                        DeclFnStmtReversed s;
                        s.body = decl.body;
                        s.name = decl.name;
                        s.paramIndex = decl.paramIndex;
                        s.type = STATEMENT_FN_DECLARATION;
                        interpreter->symbolTable.Insert(hash, function  );

                        Push<DeclFnStmtReversed>(interpreter, s);
                    }

                    u32 i = decl.paramIndex;
                    while( Cast<Token>(parser->mem + i).type != TOKEN_EOF ) {
                        i += sizeof(Token);
                    } 
                    i += sizeof(Token);

                    interpreter->program.index = i;
                }
                break;
            case STATEMENT_SCOPE_ENTER:
                {
                    Push<byte*>(interpreter , interpreter->returnAddress);
                    interpreter->returnAddress = interpreter->stackPtr;
                    interpreter->program.index += sizeof(ScopeStmt);
                    Execute(interpreter, parser, interpreter->program );
                }
                break;
            case STATEMENT_SCOPE_EXIT:
                {
                    UnwindStack(interpreter);
                    interpreter->program.index += sizeof(ScopeStmt);
                    return;
                }
                break;
            case STATEMENT_JMP:
                {
                    JmpStmt& jmp = Cast<JmpStmt>(parser->mem + interpreter->program.index);
                    interpreter->program = jmp.Target;
                }
                break;

            case STATEMENT_IF:
                {
                    BranchStmt& branch = Cast<BranchStmt>( parser->mem + interpreter->program.index );
                    Value result = Evaluate(interpreter ,parser, branch.condition );
                    if(result.type == TOKEN_BOOL_LITERAL) {
                        if( Cast<bool>(result.mem) ) {
                            interpreter->program = branch.thenBranch;
                        }
                        else {
                            interpreter->program = branch.elseBranch;
                        }
                    }
                    else {
                        std::cout << "ERROR: conditions must evaluate to boolean expressions at line :" << parser->tokenizer.line << std::endl;
                    }
                }
                break;
            
            case STATEMENT_NULL:
                running = false;
                break;
            
            default:
                break;
        }
    }
}

Value Evaluate(InterpreterState* interpreter, Parser* parser, Expr expr) {

    ExprType type =  *((ExprType*)(parser->mem + expr.index));

    switch(type) {
        case EXPRESSION_BINARY:
            {
                Value l = Evaluate(interpreter, parser ,((BinaryExpr*)(parser->mem + expr.index))->left);
                Value r = Evaluate(interpreter, parser ,((BinaryExpr*)(parser->mem + expr.index))->right);

                Value ret;
                BinaryExpr* bex = ((BinaryExpr*)(parser->mem + expr.index));

                if( bex->opr.type == TOKEN_PLUS ) {
                    ret.type = VALUE_NUMBER_LITERAL;
                    if( l.type != VALUE_NUMBER_LITERAL || r.type != VALUE_NUMBER_LITERAL ) {
                        std::cout << "ERROR: operands for + must be numeric expressions at line : " << parser->tokenizer.line << std::endl;
                    }
                    Cast<double>(ret.mem) = Cast<double>(l.mem) + Cast<double>(r.mem);
                }
                else if( bex->opr.type == TOKEN_MINUS ) {
                    ret.type = VALUE_NUMBER_LITERAL;
                    if( l.type != VALUE_NUMBER_LITERAL || r.type != VALUE_NUMBER_LITERAL ) {
                        std::cout << "ERROR: operands for - must be numeric expressions at line : " << parser->tokenizer.line << std::endl;
                    }
                    Cast<double>(ret.mem) = Cast<double>(l.mem) - Cast<double>(r.mem);
                }
                else if( bex->opr.type == TOKEN_ASTERISK ) {
                    ret.type = VALUE_NUMBER_LITERAL;
                    if( l.type != VALUE_NUMBER_LITERAL || r.type != VALUE_NUMBER_LITERAL ) {
                        std::cout << "ERROR: operands for * must be numeric expressions at line : " << parser->tokenizer.line << std::endl;
                    }
                    Cast<double>(ret.mem) = Cast<double>(l.mem) * Cast<double>(r.mem);
                }
                else if( bex->opr.type == TOKEN_SLASH ) {
                    ret.type = VALUE_NUMBER_LITERAL;
                    if( l.type != VALUE_NUMBER_LITERAL || r.type != VALUE_NUMBER_LITERAL ) {
                        std::cout << "ERROR: operands for / must be numeric expressions at line : " << parser->tokenizer.line << std::endl;
                    }
                    Cast<double>(ret.mem) = Cast<double>(l.mem) / Cast<double>(r.mem);
                }
                else if(bex->opr.type == TOKEN_EQUALS_EQUALS) {
                    ret.type = VALUE_BOOL_LITERAL;
                    if( l.type == VALUE_BOOL_LITERAL && r.type == VALUE_BOOL_LITERAL ) {
                        Cast<bool>(ret.mem) = Cast<bool>(l.mem) == Cast<bool>(r.mem);
                    }
                    else if( l.type == VALUE_NUMBER_LITERAL && r.type == VALUE_NUMBER_LITERAL ) {
                        Cast<bool>(ret.mem) = Cast<double>(l.mem) == Cast<double>(r.mem);
                    }
                    else {
                        std::cout << "ERROR: operands for == must be numeric or boolean expressions at line : " << parser->tokenizer.line << std::endl;
                    }
                }
                else if(bex->opr.type == TOKEN_EXCLAMATION_EQUALS) {
                    ret.type = VALUE_BOOL_LITERAL;
                    if( l.type == VALUE_BOOL_LITERAL && r.type == VALUE_BOOL_LITERAL ) {
                        Cast<bool>(ret.mem) = Cast<bool>(l.mem) != Cast<bool>(r.mem);
                    }
                    else if( l.type == VALUE_NUMBER_LITERAL && r.type == VALUE_NUMBER_LITERAL ) {
                        Cast<bool>(ret.mem) = Cast<double>(l.mem) != Cast<double>(r.mem);
                    }
                    else {
                        std::cout << "ERROR: operands for != must be numeric or boolean expressions at line : " << parser->tokenizer.line << std::endl;
                    }
                }
                else if(bex->opr.type == TOKEN_RSHIFT_EQUALS) {
                    ret.type = VALUE_BOOL_LITERAL;
                    if( l.type == VALUE_NUMBER_LITERAL && r.type == VALUE_NUMBER_LITERAL ) {
                        Cast<bool>(ret.mem) = Cast<double>(l.mem) >= Cast<double>(r.mem);
                    }
                    else {
                        std::cout << "ERROR: operands for >= must be numeric expressions at line : " << parser->tokenizer.line << std::endl;
                    }
                }
                else if(bex->opr.type == TOKEN_LSHIFT_EQUALS) {
                    ret.type = VALUE_BOOL_LITERAL;
                    if( l.type == VALUE_NUMBER_LITERAL && r.type == VALUE_NUMBER_LITERAL ) {
                        Cast<bool>(ret.mem) = Cast<double>(l.mem) <= Cast<double>(r.mem);
                    }
                    else {
                        std::cout << "ERROR: operands for <= must be numeric expressions at line : " << parser->tokenizer.line << std::endl;
                    }
                }
                else if(bex->opr.type == TOKEN_RSHIFT) {
                    ret.type = VALUE_BOOL_LITERAL;
                    if( l.type == VALUE_NUMBER_LITERAL && r.type == VALUE_NUMBER_LITERAL ) {
                        Cast<bool>(ret.mem) = Cast<double>(l.mem) > Cast<double>(r.mem);
                    }
                    else {
                        std::cout << "ERROR: operands for > must be numeric expressions at line : " << parser->tokenizer.line << std::endl;
                    }
                }
                else if(bex->opr.type == TOKEN_LSHIFT) {
                    ret.type = VALUE_BOOL_LITERAL;
                    if( l.type == VALUE_NUMBER_LITERAL && r.type == VALUE_NUMBER_LITERAL ) {
                        Cast<bool>(ret.mem) = Cast<double>(l.mem) < Cast<double>(r.mem);
                    }
                    else {
                        std::cout << "ERROR: operands for < must be numeric expressions at line : " << parser->tokenizer.line << std::endl;
                    }
                }


                return ret;
            }

            break;
        case EXPRESSION_UNARY:

            {
                UnaryExpr* ex = ((UnaryExpr*)(parser->mem + expr.index));
                Value r = Evaluate(interpreter ,parser , ex->right);

                if( ex->opr.type == TOKEN_EXLAMATION_MARK ) {
                    if( r.type != VALUE_BOOL_LITERAL ) {
                        std::cout << "ERROR: operand for ! must be a boolean expression at line : " << GetLineNumber(parser->source, ex->opr.text ) << std::endl;
                    }
                    else {
                        Cast<bool>(r.mem) = !Cast<bool>(r.mem);
                    }
                }
                else if( ex->opr.type == TOKEN_MINUS ) {
                    if( r.type != VALUE_NUMBER_LITERAL ) {
                        std::cout << "ERROR: operands for - be numeric expressions at line : " << GetLineNumber(parser->source, ex->opr.text ) << std::endl;
                    }
                    Cast<double>(r.mem) = - Cast<double>(r.mem);
                }
                

                return r;
            }
            break;
        case EXPRESSION_VARIABLE:
            {
                Token name = Cast<VariableExpr>(parser->mem + expr.index).name;
                u32 hash = StringHash(name.text , name.lenght);
                u32 index = interpreter->symbolTable.Find(hash);
                if(index == ~(u32(0)) ) {
                    std::cout << "ERROR: unknown indentifier ";
                    PrintToken(name);
                    std::cout << " used in expression at line " << GetLineNumber(parser->source , name.text) << std::endl;

                    Value null;
                    Cast<u64>(null.mem) = 0;
                    null.type = VALUE_NULL_LITERAL;
                    return null;
                }
                return interpreter->symbolTable.array[index].value;
            }
            break;
        case EXPRESSION_LITERAL:
            {
                LiteralExpr* literal = ((LiteralExpr*)(parser->mem + expr.index));
                Value r;
                r.type = (ValueTypes)literal->literal.type;

                if( r.type == VALUE_NUMBER_LITERAL ) {
                    Cast<double>(r.mem) = GetNumber( *literal );
                }
                else if( r.type == VALUE_STRING_LITERAL ) {
                    Cast<Token*>(r.mem) = &literal->literal;
                }
                else if(r.type == VALUE_BOOL_LITERAL) {
                    Cast<bool>(r.mem) = GetBool( *literal );
                }
                
                return r;
            }

        case EXPRESSION_ASSIGNMENT:
            {

                AssignExpr* assign = &Cast<AssignExpr>(parser->mem + expr.index);
                u32 hash = StringHash( assign->name.text, assign->name.lenght);
                u32 index = interpreter->symbolTable.Find(hash);
                if(index != ~(u32(0))) {
                    Value val = Evaluate(interpreter,parser,assign->value);

                    if( val.type == interpreter->symbolTable.array[index].value.type || interpreter->symbolTable.array[index].value.type == VALUE_NULL_LITERAL) {
                        interpreter->symbolTable.array[index].value = val;
                        return val;
                    }
                    else {
                        std::cout << "ERROR: invalid assignment at line: " << parser->tokenizer.line << std::endl;
                    }
                }
                else {
                    std::cout << "ERROR: undefined variable at line: " << parser->tokenizer.line << std::endl;
                }
            }
            break;

            break;
        case EXPRESSION_GROUPING:
            return Evaluate( interpreter, parser , ((GroupingExpr*)(parser->mem + expr.index))->expr );
            break;
        case EXPRESSION_CALL:
            {
                CallExpr& call = Cast<CallExpr>(parser->mem + expr.index);
                Value callee = Evaluate(interpreter , parser , call.callee);

                u32 i = call.args.index;
                u32 argCount = 0;
                while( Cast<Expr>(parser->mem + i).index != EXPRESSION_NULL ) {
                    argCount++;
                    i += sizeof(Expr);
                }
                Value args[max<u32>(1,argCount)];

                u32 index = 0;
                i = call.args.index;
                while( Cast<Expr>(parser->mem + i).index != EXPRESSION_NULL ) {
                    Expr arg = Cast<Expr>(parser->mem + i);
                    args[index] = Evaluate(interpreter , parser , arg);
                    index++;
                    i += sizeof(Expr);
                }

                if( callee.type == VALUE_NATIVE_FUNCTION ) {
                    return (Cast<native_function_t>(callee.mem))(args);
                }
                else if(callee.type == VALUE_NON_NATIVE_FUNCTION ) {

                    Stmt declIndex = Cast<Stmt>(callee.mem);
                    DeclFnStmt& declfn = Cast<DeclFnStmt>(parser->mem + declIndex.index);

                    Token params[argCount];
                    
                    i = 0;
                    u32 ptr = declfn.paramIndex;
                    while( Cast<Token>(parser->mem + ptr).type != TOKEN_EOF ) {
                        params[i] = Cast<Token>(parser->mem + ptr);
                        i++;
                        ptr += sizeof(Token);
                    }

                    for(u32 i = 0; i < argCount ; i++) {
                        DefineVar(interpreter , args[i] , params[i]);
                    }

                    Value returnVal;
                    returnVal.type = VALUE_NULL_LITERAL;

                    Push<byte*>(interpreter , interpreter->returnAddress);
                    interpreter->returnAddress = interpreter->stackPtr;

                    Stmt pc = interpreter->program;
                    Execute(interpreter , parser, Stmt{(declfn.body.index + sizeof(ScopeStmt))} );
                    interpreter->program = pc;

                    for(u32 i = 0; i <argCount ; i++) {
                        UnDefineVar(interpreter , params[i]);
                    }

                    return returnVal;
                }
                else {
                    std::cout << "ERROR: expressions must evaluate to functions to be called " << std::endl;
                }
            }
            break;
        
        default:
            break;
    }
}

void RegisterNativeFunction(InterpreterState* interpreter , native_function_t f , const char* name) {

    u32 c = 0;
    while( name[c] != 0) c++;

    u32 hash = StringHash(name,c);
    Value func;
    func.type = VALUE_NATIVE_FUNCTION;
    Cast<native_function_t>(func.mem) = f;
    interpreter->symbolTable.Insert(hash , func);
}

Value NativeFunction_f(Value* value) {
    Value ret;    
    Cast<double>(ret.mem) = Cast<double>(value[0].mem) + Cast<double>(value[1].mem);
    ret.type = VALUE_NUMBER_LITERAL;

    return ret;
}

Value NativeClock(Value* args) {
    auto timer = std::chrono::high_resolution_clock::now();
    auto t = std::chrono::time_point_cast<std::chrono::microseconds>(timer).time_since_epoch();
    Value ret;
    ret.type = VALUE_NUMBER_LITERAL;
    Cast<double>(ret.mem) =  t.count();
    return ret;
}

int main() {

    Parser parser;
    parser.source = ReadFileTerminated("./test.cpp");
    parser.tokenizer.at = parser.source;
    parser.tokenizer.line = 0;
    parser.mem = (byte*)malloc( parser.memCap );

    InterpreterState interpreter;
    interpreter.symbolTable.Init();
    interpreter.stack = (byte*)malloc( 64 * KILO_BYTES );
    interpreter.stackPtr = interpreter.stack;
    interpreter.returnAddress = interpreter.stack;
    
    Stmt program = Parse(&parser);

    RegisterNativeFunction(&interpreter , NativeFunction_f , "native_add");
    RegisterNativeFunction(&interpreter , NativeClock , "native_clock");

    Execute(&interpreter , &parser , program);
}