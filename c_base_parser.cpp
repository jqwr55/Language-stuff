#include <Tokenizer.h>

struct Parser {
    Tokenizer tokenizer;
    DynamicBufferSimple<Token> tokenBuffer;
    char* source = nullptr;

    byte* mem = nullptr;
    u32 memCap = 64 * KILO_BYTES;
    u32 allocator = 0;
    bool panicMode = false;
};

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
        parser->panicMode = true;
    }
}

enum ExprType {
    EXPRESSION_BINARY,
    EXPRESSION_UNARY,
    EXPRESSION_LITERAL,
    EXPRESSION_GROUPING,
};

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
struct Literal : Expr {
    Token literal;
};
struct Grouping : Expr {
    Expr expr;
};

Expr Expression(Parser* parser);

Expr Primary(Parser* parser) {

    TokenType m[3]{TOKEN_BOOL_LITERAL , TOKEN_NULL_LITERAL , TOKEN_NUMBER_LITERAL };
    TokenType c = TOKEN_OPEN_PAREN;

    if(Match(parser , &c , 1)) {
        Expr expr = Expression(parser);

        ExpectToken(parser , TOKEN_CLOSE_PAREN);

        u32 index = parser->allocator;
        parser->allocator += sizeof(Grouping);
        ((Grouping*)(parser->mem + index))->index = EXPRESSION_GROUPING;
        ((Grouping*)(parser->mem + index))->expr = expr;

        return Expr{index};
    }
    else if(Match(parser , m , 3)) {
        u32 index = parser->allocator;
        parser->allocator += sizeof(Literal);
        ((Literal*)(parser->mem + index))->index = EXPRESSION_LITERAL;
        ((Literal*)(parser->mem + index))->literal = PreviousToken(parser);
        return Expr{index};
    }

    ASSERT(false);

    return Expr{0};
}

Expr Unary(Parser* parser) {

    TokenType tokens[2]{TOKEN_EXLAMATION_MARK,TOKEN_MINUS};
    while(Match(parser , tokens , 2)) {
        Token opr = PreviousToken(parser);
        Expr right = Unary(parser);

        u32 index = parser->allocator;
        parser->allocator += sizeof(UnaryExpr);
        ((UnaryExpr*)(parser->mem + index))->index = EXPRESSION_UNARY;
        ((UnaryExpr*)(parser->mem + index))->opr = opr;
        ((UnaryExpr*)(parser->mem + index))->right = right;

        return Expr{index};
    }

    return Primary(parser);
}
Expr Factor(Parser* parser) {

    Expr expr = Unary(parser);

    TokenType tokens[2]{TOKEN_ASTERISK , TOKEN_SLASH};
    while(Match(parser, tokens , 2)) {
        Token opr = PreviousToken(parser);
        Expr right = Unary(parser);

        u32 index = parser->allocator;
        parser->allocator += sizeof(BinaryExpr);
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

        u32 index = parser->allocator;
        parser->allocator += sizeof(BinaryExpr);
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

        u32 index = parser->allocator;
        parser->allocator += sizeof(BinaryExpr);
        ((BinaryExpr*)(parser->mem + index))->index = EXPRESSION_BINARY;
        ((BinaryExpr*)(parser->mem + index))->left = expr;
        ((BinaryExpr*)(parser->mem + index))->opr = opr;
        ((BinaryExpr*)(parser->mem + index))->right = right;

        expr.index = index;
    }

    return expr;
}
Expr Eqaulity(Parser* parser) {

    Expr expr = Comparison(parser);

    TokenType tokens[2]{TOKEN_EXCLAMATION_EQUALS,TOKEN_EQUALS_EQUALS};
    while(Match(parser, tokens , 2)) {
        Token opr = PreviousToken(parser);
        Expr right = Comparison(parser);

        u32 index = parser->allocator;
        parser->allocator += sizeof(BinaryExpr);
        ((BinaryExpr*)(parser->mem + index))->index = EXPRESSION_BINARY;
        ((BinaryExpr*)(parser->mem + index))->left = expr;
        ((BinaryExpr*)(parser->mem + index))->opr = opr;
        ((BinaryExpr*)(parser->mem + index))->right = right;

        expr.index = index;
    }

    return expr;
}
Expr Expression(Parser* parser) {
    return Eqaulity(parser);
}

Expr Parse(Parser* parser) {
    return Expression(parser);
}

void PrintExprTree(Parser* parser , Expr expr, u32 indentLevel) {

    std::cout << std::setw(indentLevel*7) << "";

    switch( *((ExprType*)(parser->mem + expr.index)) ) {
        case EXPRESSION_BINARY:
            PrintToken(((BinaryExpr*)(parser->mem + expr.index))->opr);
            std::cout << std::endl;
            PrintExprTree( parser , ((BinaryExpr*)(parser->mem + expr.index))->left , indentLevel + 1);
            PrintExprTree( parser , ((BinaryExpr*)(parser->mem + expr.index))->right, indentLevel + 1);
            break;
        case EXPRESSION_UNARY:
            PrintToken(((UnaryExpr*)(parser->mem + expr.index))->opr);
            std::cout << std::endl;
            PrintExprTree( parser , ((UnaryExpr*)(parser->mem + expr.index))->right, indentLevel + 1);
            break;
        case EXPRESSION_LITERAL:
            PrintToken(((Literal*)(parser->mem + expr.index))->literal);
            std::cout << std::endl;
            break;
        case EXPRESSION_GROUPING:
            PrintExprTree( parser , ((Grouping*)(parser->mem + expr.index))->expr , indentLevel + 1);
            break;
        
        default:
            break;
    }
}

struct Value {
    TokenType type;
    byte mem[8]{0};
};

double GetNumber(Literal l) {    
    char str[l.literal.lenght]{0};
    for(u32 i = 0; i < l.literal.lenght ; i++) {
        str[i] = l.literal.text[i];
    }
    return strtod(str,nullptr);
}
bool GetBool(Literal l) {
    return TokenEquals(l.literal , "true");
}

Value Evaluate(Parser* parser, Expr expr) {

    ExprType type =  *((ExprType*)(parser->mem + expr.index));

    switch( type ) {
        case EXPRESSION_BINARY:
            {
                Value l = Evaluate(parser ,((BinaryExpr*)(parser->mem + expr.index))->left);
                Value r = Evaluate(parser ,((BinaryExpr*)(parser->mem + expr.index))->right);

                Value ret;
                BinaryExpr* bex = ((BinaryExpr*)(parser->mem + expr.index));
                //std::cout << bex->opr.type << std::endl;

                if( bex->opr.type == TOKEN_PLUS ) {
                    ret.type = TOKEN_NUMBER_LITERAL;
                    if( l.type != TOKEN_NUMBER_LITERAL || r.type != TOKEN_NUMBER_LITERAL ) {
                        std::cout << "ERROR: operands for + be numeric expressions at line : " << GetLineNumber(parser->source, bex->opr.text ) << std::endl;
                    }
                    *((double*)ret.mem) = (*((double*)l.mem)) + (*((double*)r.mem));
                }
                else if( bex->opr.type == TOKEN_MINUS ) {
                    ret.type = TOKEN_NUMBER_LITERAL;
                    if( l.type != TOKEN_NUMBER_LITERAL || r.type != TOKEN_NUMBER_LITERAL ) {
                        std::cout << "ERROR: operands for - be numeric expressions at line : " << GetLineNumber(parser->source, bex->opr.text ) << std::endl;
                    }
                    *((double*)ret.mem) = (*((double*)l.mem)) - (*((double*)r.mem));
                }
                else if( bex->opr.type == TOKEN_ASTERISK ) {
                    ret.type = TOKEN_NUMBER_LITERAL;
                    if( l.type != TOKEN_NUMBER_LITERAL || r.type != TOKEN_NUMBER_LITERAL ) {
                        std::cout << "ERROR: operands for * be numeric expressions at line : " << GetLineNumber(parser->source, bex->opr.text ) << std::endl;
                    }
                    *((double*)ret.mem) = (*((double*)l.mem)) * (*((double*)r.mem));
                }
                else if( bex->opr.type == TOKEN_SLASH ) {
                    ret.type = TOKEN_NUMBER_LITERAL;
                    if( l.type != TOKEN_NUMBER_LITERAL || r.type != TOKEN_NUMBER_LITERAL ) {
                        std::cout << "ERROR: operands for / be numeric expressions at line : " << GetLineNumber(parser->source, bex->opr.text ) << std::endl;
                    }
                    *((double*)ret.mem) = (*((double*)l.mem)) / (*((double*)r.mem));
                }
                else if(bex->opr.type == TOKEN_EQUALS_EQUALS) {
                    ret.type = TOKEN_BOOL_LITERAL;
                    if( l.type == TOKEN_BOOL_LITERAL && r.type == TOKEN_BOOL_LITERAL ) {
                        *((bool*)ret.mem) = (*((bool*)l.mem)) == (*((bool*)r.mem));
                    }
                    else if( l.type == TOKEN_NUMBER_LITERAL && r.type == TOKEN_NUMBER_LITERAL ) {
                        *((bool*)ret.mem) = (*((double*)l.mem)) == (*((double*)r.mem));
                    }
                    else {
                        std::cout << "ERROR: operands for == be numeric or boolean expressions at line : " << GetLineNumber(parser->source, bex->opr.text ) << std::endl;
                    }
                }
                else if(bex->opr.type == TOKEN_EXCLAMATION_EQUALS) {
                    ret.type = TOKEN_BOOL_LITERAL;
                    if( l.type == TOKEN_BOOL_LITERAL && r.type == TOKEN_BOOL_LITERAL ) {
                        *((bool*)ret.mem) = (*((bool*)l.mem)) != (*((bool*)r.mem));
                    }
                    else if( l.type == TOKEN_NUMBER_LITERAL && r.type == TOKEN_NUMBER_LITERAL ) {
                        *((bool*)ret.mem) = (*((double*)l.mem)) != (*((double*)r.mem));
                    }
                    else {
                        std::cout << "ERROR: operands for != be numeric or boolean expressions at line : " << GetLineNumber(parser->source, bex->opr.text ) << std::endl;
                    }
                }
                else if(bex->opr.type == TOKEN_RSHIFT_EQUALS) {
                    ret.type = TOKEN_BOOL_LITERAL;
                    if( l.type == TOKEN_NUMBER_LITERAL && r.type == TOKEN_NUMBER_LITERAL ) {
                        *((bool*)ret.mem) = (*((double*)l.mem)) >= (*((double*)r.mem));
                    }
                    else {
                        std::cout << "ERROR: operands for >= be numeric expressions at line : " << GetLineNumber(parser->source, bex->opr.text ) << std::endl;
                    }
                }
                else if(bex->opr.type == TOKEN_LSHIFT_EQUALS) {
                    ret.type = TOKEN_BOOL_LITERAL;
                    if( l.type == TOKEN_NUMBER_LITERAL && r.type == TOKEN_NUMBER_LITERAL ) {
                        *((bool*)ret.mem) = (*((double*)l.mem)) <= (*((double*)r.mem));
                    }
                    else {
                        std::cout << "ERROR: operands for <= be numeric expressions at line : " << GetLineNumber(parser->source, bex->opr.text ) << std::endl;
                    }
                }
                else if(bex->opr.type == TOKEN_RSHIFT) {
                    ret.type = TOKEN_BOOL_LITERAL;
                    if( l.type == TOKEN_NUMBER_LITERAL && r.type == TOKEN_NUMBER_LITERAL ) {
                        *((bool*)ret.mem) = (*((double*)l.mem)) > (*((double*)r.mem));
                    }
                    else {
                        std::cout << "ERROR: operands for > be numeric expressions at line : " << GetLineNumber(parser->source, bex->opr.text ) << std::endl;
                    }
                }
                else if(bex->opr.type == TOKEN_LSHIFT) {
                    ret.type = TOKEN_BOOL_LITERAL;
                    if( l.type == TOKEN_NUMBER_LITERAL && r.type == TOKEN_NUMBER_LITERAL ) {
                        *((bool*)ret.mem) = (*((double*)l.mem)) < (*((double*)r.mem));
                    }
                    else {
                        std::cout << "ERROR: operands for < be numeric expressions at line : " << GetLineNumber(parser->source, bex->opr.text ) << std::endl;
                    }
                }


                return ret;
            }

            break;
        case EXPRESSION_UNARY:

            {
                UnaryExpr* ex = ((UnaryExpr*)(parser->mem + expr.index));
                Value r = Evaluate( parser , ex->right);

                if( ex->opr.type == TOKEN_EXLAMATION_MARK ) {
                    if( r.type != TOKEN_BOOL_LITERAL ) {
                        std::cout << "ERROR: operand for ! must be a boolean expression at line : " << GetLineNumber(parser->source, ex->opr.text ) << std::endl;
                    }
                    else {
                        *((bool*)r.mem) = !(*((bool*)r.mem));
                    }

                }
                else if( ex->opr.type == TOKEN_MINUS ) {
                    if( r.type != TOKEN_NUMBER_LITERAL ) {
                        std::cout << "ERROR: operands for - be numeric expressions at line : " << GetLineNumber(parser->source, ex->opr.text ) << std::endl;
                    }
                    *((double*)r.mem) = - (*((double*)r.mem));
                }

                return r;
            }
            break;
        case EXPRESSION_LITERAL:

            {                
                Literal* literal = ((Literal*)(parser->mem + expr.index));
                Value r;
                r.type = literal->literal.type;

                if( r.type == TOKEN_NUMBER_LITERAL ) {
                    *((double*)r.mem) = GetNumber( *literal );
                }
                else if( r.type == TOKEN_STRING_LITERAL ) {
                    *((Token*)r.mem) = literal->literal;
                }
                else if(r.type == TOKEN_BOOL_LITERAL) {
                    *((bool*)r.mem) = GetBool(*literal);
                }
                
                return r;
            }

            break;
        case EXPRESSION_GROUPING:
            return Evaluate( parser , ((Grouping*)(parser->mem + expr.index))->expr );
            break;
        
        default:
            break;
    }

}

int main() {

    Parser parser;
    parser.source = ReadFileTerminated("./test.cpp");
    parser.tokenizer.at = parser.source;
    parser.tokenizer.line = 0;
    parser.mem = (byte*)malloc( parser.memCap );
    parser.allocator = 0;


    Expr expr = Parse(&parser);

    // for(u32 i = 0; i < parser.tokenBuffer.size ; i++) {
    //     PrintToken(parser.tokenBuffer[i]);
    //     std::cout << std::endl;
    // }
    PrintExprTree(&parser, expr ,0);
    

    Value val = Evaluate(&parser , expr);

    if(val.type == TOKEN_NUMBER_LITERAL) {
        std::cout << *((double*)val.mem) << std::endl;
    }
    else if(val.type == TOKEN_BOOL_LITERAL) {
        std::cout << ((*((bool*)val.mem)) ? "true" : "false") << std::endl;
    }
}