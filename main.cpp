#include <iostream>
#include <stdio.h>
#include <fstream>
#include <filesystem>
#include <thread>

typedef int8_t i8;
typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

typedef float f32;
typedef double f64;

static_assert(sizeof(f32) == 4);
static_assert(sizeof(f64) == 8);

typedef i8 byte;

constexpr u64 KILO_BYTES = 1000;
constexpr u64 MEGA_BYTES = 1000 * KILO_BYTES;
constexpr u64 GIGA_BYTES = 1000 * MEGA_BYTES;
constexpr u64 TERA_BYTES = 1000 * GIGA_BYTES;

constexpr u32 MILI_SEC = 1000;
constexpr u32 MICRO_SEC = 1000 * MILI_SEC; 
constexpr u32 NANO_SEC = 1000 * MICRO_SEC;

#define LOGASSERT(x , y ) if( !(x) ) {std::cout << #x << " " << y << " " << __FILE__ << " " << __LINE__ << std::endl; __builtin_trap(); }
#define ASSERT(x) if( !(x) ) __builtin_trap();

char* ReadFileTerminated(char* fileName) {

    char* sourceString = nullptr;
    FILE* file = fopen(fileName ,"r");
    if(file) {

        fseek(file , 0, SEEK_END);
        u32 size = ftell(file);
        fseek(file ,0, SEEK_SET);


        sourceString = (char*)malloc(size + 1);
        fread(sourceString , size , 1 , file);
        sourceString[size] = 0;

    }
    fclose(file);

    return sourceString;
}

enum TokenType {
    TOKEN_IDENTIFIER,

    TOKEN_OPEN_PAREN,
    TOKEN_CLOSE_PAREN,
    TOKEN_OPEN_BRACKET,
    TOKEN_CLOSE_BRACKET,
    TOKEN_OPEN_BRACES,
    TOKEN_CLOSE_BRACES,

    TOKEN_COLON,
    TOKEN_SEMICOLON,
    TOKEN_ASTERISK,
    TOKEN_OPERATOR,
    TOKEN_ASSIGNMENT,

    TOKEN_LITERAL_STRING,
    TOKEN_LITERAL_NUMBER,
    
    TOKEN_KEYWORD_IF,
    TOKEN_KEYWORD_ELSE,
    TOKEN_KEYWORD_FOR,
    TOKEN_KEYWORD_WHILE,
    TOKEN_KEYWORD_DO,
    TOKEN_KEYWORD_GOTO,
    TOKEN_KEYWORD_STRUCT,
    TOKEN_KEYWORD_CLASS,
    TOKEN_KEYWORD_RETURN,

    TOKEN_TYPE_VOID,
    TOKEN_TYPE_I8,
    TOKEN_TYPE_I16,
    TOKEN_TYPE_I32,
    TOKEN_TYPE_I64,
    TOKEN_TYPE_U8,
    TOKEN_TYPE_U16,
    TOKEN_TYPE_U32,
    TOKEN_TYPE_U64,
    TOKEN_TYPE_F32,
    TOKEN_TYPE_F64,

    TOKEN_UNKOWN,
    TOKEN_EOS,
    TOKEN_COUNT,
};

struct Token {
    char* text;
    u64 lenght;
    TokenType type;
};

struct Tokenizer {
    char* at;
};

bool IsWhiteSpace(char c) {
    return  (c == ' ') ||
            (c == '\n') ||
            (c == '\t') ||
            (c == '\r');
}

u32 GetLineNumber(char* source ,Tokenizer* tokenizer) {
    u32 c = 1;
    while( source != tokenizer->at ) {
        if( *source == '\n' ) c++;
        source++;
    }
    return c;
}
bool IsAlpha(char c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}
bool IsNumeric(char c) {
    return (c >= '0' && c <= '9');
}
bool TokenEquals(Token t , const char* match) {

    const char* m = match;
    for(u32 i = 0; i < t.lenght ; i++) {

        if(*m == 0 || t.text[i] != *m ) {
            return false;
        }

        m++;
    }
    return (*m == 0);
}
bool TokensEquals(Token t0 , Token t1) {

    if(t0.lenght != t1.lenght) {
        return false;
    }

    for(u32 i = 0; i < t0.lenght ; i++) {
        if( t0.text[i] != t1.text[i] ) {
            return false;
        }
    }

    return true;
}

void EatMacors(Tokenizer* tokenizer) {

    bool eat = false;

    while(tokenizer->at[0]) {

        if( tokenizer->at[0] == '\\' && tokenizer->at[1] == '\\' ) {
            eat = true;
            tokenizer->at++;
        }
        else if( tokenizer->at[0] == '\n') if(eat) eat = false; else break;

        ++tokenizer->at;
    }
}

void EatWhiteSpace(Tokenizer* tokenizer) {


    while(true) {
        if( IsWhiteSpace(tokenizer->at[0])  ) {
            tokenizer->at++;
        }
        else if( tokenizer->at[0] == '/' && tokenizer->at[1] == '/' ) {
            tokenizer->at += 2;
            while( !(tokenizer->at[0] == '\n' )) ++tokenizer->at;
            tokenizer->at += 2;
        }
        else if( tokenizer->at[0] == '/' && tokenizer->at[1] == '*' ) {
            tokenizer->at += 2;
            while( !(tokenizer->at[0] == '*' && tokenizer->at[1] == '/') ) ++tokenizer->at;
            tokenizer->at += 2;
        }
        else if( tokenizer->at[0] == '#') {
            EatMacors(tokenizer);
        }
        else {
            break;
        }
    }

}

constexpr char KEYWORD_CHARS[10][8] = {
    {0},
    {"if\0"},
    {"else\0"},
    {"for\0"},
    {"while\0"},
    {"do\0"},
    {"goto\0"},
    {"struct\0"},
    {"class\0"},
    {"return\0"},
};


u32 KeyWordLenght(Tokenizer tokenizer ,const char* keyword) {
    u32 i = 0;
    while( keyword[i] && (keyword[i] == tokenizer.at[i]) ) {
        i++;
    }
    return i;
}

Token GetKeyWordTokenType(Tokenizer tokenizer) {

    Token r;
    r.type = TOKEN_UNKOWN;
    r.text = tokenizer.at;
    r.lenght = 1;

    u32 index = 0;
    for(u32 i = 1; i < 10 ; i++) {

        u32 l = KeyWordLenght(tokenizer , KEYWORD_CHARS[i]);

        if(!KEYWORD_CHARS[i][l]) {
            index = i;
            r.lenght = l+1;
            break;
        }
    }

    switch (index)
    {
    case 0:
        r.type = TOKEN_UNKOWN;
        break;

    case 1:
        r.type = TOKEN_KEYWORD_IF;
        break;
    case 2:
        r.type = TOKEN_KEYWORD_ELSE;
        break;
    case 3:
        r.type = TOKEN_KEYWORD_FOR;
        break;
    case 4:
        r.type = TOKEN_KEYWORD_WHILE;
        break;
    case 5:
        r.type = TOKEN_KEYWORD_DO;
        break;
    case 6:
        r.type = TOKEN_KEYWORD_GOTO;
        break;
    case 7:
        r.type = TOKEN_KEYWORD_STRUCT;
        break;
    case 8:
        r.type = TOKEN_KEYWORD_CLASS;
        break;
    case 9:
        r.type = TOKEN_KEYWORD_RETURN;
        break;

    default:
        r.type = TOKEN_UNKOWN;
        break;
    }

    return r;
}

constexpr char TYPE_CHARS[12][6] = {
    {0},
    {"void\0"},
    {"i8\0"},
    {"i16\0"},
    {"i32\0"},
    {"i64\0"},
    {"u8\0"},
    {"u16\0"},
    {"u32\0"},
    {"u64\0"},
    {"f32\0"},
    {"f64\0"},
};

Token GetTypeTokenType(Tokenizer tokenizer) {

    Token r;
    r.type = TOKEN_UNKOWN;
    r.text = tokenizer.at;
    r.lenght = 1;

    u32 index = 0;
    for(u32 i = 1; i < 12 ; i++) {

        u32 l = KeyWordLenght(tokenizer , TYPE_CHARS[i]);

        if(!TYPE_CHARS[i][l]) {
            index = i;
            r.lenght = l+1;
            std::cout << i << " TYPE " << l << std::endl;
            break;
        }
    }

    switch (index)
    {
    case 0:
        r.type = TOKEN_UNKOWN;
        break;

    case 1:
        r.type = TOKEN_TYPE_VOID;
        break;
    case 2:
        r.type = TOKEN_TYPE_I8;
        break;
    case 3:
        r.type = TOKEN_TYPE_I16;
        break;
    case 4:
        r.type = TOKEN_TYPE_I32;
        break;
    case 5:
        r.type = TOKEN_TYPE_I64;
        break;
    case 6:
        r.type = TOKEN_TYPE_U8;
        break;
    case 7:
        r.type = TOKEN_TYPE_U16;
        break;
    case 8:
        r.type = TOKEN_TYPE_U32;
        break;
    case 9:
        r.type = TOKEN_TYPE_U64;
        break;
    case 10:
        r.type = TOKEN_TYPE_F32;
        break;
    case 11:
        r.type = TOKEN_TYPE_F64;
        break;

    default:
        r.type = TOKEN_UNKOWN;
        break;
    }

    return r;
}


Token GetToken(Tokenizer* tokenizer) {

    EatWhiteSpace(tokenizer);

    Tokenizer tmp = *tokenizer;
    Token token{};
    token.lenght = 1;

    char c = tokenizer->at[0];
    token.text = tokenizer->at++;

    switch (c) {

        case '\0': {token.type = TOKEN_EOS;             } break;
        case '(' : {token.type = TOKEN_OPEN_PAREN;      } break;
        case ')' : {token.type = TOKEN_CLOSE_PAREN;     } break;
        case ':' : {token.type = TOKEN_COLON;           } break;
        case ';' : {token.type = TOKEN_SEMICOLON;       } break; 
        case '*' : {token.type = TOKEN_ASTERISK;        } break;
        case '[' : {token.type = TOKEN_OPEN_BRACKET;    } break;
        case ']' : {token.type = TOKEN_CLOSE_BRACKET;   } break;
        case '{' : {token.type = TOKEN_OPEN_BRACES;     } break;
        case '}' : {token.type = TOKEN_CLOSE_BRACES;    } break;
        case '=' : {if( tokenizer->at[0] == '=') {
                        token.type = TOKEN_OPERATOR;
                        token.lenght = 2;
                        tokenizer->at++;
                    } else token.type = TOKEN_ASSIGNMENT;} break;

        case '"': 
            token.text = tokenizer->at;
            token.type = TOKEN_LITERAL_STRING;

            while(  tokenizer->at[0] && tokenizer->at[0] != '"') {
                
                if(tokenizer->at[0] == '\\' && tokenizer->at[1] ) {
                    ++tokenizer->at;
                }
                ++tokenizer->at;
            }
            token.lenght = tokenizer->at - token.text;
            if( tokenizer->at[0] == '"' ) ++tokenizer->at;

        break;

        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9':
            token.type = TOKEN_LITERAL_NUMBER;
            while(IsNumeric(tokenizer->at[0]) ) ++tokenizer->at;
            token.lenght = tokenizer->at - token.text;
            break;

        default:

            if( IsAlpha(c) ) {
                token.type = TOKEN_IDENTIFIER;
                bool onlyAlpha = true;
                while(IsAlpha(tokenizer->at[0]) || IsNumeric(tokenizer->at[0]) || tokenizer->at[0] == '_' ) {
                    if( !IsAlpha(tokenizer->at[0]) ) {
                        onlyAlpha = false;
                    }
                    ++tokenizer->at;
                }

                bool keywordFound = false;
                if(onlyAlpha) {
                    Token keyword = GetKeyWordTokenType(tmp);
                    if( keyword.type != TOKEN_UNKOWN ) {
                        token = keyword;
                        keywordFound = true;
                    }

                }

                if(!keywordFound) {
                    Token type = GetTypeTokenType(tmp);
                    if( type.type != TOKEN_UNKOWN ) {
                        token = type;
                    }
                }

                token.lenght = tokenizer->at - token.text;
            }
            else {
                token.type = TOKEN_UNKOWN;
            }
            break;
            
    }

    return token;
}


struct ParserState {
    Type* types = nullptr;
    u32 typesCap = 0;
    u32 typesSize = 0;

    u32 currentTypeIndex = 0;

    char* source = nullptr;
};
struct Member {
    Type type;
    Token name;
};
struct Type {
    Token type_name;
    Member* members = nullptr;
    u32 memberCount = 0;
};

void MemCpy(void* src ,void* dst , u32 size) {
    for(u32 i = 0; i < size ; i++) {
        ((byte*)dst)[i] = ((byte*)src)[i];
    }
}

u32 PushMember(Member m , Type* t) {

    Member* tmp = (Member*)malloc( sizeof(Member) * (t->memberCount + 1) );
    MemCpy(t->members , tmp , sizeof(Member) * t->memberCount);

    free(t->members);
    t->members = tmp;
    t->members[t->memberCount] = m;
    t->memberCount++;
    return t->memberCount-1;
}
u32 PushType(Token t , ParserState* state) {

    if( state->typesCap < state->typesSize + 1 ) {
        Type* tmp = (Type*)malloc( sizeof(Type) * (state->typesSize + 1) * 2 );

        state->typesCap = (state->typesSize + 1) * 2;
        MemCpy(state->types , tmp , state->typesSize * sizeof(Type) );
        free(state->types);
        state->types = tmp;
    }

    state->types[state->typesSize].type_name = t;
    return (++state->typesSize) - 1;
}
void FreeTypes(ParserState* state) {
    free(state->types);
    state->types = nullptr;
    state->typesCap = 0;
    state->typesSize = 0;
}

void ParseStructMembers(u32 struct_index , ParserState* state, Tokenizer* tokenizer) {

    Token t = GetToken(tokenizer);

    switch (t.type)
    {
    case TOKEN_KEYWORD_STRUCT:
        ParseStruct(state, tokenizer);
        break;

    case TOKEN_CLOSE_BRACKET:
        t = GetToken(tokenizer);
        if(t.type != TOKEN_SEMICOLON) {
            std::cout << "ERROR: expected ; at line : " << GetLineNumber(state->source , tokenizer) << std::endl;
        }
        return;
        break;

    case TOKEN_TYPE_U8:
    case TOKEN_TYPE_U16:
    case TOKEN_TYPE_U32:
    case TOKEN_TYPE_U64:
    case TOKEN_TYPE_I8:
    case TOKEN_TYPE_I16:
    case TOKEN_TYPE_I32:
    case TOKEN_TYPE_I64:
    case TOKEN_TYPE_F32:
    case TOKEN_TYPE_F64:

        Token tmp = GetToken(tokenizer);
        
        if(tmp.type == TOKEN_ASTERISK) {
            while( tmp.type != TOKEN_SEMICOLON ) {
                tmp = GetToken(tokenizer);
            }
            break;
        }
        else if(tmp.type == TOKEN_IDENTIFIER ) {
            if( TokenEquals(tmp , "const") ) {
                std::cout << "ERROR: unexpected const qualifier: " << GetLineNumber(state->source , tokenizer) << std::endl;
            }
            else {
                Member member;
                member.name = tmp;
                member.type.type_name = t;
                member.type.members = nullptr;
                member.type.memberCount = 0;
                PushMember( member , &state->types[struct_index] );

                while(tmp.type != TOKEN_SEMICOLON) tmp = GetToken(tokenizer);
            }
        }

        break;
    
    default:
        ASSERT(false);
        break;
    }
    ParseStructMembers(struct_index,state, tokenizer);
}

void ParseStruct(ParserState* state, Tokenizer* tokenizer) {

    Token t = GetToken(tokenizer);
    u32 index = PushType(t, state);

    Token t = GetToken(tokenizer);
    if( t.type == TOKEN_OPEN_BRACKET ) {
        state->types[index].memberCount = 0;
        state->types[index].members = nullptr;

        ParseStructMembers(index , state, tokenizer);
    }
    else if( t.type == TOKEN_SEMICOLON ) {
        state->types[index].memberCount = 0;
        state->types[index].members = nullptr;
    }
    else {
        std::cout << "ERROR: expected ; or { at line : " << GetLineNumber(state->source , tokenizer) << std::endl;
    }
}

void Parse(ParserState* state , Tokenizer* tokenizer) {

    Token t = GetToken(tokenizer);

    switch (t.type)
    {
    case TOKEN_KEYWORD_STRUCT:
        ParseStruct(state, tokenizer);
        break;

    case TOKEN_EOS:
        return;
    
    default:
        ASSERT(false);
        break;
    }
    Parse(state , tokenizer);
}

int main() {


    char* source = ReadFileTerminated("test.cpp");
    Tokenizer tokenizer{source};
    bool parsing = true;

    ParserState state;
    Parse(&state , &tokenizer);

    return 0;
}