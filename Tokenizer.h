#include <iostream>
#include <iomanip>
#include <stdio.h>
#include <chrono>

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

#ifdef __GNUC__
    #define _FORCE_INLINE __attribute__((always_inline))
#else
    #ifdef _MSC_VER
        #define _FORCE_INLINE __builtin_trap();
    #endif
#endif

template<typename T> struct DynamicBufferSimple {
    T*      mem = nullptr;
    u32     cap = 0;
    u32     size = 0;

    T& Back() {
        ASSERT(mem != nullptr && size != 0);
        return mem[size];
    }

    u32 PushBack(T e) {
        if( cap < size + 1) {
            T* tmp = (T*)malloc( sizeof(T) * (size + 1) * 2 );
            for(u32 i = 0; i < size * sizeof(T) ; i++) {
                ((byte*)tmp)[i] = ((byte*)mem)[i];
            }
            free(mem);
            mem = tmp;

            cap = (size + 1) * 2;
        }

        mem[size] = e;
        return size++;
    }
    u32 PopBack() {
        return size--;
    }
    u32 Shrink() {
        if(size < cap) {
            T* tmp = (T*)malloc( sizeof(T) * (size + 1) * 2 );
            for(u32 i = 0; i < size * sizeof(T) ; i++) {
                ((byte*)tmp)[i] = ((byte*)mem)[i];
            }
            free(mem);
            mem = tmp;
            cap = size;
        }
        return size;
    }
    u32 ShrinkNoBranch() {
        T* tmp = (T*)malloc( sizeof(T) * (size + 1) * 2 );
        for(u32 i = 0; i < size * sizeof(T) ; i++) {
            ((byte*)tmp)[i] = ((byte*)mem)[i];
        }
        free(mem);
        mem = tmp;
        cap = size;
        return size;
    }

    _FORCE_INLINE T& operator[] (u32 i) {
        return mem[i];
    }
    void Free() {
        fre(mem);
        mem = nullptr;
        cap = 0;
        size = 0;
    }
};

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

template<typename K , typename V> struct HashNode {
    K key;
    V value;
};
template<typename K , typename V> struct HashTable {
    HashNode<K,V>* array = nullptr;
    u32 cap = 0;
    u32 occupancy = 0;
    static constexpr f32 loadFactor = 0.5;

    void Init() {
        cap = 2;
        array = (HashNode<K,V>*)malloc( sizeof(HashNode<K,V>) * 2);
        array[0].key = (~u32(0));
        array[1].key = (~u32(0));
    }

    u32 Find(u32 key) {
    
        u32 hash = key & (cap - 1);
        u32 ogHash = hash;
        for(;;) {
            if(array[hash].key == key) {
                return hash;
            }
            hash++;

            if(hash == cap) {
                return (~u32(0));
            }
        }
    }

    void Delete(u32 key) {
        array[Find(key)].key = (~u32(0));
    }

    void Insert(u32 key , u32 val) {

        if( cap * loadFactor < (occupancy + 1) ) {

            HashNode<K,V>* tmp = (HashNode<K,V>*)malloc(sizeof(HashNode<K,V>) * cap * 2);

            u32 hash;
            u32 newCap = cap * 2;
            for(u32 i = 0; i < newCap ; i++) {
                tmp[i].key = (~u32(0));
            }

            for(u32 i = 0; i < cap ; i++) {
                if( array[i].key != (~u32(0)) ) {
                    hash = array[i].key & (newCap - 1);
                    
                    for(;;) {
                        if(tmp[hash].key == (~u32(0))) {
                            tmp[hash].key = array[i].key;
                            tmp[hash].value = array[i].value;
                            break;
                        }

                        hash++;

                        if(hash == newCap) {
                            ASSERT(false);
                        }
                    }
                }
            }

            free(array);
            array = tmp;
            cap = newCap;
        }
        occupancy++;

        u32 hash = key & (cap - 1);
        for(;;) {
            if( array[hash].key == (~u32(0)) ) {
                array[hash].key = key;
                array[hash].value = val;
                return;
            }

            hash++;

            if( hash == cap ) {
                hash = 0;
            }
        }
    }

    

    void Free() {
        free(array);
        array = nullptr;
    }
};

enum TokenType {
    TOKEN_IDENTIFIER,
    TOKEN_KEYWORD,

    TOKEN_OPEN_PAREN,
    TOKEN_CLOSE_PAREN,
    TOKEN_OPEN_BRACKET,
    TOKEN_CLOSE_BRACKET,
    TOKEN_OPEN_BRACES,
    TOKEN_CLOSE_BRACES,

    TOKEN_ADD,
    TOKEN_SUB,
    TOKEN_DIV,
    TOKEN_ASSIGNMENT,

    TOKEN_COMA,
    TOKEN_COLON,
    TOKEN_SEMICOLON,
    TOKEN_ASTERISK,
    TOKEN_AMPERSAND,
    TOKEN_NEGATION,
    TOKEN_EXLAMATION_MARK,

    TOKEN_STRING_LITERAL,
    TOKEN_NUMBER_LITERAL,

    TOKEN_UNKNOWN,
    TOKEN_EOF,
    TOKEN_COUNT,
};


struct Token {
    char*       text;
    u64         lenght;
    TokenType   type;
};

struct Tokenizer {
    char*   at;
    u32     line;
};

i64 GetI64(Token t) {
    char str[t.lenght+1]{0};
    for(u32 i = 0; i < t.lenght ; i++) {
        str[i] = t.text[i];
    }
    return atoi(str);
}
u64 GetU64(Token t) {
    char str[t.lenght+1]{0};
    for(u32 i = 0; i < t.lenght ; i++) {
        str[i] = t.text[i];
    }
    return strtoul(t.text , nullptr, 10);
}

f64 GetF64(Token t) {
    char str[t.lenght+1]{0};
    for(u32 i = 0; i < t.lenght ; i++) {
        str[i] = t.text[i];
    }
    return atof(str);
}

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

        tokenizer->line += tokenizer->at[0] == '\n';
        if( tokenizer->at[0] == '\\' && tokenizer->at[1] == '\\' ) {
            eat = true;
            tokenizer->at++;
        }
        else if( tokenizer->at[0] == '\n') if(eat) eat = false; else break;

        ++tokenizer->at;
    }
}

void EatWhiteSpace(Tokenizer* tokenizer) { // returns number lines eaten

    while(tokenizer->at[0]) {
        if( IsWhiteSpace(tokenizer->at[0])  ) {
            tokenizer->line += tokenizer->at[0] == '\n';
            tokenizer->at++;
        }
        else if( tokenizer->at[0] == '/' && tokenizer->at[1] == '/' ) {
            tokenizer->at += 2;
            while( tokenizer->at[0] && !(tokenizer->at[0] == '\n' )) ++tokenizer->at;
            tokenizer->line++;
            tokenizer->at += 2;
        }
        else if( tokenizer->at[0] == '/' && tokenizer->at[1] == '*' ) {
            tokenizer->at += 2;
            while( tokenizer->at[0] && !(tokenizer->at[0] == '*' && tokenizer->at[1] == '/') ) ++tokenizer->at;
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

Token GetToken(Tokenizer* tokenizer) {

    EatWhiteSpace(tokenizer);

    Tokenizer tmp = *tokenizer;
    Token token{};
    token.lenght = 1;

    char c = tokenizer->at[0];
    token.text = tokenizer->at++; 

    switch (c) {
        case '\0'   :token.type = TOKEN_EOF;                break;
        case '('    :token.type = TOKEN_OPEN_PAREN;         break;
        case ')'    :token.type = TOKEN_CLOSE_PAREN;        break;
        case '['    :token.type = TOKEN_OPEN_BRACKET;       break;
        case ']'    :token.type = TOKEN_CLOSE_BRACKET;      break;
        case '{'    :token.type = TOKEN_OPEN_BRACES;        break;
        case '}'    :token.type = TOKEN_CLOSE_BRACES;       break;
        case ','    :token.type = TOKEN_COMA;               break;
        case ':'    :token.type = TOKEN_COLON;              break;
        case ';'    :token.type = TOKEN_SEMICOLON;          break;
        case '*'    :token.type = TOKEN_ASTERISK;           break;
        case '&'    :token.type = TOKEN_AMPERSAND;          break;
        case '!'    :token.type = TOKEN_EXLAMATION_MARK;    break;
        case '~'    :token.type = TOKEN_NEGATION;           break;

        case '+'     :token.type = TOKEN_ADD;               break;
        case '-'     :token.type = TOKEN_SUB;               break;
        case '/'     :token.type = TOKEN_DIV;               break;
        case '='     :token.type = TOKEN_ASSIGNMENT;        break;

        case '"'    :
            {
                token.text = tokenizer->at;
                token.type = TOKEN_STRING_LITERAL;

                while(  tokenizer->at[0] && tokenizer->at[0] != '"') {
                    
                    if(tokenizer->at[0] == '\\' && tokenizer->at[1] ) {
                        ++tokenizer->at;
                    }
                    ++tokenizer->at;
                }
                token.lenght = tokenizer->at - token.text;
                if( tokenizer->at[0] == '"' ) ++tokenizer->at;
                break;
            }

        case '.':
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
            {
                bool e = true;
                bool point = (c == '.');
                token.type = TOKEN_NUMBER_LITERAL;
                while(IsNumeric(tokenizer->at[0]) || tokenizer->at[0] == '.') {

                    if(tokenizer->at[0] == '.' && !point) {
                        point = true;
                    }
                    else if( tokenizer->at[0] == '.' && point && e) {
                        e = false;
                        std::cout << "ERROR: ilformed number literal at line: " << tokenizer->line << std::endl;
                        token.type = TOKEN_UNKNOWN;
                    }

                    tokenizer->at++;
                }

                token.lenght = tokenizer->at - token.text;
                break;
            }
    
        
        default:
            if( IsAlpha(c) ) {
                token.type = TOKEN_IDENTIFIER;
                while( IsAlpha(tokenizer->at[0]) || IsNumeric(tokenizer->at[0]) || tokenizer->at[0] == '_' ) tokenizer->at++;

                token.lenght = tokenizer->at - token.text;
            }
            else {
                token.type = TOKEN_UNKNOWN;
                std::cout << "Error: unknown token at line : " << tokenizer->line << std::endl;
            }
            break;
    }

    return token;
}