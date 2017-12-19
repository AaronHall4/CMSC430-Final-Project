

#include "gc.h"
#include "stdio.h"
#include "stdlib.h"
#include "stdint.h"
#include "string.h"

#define USE_GC 1            // Comment this line to disable garbage collection.
#define MEM_CAP 268435456   // Memory cap of 256 MB.

#define GC_PTR_TAG 0
#define INT_TAG 1
#define STR_TAG 2
#define SYM_TAG 3

#define GC_CONS_TAG 1
#define GC_VECTOR_TAG 2
#define GC_CLO_TAG 3


// Hashes, Sets, gen records, can all be added here


#define V_VOID 39  //32 +7 (+7 is for anything enumerable other than null)
#define V_TRUE 31  //24 +7
#define V_FALSE 15 //8  +7
#define V_NULL 0



#define MASK64 0xffffffffffffffff // useful for tagging related operations

#define ASSERT_TAG(v,tag,msg) \
    if(((v)&7ULL) != (tag)) \
        fatal_err(msg);

#define ASSERT_VALUE(v,val,msg) \
    if(((u64)(v)) != (val))     \
        fatal_err(msg);

#define ASSERT_GC_TAG(v,tag,msg) \
    if((((u64)(v)&7ULL) != GC_PTR_TAG) || ((*((u64*) v)&7ULL) != (tag))) \
        fatal_err(msg);

#define ASSERT_GC_VALUE(v,val,msg) \
    if((((u64)(v)&7ULL) != GC_PTR_TAG) || ((*((u64*) v)) != (val))) \
        fatal_err(msg);

#define GC_TAG_MATCH(v,tag) ((((u64)(v)&7ULL) == GC_PTR_TAG) && ((*((u64*) v)&7ULL) == (tag)))

/*
 * General tagging scheme:
 *  - Raw ints, strings, and symbols are never allocated, so the tagging is normal.
 *  - cons cells, vectors, and other such data structures are always allocated, so they
 *    follow the GC_PTR tagging scheme.
 *  - Anything else that is allocated (including dynamic strings, if I implement them) will
 *    also follow the GC_PTR tagging scheme.
 *
 * GC_PTR tagging scheme:
 *  - Pointers returned by GC_MALLOC (called via alloc) have the tag of all zeros, which, 
 *    conversely, identifies a GC_PTR.
 *  - The pointer itself is only a pointer. An application should request more memory than they
 *    need and use the first i64 to identify the type of the data (and other metadata as desired).
 *  - Calling alloc(m) returns a pointer to m bytes. The first i64 is used to identify the 
 *    type of the data, and the remaining m bytes hold the actual data.
 *
 * There is no need for the OTHER tag anymore, since it has essentially been replaced by proper
 * GC usage.
 */


#define DECODE_INT(v) ((s32)((u32)(((v)&(7ULL^MASK64)) >> 32)))
#define ENCODE_INT(v) ((((u64)((u32)(v))) << 32) | INT_TAG)

#define DECODE_STR(v) ((char*)((v)&(7ULL^MASK64)))
#define ENCODE_STR(v) (((u64)(v)) | STR_TAG)

#define DECODE_SYM(v) ((char*)((v)&(7ULL^MASK64)))
#define ENCODE_SYM(v) (((u64)(v)) | SYM_TAG)


// some apply-prim macros for expecting 1 argument or 2 arguments
#define GEN_EXPECT1ARGLIST(f,g) \
    u64 f(u64 lst) \
    { \
        u64 v0 = expect_args1(lst); \
        return g(v0); \
    } 

#define GEN_EXPECT2ARGLIST(f,g) \
    u64 f(u64 lst) \
    { \
        u64 rest; \
        u64 v0 = expect_cons(lst, &rest); \
        u64 v1 = expect_cons(rest, &rest); \
        if (rest != V_NULL) \
            fatal_err("prim applied on more than 2 arguments."); \
        return g(v0,v1);                                           \
    } 

#define GEN_EXPECT3ARGLIST(f,g) \
    u64 f(u64 lst) \
    { \
        u64 rest; \
        u64 v0 = expect_cons(lst, &rest); \
        u64 v1 = expect_cons(rest, &rest); \
        u64 v2 = expect_cons(rest, &rest); \
        if (rest != V_NULL) \
            fatal_err("prim applied on more than 2 arguments."); \
        return g(v0,v1,v2);                                        \
    } 





// No mangled names
extern "C"
{



typedef uint64_t u64;
typedef int64_t s64;
typedef uint32_t u32;
typedef int32_t s32;

u64 prim_halt(u64);

    
// UTILS

u64* alloc(const u64 m)
{
    static u64 mem_usage = 0;
    mem_usage += m;
    if (mem_usage > MEM_CAP) {
        const char *errstrconst = "fatal error: memory limit exceeded";
        char *errstr = (char *) calloc(1, strlen(errstrconst) + 1);    // Using calloc instead of GC_MALLOC here because it's simpler and
        strcpy(errstr, errstrconst);                                   // the program will terminate anyway.
        prim_halt(ENCODE_STR(errstr));
    }

    #ifdef USE_GC
        return (u64*) GC_MALLOC(m);
    #else
        return (u64*) malloc(m);
    #endif
}

void fatal_err(const char* msg)
{
    printf("library run-time error: ");
    printf("%s", msg);
    printf("\n");
    exit(1);
}

void print_u64(u64 i)
{
    printf("%lu\n", i);
}

u64 expect_args0(u64 args)
{
    if (args != V_NULL)
        fatal_err("Expected value: null (in expect_args0). Prim cannot take arguments.");
    return V_NULL;
}

u64 expect_args1(u64 args)
{
    ASSERT_GC_TAG(args, GC_CONS_TAG, "Expected cons value (in expect_args1). Prim applied on an empty argument list.")
    u64* p = ((u64*) args) + 1;
    ASSERT_VALUE((p[1]), V_NULL, "Expected null value (in expect_args1). Prim can only take 1 argument.")
    return p[0];
}

u64 expect_cons(u64 p, u64* rest)
{
    // pass a pair value p and a pointer to a word *rest                          
    // verifiies (cons? p), returns the value (car p) and assigns *rest = (cdr p) 
    ASSERT_GC_TAG(p, GC_CONS_TAG, "Expected a cons value. (expect_cons)")               

    u64* pp = ((u64*) p) + 1;
    *rest = pp[1];
    return pp[0];
}

/*
u64 expect_other(u64 v, u64* rest)
{
    // returns the runtime tag value
    // puts the untagged value at *rest
    ASSERT_GC_TAG(v, OTHER_TAG, "Expected a vector or special value. (expect_other)")
    
    u64* p = DECODE_OTHER(v);
    *rest = p[1];
    return p[0];
}
*/


/////// CONSTANTS
    
    
u64 const_init_int(s64 i)
{
    return ENCODE_INT((s32)i);
}

u64 const_init_void()
{
    return V_VOID;
}


u64 const_init_null()
{
    return V_NULL;
}


u64 const_init_true()
{
    return V_TRUE;
}

    
u64 const_init_false()
{
    return V_FALSE;
}

    
u64 const_init_string(const char* s)
{
    return ENCODE_STR(s);
}
        
u64 const_init_symbol(const char* s)
{
    return ENCODE_SYM(s);
}







/////////// PRIMS

    
///// effectful prims:

    
u64 prim_print_aux(u64 v) 
{
    if (v == V_NULL)
        printf("()");
    else if (v == V_VOID)
        printf("#<void>");
    else if (v == V_TRUE)
        printf("#t");
    else if (v == V_FALSE)
        printf("#f");
    else if ((v&7) == INT_TAG)
    {
        printf("%d", (int)((s32)(v >> 32)));
    }
    else if ((v&7) == STR_TAG)
    {   // needs to handle escaping to be correct
        printf("\"%s\"", DECODE_STR(v));
    }
    else if ((v&7) == SYM_TAG)
    {   // needs to handle escaping to be correct
        printf("%s", DECODE_SYM(v));
    }
    else if ((v&7) == GC_PTR_TAG)
    {
        u64 vinnertag = *((u64*) v) & 7;
        if (vinnertag == GC_CONS_TAG)
        {
            u64* p = ((u64*) v) + 1;
            printf("(");
            prim_print_aux(p[0]);
            printf(" . ");
            prim_print_aux(p[1]);
            printf(")");
        }
        else if (vinnertag == GC_VECTOR_TAG)
        {
            printf("#(");
            u64* vec = (u64*) v;
            u64 len = vec[0] >> 3;
            prim_print_aux(vec[1]);
            for (u64 i = 2; i <= len; ++i)
            {
                printf(" ");
                prim_print_aux(vec[i]);
            }
            printf(")");
        }
        else if (vinnertag == GC_CLO_TAG)
        {
            printf("#<procedure>");
        }
    }
    else
        printf("(print.. v); unrecognized value %lu", v);
    //...
    return V_VOID; 
}

u64 prim_print(u64 v) 
{
    if (v == V_NULL)
        printf("'()");
    else if (v == V_VOID)
        printf("#<void>");
    else if (v == V_TRUE)
        printf("#t");
    else if (v == V_FALSE)
        printf("#f");
    else if ((v&7) == INT_TAG)
    {
        printf("%d", (int)((s32)(v >> 32)));
    }
    else if ((v&7) == STR_TAG)
    {   // needs to handle escaping to be correct
        printf("\"%s\"", DECODE_STR(v));
    }
    else if ((v&7) == SYM_TAG)
    {   // needs to handle escaping to be correct
        printf("'%s", DECODE_SYM(v));
    }
    else if ((v&7) == GC_PTR_TAG)
    {
        u64 vinnertag = *((u64*) v) & 7;
        if (vinnertag == GC_CONS_TAG)
        {
            u64* p = ((u64*) v) + 1;
            printf("'(");
            prim_print_aux(p[0]);
            printf(" . ");
            prim_print_aux(p[1]);
            printf(")");
        }
        else if (vinnertag == GC_VECTOR_TAG)
        {
            printf("#(");
            u64* vec = (u64*) v;
            u64 len = vec[0] >> 3;
            prim_print(vec[1]);
            for (u64 i = 2; i <= len; ++i)
            {
                printf(" ");
                prim_print_aux(vec[i]);
            }
            printf(")");
        }
        else if (vinnertag == GC_CLO_TAG)
        {
            printf("#<procedure>");
        }
    }
    else
        printf("(print v); unrecognized value %lu", v);
    //...
    return V_VOID; 
}
GEN_EXPECT1ARGLIST(applyprim_print,prim_print)


u64 prim_halt(u64 v) // halt
{
    prim_print(v); // display the final value
    printf("\n");
    exit(0);
    return V_NULL; 
}


u64 applyprim_vector(u64 lst)
{
    u64* buffer = (u64*)malloc(512*sizeof(u64));
    u64 l = 0;
    while (l < 512) {
        if (lst == 0ULL)
            break;
        else if (GC_TAG_MATCH(lst, GC_CONS_TAG)) {
            u64* pp = ((u64*) lst) + 1;
            buffer[l++] = pp[0];
            lst = pp[1];
        } else {
            break;
        }
    }
    u64* mem = alloc((l + 1) * sizeof(u64));
    mem[0] = (l << 3) | GC_VECTOR_TAG;
    for (u64 i = 0; i < l; ++i)
        mem[i+1] = buffer[i];
    delete [] buffer;
    return (u64) mem;
}


u64 prim_make_45vector(u64 lenv, u64 iv)
{
    ASSERT_TAG(lenv, INT_TAG, "first argument to make-vector must be an integer")
    
    const u64 l = DECODE_INT(lenv);
    u64* vec = (u64*)alloc((l + 1) * sizeof(u64));
    vec[0] = (l << 3) | GC_VECTOR_TAG;
    for (u64 i = 1; i <= l; ++i)
        vec[i] = iv;
    return (u64) vec;
}
GEN_EXPECT2ARGLIST(applyprim_make_45vector, prim_make_45vector)


u64 prim_vector_45ref(u64 v, u64 i)
{
    ASSERT_TAG(i, INT_TAG, "second argument to vector-ref must be an integer")
    ASSERT_GC_TAG(v, GC_VECTOR_TAG, "first argument to vector-ref must be a vector")

    return ((u64*) v)[1+(DECODE_INT(i))];
}
GEN_EXPECT2ARGLIST(applyprim_vector_45ref, prim_vector_45ref)


u64 prim_vector_45set_33(u64 a, u64 i, u64 v)
{
    ASSERT_TAG(i, INT_TAG, "second argument to vector-ref must be an integer")
    ASSERT_GC_TAG(a, GC_VECTOR_TAG, "first argument to vector-ref must be a vector")

    ((u64*) a)[1+DECODE_INT(i)] = v;
        
    return V_VOID;
}
GEN_EXPECT3ARGLIST(applyprim_vector_45set_33, prim_vector_45set_33)

u64 prim_vector_45length(u64 a)
{
    ASSERT_GC_TAG(a, GC_VECTOR_TAG, "first argument to vector-length must be a vector")
    
    return ENCODE_INT(((u64*) a)[0] >> 3);
}
GEN_EXPECT1ARGLIST(applyprim_vector_45length, prim_vector_45length)

u64 prim_vector_63(u64 a)
{
    if ((a & 7) == GC_PTR_TAG) {
        if ((((u64*) a)[0] & 7) == GC_VECTOR_TAG)
            return V_TRUE;
        else
            return V_FALSE;
    } else {
        return V_FALSE;
    }
}
GEN_EXPECT1ARGLIST(applyprim_vector_63, prim_vector_63)


///// void, ...

    
u64 prim_void()
{
    return V_VOID;
}


    



///// eq?, eqv?, equal?
///// TODO: Polish these
    
u64 prim_eq_63(u64 a, u64 b)
{
    if (a == b)
        return V_TRUE;
    else
        return V_FALSE;
}
GEN_EXPECT2ARGLIST(applyprim_eq_63, prim_eq_63)


u64 prim_eqv_63(u64 a, u64 b)
{
    if (a == b)
        return V_TRUE;
    //else if  // optional extra logic, see r7rs reference
    else
        return V_FALSE;
}
GEN_EXPECT2ARGLIST(applyprim_eqv_63, prim_eqv_63)


u64 prim_equal_63(u64 a, u64 b)
{
    if (a == b)
        return V_TRUE;
    else
        return V_FALSE;
}
GEN_EXPECT2ARGLIST(applyprim_equal_63, prim_equal_63)


///// Other predicates


u64 prim_number_63(u64 a)
{
    // We assume that ints are the only number
    if ((a&7) == INT_TAG)
        return V_TRUE;
    else
        return V_FALSE;
}
GEN_EXPECT1ARGLIST(applyprim_number_63, prim_number_63)


u64 prim_integer_63(u64 a)
{
    if ((a&7) == INT_TAG)
        return V_TRUE;
    else
        return V_FALSE;
}
GEN_EXPECT1ARGLIST(applyprim_integer_63, prim_integer_63)


u64 prim_void_63(u64 a)
{
    if (a == V_VOID)
        return V_TRUE;
    else
        return V_FALSE;
}
GEN_EXPECT1ARGLIST(applyprim_void_63, prim_void_63)


u64 prim_procedure_63(u64 a)
{
    if ((a & 7) == GC_PTR_TAG) {
        if ((((u64*) a)[0] & 7) == GC_CLO_TAG)
            return V_TRUE;
        else
            return V_FALSE;
    } else {
        return V_FALSE;
    }
}
GEN_EXPECT1ARGLIST(applyprim_procedure_63, prim_procedure_63)


/*
 * Terminate if the argument is not a closure. Ensures that we only
 * apply functions.
 */
void assert_is_clo(u64 a) {
    if (((a & 7) != GC_PTR_TAG) || ((((u64*) a)[0] & 7) != GC_CLO_TAG)) {
        const char *errstrconst = "fatal error: cannot apply non-procedure value";
        char *errstr = (char *) calloc(1, strlen(errstrconst) + 1);    // Using calloc instead of GC_MALLOC here because it's simpler and
        strcpy(errstr, errstrconst);                                   // the program will terminate anyway.
        prim_halt(ENCODE_STR(errstr));
    }
}


///// null?, cons?, cons, car, cdr


u64 prim_null_63(u64 p) // null?
{
    if (p == V_NULL)
        return V_TRUE;
    else
        return V_FALSE;
}
GEN_EXPECT1ARGLIST(applyprim_null_63, prim_null_63)    


u64 prim_cons_63(u64 p) // cons?
{
    if ((p & 7) == GC_PTR_TAG) {
        if ((((u64*) p)[0] & 7) == GC_CONS_TAG)
            return V_TRUE;
        else
            return V_FALSE;
    } else {
        return V_FALSE;
    }
}
GEN_EXPECT1ARGLIST(applyprim_cons_63, prim_cons_63)    


u64 prim_cons(u64 a, u64 b)
{
    u64* p = alloc(3*sizeof(u64));
    p[0] = GC_CONS_TAG;
    p[1] = a;
    p[2] = b;
    return (u64) p;
}
GEN_EXPECT2ARGLIST(applyprim_cons, prim_cons)


u64 prim_car(u64 p)
{
    u64 rest;
    u64 v0 = expect_cons(p,&rest);
    
    return v0;
}
GEN_EXPECT1ARGLIST(applyprim_car, prim_car)


u64 prim_cdr(u64 p)
{
    u64 rest;
    u64 v0 = expect_cons(p,&rest);
    
    return rest;
}
GEN_EXPECT1ARGLIST(applyprim_cdr, prim_cdr)


///// s32 prims, +, -, *, =, ...

    
u64 prim__43(u64 a, u64 b) // +
{
    ASSERT_TAG(a, INT_TAG, "(prim + a b); a is not an integer")
    ASSERT_TAG(b, INT_TAG, "(prim + a b); b is not an integer")

        //printf("sum: %d\n", DECODE_INT(a) + DECODE_INT(b));
    
    return ENCODE_INT(DECODE_INT(a) + DECODE_INT(b));
}

u64 applyprim__43(u64 p)
{
    if (p == V_NULL)
        return ENCODE_INT(0);
    else
    {
        ASSERT_GC_TAG(p, GC_CONS_TAG, "Tried to apply + on non list value.")
        u64* pp = ((u64*) p) + 1;
        return ENCODE_INT(DECODE_INT(pp[0]) + DECODE_INT(applyprim__43(pp[1])));
    }
}
    
u64 prim__45(u64 a, u64 b) // -
{
    ASSERT_TAG(a, INT_TAG, "(prim - a b); a is not an integer")
    ASSERT_TAG(b, INT_TAG, "(prim - a b); b is not an integer")
    
    return ENCODE_INT(DECODE_INT(a) - DECODE_INT(b));
}

u64 applyprim__45(u64 p)
{
    if (p == V_NULL)
        return ENCODE_INT(0);
    else
    {
        ASSERT_GC_TAG(p, GC_CONS_TAG, "Tried to apply - on non list value.")
        u64* pp = ((u64*) p) + 1;
        if (pp[1] == V_NULL)
            return ENCODE_INT(0 - DECODE_INT(pp[0]));
        else // ideally would be properly left-to-right
            return ENCODE_INT(DECODE_INT(pp[0]) - DECODE_INT(applyprim__43(pp[1])));
    }
}
    
u64 prim__42(u64 a, u64 b) // *
{
    ASSERT_TAG(a, INT_TAG, "(prim * a b); a is not an integer")
    ASSERT_TAG(b, INT_TAG, "(prim * a b); b is not an integer")
    
    return ENCODE_INT(DECODE_INT(a) * DECODE_INT(b));
}

u64 applyprim__42(u64 p)
{
    if (p == V_NULL)
        return ENCODE_INT(1);
    else
    {
        ASSERT_GC_TAG(p, GC_CONS_TAG, "Tried to apply + on non list value.")
        u64* pp = ((u64*) p) + 1;
        return ENCODE_INT(DECODE_INT(pp[0]) * DECODE_INT(applyprim__42(pp[1])));
    }
}
    
u64 prim__47(u64 a, u64 b) // /
{
    ASSERT_TAG(a, INT_TAG, "(prim / a b); a is not an integer")
    ASSERT_TAG(b, INT_TAG, "(prim / a b); b is not an integer")

    if (DECODE_INT(b) == 0) {
        const char *errstrconst = "fatal error: division by zero";
        char *errstr = (char *) calloc(1, strlen(errstrconst) + 1);    // Using calloc instead of GC_MALLOC here because it's simpler and
        strcpy(errstr, errstrconst);                                   // the program will terminate anyway.
        prim_halt(ENCODE_STR(errstr));
    }
    
    return ENCODE_INT(DECODE_INT(a) / DECODE_INT(b));
}
    
u64 prim__61(u64 a, u64 b)  // =
{
    ASSERT_TAG(a, INT_TAG, "(prim = a b); a is not an integer")
    ASSERT_TAG(b, INT_TAG, "(prim = a b); b is not an integer")
        
    if (DECODE_INT(a) == DECODE_INT(b))
        return V_TRUE;
    else
        return V_FALSE;
}

u64 prim__60(u64 a, u64 b) // <
{
    ASSERT_TAG(a, INT_TAG, "(prim < a b); a is not an integer")
    ASSERT_TAG(b, INT_TAG, "(prim < a b); b is not an integer")
    
    if (DECODE_INT(a) < DECODE_INT(b))
        return V_TRUE;
    else
        return V_FALSE;
}
    
u64 prim__60_61(u64 a, u64 b) // <=
{
    ASSERT_TAG(a, INT_TAG, "(prim <= a b); a is not an integer")
    ASSERT_TAG(b, INT_TAG, "(prim <= a b); b is not an integer")
        
    if (DECODE_INT(a) <= DECODE_INT(b))
        return V_TRUE;
    else
        return V_FALSE;
}

u64 prim_not(u64 a) 
{
    if (a == V_FALSE)
        return V_TRUE;
    else
        return V_FALSE;
}
GEN_EXPECT1ARGLIST(applyprim_not, prim_not)



}




