// VEBG4C
//
// A C port of the core VEBG4 interpreter without the parser.
// As such, it uses the ASTs for VEBG4 since C has no Sexps.
//
// However, some of this can be mitigated through the use of
// macros. This also makes reading the testcases easier to read.
// The issue at that point would be that it becomes more difficult
// to debug.

#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <setjmp.h>
#include <math.h>



// Data Definitions
typedef struct Expr Expr;
typedef struct Value Value;
typedef struct Env Env;

typedef enum {
    EXPR_NUM,
    EXPR_STR,
    EXPR_ID,
    EXPR_IF,
    EXPR_LAM,
    EXPR_APP
} ExprTag;

typedef enum {
    VAL_NUM,
    VAL_BOOL,
    VAL_STR,
    VAL_CLO,
    VAL_PRIM
} ValueTag;

typedef enum {
    PRIM_ADD,
    PRIM_SUB,
    PRIM_MUL,
    PRIM_DIV,
    PRIM_LEQ,
    PRIM_SUBSTRING,
    PRIM_STRLEN,
    PRIM_EQUAL,
    PRIM_ERROR
} PrimName;

struct Expr {
    ExprTag tag;
    union {
        double num;
        char *str;
        char *id;
        struct {
            Expr *test;
            Expr *thn;
            Expr *els;
        } ifx;
        struct {
            size_t param_count;
            char **params;
            Expr *body;
        } lam;
        struct {
            Expr *fun;
            size_t arg_count;
            Expr **args;
        } app;
    } as;
};

struct Value {
    ValueTag tag;
    union {
        double num;
        bool boolean;
        char *str;
        struct {
            size_t param_count;
            char **params;
            Expr *body;
            Env *env;
        } clo;
        PrimName prim;
    } as;
};

struct Env {
    char *name;
    Value *val;
    Env *next;
};

typedef struct {
    bool ok;
    char *text;
} RunResult;



// Error handling
static jmp_buf *current_error_handler = NULL;
static char last_error[2048];

static void vebg_error(const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    vsnprintf(last_error, sizeof(last_error), fmt, args);
    va_end(args);

    if (current_error_handler != NULL) {
        longjmp(*current_error_handler, 1);
    }

    fprintf(stderr, "%s\n", last_error);
    exit(1);
}



// Utility functions
static char *xstrdup(const char *s) {
    size_t n = strlen(s) + 1;
    char *copy = malloc(n);
    if (copy == NULL) {
        fprintf(stderr, "out of memory\n");
        exit(1);
    }
    memcpy(copy, s, n);
    return copy;
}

static void *xmalloc(size_t size) {
    void *p = malloc(size);
    if (p == NULL) {
        fprintf(stderr, "out of memory\n");
        exit(1);
    }
    return p;
}

static const char *prim_name(PrimName prim) {
    switch (prim) {
        case PRIM_ADD: return "+";
        case PRIM_SUB: return "-";
        case PRIM_MUL: return "*";
        case PRIM_DIV: return "/";
        case PRIM_LEQ: return "<=";
        case PRIM_SUBSTRING: return "substring";
        case PRIM_STRLEN: return "strlen";
        case PRIM_EQUAL: return "equal?";
        case PRIM_ERROR: return "error";
    }
    return "unknown";
}



// AST Node constructors
static Expr *num_c(double n) {
    Expr *e = xmalloc(sizeof(Expr));
    e->tag = EXPR_NUM;
    e->as.num = n;
    return e;
}

static Expr *str_c(const char *s) {
    Expr *e = xmalloc(sizeof(Expr));
    e->tag = EXPR_STR;
    e->as.str = xstrdup(s);
    return e;
}

static Expr *id_c(const char *name) {
    Expr *e = xmalloc(sizeof(Expr));
    e->tag = EXPR_ID;
    e->as.id = xstrdup(name);
    return e;
}

static Expr *if_c(Expr *test, Expr *thn, Expr *els) {
    Expr *e = xmalloc(sizeof(Expr));
    e->tag = EXPR_IF;
    e->as.ifx.test = test;
    e->as.ifx.thn = thn;
    e->as.ifx.els = els;
    return e;
}

static Expr *lam_c(size_t param_count, const char **params, Expr *body) {
    Expr *e = xmalloc(sizeof(Expr));
    e->tag = EXPR_LAM;
    e->as.lam.param_count = param_count;
    e->as.lam.params = xmalloc(sizeof(char *) * param_count);
    for (size_t i = 0; i < param_count; i++) {
        e->as.lam.params[i] = xstrdup(params[i]);
    }
    e->as.lam.body = body;
    return e;
}

static Expr *app_c(Expr *fun, size_t arg_count, ...) {
    Expr *e = xmalloc(sizeof(Expr));
    e->tag = EXPR_APP;
    e->as.app.fun = fun;
    e->as.app.arg_count = arg_count;
    e->as.app.args = xmalloc(sizeof(Expr *) * arg_count);

    va_list args;
    va_start(args, arg_count);
    for (size_t i = 0; i < arg_count; i++) {
        e->as.app.args[i] = va_arg(args, Expr *);
    }
    va_end(args);

    return e;
}



// Value and Environment constructors
static Value *num_v(double n) {
    Value *v = xmalloc(sizeof(Value));
    v->tag = VAL_NUM;
    v->as.num = n;
    return v;
}

static Value *bool_v(bool b) {
    Value *v = xmalloc(sizeof(Value));
    v->tag = VAL_BOOL;
    v->as.boolean = b;
    return v;
}

static Value *str_v(const char *s) {
    Value *v = xmalloc(sizeof(Value));
    v->tag = VAL_STR;
    v->as.str = xstrdup(s);
    return v;
}

static Value *prim_v(PrimName prim) {
    Value *v = xmalloc(sizeof(Value));
    v->tag = VAL_PRIM;
    v->as.prim = prim;
    return v;
}

static Value *clo_v(size_t param_count, char **params, Expr *body, Env *env) {
    Value *v = xmalloc(sizeof(Value));
    v->tag = VAL_CLO;
    v->as.clo.param_count = param_count;
    v->as.clo.params = xmalloc(sizeof(char *) * param_count);
    for (size_t i = 0; i < param_count; i++) {
        v->as.clo.params[i] = xstrdup(params[i]);
    }
    v->as.clo.body = body;
    v->as.clo.env = env;
    return v;
}

static Env *bind_env(const char *name, Value *val, Env *next) {
    Env *env = xmalloc(sizeof(Env));
    env->name = xstrdup(name);
    env->val = val;
    env->next = next;
    return env;
}



// top-env
// Uses macros to improve readability
#define BIND_VAL(env, name, value) bind_env((name), (value), (env))
#define BIND_BOOL(env, name, value) BIND_VAL((env), (name), bool_v((value)))
#define BIND_PRIM(env, name, prim) BIND_VAL((env), (name), prim_v((prim)))

static Env *top_env(void) {
    Env *env = NULL;

    env = BIND_BOOL(env, "false", false);
    env = BIND_BOOL(env, "true", true);

    env = BIND_PRIM(env, "error", PRIM_ERROR);
    env = BIND_PRIM(env, "equal?", PRIM_EQUAL);
    env = BIND_PRIM(env, "strlen", PRIM_STRLEN);
    env = BIND_PRIM(env, "substring", PRIM_SUBSTRING);
    env = BIND_PRIM(env, "<=", PRIM_LEQ);
    env = BIND_PRIM(env, "/", PRIM_DIV);
    env = BIND_PRIM(env, "*", PRIM_MUL);
    env = BIND_PRIM(env, "-", PRIM_SUB);
    env = BIND_PRIM(env, "+", PRIM_ADD);

    return env;
}



// Interp Helpers
// Uses a macro for arity checking. Makes things much more readable.
#define REQUIRE_ARITY(name, expected, got) \
    do { \
        if ((got) != (expected)) { \
            arity_error((name), (expected), (got)); \
        } \
    } while (0)

static char *serialize(Value *v);
static Value *interp(Expr *expr, Env *env);
static Value *apply_value(Value *fun_val, size_t arg_count, Value **arg_vals);
static Value *apply_prim(PrimName prim, size_t arg_count, Value **args);

static Value *lookup(const char *name, Env *env) {
    for (Env *cur = env; cur != NULL; cur = cur->next) {
        if (strcmp(name, cur->name) == 0) {
            return cur->val;
        }
    }
    vebg_error("VEBG4 unbound identifier: %s", name);
    return NULL;
}

static Env *extend_env_many(size_t count, char **names, Value **vals, Env *env) {
    // Add from right to left so lookup sees names in source order.
    Env *result = env;
    for (size_t i = count; i > 0; i--) {
        size_t idx = i - 1;
        result = bind_env(names[idx], vals[idx], result);
    }
    return result;
}

static double num_value(Value *v, const char *who) {
    if (v->tag == VAL_NUM) {
        return v->as.num;
    }
    char *actual = serialize(v);
    vebg_error("VEBG4 primitive %s expected number, got: %s", who, actual);
    return 0.0;
}

static const char *str_value(Value *v, const char *who) {
    if (v->tag == VAL_STR) {
        return v->as.str;
    }
    char *actual = serialize(v);
    vebg_error("VEBG4 primitive %s expected string, got: %s", who, actual);
    return NULL;
}

static size_t natural_value(Value *v, const char *who) {
    if (v->tag != VAL_NUM) {
        char *actual = serialize(v);
        vebg_error("VEBG4 primitive %s expected natural number, got: %s", who, actual);
    }

    double n = v->as.num;
    if (n < 0 || floor(n) != n) {
        char *actual = serialize(v);
        vebg_error("VEBG4 primitive %s expected natural number, got: %s", who, actual);
    }

    return (size_t)n;
}

static void arity_error(const char *name, size_t expected, size_t got) {
    vebg_error("VEBG4 wrong arity for primitive %s: expected %zu args, got %zu",
               name, expected, got);
}

static bool vebg_equal(Value *a, Value *b) {
    if (a->tag != b->tag) {
        return false;
    }

    switch (a->tag) {
        case VAL_NUM:
            return a->as.num == b->as.num;
        case VAL_BOOL:
            return a->as.boolean == b->as.boolean;
        case VAL_STR:
            return strcmp(a->as.str, b->as.str) == 0;
        case VAL_CLO:
        case VAL_PRIM:
            return false;
    }

    return false;
}

static char *serialize_string_literal(const char *s) {
    size_t cap = strlen(s) * 2 + 3;
    char *out = xmalloc(cap);
    size_t j = 0;
    out[j++] = '"';

    for (size_t i = 0; s[i] != '\0'; i++) {
        if (j + 3 >= cap) {
            cap *= 2;
            char *grown = realloc(out, cap);
            if (grown == NULL) {
                fprintf(stderr, "out of memory\n");
                exit(1);
            }
            out = grown;
        }

        if (s[i] == '"' || s[i] == '\\') {
            out[j++] = '\\';
            out[j++] = s[i];
        } else if (s[i] == '\n') {
            out[j++] = '\\';
            out[j++] = 'n';
        } else {
            out[j++] = s[i];
        }
    }

    out[j++] = '"';
    out[j] = '\0';
    return out;
}

static char *serialize(Value *v) {
    char buffer[128];

    switch (v->tag) {
        case VAL_NUM:
            snprintf(buffer, sizeof(buffer), "%.15g", v->as.num);
            return xstrdup(buffer);
        case VAL_BOOL:
            return xstrdup(v->as.boolean ? "true" : "false");
        case VAL_STR:
            return serialize_string_literal(v->as.str);
        case VAL_CLO:
            return xstrdup("#<procedure>");
        case VAL_PRIM:
            return xstrdup("#<primop>");
    }

    return xstrdup("#<unknown>");
}



// Interpreter
static Value *interp(Expr *expr, Env *env) {
    switch (expr->tag) {
        case EXPR_NUM:
            return num_v(expr->as.num);

        case EXPR_STR:
            return str_v(expr->as.str);

        case EXPR_ID:
            return lookup(expr->as.id, env);

        case EXPR_IF: {
            Value *test_val = interp(expr->as.ifx.test, env);
            if (test_val->tag != VAL_BOOL) {
                char *actual = serialize(test_val);
                vebg_error("VEBG4 if expected boolean test, got: %s", actual);
            }
            if (test_val->as.boolean) {
                return interp(expr->as.ifx.thn, env);
            }
            return interp(expr->as.ifx.els, env);
        }

        case EXPR_LAM:
            return clo_v(expr->as.lam.param_count,
                         expr->as.lam.params,
                         expr->as.lam.body,
                         env);

        case EXPR_APP: {
            Value *fun_val = interp(expr->as.app.fun, env);
            size_t n = expr->as.app.arg_count;
            Value **arg_vals = xmalloc(sizeof(Value *) * n);
            for (size_t i = 0; i < n; i++) {
                arg_vals[i] = interp(expr->as.app.args[i], env);
            }
            return apply_value(fun_val, n, arg_vals);
        }
    }

    vebg_error("VEBG4 internal error: unknown expression tag");
    return NULL;
}

static Value *apply_value(Value *fun_val, size_t arg_count, Value **arg_vals) {
    if (fun_val->tag == VAL_CLO) {
        if (fun_val->as.clo.param_count != arg_count) {
            vebg_error("VEBG4 wrong arity for function: expected %zu args, got %zu",
                       fun_val->as.clo.param_count, arg_count);
        }
        Env *new_env = extend_env_many(arg_count,
                                       fun_val->as.clo.params,
                                       arg_vals,
                                       fun_val->as.clo.env);
        return interp(fun_val->as.clo.body, new_env);
    }

    if (fun_val->tag == VAL_PRIM) {
        return apply_prim(fun_val->as.prim, arg_count, arg_vals);
    }

    char *actual = serialize(fun_val);
    vebg_error("VEBG4 tried to apply a non-function value, got: %s", actual);
    return NULL;
}

static Value *num_binop(PrimName prim, size_t arg_count, Value **args,
                        double (*op)(double, double)) {
    const char *name = prim_name(prim);
    REQUIRE_ARITY(name, 2, arg_count);
    return num_v(op(num_value(args[0], name), num_value(args[1], name)));
}

static double add_op(double a, double b) { return a + b; }
static double sub_op(double a, double b) { return a - b; }
static double mul_op(double a, double b) { return a * b; }

static Value *apply_prim(PrimName prim, size_t arg_count, Value **args) {
    switch (prim) {
        case PRIM_ADD:
            return num_binop(prim, arg_count, args, add_op);
        case PRIM_SUB:
            return num_binop(prim, arg_count, args, sub_op);
        case PRIM_MUL:
            return num_binop(prim, arg_count, args, mul_op);
        case PRIM_DIV: {
            REQUIRE_ARITY("/", 2, arg_count);

            double left = num_value(args[0], "/");
            double right = num_value(args[1], "/");
            if (right == 0.0) {
                vebg_error("VEBG4 division by zero in /");
            }
            return num_v(left / right);
        }
        case PRIM_LEQ: {
            REQUIRE_ARITY("<=", 2, arg_count);
            return bool_v(num_value(args[0], "<=") <= num_value(args[1], "<="));
        }
        case PRIM_SUBSTRING: {
            REQUIRE_ARITY("substring", 3, arg_count);
            const char *s = str_value(args[0], "substring");
            size_t start = natural_value(args[1], "substring");
            size_t stop = natural_value(args[2], "substring");
            size_t len = strlen(s);

            if (start > len) {
                vebg_error("VEBG4 substring start index out of range");
            }
            if (stop > len) {
                vebg_error("VEBG4 substring stop index out of range");
            }
            if (start > stop) {
                vebg_error("VEBG4 substring stop index before start index");
            }

            size_t out_len = stop - start;
            char *out = xmalloc(out_len + 1);
            memcpy(out, s + start, out_len);
            out[out_len] = '\0';
            Value *result = str_v(out);
            return result;
        }
        case PRIM_STRLEN: {
            REQUIRE_ARITY("strlen", 1, arg_count);
            return num_v((double)strlen(str_value(args[0], "strlen")));
        }
        case PRIM_EQUAL: {
            REQUIRE_ARITY("equal?", 2, arg_count);
            return bool_v(vebg_equal(args[0], args[1]));
        }
        case PRIM_ERROR: {
            REQUIRE_ARITY("error", 1, arg_count);
            char *msg = serialize(args[0]);
            vebg_error("VEBG4 user-error: %s", msg);
        }
    }

    vebg_error("VEBG4 unknown primitive operator");
    return NULL;
}

static RunResult top_interp_expr(Expr *expr) {
    jmp_buf handler;
    jmp_buf *old_handler = current_error_handler;
    current_error_handler = &handler;

    if (setjmp(handler) == 0) {
        Env *env = top_env();
        Value *v = interp(expr, env);
        char *text = serialize(v);
        current_error_handler = old_handler;
        return (RunResult){ true, text };
    }

    current_error_handler = old_handler;
    return (RunResult){ false, xstrdup(last_error) };
}



// The VEBG4 Macro Layer
// These macros are designed to make writing tests more concise and readable, at the cost of
// some complexity in the macro definitions themselves. 
#define VEBG_NARGS_IMPL(_0, _1, _2, _3, _4, _5, _6, _7, _8, N, ...) N
#define VEBG_NARGS(...) VEBG_NARGS_IMPL(dummy, __VA_ARGS__, 8, 7, 6, 5, 4, 3, 2, 1, 0)

// Structs for param and binding lists used in the macros
typedef struct {
    size_t count;
    const char **names;
} ParamList;

typedef struct {
    const char *name;
    Expr *rhs;
} Binding;

typedef struct {
    size_t count;
    Binding *items;
} BindingList;

// Macro-based constructors
static Expr *app_list_c(Expr *fun, size_t arg_count, Expr **args) {
    Expr *e = xmalloc(sizeof(Expr));
    e->tag = EXPR_APP;
    e->as.app.fun = fun;
    e->as.app.arg_count = arg_count;
    e->as.app.args = xmalloc(sizeof(Expr *) * arg_count);

    for (size_t i = 0; i < arg_count; i++) {
        e->as.app.args[i] = args[i];
    }

    return e;
}

static Expr *fn_c(ParamList params, Expr *body) {
    return lam_c(params.count, params.names, body);
}

static Expr *given_c(BindingList bindings, Expr *body) {
    const char **params = xmalloc(sizeof(char *) * bindings.count);
    Expr **rhses = xmalloc(sizeof(Expr *) * bindings.count);

    for (size_t i = 0; i < bindings.count; i++) {
        params[i] = bindings.items[i].name;
        rhses[i] = bindings.items[i].rhs;
    }

    return app_list_c(lam_c(bindings.count, params, body),
                      bindings.count,
                      rhses);
}

#define CHECK(name, expr, expected) check_eval((name), (expr), (expected))
#define CHECK_ERR(name, expr, needle) check_error_contains((name), (expr), (needle))

#define N(n) num_c((n))
#define S(s) str_c((s))
#define ID(name) id_c((name))

#define TRUE_ ID("true")
#define FALSE_ ID("false")

#define IF_(test, thn, els) if_c((test), (thn), (els))

#define APP(fun, ...) app_c((fun), VEBG_NARGS(__VA_ARGS__), __VA_ARGS__)

#define PARAMS(...) ((ParamList){ VEBG_NARGS(__VA_ARGS__), (const char *[]){ __VA_ARGS__ } })
#define FN(params, body) fn_c((params), (body))

#define BIND(name, rhs) ((Binding){ (name), (rhs) })
#define BINDS(...) ((BindingList){ VEBG_NARGS(__VA_ARGS__), (Binding[]){ __VA_ARGS__ } })
#define GIVEN(bindings, body) given_c((bindings), (body))

#define ADD(a, b) APP(ID("+"), (a), (b))
#define SUB(a, b) APP(ID("-"), (a), (b))
#define MUL(a, b) APP(ID("*"), (a), (b))
#define DIV(a, b) APP(ID("/"), (a), (b))
#define LEQ(a, b) APP(ID("<="), (a), (b))
#define STRLEN_(s) APP(ID("strlen"), (s))
#define SUBSTRING(s, start, stop) APP(ID("substring"), (s), (start), (stop))
#define EQUAL(a, b) APP(ID("equal?"), (a), (b))
#define USER_ERROR(msg) APP(ID("error"), (msg))



// Tests
static int tests_run = 0;
static int tests_failed = 0;

static void check_eval(const char *name, Expr *expr, const char *expected) {
    tests_run++;
    RunResult r = top_interp_expr(expr);
    if (!r.ok) {
        tests_failed++;
        printf("FAIL %-35s expected %s, but got error: %s\n", name, expected, r.text);
        return;
    }
    if (strcmp(r.text, expected) != 0) {
        tests_failed++;
        printf("FAIL %-35s expected %s, got %s\n", name, expected, r.text);
        return;
    }
    printf("pass %-35s => %s\n", name, r.text);
}

static void check_error_contains(const char *name, Expr *expr, const char *needle) {
    tests_run++;
    RunResult r = top_interp_expr(expr);
    if (r.ok) {
        tests_failed++;
        printf("FAIL %-35s expected error containing %s, got value %s\n", name, needle, r.text);
        return;
    }
    if (strstr(r.text, needle) == NULL) {
        tests_failed++;
        printf("FAIL %-35s expected error containing %s, got error %s\n", name, needle, r.text);
        return;
    }
    printf("pass %-35s => error contains %s\n", name, needle);
}

int main(void) {
    CHECK("number", N(5), "5");
    CHECK("string", S("hi"), "\"hi\"");
    CHECK("true id", TRUE_, "true");
    CHECK("false id", FALSE_, "false");

    CHECK("+", ADD(N(1), N(2)), "3");
    CHECK("-", SUB(N(10), N(3)), "7");
    CHECK("*", MUL(N(4), N(5)), "20");
    CHECK("/", DIV(N(20), N(4)), "5");
    CHECK("<= true", LEQ(N(1), N(2)), "true");
    CHECK("<= false", LEQ(N(3), N(2)), "false");

    CHECK("if true",
          IF_(TRUE_, N(7), N(9)),
          "7");

    CHECK("if false",
          IF_(FALSE_, N(7), N(9)),
          "9");

    CHECK("if computed test",
          IF_(LEQ(N(1), N(2)), S("yes"), S("no")),
          "\"yes\"");

    CHECK("strlen",
          STRLEN_(S("hello")),
          "5");

    CHECK("substring",
          SUBSTRING(S("hello"), N(1), N(4)),
          "\"ell\"");

    CHECK("empty substring",
          SUBSTRING(S("hello"), N(1), N(1)),
          "\"\"");

    CHECK("equal? nums true",
          EQUAL(N(1), N(1)),
          "true");

    CHECK("equal? nums false",
          EQUAL(N(1), N(2)),
          "false");

    CHECK("equal? strings",
          EQUAL(S("hi"), S("hi")),
          "true");

    CHECK("equal? mixed",
          EQUAL(N(1), S("1")),
          "false");

    CHECK("equal? primitive",
          EQUAL(ID("+"), ID("+")),
          "false");

    CHECK("one-arg function",
          APP(FN(PARAMS("x"), ADD(ID("x"), N(1))),
              N(10)),
          "11");

    CHECK("two-arg function",
          APP(FN(PARAMS("x", "y"), ADD(ID("x"), ID("y"))),
              N(3),
              N(4)),
          "7");

    CHECK("higher-order function",
          APP(FN(PARAMS("f", "x"), APP(ID("f"), ID("x"))),
              FN(PARAMS("y"), ADD(ID("y"), N(1))),
              N(10)),
          "11");

    CHECK("function from if",
          APP(IF_(TRUE_, ID("+"), ID("-")),
              N(10),
              N(3)),
          "13");

    // {given {[x = 10] [y = 20]} do {+ x y}}
    CHECK("given two bindings",
          GIVEN(BINDS(BIND("x", N(10)),
                      BIND("y", N(20))),
                ADD(ID("x"), ID("y"))),
          "30");

    // {given {[x = 10]} do {given {[x = 20]} do x}}
    CHECK("given shadows x",
          GIVEN(BINDS(BIND("x", N(10))),
                GIVEN(BINDS(BIND("x", N(20))),
                      ID("x"))),
          "20");

    // {given {[true = 5]} do true}
    CHECK("given shadows true",
          GIVEN(BINDS(BIND("true", N(5))),
                TRUE_),
          "5");

    // {given {[+ = {fn (x y) -> 100}]} do {+ 1 2}}
    CHECK("given shadows +",
          GIVEN(BINDS(BIND("+", FN(PARAMS("x", "y"), N(100)))),
                ADD(N(1), N(2))),
          "100");

    // {given {[x = 10]}
    //   do {given {[f = {fn (y) -> {+ x y}}]}
    //        do {given {[x = 100]} do {f 5}}}}
    Expr *lexical_scope =
        GIVEN(BINDS(BIND("x", N(10))),
              GIVEN(BINDS(BIND("f", FN(PARAMS("y"),
                                        ADD(ID("x"), ID("y"))))),
                    GIVEN(BINDS(BIND("x", N(100))),
                          APP(ID("f"), N(5)))));

    CHECK("lexical scope", lexical_scope, "15");

    // {{{fn (x) -> {fn (y) -> {+ x y}}} 4} 5}
    Expr *curried =
        APP(APP(FN(PARAMS("x"),
                   FN(PARAMS("y"),
                      ADD(ID("x"), ID("y")))),
                N(4)),
            N(5));

    CHECK("closure returns closure", curried, "9");

    CHECK_ERR("unbound id",
              ID("missing"),
              "unbound identifier");

    CHECK_ERR("division by zero",
              DIV(N(1), N(0)),
              "division by zero");

    CHECK_ERR("+ wrong type",
              ADD(N(1), S("bad")),
              "expected number");

    CHECK_ERR("+ wrong arity",
              APP(ID("+"), N(1)),
              "wrong arity");

    CHECK_ERR("if non-bool",
              IF_(N(0), N(1), N(2)),
              "expected boolean");

    CHECK_ERR("strlen wrong type",
              STRLEN_(N(100)),
              "expected string");

    CHECK_ERR("substring out of range",
              SUBSTRING(S("hello"), N(0), N(99)),
              "out of range");

    CHECK_ERR("substring backwards",
              SUBSTRING(S("hello"), N(4), N(1)),
              "before start");

    CHECK_ERR("function wrong arity",
              APP(FN(PARAMS("x", "y"), ADD(ID("x"), ID("y"))),
                  N(1)),
              "wrong arity");

    CHECK_ERR("apply non-function",
              APP(N(5), N(1), N(2)),
              "non-function");

    CHECK_ERR("user error",
              USER_ERROR(S("oops")),
              "user-error");

    printf("\n%d tests run, %d failed.\n", tests_run, tests_failed);
    return tests_failed == 0 ? 0 : 1;
}