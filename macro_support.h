/* modified from https://stackoverflow.com/questions/6707148/foreach-macro-on-macros-arguments */
#define _XPASTE(a, b) a ## b
#define XPASTE(a, b) _XPASTE(a, b)
#define PP_NARG(...)    PP_NARG_(__VA_ARGS__,PP_RSEQ_N())
#define PP_NARG_(...)   PP_ARG_N(__VA_ARGS__)

#define PP_ARG_N( \
        _1, _2, _3, _4, _5, _6, _7, _8, _9,_10,  \
        _11,_12,_13,_14,_15,_16,_17,_18,_19,_20, \
        _21,_22,_23,_24,_25,_26,_27,_28,_29,_30, \
        _31,_32,_33,_34,_35,_36,_37,_38,_39,_40, \
        _41,_42,_43,_44,_45,_46,_47,_48,_49,_50, \
        _51,_52,_53,_54,_55,_56,_57,_58,_59,_60, \
        _61,_62,_63,N,...) N

#define PP_RSEQ_N() \
        63,62,61,60,                   \
        59,58,57,56,55,54,53,52,51,50, \
        49,48,47,46,45,44,43,42,41,40, \
        39,38,37,36,35,34,33,32,31,30, \
        29,28,27,26,25,24,23,22,21,20, \
        19,18,17,16,15,14,13,12,11,10, \
        9,8,7,6,5,4,3,2,1,0

// TODO: include a seperator
#define MAPX1(X, a)           X(a)
#define MAPX2(X, a,b)         X(a) X(b)
#define MAPX3(X, a,b,c)       X(a) X(b) X(c)
#define MAPX4(X, a,b,c,d)     X(a) X(b) X(c) X(d)
#define MAPX5(X, a,b,c,d,e)   X(a) X(b) X(c) X(d) X(e)
#define MAPX6(X, a,b,c,d,e,f) X(a) X(b) X(c) X(d) X(e) X(f)
#define MAPX7(X, a,b,c,d,e,f,g) X(a) X(b) X(c) X(d) X(e) X(f) X(g)
#define MAPX8(X, a,b,c,d,e,f,g,h) X(a) X(b) X(c) X(d) X(e) X(f) X(g) X(h)
#define MAPX9(X, a,b,c,d,e,f,g,h,i) X(a) X(b) X(c) X(d) X(e) X(f) X(g) X(h) X(i)
#define MAPX10(X, a,b,c,d,e,f,g,h,i,j) X(a) X(b) X(c) X(d) X(e) X(f) X(g) X(h) X(i) X(j)
#define MAPX11(X, a,b,c,d,e,f,g,h,i,j,k) X(a) X(b) X(c) X(d) X(e) X(f) X(g) X(h) X(i) X(j) X(k)
#define MAPX12(X, a,b,c,d,e,f,g,h,i,j,k,l) X(a) X(b) X(c) X(d) X(e) X(f) X(g) X(h) X(i) X(j) X(k) X(l)
#define MAPX13(X, a,b,c,d,e,f,g,h,i,j,k,l,m) X(a) X(b) X(c) X(d) X(e) X(f) X(g) X(h) X(i) X(j) X(k) X(l) X(m)
#define MAPX14(X, a,b,c,d,e,f,g,h,i,j,k,l,m,n) X(a) X(b) X(c) X(d) X(e) X(f) X(g) X(h) X(i) X(j) X(k) X(l) X(m) X(n)
#define MAPX15(X, a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) X(a) X(b) X(c) X(d) X(e) X(f) X(g) X(h) X(i) X(j) X(k) X(l) X(m) X(n) X(o)
#define MAPX_(M, X,  ...) M(X, __VA_ARGS__)
#define MAP(X, ...) MAPX_(XPASTE(MAPX, PP_NARG(__VA_ARGS__)), X, __VA_ARGS__)
