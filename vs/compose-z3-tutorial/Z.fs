module Z

open Microsoft.Z3

//let conj = ctx.MkIff(ctx.MkNot(ctx.MkAnd(x,y)), ctx.MkOr(ctx.MkNot(x), ctx.MkNot(y)))
//let nconj = ctx.MkNot(conj)

// Wrappers 

// int sort
let mk_int_var (z3:Context) (name:string) = 
    z3.MkIntConst(name)

let mk_int (z3:Context) (i:int) = 
    z3.MkInt(i)

// arith expr
let mk_ge (z3:Context) (l:ArithExpr) (r:ArithExpr) = 
    z3.MkGe (l,r)

let mk_le (z3:Context) (l:ArithExpr) (r:ArithExpr) = 
    z3.MkLe (l,r)

// bool expr 
let mk_eq (z3:Context) (l:IntExpr) (r:IntExpr) = 
    z3.MkEq(l,r)

let mk_true (z3:Context) = 
    z3.MkTrue ()

let mk_and (z3:Context) (l:BoolExpr) (r:BoolExpr) = 
    z3.MkAnd ([|l;r|])

let mk_ands (z3:Context) (tt:BoolExpr list) = 
    z3.MkAnd(List.toArray tt)

// m <= x <= n
let m_le_x_le_n (z3:Context) (m:ArithExpr) (x:ArithExpr) (n:ArithExpr) = 
    mk_and z3 (mk_le z3 m x) (mk_le z3 x n)


let mk_distinct (z3:Context) (tt:Expr list) = 
    z3.MkDistinct (List.toArray tt)
