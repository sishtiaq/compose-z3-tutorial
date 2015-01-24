
#I @"C:\\Users\\sishtiaq\\work\compose-z3-tutorial-0\\platform\\windows"
#r "Microsoft.Z3.dll"

open Microsoft.Z3 

//
// Convenience wrappers for Z3 
//

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


// Declare a Z3 context 
let ctx = new Context()

// 
// The sudoku grid (mutable 2D array) and the constraints on it.
//
type sudoku_grid = IntExpr[,]

// Declare 9X9 variables

// SI: arrays are 0-based, I think. Line 73 crashes "outside the bounds"
let mk_grid ctx = 
    let x = Array2D.init 9 9 (fun i j -> mk_int_var ctx ("x"+(string)i+(string)j)) 
    x

// 1 <= x_{i,j} <= 9
let range ctx (x:IntExpr[,]) =        
    let r x = m_le_x_le_n ctx (mk_int ctx 1) x (mk_int ctx 9)
    mk_ands ctx [ r x.[0,0]; r x.[1,0]; r x.[2,0]; r x.[3,0]; r x.[4,0]; r x.[5,0]; r x.[6,0]; r x.[7,0]; r x.[8,0];
                  r x.[0,1]; r x.[1,1]; r x.[2,1]; r x.[3,1]; r x.[4,1]; r x.[5,1]; r x.[6,1]; r x.[7,1]; r x.[8,1];
                  r x.[0,2]; r x.[1,2]; r x.[2,2]; r x.[3,2]; r x.[4,2]; r x.[5,2]; r x.[6,2]; r x.[7,2]; r x.[8,2];
                  r x.[0,3]; r x.[1,3]; r x.[2,3]; r x.[3,3]; r x.[4,3]; r x.[5,3]; r x.[6,3]; r x.[7,3]; r x.[8,3];
                  r x.[0,4]; r x.[1,4]; r x.[2,4]; r x.[3,4]; r x.[4,4]; r x.[5,4]; r x.[6,4]; r x.[7,4]; r x.[8,4];
                  r x.[0,5]; r x.[1,5]; r x.[2,5]; r x.[3,5]; r x.[4,5]; r x.[5,5]; r x.[6,5]; r x.[7,5]; r x.[8,5];
                  r x.[0,6]; r x.[1,6]; r x.[2,6]; r x.[3,6]; r x.[4,6]; r x.[5,6]; r x.[6,6]; r x.[7,6]; r x.[8,6];
                  r x.[0,7]; r x.[1,7]; r x.[2,7]; r x.[3,7]; r x.[4,7]; r x.[5,7]; r x.[6,7]; r x.[7,7]; r x.[8,7];
                  r x.[0,8]; r x.[1,8]; r x.[2,8]; r x.[3,8]; r x.[4,8]; r x.[5,8]; r x.[6,8]; r x.[7,8]; r x.[8,8] ]
// x11 # x21 # ... # x91        
let row_distinct ctx (x:IntExpr[,]) = 
    mk_ands ctx [ (mk_distinct ctx [x.[0,0]; x.[1,0]; x.[2,0]; x.[3,0]; x.[4,0]; x.[5,0]; x.[6,0]; x.[7,0]; x.[8,0]]);
                  (mk_distinct ctx [x.[0,1]; x.[1,1]; x.[2,1]; x.[3,1]; x.[4,1]; x.[5,1]; x.[6,1]; x.[7,1]; x.[8,1]]);
                  (mk_distinct ctx [x.[0,2]; x.[1,2]; x.[2,2]; x.[3,2]; x.[4,2]; x.[5,2]; x.[6,2]; x.[7,2]; x.[8,2]]);
                  (mk_distinct ctx [x.[0,3]; x.[1,3]; x.[2,3]; x.[3,3]; x.[4,3]; x.[5,3]; x.[6,3]; x.[7,3]; x.[8,3]]);
                  (mk_distinct ctx [x.[0,4]; x.[1,4]; x.[2,4]; x.[3,4]; x.[4,4]; x.[5,4]; x.[6,4]; x.[7,4]; x.[8,4]]);
                  (mk_distinct ctx [x.[0,5]; x.[1,5]; x.[2,5]; x.[3,5]; x.[4,5]; x.[5,5]; x.[6,5]; x.[7,5]; x.[8,5]]);
                  (mk_distinct ctx [x.[0,6]; x.[1,6]; x.[2,6]; x.[3,6]; x.[4,6]; x.[5,6]; x.[6,6]; x.[7,6]; x.[8,6]]);
                  (mk_distinct ctx [x.[0,7]; x.[1,7]; x.[2,7]; x.[3,7]; x.[4,7]; x.[5,7]; x.[6,7]; x.[7,7]; x.[8,7]]);
                  (mk_distinct ctx [x.[0,8]; x.[1,8]; x.[2,8]; x.[3,8]; x.[4,8]; x.[5,8]; x.[6,8]; x.[7,8]; x.[8,8]]) ]
// x11 # x12 # ... # x19
let col_distinct ctx (x:IntExpr[,]) = 
    mk_ands ctx [ (mk_distinct ctx [x.[0,0]; x.[0,1]; x.[0,2]; x.[0,3]; x.[0,4]; x.[0,5]; x.[0,6]; x.[0,7]; x.[0,8]]);
                  (mk_distinct ctx [x.[1,0]; x.[1,1]; x.[1,2]; x.[1,3]; x.[1,4]; x.[1,5]; x.[1,6]; x.[1,7]; x.[1,8]]);
                  (mk_distinct ctx [x.[2,0]; x.[2,1]; x.[2,2]; x.[2,3]; x.[2,4]; x.[2,5]; x.[2,6]; x.[2,7]; x.[2,8]]);
                  (mk_distinct ctx [x.[3,0]; x.[3,1]; x.[3,2]; x.[3,3]; x.[3,4]; x.[3,5]; x.[3,6]; x.[3,7]; x.[3,8]]);
                  (mk_distinct ctx [x.[4,0]; x.[4,1]; x.[4,2]; x.[4,3]; x.[4,4]; x.[4,5]; x.[4,6]; x.[4,7]; x.[4,8]]);
                  (mk_distinct ctx [x.[5,0]; x.[5,1]; x.[5,2]; x.[5,3]; x.[5,4]; x.[5,5]; x.[5,6]; x.[5,7]; x.[5,8]]);
                  (mk_distinct ctx [x.[6,0]; x.[6,1]; x.[6,2]; x.[6,3]; x.[6,4]; x.[6,5]; x.[6,6]; x.[6,7]; x.[6,8]]);
                  (mk_distinct ctx [x.[7,0]; x.[7,1]; x.[7,2]; x.[7,3]; x.[7,4]; x.[7,5]; x.[7,6]; x.[7,7]; x.[7,8]]);
                  (mk_distinct ctx [x.[8,0]; x.[8,1]; x.[8,2]; x.[8,3]; x.[8,4]; x.[8,5]; x.[8,6]; x.[8,7]; x.[8,8]]) ]

let subgrid_distinct ctx (x:IntExpr[,]) = 
    mk_ands ctx [ (mk_distinct ctx [x.[0,0]; x.[0,1]; x.[0,2];    //X--
                                    x.[1,0]; x.[1,1]; x.[1,2];    //---
                                    x.[2,0]; x.[2,1]; x.[2,2];]); //---
                  (mk_distinct ctx [x.[3,0]; x.[3,1]; x.[3,2];    //---
                                    x.[4,0]; x.[4,1]; x.[4,2];    //X--
                                    x.[5,0]; x.[5,1]; x.[5,2];]);  //---
                  (mk_distinct ctx [x.[6,0]; x.[6,1]; x.[6,2];    //---
                                    x.[7,0]; x.[7,1]; x.[7,2];    //---
                                    x.[8,0]; x.[8,1]; x.[8,2];]);  //X--
                //
                  (mk_distinct ctx [x.[0,3]; x.[0,4]; x.[0,5];
                                    x.[1,3]; x.[1,4]; x.[1,5];
                                    x.[2,3]; x.[2,4]; x.[2,5];]); 
                  (mk_distinct ctx [x.[3,3]; x.[3,4]; x.[3,5];
                                    x.[4,3]; x.[4,4]; x.[4,5];
                                    x.[5,3]; x.[5,4]; x.[5,5];]); 
                  (mk_distinct ctx [x.[6,3]; x.[6,4]; x.[6,5];
                                    x.[7,3]; x.[7,4]; x.[7,5];
                                    x.[8,3]; x.[8,4]; x.[8,5];]); 
                //
                  (mk_distinct ctx [x.[0,6]; x.[0,7]; x.[0,8];
                                    x.[1,6]; x.[1,7]; x.[1,8];
                                    x.[2,6]; x.[2,7]; x.[2,8]]); 
                  (mk_distinct ctx [x.[3,6]; x.[3,7]; x.[3,8];
                                    x.[4,6]; x.[4,7]; x.[4,8];
                                    x.[5,6]; x.[5,7]; x.[5,8]]); 
                  (mk_distinct ctx [x.[6,6]; x.[6,7]; x.[6,8];
                                    x.[7,6]; x.[7,7]; x.[7,8];
                                    x.[8,6]; x.[8,7]; x.[8,8]]) ]
// Known values
let this_grid ctx (x:IntExpr[,]) = 
    mk_ands ctx [ (mk_eq ctx x.[0,2] (mk_int ctx 2));
                  (mk_eq ctx x.[0,5] (mk_int ctx 1));
                  (mk_eq ctx x.[0,7] (mk_int ctx 6));
                  (mk_eq ctx x.[1,2] (mk_int ctx 7));
                  (mk_eq ctx x.[1,5] (mk_int ctx 4));
                  (mk_eq ctx x.[2,0] (mk_int ctx 5));
                  (mk_eq ctx x.[2,6] (mk_int ctx 9));
                  (mk_eq ctx x.[3,1] (mk_int ctx 1));
                  (mk_eq ctx x.[3,3] (mk_int ctx 3));
                  (mk_eq ctx x.[4,0] (mk_int ctx 8));
                  (mk_eq ctx x.[4,4] (mk_int ctx 5));
                  (mk_eq ctx x.[4,8] (mk_int ctx 4));
                  (mk_eq ctx x.[5,5] (mk_int ctx 6));
                  (mk_eq ctx x.[5,7] (mk_int ctx 2));
                  (mk_eq ctx x.[6,2] (mk_int ctx 6));
                  (mk_eq ctx x.[6,8] (mk_int ctx 7));
                  (mk_eq ctx x.[7,3] (mk_int ctx 8));
                  (mk_eq ctx x.[7,6] (mk_int ctx 3));
                  (mk_eq ctx x.[8,1] (mk_int ctx 4));
                  (mk_eq ctx x.[8,3] (mk_int ctx 9));
                  (mk_eq ctx x.[8,6] (mk_int ctx 2)) ]

let main _ = 
    let g0 = mk_grid ctx 
    let solver = ctx.MkSolver()
    solver.Add (mk_ands ctx [range ctx g0; row_distinct ctx g0; col_distinct ctx g0; subgrid_distinct ctx g0; this_grid ctx g0])

    match solver.Check([||]) with
    | Status.UNSATISFIABLE -> 
        Printf.printf "valid"
    | Status.SATISFIABLE -> 
        Printf.printfn "Sat."                
        let m = solver.Model
        printf 
            "%O %O %O %O %O %O %O %O %O\n%O %O %O %O %O %O %O %O %O\n%O %O %O %O %O %O %O %O %O\n%O %O %O %O %O %O %O %O %O\n%O %O %O %O %O %O %O %O %O\n%O %O %O %O %O %O %O %O %O\n%O %O %O %O %O %O %O %O %O\n%O %O %O %O %O %O %O %O %O\n%O %O %O %O %O %O %O %O %O\n" 
            (m.ConstInterp(g0.[0,0])) (m.ConstInterp(g0.[1,0])) (m.ConstInterp(g0.[2,0])) (m.ConstInterp(g0.[3,0])) (m.ConstInterp(g0.[4,0])) (m.ConstInterp(g0.[5,0])) (m.ConstInterp(g0.[6,0])) (m.ConstInterp(g0.[7,0])) (m.ConstInterp(g0.[8,0]))
            (m.ConstInterp(g0.[0,1])) (m.ConstInterp(g0.[1,1])) (m.ConstInterp(g0.[2,1])) (m.ConstInterp(g0.[3,1])) (m.ConstInterp(g0.[4,1])) (m.ConstInterp(g0.[5,1])) (m.ConstInterp(g0.[6,1])) (m.ConstInterp(g0.[7,1])) (m.ConstInterp(g0.[8,1]))
            (m.ConstInterp(g0.[0,2])) (m.ConstInterp(g0.[1,2])) (m.ConstInterp(g0.[2,2])) (m.ConstInterp(g0.[3,2])) (m.ConstInterp(g0.[4,2])) (m.ConstInterp(g0.[5,2])) (m.ConstInterp(g0.[6,2])) (m.ConstInterp(g0.[7,2])) (m.ConstInterp(g0.[8,2]))
            (m.ConstInterp(g0.[0,3])) (m.ConstInterp(g0.[1,3])) (m.ConstInterp(g0.[2,3])) (m.ConstInterp(g0.[3,3])) (m.ConstInterp(g0.[4,3])) (m.ConstInterp(g0.[5,3])) (m.ConstInterp(g0.[6,3])) (m.ConstInterp(g0.[7,3])) (m.ConstInterp(g0.[8,3]))
            (m.ConstInterp(g0.[0,4])) (m.ConstInterp(g0.[1,4])) (m.ConstInterp(g0.[2,4])) (m.ConstInterp(g0.[3,4])) (m.ConstInterp(g0.[4,4])) (m.ConstInterp(g0.[5,4])) (m.ConstInterp(g0.[6,4])) (m.ConstInterp(g0.[7,4])) (m.ConstInterp(g0.[8,4]))
            (m.ConstInterp(g0.[0,5])) (m.ConstInterp(g0.[1,5])) (m.ConstInterp(g0.[2,5])) (m.ConstInterp(g0.[3,5])) (m.ConstInterp(g0.[4,5])) (m.ConstInterp(g0.[5,5])) (m.ConstInterp(g0.[6,5])) (m.ConstInterp(g0.[7,5])) (m.ConstInterp(g0.[8,5]))
            (m.ConstInterp(g0.[0,6])) (m.ConstInterp(g0.[1,6])) (m.ConstInterp(g0.[2,6])) (m.ConstInterp(g0.[3,6])) (m.ConstInterp(g0.[4,6])) (m.ConstInterp(g0.[5,6])) (m.ConstInterp(g0.[6,6])) (m.ConstInterp(g0.[7,6])) (m.ConstInterp(g0.[8,6]))
            (m.ConstInterp(g0.[0,7])) (m.ConstInterp(g0.[1,7])) (m.ConstInterp(g0.[2,7])) (m.ConstInterp(g0.[3,7])) (m.ConstInterp(g0.[4,7])) (m.ConstInterp(g0.[5,7])) (m.ConstInterp(g0.[6,7])) (m.ConstInterp(g0.[7,7])) (m.ConstInterp(g0.[8,7]))
            (m.ConstInterp(g0.[0,8])) (m.ConstInterp(g0.[1,8])) (m.ConstInterp(g0.[2,8])) (m.ConstInterp(g0.[3,8])) (m.ConstInterp(g0.[4,8])) (m.ConstInterp(g0.[5,8])) (m.ConstInterp(g0.[6,8])) (m.ConstInterp(g0.[7,8])) (m.ConstInterp(g0.[8,8]))
        
    | Status.UNKNOWN -> 
        Printf.printf "unknown"
    | _ -> Printf.printf "x"
    
    ctx.Dispose ()


