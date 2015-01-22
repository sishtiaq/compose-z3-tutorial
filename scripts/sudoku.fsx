
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
//let x11,x21,x31,x41,x51,x61,x71,x81,x91 = (mk_int_var ctx "x11"), (mk_int_var ctx "x21"), (mk_int_var ctx "x31"), (mk_int_var ctx "x41"), (mk_int_var ctx "x51"), (mk_int_var ctx "x61"), (mk_int_var ctx "x71"), (mk_int_var ctx "x81"), (mk_int_var ctx "x91")
//let x12,x22,x32,x42,x52,x62,x72,x82,x92 = (mk_int_var ctx "x12"), (mk_int_var ctx "x22"), (mk_int_var ctx "x32"), (mk_int_var ctx "x42"), (mk_int_var ctx "x52"), (mk_int_var ctx "x62"), (mk_int_var ctx "x72"), (mk_int_var ctx "x82"), (mk_int_var ctx "x92")
//let x13,x23,x33,x43,x53,x63,x73,x83,x93 = (mk_int_var ctx "x13"), (mk_int_var ctx "x23"), (mk_int_var ctx "x33"), (mk_int_var ctx "x43"), (mk_int_var ctx "x53"), (mk_int_var ctx "x63"), (mk_int_var ctx "x73"), (mk_int_var ctx "x83"), (mk_int_var ctx "x93")
//let x14,x24,x34,x44,x54,x64,x74,x84,x94 = (mk_int_var ctx "x14"), (mk_int_var ctx "x24"), (mk_int_var ctx "x34"), (mk_int_var ctx "x44"), (mk_int_var ctx "x54"), (mk_int_var ctx "x64"), (mk_int_var ctx "x74"), (mk_int_var ctx "x84"), (mk_int_var ctx "x94")
//let x15,x25,x35,x45,x55,x65,x75,x85,x95 = (mk_int_var ctx "x15"), (mk_int_var ctx "x25"), (mk_int_var ctx "x35"), (mk_int_var ctx "x45"), (mk_int_var ctx "x55"), (mk_int_var ctx "x65"), (mk_int_var ctx "x75"), (mk_int_var ctx "x85"), (mk_int_var ctx "x95")
//let x16,x26,x36,x46,x56,x66,x76,x86,x96 = (mk_int_var ctx "x16"), (mk_int_var ctx "x26"), (mk_int_var ctx "x36"), (mk_int_var ctx "x46"), (mk_int_var ctx "x56"), (mk_int_var ctx "x66"), (mk_int_var ctx "x76"), (mk_int_var ctx "x86"), (mk_int_var ctx "x96")
//let x17,x27,x37,x47,x57,x67,x77,x87,x97 = (mk_int_var ctx "x17"), (mk_int_var ctx "x27"), (mk_int_var ctx "x37"), (mk_int_var ctx "x47"), (mk_int_var ctx "x57"), (mk_int_var ctx "x67"), (mk_int_var ctx "x77"), (mk_int_var ctx "x87"), (mk_int_var ctx "x97")
//let x18,x28,x38,x48,x58,x68,x78,x88,x98 = (mk_int_var ctx "x18"), (mk_int_var ctx "x28"), (mk_int_var ctx "x38"), (mk_int_var ctx "x48"), (mk_int_var ctx "x58"), (mk_int_var ctx "x68"), (mk_int_var ctx "x78"), (mk_int_var ctx "x88"), (mk_int_var ctx "x98")
//let x19,x29,x39,x49,x59,x69,x79,x89,x99 = (mk_int_var ctx "x19"), (mk_int_var ctx "x29"), (mk_int_var ctx "x39"), (mk_int_var ctx "x49"), (mk_int_var ctx "x59"), (mk_int_var ctx "x69"), (mk_int_var ctx "x79"), (mk_int_var ctx "x89"), (mk_int_var ctx "x99")

// SI: arrays are 0-based, I think. Line 73 crashes "outside the bounds"
let mk_grid ctx = 
    let x = Array2D.init 9 9 (fun i j -> mk_int_var ctx ("x"+(string)i+(string)j)) 
    x

// 1 <= x_{i,j} <= 9
let range ctx (x:IntExpr[,]) =        
    let r x = m_le_x_le_n ctx (mk_int ctx 1) x (mk_int ctx 9)
    mk_ands ctx [ r x.[1,1]; r x.[2,1]; r x.[3,1]; r x.[4,1]; r x.[5,1]; r x.[6,1]; r x.[7,1]; r x.[8,1]; r x.[9,1];
                  r x.[1,2]; r x.[2,2]; r x.[3,2]; r x.[4,2]; r x.[5,2]; r x.[6,2]; r x.[7,2]; r x.[8,2]; r x.[9,2];
                  r x.[1,3]; r x.[2,3]; r x.[3,3]; r x.[4,3]; r x.[5,3]; r x.[6,3]; r x.[7,3]; r x.[8,3]; r x.[9,3];
                  r x.[1,4]; r x.[2,4]; r x.[3,4]; r x.[4,4]; r x.[5,4]; r x.[6,4]; r x.[7,4]; r x.[8,4]; r x.[9,4];
                  r x.[1,5]; r x.[2,5]; r x.[3,5]; r x.[4,5]; r x.[5,5]; r x.[6,5]; r x.[7,5]; r x.[8,5]; r x.[9,5];
                  r x.[1,6]; r x.[2,6]; r x.[3,6]; r x.[4,6]; r x.[5,6]; r x.[6,6]; r x.[7,6]; r x.[8,6]; r x.[9,6];
                  r x.[1,7]; r x.[2,7]; r x.[3,7]; r x.[4,7]; r x.[5,7]; r x.[6,7]; r x.[7,7]; r x.[8,7]; r x.[9,7];
                  r x.[1,8]; r x.[2,8]; r x.[3,8]; r x.[4,8]; r x.[5,8]; r x.[6,8]; r x.[7,8]; r x.[8,8]; r x.[9,8];
                  r x.[1,9]; r x.[2,9]; r x.[3,9]; r x.[4,9]; r x.[5,9]; r x.[6,9]; r x.[7,9]; r x.[8,9]; r x.[9,9] ]
// x11 # x21 # ... # x91
let row_distinct ctx (x:IntExpr[,]) = 
    mk_ands ctx [ (mk_distinct ctx [x.[1,1]; x.[2,1]; x.[3,1]; x.[4,1]; x.[5,1]; x.[6,1]; x.[7,1]; x.[8,1]; x.[9,1]]);
                  (mk_distinct ctx [x.[1,2]; x.[2,2]; x.[3,2]; x.[4,2]; x.[5,2]; x.[6,2]; x.[7,2]; x.[8,2]; x.[9,2]]);
                  (mk_distinct ctx [x.[1,3]; x.[2,3]; x.[3,3]; x.[4,3]; x.[5,3]; x.[6,3]; x.[7,3]; x.[8,3]; x.[9,3]]);
                  (mk_distinct ctx [x.[1,4]; x.[2,4]; x.[3,4]; x.[4,4]; x.[5,4]; x.[6,4]; x.[7,4]; x.[8,4]; x.[9,4]]);
                  (mk_distinct ctx [x.[1,5]; x.[2,5]; x.[3,5]; x.[4,5]; x.[5,5]; x.[6,5]; x.[7,5]; x.[8,5]; x.[9,5]]);
                  (mk_distinct ctx [x.[1,6]; x.[2,6]; x.[3,6]; x.[4,6]; x.[5,6]; x.[6,6]; x.[7,6]; x.[8,6]; x.[9,6]]);
                  (mk_distinct ctx [x.[1,7]; x.[2,7]; x.[3,7]; x.[4,7]; x.[5,7]; x.[6,7]; x.[7,7]; x.[8,7]; x.[9,7]]);
                  (mk_distinct ctx [x.[1,8]; x.[2,8]; x.[3,8]; x.[4,8]; x.[5,8]; x.[6,8]; x.[7,8]; x.[8,8]; x.[9,8]]);
                  (mk_distinct ctx [x.[1,9]; x.[2,9]; x.[3,9]; x.[4,9]; x.[5,9]; x.[6,9]; x.[7,9]; x.[8,9]; x.[9,9]]) ]
// x11 # x12 # ... # x19
let col_distinct ctx (x:IntExpr[,]) = 
    mk_ands ctx [ (mk_distinct ctx [x.[1,1]; x.[1,2]; x.[1,3]; x.[1,4]; x.[1,5]; x.[1,6]; x.[1,7]; x.[1,8]; x.[1,9]]);
                  (mk_distinct ctx [x.[2,1]; x.[2,2]; x.[2,3]; x.[2,4]; x.[2,5]; x.[2,6]; x.[2,7]; x.[2,8]; x.[2,9]]);
                  (mk_distinct ctx [x.[3,1]; x.[3,2]; x.[3,3]; x.[3,4]; x.[3,5]; x.[3,6]; x.[3,7]; x.[3,8]; x.[3,9]]);
                  (mk_distinct ctx [x.[4,1]; x.[4,2]; x.[4,3]; x.[4,4]; x.[4,5]; x.[4,6]; x.[4,7]; x.[4,8]; x.[4,9]]);
                  (mk_distinct ctx [x.[5,1]; x.[5,2]; x.[5,3]; x.[5,4]; x.[5,5]; x.[5,6]; x.[5,7]; x.[5,8]; x.[5,9]]);
                  (mk_distinct ctx [x.[6,1]; x.[6,2]; x.[6,3]; x.[6,4]; x.[6,5]; x.[6,6]; x.[6,7]; x.[6,8]; x.[6,9]]);
                  (mk_distinct ctx [x.[7,1]; x.[7,2]; x.[7,3]; x.[7,4]; x.[7,5]; x.[7,6]; x.[7,7]; x.[7,8]; x.[7,9]]);
                  (mk_distinct ctx [x.[8,1]; x.[8,2]; x.[8,3]; x.[8,4]; x.[8,5]; x.[8,6]; x.[8,7]; x.[8,8]; x.[8,9]]);
                  (mk_distinct ctx [x.[9,1]; x.[9,2]; x.[9,3]; x.[9,4]; x.[9,5]; x.[9,6]; x.[9,7]; x.[9,8]; x.[9,9]]) ]

let subgrid_distinct ctx (x:IntExpr[,]) = 
    mk_ands ctx [ (mk_distinct ctx [x.[1,1]; x.[2,1]; x.[3,1];    //X--
                                    x.[1,2]; x.[2,2]; x.[3,2];    //---
                                    x.[1,3]; x.[2,3]; x.[3,3];]); //---
                  (mk_distinct ctx [x.[4,1]; x.[5,1]; x.[6,1];    //---
                                    x.[4,2]; x.[5,2]; x.[6,2];    //X--
                                    x.[4,3]; x.[5,3]; x.[6,3]]);  //---
                  (mk_distinct ctx [x.[7,1]; x.[8,1]; x.[9,1];    //---
                                    x.[7,2]; x.[8,2]; x.[9,2];    //---
                                    x.[7,3]; x.[8,3]; x.[9,3]]);  //X--
                  (mk_distinct ctx [x.[1,4]; x.[2,4]; x.[3,4];
                                    x.[1,5]; x.[2,5]; x.[3,5];
                                    x.[1,6]; x.[2,6]; x.[3,6]]); 
                  (mk_distinct ctx [x.[4,4]; x.[5,4]; x.[6,4];
                                    x.[4,5]; x.[5,5]; x.[6,5];
                                    x.[4,6]; x.[5,6]; x.[6,6]]); 
                  (mk_distinct ctx [x.[7,4]; x.[8,4]; x.[9,4];
                                    x.[7,5]; x.[8,5]; x.[9,5];
                                    x.[7,6]; x.[8,6]; x.[9,6]]); 
                  (mk_distinct ctx [x.[1,7]; x.[2,7]; x.[3,7];
                                    x.[1,8]; x.[2,8]; x.[3,8];
                                    x.[1,9]; x.[2,9]; x.[3,9]]); 
                  (mk_distinct ctx [x.[4,7]; x.[5,7]; x.[6,7];
                                    x.[4,8]; x.[5,8]; x.[6,8];
                                    x.[4,9]; x.[5,9]; x.[6,9]]); 
                  (mk_distinct ctx [x.[7,7]; x.[8,7]; x.[9,7];
                                    x.[7,8]; x.[8,8]; x.[9,8];
                                    x.[7,9]; x.[8,9]; x.[9,9]]) ]
// Known values
let this_grid ctx (x:IntExpr[,]) = 
    mk_ands ctx [ (mk_eq ctx x.[1,3] (mk_int ctx 2));
                  (mk_eq ctx x.[1,6] (mk_int ctx 1));
                  (mk_eq ctx x.[1,8] (mk_int ctx 6));
                  (mk_eq ctx x.[2,3] (mk_int ctx 7));
                  (mk_eq ctx x.[2,6] (mk_int ctx 4));
                  (mk_eq ctx x.[3,1] (mk_int ctx 5));
                  (mk_eq ctx x.[3,7] (mk_int ctx 9));
                  (mk_eq ctx x.[4,2] (mk_int ctx 1));
                  (mk_eq ctx x.[4,4] (mk_int ctx 3));
                  (mk_eq ctx x.[5,1] (mk_int ctx 8));
                  (mk_eq ctx x.[5,5] (mk_int ctx 5));
                  (mk_eq ctx x.[5,9] (mk_int ctx 4));
                  (mk_eq ctx x.[6,6] (mk_int ctx 6));
                  (mk_eq ctx x.[6,8] (mk_int ctx 2));
                  (mk_eq ctx x.[7,3] (mk_int ctx 6));
                  (mk_eq ctx x.[7,9] (mk_int ctx 7));
                  (mk_eq ctx x.[8,4] (mk_int ctx 8));
                  (mk_eq ctx x.[8,7] (mk_int ctx 3));
                  (mk_eq ctx x.[9,2] (mk_int ctx 4));
                  (mk_eq ctx x.[9,4] (mk_int ctx 9));
                  (mk_eq ctx x.[9,7] (mk_int ctx 2)) ]


let g0 = mk_grid ctx 
let solver = ctx.MkSolver()
solver.Add (mk_ands ctx [range ctx g0; row_distinct ctx g0; col_distinct ctx g0; subgrid_distinct ctx g0; this_grid ctx g0])

match solver.Check([||]) with
| Status.UNSATISFIABLE -> 
    Printf.printf "valid"
| Status.SATISFIABLE -> 
    Printf.printfn "Sat."                
//    let m = solver.Model
//    printf 
//        "%O %O %O %O %O %O %O %O %O\n%O %O %O %O %O %O %O %O %O\n%O %O %O %O %O %O %O %O %O\n%O %O %O %O %O %O %O %O %O\n%O %O %O %O %O %O %O %O %O\n%O %O %O %O %O %O %O %O %O\n%O %O %O %O %O %O %O %O %O\n%O %O %O %O %O %O %O %O %O\n%O %O %O %O %O %O %O %O %O\n" 
//        (m.ConstInterp(x11)) (m.ConstInterp(x21)) (m.ConstInterp(x31)) (m.ConstInterp(x41)) (m.ConstInterp(x51)) (m.ConstInterp(x61)) (m.ConstInterp(x71)) (m.ConstInterp(x81)) (m.ConstInterp(x91))
//        (m.ConstInterp(x12)) (m.ConstInterp(x22)) (m.ConstInterp(x32)) (m.ConstInterp(x42)) (m.ConstInterp(x52)) (m.ConstInterp(x62)) (m.ConstInterp(x72)) (m.ConstInterp(x82)) (m.ConstInterp(x92))
//        (m.ConstInterp(x13)) (m.ConstInterp(x23)) (m.ConstInterp(x33)) (m.ConstInterp(x43)) (m.ConstInterp(x53)) (m.ConstInterp(x63)) (m.ConstInterp(x73)) (m.ConstInterp(x83)) (m.ConstInterp(x93))
//        (m.ConstInterp(x14)) (m.ConstInterp(x24)) (m.ConstInterp(x34)) (m.ConstInterp(x44)) (m.ConstInterp(x54)) (m.ConstInterp(x64)) (m.ConstInterp(x74)) (m.ConstInterp(x84)) (m.ConstInterp(x94))
//        (m.ConstInterp(x15)) (m.ConstInterp(x25)) (m.ConstInterp(x35)) (m.ConstInterp(x45)) (m.ConstInterp(x55)) (m.ConstInterp(x65)) (m.ConstInterp(x75)) (m.ConstInterp(x85)) (m.ConstInterp(x95))
//        (m.ConstInterp(x16)) (m.ConstInterp(x26)) (m.ConstInterp(x36)) (m.ConstInterp(x46)) (m.ConstInterp(x56)) (m.ConstInterp(x66)) (m.ConstInterp(x76)) (m.ConstInterp(x86)) (m.ConstInterp(x96))
//        (m.ConstInterp(x17)) (m.ConstInterp(x27)) (m.ConstInterp(x37)) (m.ConstInterp(x47)) (m.ConstInterp(x57)) (m.ConstInterp(x67)) (m.ConstInterp(x77)) (m.ConstInterp(x87)) (m.ConstInterp(x97))
//        (m.ConstInterp(x18)) (m.ConstInterp(x28)) (m.ConstInterp(x38)) (m.ConstInterp(x48)) (m.ConstInterp(x58)) (m.ConstInterp(x68)) (m.ConstInterp(x78)) (m.ConstInterp(x88)) (m.ConstInterp(x98))
//        (m.ConstInterp(x19)) (m.ConstInterp(x29)) (m.ConstInterp(x39)) (m.ConstInterp(x49)) (m.ConstInterp(x59)) (m.ConstInterp(x69)) (m.ConstInterp(x79)) (m.ConstInterp(x89)) (m.ConstInterp(x99))                 
        
| Status.UNKNOWN -> 
    Printf.printf "unknown"
| _ -> Printf.printf "x"
    
ctx.Dispose ()


