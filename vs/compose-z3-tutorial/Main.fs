module Main

// Uses 32 bit dll
open Microsoft.Z3

let main _  = 

    let ctx = new Context()

    // SI: these are same for every board. Put in Board.fs. 

    // Declare 9X9 variables
    let x11,x21,x31,x41,x51,x61,x71,x81,x91 = (Z.mk_int_var ctx "x11"), (Z.mk_int_var ctx "x21"), (Z.mk_int_var ctx "x31"), (Z.mk_int_var ctx "x41"), (Z.mk_int_var ctx "x51"), (Z.mk_int_var ctx "x61"), (Z.mk_int_var ctx "x71"), (Z.mk_int_var ctx "x81"), (Z.mk_int_var ctx "x91")
    let x12,x22,x32,x42,x52,x62,x72,x82,x92 = (Z.mk_int_var ctx "x12"), (Z.mk_int_var ctx "x22"), (Z.mk_int_var ctx "x32"), (Z.mk_int_var ctx "x42"), (Z.mk_int_var ctx "x52"), (Z.mk_int_var ctx "x62"), (Z.mk_int_var ctx "x72"), (Z.mk_int_var ctx "x82"), (Z.mk_int_var ctx "x92")
    let x13,x23,x33,x43,x53,x63,x73,x83,x93 = (Z.mk_int_var ctx "x13"), (Z.mk_int_var ctx "x23"), (Z.mk_int_var ctx "x33"), (Z.mk_int_var ctx "x43"), (Z.mk_int_var ctx "x53"), (Z.mk_int_var ctx "x63"), (Z.mk_int_var ctx "x73"), (Z.mk_int_var ctx "x83"), (Z.mk_int_var ctx "x93")
    let x14,x24,x34,x44,x54,x64,x74,x84,x94 = (Z.mk_int_var ctx "x14"), (Z.mk_int_var ctx "x24"), (Z.mk_int_var ctx "x34"), (Z.mk_int_var ctx "x44"), (Z.mk_int_var ctx "x54"), (Z.mk_int_var ctx "x64"), (Z.mk_int_var ctx "x74"), (Z.mk_int_var ctx "x84"), (Z.mk_int_var ctx "x94")
    let x15,x25,x35,x45,x55,x65,x75,x85,x95 = (Z.mk_int_var ctx "x15"), (Z.mk_int_var ctx "x25"), (Z.mk_int_var ctx "x35"), (Z.mk_int_var ctx "x45"), (Z.mk_int_var ctx "x55"), (Z.mk_int_var ctx "x65"), (Z.mk_int_var ctx "x75"), (Z.mk_int_var ctx "x85"), (Z.mk_int_var ctx "x95")
    let x16,x26,x36,x46,x56,x66,x76,x86,x96 = (Z.mk_int_var ctx "x16"), (Z.mk_int_var ctx "x26"), (Z.mk_int_var ctx "x36"), (Z.mk_int_var ctx "x46"), (Z.mk_int_var ctx "x56"), (Z.mk_int_var ctx "x66"), (Z.mk_int_var ctx "x76"), (Z.mk_int_var ctx "x86"), (Z.mk_int_var ctx "x96")
    let x17,x27,x37,x47,x57,x67,x77,x87,x97 = (Z.mk_int_var ctx "x17"), (Z.mk_int_var ctx "x27"), (Z.mk_int_var ctx "x37"), (Z.mk_int_var ctx "x47"), (Z.mk_int_var ctx "x57"), (Z.mk_int_var ctx "x67"), (Z.mk_int_var ctx "x77"), (Z.mk_int_var ctx "x87"), (Z.mk_int_var ctx "x97")
    let x18,x28,x38,x48,x58,x68,x78,x88,x98 = (Z.mk_int_var ctx "x18"), (Z.mk_int_var ctx "x28"), (Z.mk_int_var ctx "x38"), (Z.mk_int_var ctx "x48"), (Z.mk_int_var ctx "x58"), (Z.mk_int_var ctx "x68"), (Z.mk_int_var ctx "x78"), (Z.mk_int_var ctx "x88"), (Z.mk_int_var ctx "x98")
    let x19,x29,x39,x49,x59,x69,x79,x89,x99 = (Z.mk_int_var ctx "x19"), (Z.mk_int_var ctx "x29"), (Z.mk_int_var ctx "x39"), (Z.mk_int_var ctx "x49"), (Z.mk_int_var ctx "x59"), (Z.mk_int_var ctx "x69"), (Z.mk_int_var ctx "x79"), (Z.mk_int_var ctx "x89"), (Z.mk_int_var ctx "x99")
    // 1 <= x_{i,j} <= 9
    let range =        
        let r x = Z.m_le_x_le_n ctx (Z.mk_int ctx 1) x (Z.mk_int ctx 9)
        Z.mk_ands ctx [ r x11; r x21; r x31; r x41; r x51; r x61; r x71; r x81; r x91;
                        r x12; r x22; r x32; r x42; r x52; r x62; r x72; r x82; r x92;
                        r x13; r x23; r x33; r x43; r x53; r x63; r x73; r x83; r x93;
                        r x14; r x24; r x34; r x44; r x54; r x64; r x74; r x84; r x94;
                        r x15; r x25; r x35; r x45; r x55; r x65; r x75; r x85; r x95;
                        r x16; r x26; r x36; r x46; r x56; r x66; r x76; r x86; r x96;
                        r x17; r x27; r x37; r x47; r x57; r x67; r x77; r x87; r x97;
                        r x18; r x28; r x38; r x48; r x58; r x68; r x78; r x88; r x98;
                        r x19; r x29; r x39; r x49; r x59; r x69; r x79; r x89; r x99 ]
    // x11 # x21 # ... # x91
    let row_distinct = 
        Z.mk_ands ctx [ (Z.mk_distinct ctx [x11; x21; x31; x41; x51; x61; x71; x81; x91]);
                        (Z.mk_distinct ctx [x12; x22; x32; x42; x52; x62; x72; x82; x92]);
                        (Z.mk_distinct ctx [x13; x23; x33; x43; x53; x63; x73; x83; x93]);
                        (Z.mk_distinct ctx [x14; x24; x34; x44; x54; x64; x74; x84; x94]);
                        (Z.mk_distinct ctx [x15; x25; x35; x45; x55; x65; x75; x85; x95]);
                        (Z.mk_distinct ctx [x16; x26; x36; x46; x56; x66; x76; x86; x96]);
                        (Z.mk_distinct ctx [x17; x27; x37; x47; x57; x67; x77; x87; x97]);
                        (Z.mk_distinct ctx [x18; x28; x38; x48; x58; x68; x78; x88; x98]);
                        (Z.mk_distinct ctx [x19; x29; x39; x49; x59; x69; x79; x89; x99]) ]
    // x11 # x12 # ... # x19
    let col_distinct = 
        Z.mk_ands ctx [ (Z.mk_distinct ctx [x11; x12; x13; x14; x15; x16; x17; x18; x19]);
                        (Z.mk_distinct ctx [x21; x22; x23; x24; x25; x26; x27; x28; x29]);
                        (Z.mk_distinct ctx [x31; x32; x33; x34; x35; x36; x37; x38; x39]);
                        (Z.mk_distinct ctx [x41; x42; x43; x44; x45; x46; x47; x48; x49]);
                        (Z.mk_distinct ctx [x51; x52; x53; x54; x55; x56; x57; x58; x59]);
                        (Z.mk_distinct ctx [x61; x62; x63; x64; x65; x66; x67; x68; x69]);
                        (Z.mk_distinct ctx [x71; x72; x73; x74; x75; x76; x77; x78; x79]);
                        (Z.mk_distinct ctx [x81; x82; x83; x84; x85; x86; x87; x88; x89]);
                        (Z.mk_distinct ctx [x91; x92; x93; x94; x95; x96; x97; x98; x99]) ]
    let subgrid_distinct = 
        Z.mk_ands ctx [ (Z.mk_distinct ctx [x11; x21; x31;    //X--
                                          x12; x22; x32;    //---
                                          x13; x23; x33;]); //---
                        (Z.mk_distinct ctx [x41; x51; x61;    //X--
                                          x42; x52; x62;    //X--
                                          x43; x53; x63]);  //---
                        (Z.mk_distinct ctx [x71; x81; x91;    //X--
                                          x72; x82; x92;    //X--
                                          x73; x83; x93]);  //X--
                        (Z.mk_distinct ctx [x14; x24; x34;
                                          x15; x25; x35;
                                          x16; x26; x36]); 
                        (Z.mk_distinct ctx [x44; x54; x64;
                                          x45; x55; x65;
                                          x46; x56; x66]); 
                        (Z.mk_distinct ctx [x74; x84; x94;
                                          x75; x85; x95;
                                          x76; x86; x96]); 
                        (Z.mk_distinct ctx [x17; x27; x37;
                                          x18; x28; x38;
                                          x19; x29; x39]); 
                        (Z.mk_distinct ctx [x47; x57; x67;
                                          x48; x58; x68;
                                          x49; x59; x69]); 
                        (Z.mk_distinct ctx [x77; x87; x97;
                                          x78; x88; x98;
                                          x79; x89; x99]) ]
    // Known values
    let this_grid = 
        Z.mk_ands ctx [ (Z.mk_eq ctx x13 (Z.mk_int ctx 2));
                        (Z.mk_eq ctx x16 (Z.mk_int ctx 1));
                        (Z.mk_eq ctx x18 (Z.mk_int ctx 6));
                        (Z.mk_eq ctx x23 (Z.mk_int ctx 7));
                        (Z.mk_eq ctx x26 (Z.mk_int ctx 4));
                        (Z.mk_eq ctx x31 (Z.mk_int ctx 5));
                        (Z.mk_eq ctx x37 (Z.mk_int ctx 9));
                        (Z.mk_eq ctx x42 (Z.mk_int ctx 1));
                        (Z.mk_eq ctx x44 (Z.mk_int ctx 3));
                        (Z.mk_eq ctx x51 (Z.mk_int ctx 8));
                        (Z.mk_eq ctx x55 (Z.mk_int ctx 5));
                        (Z.mk_eq ctx x59 (Z.mk_int ctx 4));
                        (Z.mk_eq ctx x66 (Z.mk_int ctx 6));
                        (Z.mk_eq ctx x68 (Z.mk_int ctx 2));
                        (Z.mk_eq ctx x73 (Z.mk_int ctx 6));
                        (Z.mk_eq ctx x79 (Z.mk_int ctx 7));
                        (Z.mk_eq ctx x84 (Z.mk_int ctx 8));
                        (Z.mk_eq ctx x87 (Z.mk_int ctx 3));
                        (Z.mk_eq ctx x92 (Z.mk_int ctx 4));
                        (Z.mk_eq ctx x94 (Z.mk_int ctx 9));
                        (Z.mk_eq ctx x97 (Z.mk_int ctx 2)) ]
    let solver = ctx.MkSolver()
    solver.Add (Z.mk_ands ctx [range; row_distinct; col_distinct; subgrid_distinct; this_grid])

    match solver.Check([||]) with
    | Status.UNSATISFIABLE -> 
        Printf.printf "valid"
    | Status.SATISFIABLE -> 
        Printf.printfn "Sat."                
        let m = solver.Model
        printf 
            "%O %O %O %O %O %O %O %O %O\n%O %O %O %O %O %O %O %O %O\n%O %O %O %O %O %O %O %O %O\n%O %O %O %O %O %O %O %O %O\n%O %O %O %O %O %O %O %O %O\n%O %O %O %O %O %O %O %O %O\n%O %O %O %O %O %O %O %O %O\n%O %O %O %O %O %O %O %O %O\n%O %O %O %O %O %O %O %O %O\n" 
            (m.ConstInterp(x11)) (m.ConstInterp(x21)) (m.ConstInterp(x31)) (m.ConstInterp(x41)) (m.ConstInterp(x51)) (m.ConstInterp(x61)) (m.ConstInterp(x71)) (m.ConstInterp(x81)) (m.ConstInterp(x91))
            (m.ConstInterp(x12)) (m.ConstInterp(x22)) (m.ConstInterp(x32)) (m.ConstInterp(x42)) (m.ConstInterp(x52)) (m.ConstInterp(x62)) (m.ConstInterp(x72)) (m.ConstInterp(x82)) (m.ConstInterp(x92))
            (m.ConstInterp(x13)) (m.ConstInterp(x23)) (m.ConstInterp(x33)) (m.ConstInterp(x43)) (m.ConstInterp(x53)) (m.ConstInterp(x63)) (m.ConstInterp(x73)) (m.ConstInterp(x83)) (m.ConstInterp(x93))
            (m.ConstInterp(x14)) (m.ConstInterp(x24)) (m.ConstInterp(x34)) (m.ConstInterp(x44)) (m.ConstInterp(x54)) (m.ConstInterp(x64)) (m.ConstInterp(x74)) (m.ConstInterp(x84)) (m.ConstInterp(x94))
            (m.ConstInterp(x15)) (m.ConstInterp(x25)) (m.ConstInterp(x35)) (m.ConstInterp(x45)) (m.ConstInterp(x55)) (m.ConstInterp(x65)) (m.ConstInterp(x75)) (m.ConstInterp(x85)) (m.ConstInterp(x95))
            (m.ConstInterp(x16)) (m.ConstInterp(x26)) (m.ConstInterp(x36)) (m.ConstInterp(x46)) (m.ConstInterp(x56)) (m.ConstInterp(x66)) (m.ConstInterp(x76)) (m.ConstInterp(x86)) (m.ConstInterp(x96))
            (m.ConstInterp(x17)) (m.ConstInterp(x27)) (m.ConstInterp(x37)) (m.ConstInterp(x47)) (m.ConstInterp(x57)) (m.ConstInterp(x67)) (m.ConstInterp(x77)) (m.ConstInterp(x87)) (m.ConstInterp(x97))
            (m.ConstInterp(x18)) (m.ConstInterp(x28)) (m.ConstInterp(x38)) (m.ConstInterp(x48)) (m.ConstInterp(x58)) (m.ConstInterp(x68)) (m.ConstInterp(x78)) (m.ConstInterp(x88)) (m.ConstInterp(x98))
            (m.ConstInterp(x19)) (m.ConstInterp(x29)) (m.ConstInterp(x39)) (m.ConstInterp(x49)) (m.ConstInterp(x59)) (m.ConstInterp(x69)) (m.ConstInterp(x79)) (m.ConstInterp(x89)) (m.ConstInterp(x99))                 
        
    | Status.UNKNOWN -> 
        Printf.printf "unknown"
    | _ -> Printf.printf "x"
    
    ctx.Dispose ()
    0


