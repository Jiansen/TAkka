def
    a(1) & b(1) & c1(n) = c1(n+1)
or  c(2) & b(2) & d(2) & c2(n) = c2(n+1)
or  b(3) & d(3) & e(3) & c3(n) = c3(n+1)
or  e(4) & c4(n) = c4(n+1) 
or  c1(n) & get1() = c1(n) & reply n to get1
or  c2(n) & get2() = c2(n) & reply n to get2
or  c3(n) & get3() = c3(n) & reply n to get3
or  c4(n) & get4() = c4(n) & reply n to get4
;;

(*
join {
case A(1) and B(1) => { c1 +=
case C(2) and B(2) and D(2) =>
case B(3) and D(3) and E(3) =>
case E(4)=> { c4 += 1 }
}
*)





spawn c1(0) & c2(0) & c3(0) & c4(0)
;;
(*
spawn
e(4) & b(2) & b(3) & b(1) & c(2) & 
a(1) & b(2) & b(2) & b(2) & b(1) & 
b(2) & e(3) & d(2) & b(2) & b(3) & 
e(3) & d(3) & b(2) & d(2) & a(1) & 
d(3) & d(3) & c(2) & b(2) & e(3) & 
e(4) & b(3) & e(4) & b(3) & b(2) & 
b(3) & b(2) & d(3) & d(2) & b(2) & 
d(2) & e(3) & a(1) & d(3) & d(3) & 
d(2) & a(1) & d(2) & b(2) & b(3) & 
e(3) & b(2) & e(3) & b(3) & b(3) & 
e(4) & a(1) & e(3) & a(1) & d(3) & 
c(2) & d(3) & b(2) & c(2) & d(2) & 
b(3) & e(4) & e(3) & b(3) & a(1) & 
b(3) & a(1) & b(2) & b(3) & b(3) & 
d(3) & e(3) & d(3) & e(4) & b(2) & 
b(3) & b(3) & e(4) & b(2) & b(1) & 
e(3) & c(2) & b(3) & e(3) & b(3) & 
b(1) & d(2) & e(3) & c(2) & d(3) & 
a(1) & b(3) & b(2) & b(2) & b(2) & 
b(2) & a(1) & b(1) & a(1) & e(4)
;;
*)


spawn 
c(1) & e(3) & d(1) & e(4) & c(1) &
b(1) & e(1) & c(3) & b(2) & e(4) & 
b(1) & c(2) & b(3) & b(3) & e(4) & 
e(4) & c(3) & e(1) & e(2) & e(3) & 
b(3) & d(2) & a(2) & e(3) & a(2) & 
e(2) & d(1) & d(2) & b(3) & a(3) & 
c(2) & a(1) & e(4) & e(1) & b(2) & 
e(1) & b(2) & a(3) & c(3) & b(2) & 
a(1) & a(3) & e(2) & d(3) & a(2) & 
b(3) & c(1) & e(2) & b(1) & c(3) & 
d(2) & c(2) & e(4) & a(2) & a(2) & 
d(2) & c(3) & c(3) & a(3) & a(3) & 
a(3) & d(2) & c(1) & a(3) & b(1) & 
c(1) & b(2) & a(1) & e(2) & b(3) & 
b(3) & a(1) & e(4) & d(1) & a(2) & 
d(1) & c(1) & b(3) & a(1) & d(3) & 
e(1) & c(3) & e(3) & d(1) & c(1) & 
b(2) & b(3) & c(1) & b(2) & d(1) & 
e(1) & b(1) & a(3) & e(4) & b(2) & 
b(2) & e(4) & b(2) & e(2) & b(3)
;;


(*print_string "c1 = ";; *)print_int(get1());; (*print_newline();;*)

(*print_string "c2 = ";; *)print_int(get2());; (*print_newline();;*)

(*print_string "c3 = ";; *)print_int(get3());; (*print_newline();;*)

(*print_string "c4 = ";; *)print_int(get4());; (*print_newline();;*)
