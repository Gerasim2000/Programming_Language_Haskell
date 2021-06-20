show (x1,x2,x3,x4)
where "P"(x1,x2,x3,x4) & "Q"(z1,z2,z3,z4)
& x1=z1
& if (x2="") then (x2<-z2) else (x2<-x2)
& if (x3="") then (x3<-z3) else (x3<-x3)
& if (x4="") then (x4<-z4) else (x4<-x4)