show (q1,q2,q3,q4)
where "P"(p1,p2,p3,p4) & "Q"(q1,q2,q3,q4)
& p4 = q4
& if (q1="") then (q1<-p1)
& if (q2="") then (q2<-p2)
& if (q3="") then (q3<-p3)