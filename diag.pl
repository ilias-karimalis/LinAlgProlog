/**/
disease(Rabies).
symptoms(Ln, Rabies) :- member(aggressiveness, Ln). 
symptoms(Ln, Rabies) :- write("y/n have you been bitten by an animal within the last 20 to 60 days?"),
     readln(Newln), member(y, Newln).

q(Ans) :- 
    write("Enter symptoms: "), flush_output(current_output),
    readln(Ln),
    symptoms(Ln, Ans).
