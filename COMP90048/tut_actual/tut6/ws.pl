win(r,s).
win(s,p).
win(p,r).
friend(john, julia).
friend(john, jack).
friend(julia, sam).
friend(julia, molly).
canwin:-win(W,X).