





x = 1 : [ ];
y = match x with 
   (k:ks) >>> k;
main = y;





-- BAD BAD BAD BAD




x = 1 : [ ];
y = match x with 
   ([]) >>> True,
   (k:ks) >>> 3;
main = y;




x = 1 : [ ];
y = match x with 
   ([]) >>> True,
   (k:ks) >>> k;
main = y;


