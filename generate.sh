clear && bnfc -d --functor -m Atuan.cf && mv Makefile.bak Makefile && make && happy -gca --info=bad.txt Atuan/Par.y
