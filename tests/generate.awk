#!/usr/bin/awk -f

BEGIN {
    ntests = 0;
    print "import Test.HUnit";
    print "import System.Exit (exitSuccess, exitFailure)";
}

/--/ { sub(/[ \t]*--.*$/, ""); } # rstrip comments

/^>/ {
    sub(/>[ \t]*/, "");
    print;
    next;
}

/==>/ {
    printf "\n%s = TestCase", sprintf("t_%d", ++ntests);
    match($0, /^(.*[^ \t])[ \t]+==>[ \t]+(.*)$/, m);
    printf " $ assertEqual \"\" (%s) (%s)\n", m[2], m[1];
}

END {
    printf "\nmain = do\n";
    printf "  counts <- runTestTT $ TestList [";
    if (ntests) {
	printf "t_1";
	for (i = 2; i <= ntests; i++)
	    printf ", t_%d", i;
    }
    print "]";
    print "  if errors counts + failures counts == 0";
    print "    then exitSuccess else exitFailure";
}