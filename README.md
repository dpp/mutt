= Monetary Unifaction Theory Thingy

An attempt by https://twitter.com/dpp[dpp] to unify
a bunch of monetary theory.

<img src='https://g.gravizo.com/svg?
 digraph G {
   main -> parse -> execute;
   main -> init;
   main -> cleanup;
   execute -> make_string;
   execute -> printf
   init -> make_string;
   main -> printf;
   execute -> compare;
 }
'/>
