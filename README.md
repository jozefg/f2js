## f2js

f2js is a backend for a lazy functional language to compile to
JavaScript. It won't be particularly fast or featureful, but it will
hopefully be correct.

The goal is to build a tool so going from "This is a cute type
checker" to "This is a cute language" is a smaller leap for me.

Current list of things to do:

 - Implement code generation
 - Implement the run time system
 - Add lots of tests
 - Generate better STG by grouping LetRec's where possible
 - Drop the three-stack STG machine for tagging
