# Maxent Phonotactic Learner

An implementation of the constraint-learning algorithm by Hayes and Wilson in [A Maximum Entropy Model of Phonotactics and Phonotactic Learning](http://www.linguistics.ucla.edu/people/hayes/Phonotactics/Index.htm). Implemented in [Haskell](https://www.haskell.org/).

At this point in time, the program is used as a library, passing data to the main functions via a problem-specific script. An example of such a run be seen in `TestingMain.hs`, which compiles to an executable which runs the algorithm on the H&W English onset data.

The main entry point for the algorithm is the function `ConstraintLearner.generateGrammarIO`, which takes a lexicon and list of constraint candidates and learns a grammar from those constraints (returning a list of constraint/weight pairs). The constraint candidates given to this functions as DFAs which count their violations in a string, which can be used for both local and long-distance constraints (projection is not necessary).

The module `PhonotacticGrammar` contains several Universal Grammar fuinctions which generate lists of consrtraint candidates based on a phopnological feature table. This is still under construction, but currently contains functions for generating both simple n-grams as well as the resticted trigams from the H&W paper.
