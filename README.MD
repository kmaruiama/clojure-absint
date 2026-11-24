# Abstract Interpretation in Clojure

This project is part of my upcoming undergraduate thesis. What I aim to achieve
with it is to do some handy analysis for Clojure programs, including:

- flagging unsafe nils to avoid NPE throws;
- inferring what a certain dataflow may need to properly function, like keys 
from a map, categorization of the information as present/optional/co-dependent
(meaning, if :foo exists as a key in a map, :bar should exist as well), (note
that this may not be the exact final taxonomy used);
- flagging predicate application over lazy-seqs.

Documentation will be provided in the future, for now the project supports 
concretely interpreting single s-expressions. 

LLM DISCLAIMER:
- All code is human-written unless explicitly specified in the docstrings
(search by using CTRL + F + "@llm"). Namespaces flagged with @llm are 
100% LLM-written unless explicitly specified with @person;
- LLMs are used to research Clojure's internal evaluation processes, together
with first-hand documentation (which will be referenced in the future);
- LLMs are used to fix grammatical mistakes;
- LLMs are used as an expensive VIM for boring find/replaces.