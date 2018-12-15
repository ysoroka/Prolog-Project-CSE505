# Prolog-Project-CSE505
### Bi-directional definite-clause grammar for converting problems specified in Controlled Natural Language to Answer Set Programs, and verbalizing them back into language

This project is based on the paper ["Specifying and Verbalising Answer Set Programs in Controlled Natural Language" by Rolf Schwitter](https://www-cambridge-org.proxy.library.stonybrook.edu/core/journals/theory-and-practice-of-logic-programming/article/specifying-and-verbalising-answer-set-programs-in-controlled-natural-language/F9B6775E7B491C8C6B22194435E22AFB). I recreated what was done in the paper but on a much smaller scale, focusing on creating a bi-directional grammar suitable for both processing and generation of program specifications in CNL.

## Usage Instructions
### Running the program
The program **big.pl** (stands for **Bi**-directional **G**rammar) should be run using [SWI-Prolog](http://www.swi-prolog.org/Download.html). To give it a test run, simply load the file using command: ``` [big]. ``` And then query: ``` test. ```

Having done that, you will see the output of the program for a pre-defined example.

### Customizing input 
**If you want to run the program on YOUR OWN INPUT SENTENCES**, you have to modify the file **big.pl**:
1. Change the variable **Sentences** in **test** (line 203).

Note that each sentence must be a list of words, where all words must be separated by a comma, and a sentence must end with the special symbol **'.'**. Each sentence must start with the system word **'every'**/**'no'** or have an agent in it, e.g. like in **[bob,dreams,'.']**, where **bob** is an agent. System words are: **'every', 'no', 'is', 'is not', 'does not', '.'**.
See **examples.txt** for samples of input sentences that you can try running.

2. Broaden the **lexicon**.

In case you'd like to introduce new words and/or agents not included in the program, you have to add them yourself in **big.pl**. Current **lexicon** is defined in lines 175-195. For each of your custom words, add a line following the format of:
```
lexicon(cat:noun, wform:[student], arg:X, term:student(X)).
```
where **cat** argument is the category of the word (noun, verb, det, etc.); **wform** stands for word-form, i.e. the exact word that appears in a sentence; **arg** specifies argument of the term that the word refers to; **term** denotes the actual term that will be used in the internal format and in the ASP. For each of your custom agents, add a line of the format ``` agent(name). ```, where **name** is the name of your agent, e.g. ```bob```.

Note that currently the program only supports the following categories: noun (```cat:noun```), verb (```cat:iv```), determinant (```'every', 'no'```), adjective (```cat:jj```). All sentences must have a simple structure, no articles ("a", "an", "the"), no "and"/"or"-linked components.



## Copyright and attribution
Created by [Eugenia Soroka](https://github.com/EugeniaSoroka), Ph.D. student, Stony Brook University, NY (CSE505: Computing with Logic, Fall 2018)

Special thanks to [Rolf Schwitter](http://web.science.mq.edu.au/~rolfs/) for guidance on how to get started and sharing useful resources.
