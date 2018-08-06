# Rockstar Lexer
 A simple Lexer for the Rockstar programming language specification.

## About

The **Rockstar Lexer** is a simple lexer (lexical analyser) written in *Haskell* with the *Alex* tool for generating lexical analysers, that recognizes the *Rockstar programming language* specification (see below). The **Rockstar Lexer** can read a file and analyse its contents, determining which valid (and invalid) tokens are present, and showing them in the standard output. Its tokens can also be used with *Happy* to create a parser (see: *Improvements and ideas*). 

## About the Rockstar programming language
According to its creator, [Dylan Beattie](https://github.com/dylanbeattie/):

>Rockstar is a dynamically typed Turing-complete programming language.
>Rockstar is designed for creating computer programs that are also song lyrics, and is heavily influenced by the lyrical conventions of 1980s hard rock and power ballads.

You can find the *Rockstar programming language* specification [here (dylanbeattie/rockstar)](https://github.com/dylanbeattie/rockstar).

## Installation
After cloning the repository,
```bash
cd rockstar-lexer
stack build --fast
```
It will prompt you the path to executable.

## Usage

If you want to analyse a file, use
```bash
/PATH_TO_EXECUTABLE/rockstar-lexer filepath 
```
or
```bash
stack exec -- rockstar-lexer filepath
```
where *filepath* is an absolute or relative path to the file you want to analyse.

You can also get (basic) help (pretty much what's stated in this section) if you use ``rockstar-lexer --help `` or ``rockstar-lexer -h `` .

## Implementation details
* Linebreaks are parsed and given their own *LineBreak* token in order to keep track of them, given that according to the *Rockstar programming language* blocks are closed by line breaks.
* Comments are also parsed as the *LineBreak* token, due to certain misbehavior when they took up a whole line (a linebreak was not accounted for, which might have been tedious for a parsing stage).
* Due to the different "formats" a number can be entered in (integers, like *2* and *35*; floating numbers, like *13.5* and *4.0*), there are two separate tokens, one for each type of number. This, however, can easily be changed in order to keep only one token that stores integers as their real counterpart (*e.g.* storing *4* as *4.0*), but this can also be taken care of in a further step.
* After the tokens are scanned, the token list is processed in order to keep only the *LineBreak* tokens that match whole blank lines (only one per line). 

## Known issues
* Due to the way Alex parses tokens, and the fact that both reserved keywords and proper variables can start with a capital letter, it was not possible at first to have a rule that considered a proper variable such as "*Mister Sandman*" or "*Mister Crowley*" as a whole. 
    * **Workaround**: The lexer parses each proper variable word as a separate proper variable. For example, in the examples above it would have reported "*Proper variable: Mister; Proper variable: Sandman*" and "*Proper variable: Mister; Proper variable: Crowley*". Therefore, a "compound" proper variable can be seen as a list of separate proper variables that can be found all together, joined by a space character.

## Improvements and ideas
* Implement an unique token for numbers (both reals and integers).
* Fix the *proper variable* rules, in order to have it recognize a whole proper variable and not each word separately.
* **Take it to the top** and build a *Rockstar interpreter*. 

## Final remarks
This the my first personal project I publish online. I'm a programming languages enthusiast and this is my second attempt on writing an interpreter (or at least one of its modules). I'm also (kind of) a newbie using Haskell, so if you check the code and have any

* Styling suggestions
* Haskell tips
* Improvement ideas
* Bug reports
* Complaints
* Cool songs you want to share with me

then feel free to open an issue or submit a pull request to let me know!