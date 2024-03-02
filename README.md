# Cheap-Patmatch for Common Lisp

Cheap-patmatch is a DSL for pattern matching in Common Lisp. It's a regular expression language for text strings.
It is not a pattern matcher for deeply-nested cons cells. In other words, it's for textual S-expressions, not for S-expressions
that have already been parsed into memory by the Lisp reader. If you need the latter, you probably want something like [trivia](https://github.com/guicho271828/trivia).

Having said that, Although I wrote cheap-patmatch for mining Lisp source code, it's a general text analyzer and I've used it 
for parsing text other than Lisp source code. This means cheap-patmatch is much like grep but with Lispy syntax to describe patterns.
And cheap-patmatch can create recursive patterns, which are necessary for parenthesis matching. Grep cannot do this.

## Why did I write Cheap-Patmatch?

- I'm sick of trying to understand 17 different flavors of regular expressions.
- I'm sick of regular expressions that are write-only. I don't care if the regex is verbose; I
   just want it to be understandable by me tomorrow.
- I don't particularly care if the regex matcher is as fast as possible; clarity trumps speed.
- I'm sick of regular expressions that confuse and intermix the desire for

  1. A single binary result of whether a match happened.
  2. A numeric position within the string where the match happened, or where it failed as may be the case.
  3. A captured, named substring that was matched in the string.

## Why not just use CL-PPCRE?

Mainly because I didn't know how to use it, and I figured if I wrote the thing from scratch I'd remember how to use it. Ha!
And because I wanted to dig deeply into what was necessary to match parentheses recursively. Mission accomplished.

## Why not just use the Lisp reader?

The Lisp reader behaves according to context: Variables such as \*read-eval\*, \*read-suppress*\, \*read-base\*, etc change what it does,
as do read-time conditionals and packages. Package names will throw errors if they're encountered before they're defined or they have a single
colon after them but the symbol after the colon hasn't been exported. In addition, the Lisp reader throws away too much information. Because
it drops comments on the floor, #'read is useless for capturing comments. It also (by default) upcases all symbols so there's no way
to determine what the original case of a symbol was after it's been read.

Cheap-Patmatch doesn't depend on the Lisp reader (though the :read-token mechanism in the DSL lets the programmer invoke it if
desired). It strictly deals with sequences of characters. It has lookahead and capture. The presence of lookahead means it only works
on strings#| for now; lookahead with streams is difficult and requires more infrastructure wrapped around Lisp streams.

** Syntax
See examples and tests for syntax.

** Clauses

These are the pattern clauses.
In the below,
- \<clause\> is one of the pattern clauses.
- \<clause\>\* is one or more pattern clauses.
- \<s\> must be a literal string.
- \<fn\> must be one of: (see #'massage-arg-into-fn)
  - A single-argument function which is given the next character and is treated as a predicate.
  - A symbol, which is assumed to be a single-argument function as above.
  - A character, which gets turned into a function that matches just that character, case-sensitively.
  - \<s\>, which gets turned into a function that matches any character in that string, case-sensitively.
  - A 2-list of (:case-insensitive \<s\>), which gets turned into a function that matches any character in that string, case-insensitively.
  - A 2-list of (:case-sensitive \<s\>). This behaves exactly like the case of \<s\> by itself.
  - \<name\> must be a symbol or the literal NIL.
  - \<nns\>  must be a non-nil symbol.


| \<s\> |  A solitary string can itself be a clause. In which case the characters in that string will be matched sequentially and case-insensitively. |
|:-|:---|
| \<nns\> | A non-nil symbol can itself be a clause, in which case it is assumed to be the name of a set of clauses previously defined with the :named construct. |
| \(\:string \<s\>) |  Match a literal string e.g. (:string "foo"). This form is not really necessary; you can just say "foo" by itself instead. |
| \(\:lookahead-string \<s\>) | Like :string but this one does not change the current position. Therefore any subsequent pattern will start at the same place this pattern did. e.g. (:seq (:lookahead-string "foo") "foobar") will succeed on the string "foobar". |
| \(:one-or-more \<fn\>) | Require at least one character at current position for which fn returns true. New state's pos will be one beyond the last char where the fn returned true. |
| (:zero-or-more \<fn\>)) | Require zero or more characters at current position for which fn returns true. New state's pos will be one beyond the last char where the fn returned true. The key distinction here is that if we reach the end of the string before we even have a chance to call the function, we still succeed. |
| (:one \<fn\>) | Require a character at current position for which fn returns true. If there's no next character after that, we succeed and increment pos by 1. If there is a next character after that, require fn called on it to return false, and if so we succeed and increment pos by 1. Thus this does single-character lookahead beyond the current position. |
|  (:one-nongreedy \<fn\>) | Require a character at current position for which fn returns true. Does not do lookahead beyond the current position; this only looks at a single character and succeeds if it finds one, and increments the state position by 1. |
| (:seq <clause>*) | Perform pattern clauses in order. Each updates position and captures before proceeding to next clause. If any clause fails, overall pattern fails. (The literal :seq keyword is not usually required except to create a sequential pattern inside another meta-pattern keyword like :and, :or, or :not. In all other cases :seq is implied.) |
| (:capture \<name\> \<clause\>\*) | Perform pattern clauses in order. This is identical to :seq except if all the pattern clauses result in a successful match, the substring that matches will be pushed onto the captures list of the state object, and #'ppatmatch will return them. \<name\> can be a string or a non-NIL symbol in which case that name will be consed onto the matching string before being pushed on the captures list so you can find it quickly. \<name\> can also be NIL, in which case the matching string will be pushed 'naked' onto the captures list.
| (:named \<nns\> \<clause\>\*) | Perform pattern clauses in order. This is identical to :seq except it gives the set of clauses that follow a name. That name can then be used _within_ those clauses to create recursive patterns. |
| (:not \<clause\>) | Only a single pattern clause should follow. If that clause fails, overall clause succeeds, and vice-versa.|
| (:or \<clause\>\*)  | Perform pattern clauses in order. Each starts at same string position as previous, and updates captures where appropriate. If any clause succeeds, overall pattern succeeds. Has short-circuit behavior. |
| (:and \<clause\>\*) | Perform pattern clauses in order. Each starts at same string position as previous, and updates captures where appropriate. If any clause fails, overall pattern fails. Has short-circuit behavior. |
| (:break) | Throws a break. Useful for debugging. Breaks always succeed, so continuing after the break just continues. :or makes a special provision for :break so it won't short-circuit. :not makes no such provision; an enclosed (:break) just causes overall failure. |

