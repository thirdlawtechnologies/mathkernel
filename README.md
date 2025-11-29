# mathkernel


Generate math kernels for non-linear optimization and dynamics.


## Usage

```
sbcl
(ql:quickload :cando-kernels)
(energy-kernels:write-all "/tmp/code/")
```
This builds molecular modeling kernels for evaluating stretch, angle, dihedral, nonbond and nonbond+distance-dependent-dielectric+cutoff terms
and writes optimized C code into /tmp/code/.

Ask ChatGPT what that means if you want to learn more.  It's math for building molecules.


## Documentation

[Documentation for defkernel](docs/mathkernel.md)


## Authors

This application was vibe-coded using ChatGPT5.1 by Christian Schafmeister over five days starting on Nov 23 2025.
I implemented an application like this 25 years ago in Mathematica and it drives me crazy everytime I have to get it
working again to make changes. We implemented Cando (https://github.com/cando-developers/cando.git) and 
Clasp (https://github.com/clasp-developers/clasp.git). Cando is a Common Lisp implementation to write complex molecular
modelling code. Why?  Because if you are going to write software to build molecules you should write it in the language
of the universe (lisp).  Also - Python sucks.

I started with a $20/month OpenAI account and things started very nicely but after a day I noticed
that ChatGPT was running out of steam and starting to ping-pong between approaches and forgetting that
I had given up on approaches.  So I upgraded my account to the $200/month and pasted all the code
I had up to that point into a project and started from there.  It was impressively "smarter", it suggested ideas
that had more depth and it seemed to have a much better grasp of the problem.
Over the next four days I asked for features one at a time and ChatGPT kept giving me code that I would test.
I asked it for regression tests and kept adding them and testing functions and finding and fixing bugs.
There were no serious bugs and the project seemed to improve smoothly.  I learned a lot about automatic
differentiation, rule rewriting and optimization passes. 

After five days - 10,846 lines of code (simple wc count)

Thoughts:
1. I'm amazed... and so excited by how powerful this is.
2. ChatGPT generated bugs - no question. I spent about half the time finding and fixing them with
ChatGPT help
3. ChatGPT has a problem with closing parentheses for very long functions.  I found that it helped
to ask it to write the code as an abstract syntax tree and then convert that to code.  It also likes to
use 't' as a variable (a Common Lisp no no).  I told it to stop doing that.
