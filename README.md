# singularity
A very simple purely functional language for the JVM platform with the compiler written in Scala. This project was part of my bachelor's degree final thesis and even the written thesis itself can be found in this repo (it is in Polish though and of no great value anyways).

The language is a subset of Scheme enriched with a JVM directive which implements Java interoperability - user can use functions from Java standard library. There are plenty of syntactic constructs that are handled only partially, but, e.g. following programs compile correctly:

```
(define fib (lambda (n) (if (<= n 2) 1 (+ (fib (- n 1)) (fib (- n 2))))))
(write (fib 6))
```

```
(write (JVM.String.length "Answer to the Ultimate Question of Life, the Universe, and Everything"))
```

```
(define nwd (lambda (a b) (if (= b 0) a (nwd b (% a b) ) ) ))
(write (nwd 133 32))
(write (nwd 42 56))
(write (nwd 100 100))
```

You can find more examples in the `examples` directory.


## Dependencies
`scala 2.13`
`java 11`
`sbt`

## How to run
1. Inside `singularity` directory run `sbt assembly`. Fat jar will be built in `target` directory. 
2. To compile the code, run `scala FAT_JAR FILE_WITH_CODE`
where `FILE_WITH_CODE` contains valid code written in the language.
3. After executing the previous step directory named `WYNIK_KOMPILACJI` will be created. The directory name can be changed by altering the configuration parameters.
4. `cd` into newly created directory and issue `java Main` command to run the program.

## Development plan
1. Lexer & parser 
- [x] Basic data types (int, str, float, char, bool)
- [x] read and write instructions
- [x] function invokation syntax
- [x] if else then
- [x] let exprs 
- [x] pattern matching

2. ast traversal & bytecode gen
- [x] main class with main method 
- [x] read and write bytecode gen
- [x] function invokation
- [x] if else then


3. optimization & utility
- [x] import native java classes
- [x] recursion tco
