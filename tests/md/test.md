<!-- (|
  registries = [
    (|
      name = `default`,
      remote =
        Git(|
          url = `https://github.com/SATySFi/default-registry`,
          branch = `format-1`,
        |)
    |)
  ],
  dependencies = [
    (| name = `md-ja`, registry = `default`, requirement = `^0.0.1` |),
  ],
|) -->
<!-- MDJa -->
<!-- (|
  title = {A Type System of Labeled Optional Parameters},
  author = {gfn},
|) -->

I am in the process of changing the language design for SATySFi v0.1.0 about optional parameters. It will be a breaking change (i.e. not backward compatible with SATySFi v0.0.x), and I will introduce it here.

First appearance:

- Dec 30, 2021. [SATySFi Wiki](https://github.com/gfngfn/SATySFi/wiki/%E3%83%A9%E3%83%99%E3%83%AB%E3%81%A4%E3%81%8D%E3%82%AA%E3%83%97%E3%82%B7%E3%83%A7%E3%83%B3%E5%BC%95%E6%95%B0%E3%81%AE%E5%9E%8B%E3%82%B7%E3%82%B9%E3%83%86%E3%83%A0) (written in Japanese)


# Motivation of the Reformulation

Originally, SATySFi v0.0.x has a mechanism of non-labeled optional parameters/arguments, and it can be used with `?:` like the following:

```
+section?:(`sec:sample`)?:(`Sample`){Sample}<
  ... the contents of the section ...
>
```

Optional arguments do not have a label, and thereby are distinguished by their order. In the example above, the first optional argument is a tag for the cross reference, and the second one is for the title of the section displayed in the Outline of the resulting PDF file. As well as commands, ordinary functions can take optional parameters:

```
let increment ?diff-opt n =
  match diff-opt with
  | Some(d) -> n + d
  | None    -> n + 1
in
(increment 42, increment ?:57 42)
  % --> (43, 99)
```

The reason why a mechanism of non-labeled optional parameters was adopted is that the language design was inspired by that of LaTeX, where optional parameters are conventionally given in a non-labeled manner:

```
\parbox[c]{6em}{…}, $\sqrt[3]{2}$
```

It has, however, lead to the following significant inconvenience, partly because there came to be more use cases of optional parameters than expected:

- Since optional arguments are distinguished only by their order, in many cases where you use or read functions that have optional parameters it is difficult to comprehend in what order each argument should be specified.

- When you want to omit a former parameter and to give an argument to a latter one, you have to do some cumbersome workaround–to “explicitly omit” the former one by `?*`.

  ```
  +section?*?:(`Sample`){Sample}<
    ... the contents of the section ...
  >
  ```

To remove this kind of burden, I came upon the idea of reformulating the mechanism of optional parameters into labeled one where one can give optional arguments like the following:

```
+section?(ref = `sec:sample`, outline = `Sample`){Sample}<
  ... the contents of the section ...
>
```

As you can see, optional arguments can be specified by `?(label1 = arg1, …, labelN = argN)`. You can omit the whole part `?(...)` if you don’t want to specify any optional argument. Ordinary functions can take labeled optional parameters as well:

```
let increment ?(diff = diff-opt) n =
  match diff-opt with
  | Some(d) -> n + d
  | None    -> n + 1
  end
in
(increment 42, increment ?(diff = 57) 42)
```

This will bring the following benefits:

- Since optional arguments are labeled, it is easy to read for what purpose each argument is specified.
- Since optional arguments can be in a ramdom order, you don’t have to do any workaround like the one using `?*`.
- Because types of optional parameters/arguments can be statically inferred as usual, type errors will be reported when one gives an argument of a type incompatible to the expected one.

It seems also good to me that one can give default values as follows when defining optional parameters:

```
let increment ?(diff = d = 1) n =
  n + d
in
(increment 42, increment ?(diff = 57) 42)
```


# The Detail of the Type System

## Extension to Function Types, Function Abstractions, and Function Applications

(omitted)


## Polymorphism

(omitted)


# Conclusion

This article has introduced a mechanism of labeled optional parameters designed and implemented for SATySFi v0.1.0, and explained that it is based on the row polymorphism.


# References

1. Benedict R. Gaster and Mark P. Jones. [A polymorphic type system for extensible records and variants](https://web.cecs.pdx.edu/~mpj/pubs/96-3.pdf), 1996.


# Console

```console
$ satysfi build test.md -o test.tex
```
