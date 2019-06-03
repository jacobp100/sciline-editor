# Sciline Editor

Represents a math AST that can be converted to both MathML and an AST for Sciline Calculator

## AST Representation

The primary AST is represented as an array of elements in the form `` `DecimalSeparator ``

For elements that have arguments, like `nlog`, there is an `` `Arg `` element, which acts as either an argument separator or a close bracket (where the `nlog` element acted as the open bracket)

By convention, arguments that accept a number of arguments are prefixed with the number of arguments they accept. If they accept a varying number, they are prefixed by `N`. In the example above, the elements might look like,

```reasonml
[|`NLog1, `Arg|]
```

For a summation is represented as,

```reasonml
[|`Sum, `Arg, `Arg|]
```

There is additionally a superscript element, `` `Superscript1 ``, which accepts one argument. This is treated as its own element, and is rendered as a placeholder square with a superscript. However, in the case we are converting to MathML or a Sciline Calculator AST, and the superscript immediately precedes an element that accepts a superscript, they are merged together, much like ligatures in fonts. By convention, elements that accept a superscript are prefixed with an `S` after their argument indicator. For example, fractions accept two arguments (a numerator and denominator), and a superscript. A fraction of a half, raised to the power of 3 is represented as,

```reasonml
[|`Frac2S, `DigitS("1"), `Arg, `DigitS("2"), `Arg, `Superscript1, `DigitS("3"), `Arg|]
```

This does lead to it being possible to represent invalid ASTs, although this should not normally occur. In these cases, extraneous `` `Arg`` elements are dropped, and missing ones are appended to the end

Insertion and deletion of elements is able to happen directly on the array. This is useful, as it is sometimes required to get the surrounding context

For converting to either MathML or a Sciline Calculator AST, we first do a transformation to a node-based AST. For example, The fraction example above transforms to,

```reasonml
type node('t) = `Frac({ fracNum: node('t), den: node('t), superscript: option(node('t)) })
```

The representation of the entire tree would be `type ast = node(list(ast))`

From there, we reduce the tree using a fold function.

```reasonml
type fold('accumulator, 'output) = (
  array(element),
  ~reduce: ('accumulator, node('output)) => 'accumulator,
  ~map: ('accumulator) => 'output,
  ~initualAccumulator: 'accumulator
) => 'output
```

It's worth highlighting that nodes that contain child nodes are reduced with their child nodes already reduced. When converting the fraction example to MathML (where `'output` is a string), `fracNum` and `den` would also be strings, and `superscript` would be `option(string)`

A side note is that we never fully construct a node-based AST, as the reduction can be done at the same time as we form the AST

## BuckleScript

### Build

```
npm run build
```

### Watch

```
npm run watch
```

### Editor

If you use `vscode`, Press `Windows + Shift + B` it will build automatically
