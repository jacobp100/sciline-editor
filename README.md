# Sciline Editor

Represents a math AST that can be converted to both MathML and an AST for Sciline Calculator

## AST Representation

The primary AST is represented as an array of elements in the form `` `DecimalSeparator ``

For elements that have arguments, like `nlog`, there is an `` `Arg `` element, which acts as either an argument separator or a close bracket (where the `nlog` element acted as the open bracket)

By convention, arguments that accept a number of arguments are prefixed with the number of arguments they accept. If they accept a varying number, they are prefixed by `N`

```reason
/* Empty nlog */
[|`NLog1, `Arg|]

/* Empty summation */
[|`Sum2, `Arg, `Arg|]

/* Empty 2x2 matrix (almost, see below) */
[|TableN({ numRows: 2, numColumns: 2}), `Arg, `Arg, `Arg, `Arg|]
```

There is additionally a superscript element, `` `Superscript1 ``, which accepts one argument. This is treated as its own element, and is rendered as a placeholder square with a superscript. However, in the case we are converting to MathML or a Sciline Calculator AST, and the superscript immediately precedes an element that accepts a superscript, they are merged together, much like ligatures in fonts. By convention, elements that accept a superscript are prefixed with an `S` after their argument indicator. For example, fractions accept two arguments (a numerator and denominator), and a superscript. A fraction of a half, raised to the power of 3 is represented as,

```reason
[|`Frac2S, `DigitS("1"), `Arg, `DigitS("2"), `Arg, `Superscript1, `DigitS("3"), `Arg|]
```

This does lead to it being possible to represent invalid ASTs, although this should not normally occur. In these cases, extraneous `` `Arg`` elements are dropped, and missing ones are appended to the end

Insertion and deletion of elements is able to happen directly on the array. This is useful, as it is sometimes required to get the surrounding context

### Convertion to MathML and Sciline Calculator AST

For converting to either MathML or a Sciline Calculator AST, we first do a transformation to a node-based AST. For example, The fraction example above transforms to,

```reason
type node('t) = `Frac({ fracNum: node('t), den: node('t), superscript: option(node('t)) })
```

The representation of the entire tree would be `type ast = node(list(ast))`

From there, we reduce the tree using a fold function.

```reason
type fold('accumulator, 'output) = (
  array(element),
  ~reduce: ('accumulator, node('output)) => 'accumulator,
  ~map: ('accumulator) => 'output,
  ~initialAccumulator: 'accumulator
) => 'output
```

The initial accumulator is reset for every new list of elements. For the fraction example, `fracNum`, `den`, and `superscript` each start with a fresh value of `initialAccumulator`, and the list that contained the `Frac({ ... })` would also have a fresh value

It's worth highlighting that nodes that contain child nodes are reduced with their child nodes already folded. When converting the fraction example to MathML (where `'output` is a string), `fracNum` and `den` would be strings, and `superscript` would be `option(string)`

A side note is that we never fully construct a node-based AST, as the reduction can be done at the same time as we form the AST

### Indices

Every element of the elements array is addressable by a single index. This index is imporant for MathML, so we know where to put the cursor; and also in the conversion to a Sciline Calculator AST, as if there is a parsing error, we need to return the index

To handle this, the `~reduce` and `~map` functions are the start and end index of the node (`i` and `i'`, respectively). The the `~reduce` function also gets the index of the superscript, `s`. This is `-1` if there is no superscript to make errors more obvious

MathML will attach these indices to certain MathML elements in the form `id="startIndex:endIndex` (e.g. `id="5:8"`). Every index is representable by at least one of `startIndex` or `endIndex`, with the start index taking precedence. There are never duplicates in the start indices, nor the end indices. If the start or end index, it is ignored. E.g. `id=":5"` only sets the end index to `5`, and does not record a start index

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
