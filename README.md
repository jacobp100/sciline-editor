# Sciline Editor

Represents a math AST that can be converted to both MathML and an AST for Sciline Calculator

## AST Representation

The main aim of the editor is to make editing as natural as possible. And I decided having it behave mostly like a text input is natural. For reason too, every other editor decided this as well!

Because of this reason, the primary AST is represented as a a flat array of elements. An element can be anything from a digit, to an operator, to a function like `sin`. In text input analogy, each element is like a character. And in OCaml terms, each element are represented as polymorphic variant type. Some elements are just raw polymorphic variant tags that without take arguments, but where it makes parsing easier - or where required for other reasons - you can get a variant with an argument. E.g. `` `Digit("1") ``

In reality, we quickly have to break this 'just a text input' analogy. For example, a fraction has a numerator and denominator, which are both editable, and affect both rendering and parsing. Elements that act in this way accept element arguments. These completely separate from the arguments of the polymorphic variant types, and we'll get into more detail later

As well as element arguments, optional superscripts are handled in a special way

There is a strict naming convention with element. We start with the element name. If the element takes element arguments, the number of arguments is added as a suffix. If the number is dynamic, we suffix with an `N`. If they accept an optional superscript, the `S` suffix is added (after any element argument suffix)

- `` `SomeElement `` - No element arguments, no superscript
- `` `SomeElement1 `` - 1 element argument, no superscript
- `` `SomeElement2 `` - 2 element arguments, no superscript
- `` `SomeElementS `` - No element arguments, has an optional superscript
- `` `SomeElement2S `` - 2 element arguments, has an optional superscript
- `` `SomeElementNS `` - dynamic number of element arguments, has an optional superscript
- `` `DigitS("1") `` - The digit `1`, which accepts no element arguments, has an optional superscript

Putting this all together, and going back to our 'just a text input' analogy, we have can make the following inputs

```reason
/* 1 + 2 */
[|`DigitS("1"), `Add, `DigitS("2")|]

/* 1 + 2 * 3 */
[|`DigitS("1"), `Add, `DigitS("2"), `Mul, `DigitS("3")|]

/* sin 45 degrees */
[|`Function(Sin), `DigitS("4"), `DigitS("5"), `Degree|]
```

It's worth highlighting that operator precedence isn't encoded here

This format was picked because

- It makes editing easy
- It's relatively sound
- When run through BuckleScript, all the data is directly JSON encodable and decodable

### Element Arguments

Element arguments alter the way you input expressions for the element. For example, a fraction has a numerator and denominator placed out of line. This element has two element arguments and accepts a superscript, so is represented as `` `Frac2S ``

Here, we need to introduce a special element type, `` `Arg ``. This is purely semantical, and is used to indicate end of one argument. It does not render anything.

Every element that accepts element arguments must be preceded by an amount of `` `Arg `` elements equal to the number of element arguments accepted. For example, the fraction must be preceded by two `` `Arg `` elements

There is an analogy to function calls here - if you had a call to `frac(num, den)`, the function name **and** the opening bracket (`frac(`) are represented as `` `Frac2S ``, and the commas closing bracket are both represented as `` `Arg ``. Putting this together, we get

```reason
/* Empty fraction */
[|`Frac2S, `Arg, `Arg|]

/* Fraction of one half */
[|`Frac2S, `DigitS("1"), `Arg, `DigitS("2"), `Arg|]

/* Summation from 1 to 10 */
[|`Sum2, `DigitS("1"), `Arg, `DigitS("1"), `DigitS("0"), `Arg|]

/* Empty 2x2 matrix */
[|TableNS({ numRows: 2, numColumns: 2 }), `Arg, `Arg, `Arg, `Arg|]
```

It is possible to nest elements accepting arguments. An `` `Arg `` element corresponds to the most recent element accepting element arguments, until it has received all its arguments. Then it goes to the second most recent, and so forth

```reason
/* Fraction of one over another fraction of a half */
[|`Frac2S, `DigitS("1"), `Arg, `Frac2S, `DigitS("1"), `Arg, `DigitS("2"), `Arg, `Arg|]

/* Fraction of a summation from 1 to 10 over 2 */
[|`Frac2S, `Sum2, `DigitS("1"), `Arg, `DigitS("1"), `DigitS("0"), `Arg, `Arg, `DigitS("2"), `Arg|]
```

This does lead to it being possible to represent invalid ASTs, although this should not normally occur. In these cases, extraneous `` `Arg `` elements are dropped, and missing ones are appended to the end

### Superscripts

There is additionally a superscript element, `` `Superscript1 ``, which accepts one argument. This is a regular element, and - when isollated - is rendered as a placeholder square with a superscript

&#x25a1;<sup>&#x25a1;</sup>

However, in the case we are converting to MathML or a Sciline Calculator AST, and the superscript immediately precedes an element that accepts a superscript (the element is suffixed with an `S`), they are merged together, much like ligatures in fonts. If an element does not accept a superscript, it is left unaltered

1 &#x25a1;<sup>&#x25a1;</sup> &#x2192; 1<sup>&#x25a1;</sup>

1 &#x25a1;<sup>2</sup> &#x2192; 1<sup>2</sup>

sin &#x25a1;<sup>&#x25a1;</sup> &#x2192; sin &#x25a1;<sup>&#x25a1;</sup> (functions don't accept superscripts; no change)

```reason
/* Fraction of a half with an empty superscript */
[|`Frac2S, `DigitS("1"), `Arg, `DigitS("2"), `Arg, `Superscript1, `Arg|]

/* Fraction of a half raised to the power of 3 */
[|`Frac2S, `DigitS("1"), `Arg, `DigitS("2"), `Arg, `Superscript1, `DigitS("3"), `Arg|]
```

### Mutation

Insertion and deletion of elements happen directly on the array. There is no preprocessing step. This is useful, as it is sometimes required to get the surrounding context

It is impossible for a user to insert of delete `` `Arg `` elements directly. There is care to ensure that when inserting, we add the right amount of `` `Arg `` elements, and when we delete an element, that we delete the right amount too

Some elements have special insertion and deletion logic. For example, if you insert a fraction in the middle of `1 2`, you'll get a fraction of a half, and if you delete that fraction with the numerator and denominator in tact, it will revert to `1 2`. Most other elements don't let you delete them unless they're empty

The superscript encoding leads to a really natural eding experience. If you have one digit raised to a power, you could insert another digit between the the first and the superscript to move the superscript. You could also put brackets between, and the superscript is still maintained

### Convertion to MathML and Sciline Calculator AST

For converting to either MathML or a Sciline Calculator AST, we first do a transformation to a node-based AST. For example, The fraction example above transforms to

```reason
type node('t) = `Frac({ fracNum: node('t), den: node('t), superscript: option(node('t)) })
```

The representation of the entire tree would be `type ast = node(list(ast))`

From there, we reduce the tree using a fold function

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

To handle this, the `~reduce` and `~map` functions are the start (`i`) and end index of the node (`i'`)

MathML will attach these indices to certain MathML elements in the form `id="startIndex:endIndex` (e.g. `id="5:8"`). Every index is representable by at least one of `startIndex` or `endIndex`, with the start index taking precedence. There are never duplicates in the start indices, nor the end indices. If the start or end index, it is ignored. E.g. `id=":5"` only sets the end index to `5`, and does not record a start index

For an atom with a superscript, `i'` would be after the superscript. However, MathML actually needs the index of the superscript to allow inserting something before the superscript, moving the superscript. For this, any ast node with a superscript is actually represented as,

```reason
type superscript('t) = { superscriptBody: 't, index: int };
type node('t) = `Frac({ fracNum: node('t), den: node('t), superscript: option(superscript(node('t))) })
```

Using this, superscripts can have their incides encoded as follows,

```xml
<!-- 2^3 -->
<msup id="0:4">
  <mi id=":1">2</mi>
  <mi id="2:3">3</mi>
</msup>
```

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
