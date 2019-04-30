open Types;

let ensurePlaceholder = x =>
  switch (x) {
  | [] => [`Placeholder([])]
  | args => args
  };
let ensurePlaceholders = (element: t): t =>
  switch (element) {
  | `Magnitude(exponent) => `Magnitude(ensurePlaceholder(exponent))
  | `Frac({fracNum, den, superscript}) =>
    `Frac({
      fracNum: ensurePlaceholder(fracNum),
      den: ensurePlaceholder(den),
      superscript,
    })
  | `Sqrt({rootRadicand, superscript}) =>
    `Sqrt({rootRadicand: ensurePlaceholder(rootRadicand), superscript})
  | `NRoot({nrootDegree, radicand, superscript}) =>
    `NRoot({
      nrootDegree: ensurePlaceholder(nrootDegree),
      radicand: ensurePlaceholder(radicand),
      superscript,
    })
  | `NLog({nlogBase}) => `NLog({nlogBase: ensurePlaceholder(nlogBase)})
  | `Abs({unaryArg, superscript}) =>
    `Abs({unaryArg: ensurePlaceholder(unaryArg), superscript})
  | `Floor({unaryArg, superscript}) =>
    `Floor({unaryArg: ensurePlaceholder(unaryArg), superscript})
  | `Ceil({unaryArg, superscript}) =>
    `Ceil({unaryArg: ensurePlaceholder(unaryArg), superscript})
  | `Round({unaryArg, superscript}) =>
    `Round({unaryArg: ensurePlaceholder(unaryArg), superscript})
  | `RandInt({randIntA, b, superscript}) =>
    `RandInt({
      randIntA: ensurePlaceholder(randIntA),
      b: ensurePlaceholder(b),
      superscript,
    })
  | `NPR({statN, r}) =>
    `NPR({statN: ensurePlaceholder(statN), r: ensurePlaceholder(r)})
  | `NCR({statN, r}) =>
    `NCR({statN: ensurePlaceholder(statN), r: ensurePlaceholder(r)})
  | `Differential({differentialX, body}) =>
    `Differential({
      differentialX: ensurePlaceholder(differentialX),
      body: ensurePlaceholder(body),
    })
  | `Integral({integralA, b, body}) =>
    `Integral({
      integralA: ensurePlaceholder(integralA),
      b: ensurePlaceholder(b),
      body: ensurePlaceholder(body),
    })
  | `Sum({rangeStart, rangeEnd}) =>
    `Sum({
      rangeStart: ensurePlaceholder(rangeStart),
      rangeEnd: ensurePlaceholder(rangeEnd),
    })
  | `Product({rangeStart, rangeEnd}) =>
    `Product({
      rangeStart: ensurePlaceholder(rangeStart),
      rangeEnd: ensurePlaceholder(rangeEnd),
    })
  | `Table({tableElements} as t) =>
    `Table({
      ...t,
      tableElements: Belt.Array.map(tableElements, ensurePlaceholder),
    })
  | (
      `Base(_) | `Operator(_) | `Function(_) | `OpenBracket | `DecimalSeparator |
      `Conj |
      `Factorial |
      `Degree |
      `ArcMinute |
      `ArcSecond |
      `Placeholder(_) |
      `ImaginaryUnit(_) |
      `CloseBracket(_) |
      `Digit(_) |
      `Variable(_) |
      `Constant(_) |
      `CustomAtom(_) |
      `Rand(_)
    ) as e => e
  };

let normalizeRow = elements =>
  switch (elements) {
  | [`Placeholder([])] as x => x
  | x =>
    Belt.List.keepMap(x, element =>
      switch (element) {
      | `Placeholder([]) => None
      | _ => ensurePlaceholders(element)->Some
      }
    )
  };

let isDigitLike = x =>
  switch (x) {
  | `Digit(_) => true
  | `DecimalSeparator => true
  | _ => false
  };
let getRightArguments = (elements, i, rest) => {
  let before = elements->ListUtil.takeUpto(i);
  let (rightArg, after) = rest->ListUtil.splitWhile(isDigitLike);
  (rightArg, (before, after));
};
let getBinaryArguments = (elements, i, rest) => {
  let (leftArg, before) =
    elements
    ->ListUtil.takeUpto(i)
    ->Belt.List.reverse
    ->ListUtil.splitWhile(isDigitLike);
  let leftArg = Belt.List.reverse(leftArg);
  let before = Belt.List.reverse(before);
  let (rightArg, after) = rest->ListUtil.splitWhile(isDigitLike);
  (leftArg, rightArg, (before, after));
};

let rec expandInsertedElementsInRow = elements => {
  let rec iter = (i, e) =>
    switch (e) {
    | [`Frac({fracNum: [], den: [], superscript}), ...rest] =>
      let (fracNum, den, accum) = getBinaryArguments(elements, i, rest);
      combineElements(`Frac({fracNum, den, superscript}), accum);
    | [`NRoot({nrootDegree: [], radicand: [], superscript}), ...rest] =>
      let (nrootDegree, radicand, accum) =
        getBinaryArguments(elements, i, rest);
      combineElements(`NRoot({nrootDegree, radicand, superscript}), accum);
    | [`Sqrt({rootRadicand: [], superscript}), ...rest] =>
      let (rootRadicand, accum) = getRightArguments(elements, i, rest);
      combineElements(`Sqrt({rootRadicand, superscript}), accum);
    | [_, ...rest] => iter(i + 1, rest)
    | [] => elements
    };
  iter(0, elements);
}
and combineElements = (element, (before, after)) =>
  if (before == [] && after == []) {
    [element];
  } else {
    Belt.List.concat(
      before,
      [element, ...expandInsertedElementsInRow(after)],
    );
  };

/* This preserves indices */
let separateAtomLikeSuperscript = element =>
  switch (element) {
  | `Digit({atomNucleus, superscript}) =>
    Some((`Digit({atomNucleus, superscript: []}), superscript))
  | `Variable({atomNucleus, superscript}) =>
    Some((`Variable({atomNucleus, superscript: []}), superscript))
  | `Constant({constant, superscript}) =>
    Some((`Constant({constant, superscript: []}), superscript))
  | `CustomAtom({customAtomValue, mml, superscript}) =>
    Some((`CustomAtom({customAtomValue, mml, superscript: []}), superscript))
  | `ImaginaryUnit(superscript) => Some((`ImaginaryUnit([]), superscript))
  /* | `CloseBracket(superscript) => Some((`CloseBracket([]), superscript)) */
  | _ => None
  };
let separateSuperscripts = (elements: list(t)): list(t) =>
  TreeUtil.map(
    elements,
    MutableListBuilder.empty,
    ({TreeUtil.accum: elements}) => MutableListBuilder.toList(elements),
    ({TreeUtil.accum}, element) =>
      switch (separateAtomLikeSuperscript(element)) {
      | Some((element, superscript)) =>
        accum
        ->MutableListBuilder.append(element)
        ->MutableListBuilder.append(`Placeholder(superscript))
      | _ => MutableListBuilder.append(accum, element)
      },
  );
let prepareForMutation = separateSuperscripts;

let coalesceWithSuperscript = (element: t, superscript: list(t)): option(t) =>
  switch (element) {
  | `CloseBracket([]) => Some(`CloseBracket(superscript))
  | `ImaginaryUnit([]) => Some(`ImaginaryUnit(superscript))
  | `Digit({atomNucleus, superscript: []}) =>
    Some(`Digit({atomNucleus, superscript}))
  | `Variable({atomNucleus, superscript: []}) =>
    Some(`Variable({atomNucleus, superscript}))
  | `Constant({constant, superscript: []}) =>
    Some(`Constant({constant, superscript}))
  | `CustomAtom({customAtomValue, mml, superscript: []}) =>
    Some(`CustomAtom({customAtomValue, mml, superscript}))
  | `Frac({fracNum, den, superscript: []}) =>
    Some(`Frac({fracNum, den, superscript}))
  | `Sqrt({rootRadicand, superscript: []}) =>
    Some(`Sqrt({rootRadicand, superscript}))
  | `NRoot({nrootDegree, radicand, superscript: []}) =>
    Some(`NRoot({nrootDegree, radicand, superscript}))
  | `Abs({unaryArg, superscript: []}) => Some(`Abs({unaryArg, superscript}))
  | `Floor({unaryArg, superscript: []}) =>
    Some(`Floor({unaryArg, superscript}))
  | `Ceil({unaryArg, superscript: []}) =>
    Some(`Ceil({unaryArg, superscript}))
  | `Round({unaryArg, superscript: []}) =>
    Some(`Round({unaryArg, superscript}))
  | _ => None
  };
let rec coalescePlaceholderSuperscripts = elements =>
  switch (elements) {
  | [element, `Placeholder(superscript) as kept, ...rest] =>
    switch (element->coalesceWithSuperscript(superscript)) {
    | Some(coalescedElement) => [
        coalescedElement,
        ...coalescePlaceholderSuperscripts(rest),
      ]
    | None => [element, kept, ...coalescePlaceholderSuperscripts(rest)]
    }
  | [x, ...rest] => [x, ...coalescePlaceholderSuperscripts(rest)]
  | [] => []
  };
let finalizeMutation = elements => coalescePlaceholderSuperscripts(elements);

type insertionFlags =
  | AnyElement
  | NonTableElements;

let reduceSelectorFlags = (existingFlags, selector) =>
  switch (selector) {
  | TreeUtil.Superscript
  | Subscript
  | Upper
  | Lower
  | FracDen
  | RootRadicand
  | RootDegree
  | TableCell => NonTableElements
  | FracNum
  | Inner => existingFlags
  };

let canInsert = (flags, element) =>
  switch (flags, element) {
  | (NonTableElements, `Table(_)) => false
  | _ => true
  };

type insertion =
  | NotInserted
  | Inserted
  | InsertionFailed;

let insertIndex = (elements, newElement, index) =>
  if (elements == []) {
    Some((normalizeRow([newElement]), 1));
  } else {
    let finalize = arg => {
      let {
        TreeUtil.accum: elements,
        rollup: inserted,
        context,
        rangeStart,
        rangeEnd,
      } = arg;

      let (inserted, elements) =
        if (inserted != NotInserted
            || rangeStart == rangeEnd
            || index != rangeEnd) {
          (inserted, elements);
        } else if (!canInsert(context, newElement)) {
          (InsertionFailed, elements);
        } else {
          let nextElements = MutableListBuilder.append(elements, newElement);
          (Inserted, nextElements);
        };
      let elements =
        MutableListBuilder.toList(elements)
        ->finalizeMutation
        ->expandInsertedElementsInRow
        ->normalizeRow;
      (inserted, elements);
    };

    let reduceFn = (arg, element) => {
      let {TreeUtil.accum: elements, rollup: inserted, rangeStart: i, context} = arg;

      let (inserted, elements) =
        if (inserted != NotInserted || index != i) {
          (inserted, elements);
        } else if (!canInsert(context, newElement)) {
          (InsertionFailed, elements);
        } else {
          let nextElements = elements->MutableListBuilder.append(newElement);
          (Inserted, nextElements);
        };
      (inserted, elements->MutableListBuilder.append(element));
    };

    let (inserted, elements) =
      TreeUtil.rollup(
        ~initialRollup=NotInserted,
        ~mapContext=reduceSelectorFlags,
        ~initialContext=AnyElement,
        prepareForMutation(elements),
        MutableListBuilder.empty,
        finalize,
        reduceFn,
      );

    switch (inserted) {
    | Inserted => Some((elements, index + 1))
    | _ => None
    };
  };

let isEmpty = element =>
  switch (element) {
  | []
  | [`Placeholder([])] => true
  | _ => false
  };
let shouldDeleteElement = (element: t): bool =>
  switch (element) {
  | `Base(_)
  | `Operator(_)
  | `OpenBracket
  | `DecimalSeparator
  | `Conj
  | `Factorial
  | `Degree
  | `ArcMinute
  | `ArcSecond
  | `Function(_) => true
  | `Digit({atomNucleus: _, superscript})
  | `Variable({atomNucleus: _, superscript})
  | `Constant({constant: _, superscript})
  | `CustomAtom({customAtomValue: _, superscript})
  | `ImaginaryUnit(superscript)
  | `CloseBracket(superscript)
  | `Rand(superscript) => superscript == []
  | `Placeholder(superscript) => isEmpty(superscript)
  | `Magnitude(exponent) => isEmpty(exponent)
  | `Frac({fracNum, den}) => isEmpty(fracNum) && isEmpty(den)
  | `NRoot({nrootDegree, radicand}) =>
    isEmpty(nrootDegree) && isEmpty(radicand)
  | `RandInt({randIntA, b, superscript}) =>
    isEmpty(randIntA) && isEmpty(b) && isEmpty(superscript)
  | `NPR({statN, r})
  | `NCR({statN, r}) => isEmpty(statN) && isEmpty(r)
  | `Differential({differentialX, body}) =>
    isEmpty(differentialX) && isEmpty(body)
  | `Integral({integralA, b, body}) =>
    isEmpty(integralA) && isEmpty(b) && isEmpty(body)
  | `Sum({rangeStart, rangeEnd})
  | `Product({rangeStart, rangeEnd}) =>
    isEmpty(rangeStart) && isEmpty(rangeEnd)
  | `Sqrt({rootRadicand: arg})
  | `NLog({nlogBase: arg})
  | `Abs({unaryArg: arg})
  | `Floor({unaryArg: arg})
  | `Ceil({unaryArg: arg})
  | `Round({unaryArg: arg}) => isEmpty(arg)
  | `Table({tableElements}) => tableElements->Belt.Array.every(isEmpty)
  };

let deleteIndex = (elements, index) => {
  let nextIndex = index - 1;

  let (didDelete, elements) =
    TreeUtil.rollup(
      ~initialRollup=false,
      ~initialContext=(),
      elements->prepareForMutation,
      MutableListBuilder.empty,
      ({TreeUtil.accum: elements, rollup}) => (
        rollup,
        MutableListBuilder.toList(elements)->finalizeMutation->normalizeRow,
      ),
      ({TreeUtil.accum, rollup, rangeStart: i}, element) =>
        if (i == nextIndex && shouldDeleteElement(element)) {
          (true, accum);
        } else {
          (rollup, MutableListBuilder.append(accum, element));
        },
    );

  switch (elements, didDelete) {
  | ([`Placeholder([])], _) => Some(([], 0))
  | (_, true) => Some((elements, nextIndex))
  | _ => None
  };
};
