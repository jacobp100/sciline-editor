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

let separateSuperscript = element =>
  switch (element) {
  | `Placeholder(superscript) => Some((`Placeholder([]), superscript))
  | `ImaginaryUnit(superscript) => Some((`ImaginaryUnit([]), superscript))
  | `CloseBracket(superscript) => Some((`CloseBracket([]), superscript))
  | `Rand(superscript) => Some((`Rand([]), superscript))
  | `Digit({atomNucleus, superscript}) =>
    Some((`Digit({atomNucleus, superscript: []}), superscript))
  | `Variable({atomNucleus, superscript}) =>
    Some((`Variable({atomNucleus, superscript: []}), superscript))
  | `Constant({constant, superscript}) =>
    Some((`Constant({constant, superscript: []}), superscript))
  | `CustomAtom({customAtomValue, mml, superscript}) =>
    Some((`CustomAtom({customAtomValue, mml, superscript: []}), superscript))
  | `Frac({fracNum, den, superscript}) =>
    Some((`Frac({fracNum, den, superscript: []}), superscript))
  | `Sqrt({rootRadicand, superscript}) =>
    Some((`Sqrt({rootRadicand, superscript: []}), superscript))
  | `NRoot({nrootDegree, radicand, superscript}) =>
    Some((`NRoot({nrootDegree, radicand, superscript: []}), superscript))
  | `Abs({unaryArg, superscript}) => Some((`Abs({unaryArg, superscript: []}), superscript))
  | `Floor({unaryArg, superscript}) => Some((`Floor({unaryArg, superscript: []}), superscript))
  | `Ceil({unaryArg, superscript}) => Some((`Ceil({unaryArg, superscript: []}), superscript))
  | `Round({unaryArg, superscript}) => Some((`Round({unaryArg, superscript: []}), superscript))
  | `RandInt({randIntA, b, superscript}) =>
    Some((`RandInt({randIntA, b, superscript: []}), superscript))
  | `Table({tableElements: _, superscript} as t) =>
    Some((`Table({...t, superscript: []}), superscript))
  | _ => None
  };

let coalesceWithSuperscript = (element: t, superscript: list(t)): option(t) =>
  switch (element) {
  | `Placeholder([]) => Some(`Placeholder(superscript))
  | `ImaginaryUnit([]) => Some(`ImaginaryUnit(superscript))
  | `CloseBracket([]) => Some(`CloseBracket(superscript))
  | `Rand([]) => Some(`Rand(superscript))
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
  | `RandInt({randIntA, b, superscript: []}) =>
    Some(`RandInt({randIntA, b, superscript}))
  | `Table({tableElements: _, superscript: []} as t) =>
    Some(`Table({...t, superscript}))
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

type insertionFlags =
  | AnyElement
  | NonTableElements;

let reduceSelectorFlags = (existingFlags, selector) =>
  switch (selector) {
  | Tree.Superscript
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
    let finalize = ({Tree.accum, rollup, context, rangeStart, rangeEnd}) => {
      let (inserted, elements) =
        if (rollup != NotInserted
            || rangeStart == rangeEnd
            || index != rangeEnd) {
          (rollup, accum);
        } else if (!canInsert(context, newElement)) {
          (InsertionFailed, accum);
        } else {
          let nextElements = MutableListBuilder.append(accum, newElement);
          (Inserted, nextElements);
        };
      let elements =
        MutableListBuilder.toList(elements)
        ->coalescePlaceholderSuperscripts
        ->expandInsertedElementsInRow
        ->normalizeRow;
      (inserted, elements);
    };

    let reduceFn =
        (
          {Tree.accum, rollup, context, rangeStart, superscriptIndex},
          element,
        ) => {
      let editMode =
        if (index == rangeStart) {
          `Normal;
        } else if (index == superscriptIndex) {
          `Superscript;
        } else {
          `None;
        };

      if (rollup != NotInserted || editMode == `None) {
        (rollup, accum->MutableListBuilder.append(element));
      } else if (!canInsert(context, newElement)) {
        (InsertionFailed, accum->MutableListBuilder.append(element));
      } else if (editMode == `Normal) {
        (
          Inserted,
          accum
          ->MutableListBuilder.append(newElement)
          ->MutableListBuilder.append(element),
        );
      } else {
        let (elementNoSuperscript, superscript) =
          separateSuperscript(element)->Belt.Option.getExn;
        let accum = accum->MutableListBuilder.append(elementNoSuperscript);
        let accum =
          switch (coalesceWithSuperscript(newElement, superscript)) {
          | Some(newElement) => accum->MutableListBuilder.append(newElement)
          | None =>
            accum
            ->MutableListBuilder.append(newElement)
            ->MutableListBuilder.append(`Placeholder(superscript))
          };
        (Inserted, accum);
      };
    };

    let (inserted, elements) =
      Tree.rollup(
        ~initialRollup=NotInserted,
        ~mapContext=reduceSelectorFlags,
        ~initialContext=AnyElement,
        elements,
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
let elementDeletionMode = (element: t) =>
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
  | `Function(_) => `Delete
  | `Digit({atomNucleus: _, superscript})
  | `Variable({atomNucleus: _, superscript})
  | `Constant({constant: _, superscript})
  | `CustomAtom({customAtomValue: _, superscript})
  | `ImaginaryUnit(superscript)
  | `CloseBracket(superscript)
  | `Rand(superscript) =>
    superscript == [] ? `Delete : `Spread([`Placeholder(superscript)])
  | `Placeholder(superscript) => isEmpty(superscript) ? `Delete : `Skip
  | `Magnitude(exponent) => isEmpty(exponent) ? `Delete : `Skip
  | `NLog({nlogBase}) => isEmpty(nlogBase) ? `Delete : `Skip
  | `Frac({fracNum, den, superscript}) =>
    superscript == [] ? `Spread(Belt.List.concat(fracNum, den)) : `Skip
  | `Sqrt({rootRadicand, superscript}) =>
    superscript == [] ? `Spread(rootRadicand) : `Skip
  | `NRoot({nrootDegree, radicand, superscript}) =>
    isEmpty(nrootDegree) && isEmpty(radicand) && isEmpty(superscript) ?
      `Delete : `Skip
  | `RandInt({randIntA, b, superscript}) =>
    isEmpty(randIntA) && isEmpty(b) && isEmpty(superscript) ?
      `Delete : `Skip
  | `NPR({statN, r})
  | `NCR({statN, r}) => isEmpty(statN) && isEmpty(r) ? `Delete : `Skip
  | `Differential({differentialX, body}) =>
    isEmpty(differentialX) && isEmpty(body) ? `Delete : `Skip
  | `Integral({integralA, b, body}) =>
    isEmpty(integralA) && isEmpty(b) && isEmpty(body) ? `Delete : `Skip
  | `Sum({rangeStart, rangeEnd})
  | `Product({rangeStart, rangeEnd}) =>
    isEmpty(rangeStart) && isEmpty(rangeEnd) ? `Delete : `Skip
  | `Abs({unaryArg: arg, superscript})
  | `Floor({unaryArg: arg, superscript})
  | `Ceil({unaryArg: arg, superscript})
  | `Round({unaryArg: arg, superscript}) =>
    isEmpty(arg) && isEmpty(superscript) ? `Delete : `Skip
  | `Table({tableElements, superscript}) =>
    tableElements->Belt.Array.every(isEmpty) && isEmpty(superscript) ?
      `Delete : `Skip
  };

let deleteIndex = (inputElements, index) => {
  let index = max(index - 1, 0);

  let (didDelete, elements) =
    Tree.rollup(
      ~initialRollup=false,
      ~initialContext=(),
      inputElements,
      MutableListBuilder.empty,
      ({Tree.accum, rollup}) =>
        (
          rollup,
          MutableListBuilder.toList(accum)
          ->coalescePlaceholderSuperscripts
          ->normalizeRow,
        ),
      ({Tree.accum, rollup, rangeStart}, element) =>
        switch (index == rangeStart ? elementDeletionMode(element) : `Skip) {
        | `Delete => (true, accum)
        | `Spread(elements) => (
            true,
            elements->Belt.List.reduce(accum, MutableListBuilder.append),
          )
        | `Skip => (rollup, accum->MutableListBuilder.append(element))
        },
    );

  switch (elements, didDelete) {
  | ([`Placeholder([])], _) => ([], 0)
  | (_, true) => (elements, index)
  | _ => (inputElements, index)
  };
};
