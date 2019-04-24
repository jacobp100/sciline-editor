open Types;

let rec ensurePlaceholders = (element: t): t =>
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
  | `Abs({absArg, superscript}) =>
    `Abs({absArg: ensurePlaceholder(absArg), superscript})
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
      tableElements: Array.map(ensurePlaceholder, tableElements),
    })
  | (
      `Base(_) | `Operator(_) | `Function(_) | `OpenBracket | `DecimalSeparator |
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
  }
and ensurePlaceholder = x =>
  switch (x) {
  | [] => [`Placeholder([])]
  | args => args
  };

let notEmptyPlaceholder = e =>
  switch (e) {
  | `Placeholder([]) => false
  | _ => true
  };
let ensureRowPlaceholders = elements =>
  elements->Belt.List.map(ensurePlaceholders);
let removeRowFloatingPlaceholders = elements =>
  switch (elements) {
  | [`Placeholder([])] as x => x
  | x => x->Belt.List.keep(notEmptyPlaceholder)
  };

let normalizeRow = elements =>
  elements->removeRowFloatingPlaceholders->ensureRowPlaceholders;

let removeTopLevelPlaceholder = elements =>
  switch (elements) {
  | [`Placeholder([])] => []
  | _ => elements
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
let separateSuperscripts = elements =>
  TreeUtil.walk(
    elements,
    MutableListBuilder.empty,
    (elements, _, _) => MutableListBuilder.toList(elements),
    (accum, element, _, _) =>
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
  | `Abs({absArg, superscript: []}) => Some(`Abs({absArg, superscript}))
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

let withLength = elements => {
  let length = TreeUtil.length(elements);
  (length, elements);
};

let insertIndex = (elements, newElement, index) => {
  let elements =
    if (elements == []) {
      normalizeRow([newElement]);
    } else {
      let finalize = ((inserted, elements), startIndex, finalIndex) => {
        let elements =
          if (!inserted && startIndex !== finalIndex && index == finalIndex) {
            MutableListBuilder.append(elements, newElement);
          } else {
            elements;
          };
        MutableListBuilder.toList(elements)
        ->finalizeMutation
        ->expandInsertedElementsInRow
        ->normalizeRow;
      };

      let reduceFn = ((inserted, elements), element, i, _) =>
        if (!inserted && index == i) {
          (
            true,
            elements
            ->MutableListBuilder.append(newElement)
            ->MutableListBuilder.append(element),
          );
        } else {
          (false, MutableListBuilder.append(elements, element));
        };

      elements
      ->prepareForMutation
      ->TreeUtil.walk((false, MutableListBuilder.empty), finalize, reduceFn);
    };
  elements->withLength;
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
  | `Abs({absArg: arg}) => isEmpty(arg)
  | `Table({tableElements}) => tableElements->Belt.Array.every(isEmpty)
  };

let deleteIndex = (elements, index) => {
  let index = index - 1;
  elements
  ->prepareForMutation
  ->TreeUtil.walk(
      MutableListBuilder.empty,
      (elements, _, _) =>
        MutableListBuilder.toList(elements)->finalizeMutation->normalizeRow,
      (accum, element, i, _) =>
        if (index == i && shouldDeleteElement(element)) {
          accum;
        } else {
          MutableListBuilder.append(accum, element);
        },
    )
  ->removeTopLevelPlaceholder
  ->withLength;
};
