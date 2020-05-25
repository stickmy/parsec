import { error, ok, parse, Parser, Reply, State, SeqController } from './State';

export * from './State';

export const empty: Parser<void> = new Parser((state: State<any>) => ok(state, undefined));

export const eof: Parser<void> = new Parser((state: State<any>) =>
	state.position === state.source.length ? ok(state.seek(1), undefined) : error(state, () => 'end of file'),
);

export function fail(message: string): Parser<any> {
	return new Parser((state: State<any>) => error(state, () => 'failed: ' + message));
}

export function unexpected(message: string): Parser<any> {
	return new Parser((state: State<any>) => error(state, () => 'unexpected: ' + message));
}

/**
 * `try` in haskell
 *
 * @param p
 */
export function triable<T>(p: Parser<T>): Parser<T> {
	function tryParser<U>(s: State<U>) {
		const st = parse(p, s);
		return st.success ? st : error<T, U>(s, st.expected);
	}
	return new Parser(tryParser);
}

/**
 * `<|>` operator in haskell
 *
 * @param ps
 */
export function choice<T>(ps: Parser<T>[]): Parser<T> {
	function choiceParser<U>(state: State<U>) {
		const errorSts: Reply<T, U>[] = [];
		for (let i = 0; i < ps.length; i++) {
			const st = parse(ps[i], state);
			if (st.success) {
				return st;
			}
			errorSts.push(st);
		}
		return error<T, U>(state, () => 'one of ' + errorSts.map(st => st.expected!()).join(', '));
	}
	return new Parser(choiceParser);
}

/**
 * Variable parameter version of `choice`
 * @see choice
 * @param ps
 */
export function or<T>(...ps: Parser<T>[]): Parser<T> {
	return choice(ps);
}

/**
 * parses `n` occurrences of `p` at minimum, parses `m` occurrences of `p` at maximum.
 *
 * @param min
 * @param max
 * @param p
 */
export function repeat<T>(min: number, max: number, p: Parser<T>): Parser<T[]> {
	function repeatParser<U>(s: State<U>) {
		const xs: T[] = [];
		let st = ok<T, U>(s, undefined);
		for (let i = 0; i < max; i++) {
			const _st = parse(p, st.state);
			if (_st.success) {
				st = _st;
				xs.push(st.value!);
			} else if (st.state.position < _st.state.position) {
				// ensure do not consume any char when p parsing failed
				return error<T[], U>(_st.state, _st.expected);
			} else if (i < min) {
				return error<T[], U>(_st.state, _st.expected);
			} else {
				break;
			}
		}
		return ok(st.state, xs);
	}
	return new Parser(repeatParser);
}

export function count<T>(n: number, p: Parser<T>) {
	return repeat(n, n, p);
}

export function many<T>(p: Parser<T>) {
	return repeat(0, Number.MAX_VALUE, p);
}

/**
 * @haskell Text.ParserCombinators.Parsec.many1
 * @param p
 */
export function many1<T>(p: Parser<T>) {
	return repeat(1, Number.MAX_VALUE, p);
}

export function array<T>(ps: Parser<T>[]): Parser<T[]> {
	function arrayParser<U>(s: State<U>) {
		const values: T[] = [];
		let st: Reply<T, U> = ok(s);
		for (let i = 0; i < ps.length; i++) {
			st = parse(ps[i], s);
			if (!st.success) return error<T[], U>(st.state, st.expected);
			values.push(st.value!);
		}
		return ok(st.state, values);
	}
	return new Parser(arrayParser);
}

export function series<T>(...ps: Parser<T>[]) {
	return array(ps);
}

/**
 * (a, b, c, d, ...etc) parses all of them, and returns reply of `a`
 * If one of them fails, the rest will be ignore.
 *
 * @param p
 * @param ps
 * @return parser Parser<T>
 */
export function head<T>(p: Parser<T>, ...ps: Parser<any>[]): Parser<T> {
	function headParser<U>(s: State<U>) {
		let st: Reply<T, U> = parse(p, s);
		const value: T = st.value!;
		for (let i = 0; i < ps.length && st.success; i++) {
			st = parse(ps[i], st.state);
		}
		return st.success ? ok(st.state, value) : st;
	}
	return new Parser(headParser);
}

/**
 * Parser control flow (Simulating `do` operator in haskell)
 *
 * If apply parser `p` succeed, p return the parsed value. Otherwise return undefined
 * If parsed failed, the parser `p` will be ignored and return undefined
 *
 * <pre>
 *   p.seq(m => {
 *     m(p.string('abcdef'));
 *     m(p.string('gh'));
 *     return 123;
 *   })
 * </pre>
 *
 * The input is "abcdefgh", first and second all parsed succeed.
 * Seq parser return 123 which meaning the state.value is 123.
 *
 * <pre>
 *   p.seq(m => {
 *     m(p.string('1abcdef'));
 *     m(p.string('abc'));
 *   })
 * </pre>
 *
 * The input is "abcdefgh", the first parsed failed, the second parsed will be ignore.
 *
 * @param f
 */
export function seq<T>(f: (m: SeqController) => T | undefined): Parser<T> {
	function seqParser<U>(s: State<U>): Reply<T, U> {
		let st: Reply<any, U> = ok(s, undefined);

		const m: SeqController<U> = <S>(p: Parser<S>) => {
			if (st.success) {
				st = parse(p, st.state);
				m.success = st.success;
				m.state = st.state;
				return st.value;
			}
		};

		m.success = true;
		m.state = st.state;
		m._userState = st.state._userState!;

		const value: T | undefined = f(m);
		st.state._userState = m._userState;

		return st.success ? ok(st.state, value) : st;
	}
	return new Parser(seqParser);
}

export function between<T>(open: Parser<any>, p: Parser<T>, close: Parser<any>): Parser<T> {
	return seq(m => {
		m(open);
		const x = m(p);
		m(close);
		return x;
	});
}

/**
 *
 * @param min parses `min` or more occurrences of p
 * @param max parses `max` or less occurrences of p
 * @param p
 * @param sep
 * @return parser Parser<T[]>
 */
export function sepByN<T>(min: number, max: number, p: Parser<T>, sep: Parser<any>): Parser<T[]> {
	function sepByNParser<U>(s: State<U>) {
		let st: Reply<T, U> = ok(s);
		const xs: T[] = [];
		let _st = parse(p, st.state);
		if (_st.success) {
			st = _st;
			xs.push(_st.value!);
			for (let i = 1; i < max; i++) {
				const sepSt = parse(sep, st.state);
				if (sepSt.success) {
					st = parse(p, sepSt.state);
					if (st.success) {
						xs.push(st.value!);
						continue;
					}
				} else if (xs.length < min) {
					return error<T[], U>(_st.state, _st.expected);
				}
				break;
			}
		} else if (xs.length < min) {
			return error<T[], U>(_st.state, _st.expected);
		}
		return st.success ? ok(st.state, xs) : error<T[], U>(st.state, st.expected);
	}
	return new Parser(sepByNParser);
}

export function sepBy1<T>(p: Parser<T>, sep: Parser<any>): Parser<T[]> {
	return sepByN(1, Number.MAX_VALUE, p, sep);
}

export function sepBy<T>(p: Parser<T>, sep: Parser<any>): Parser<T[]> {
	return sepByN(0, Number.MAX_VALUE, p, sep);
}

/**
 *
 * @param min
 * @param max
 * @param p
 * @param sep
 */
export function endByN<T>(min: number, max: number, p: Parser<T>, sep: Parser<any>) {
	return repeat(min, max, head(p, sep));
}

/**
 * p sep parses one or more occurrences of p, separated and ended by sep. Returns a list of values returned by p.
 *
 * @param p
 * @param sep
 * @return Parser<T[]>
 */
export function endBy1<T>(p: Parser<T>, sep: Parser<any>): Parser<T[]> {
	return endByN(1, Number.MAX_VALUE, p, sep);
}

/**
 * p sep parses zero or more occurrences of p, separated and ended by sep. Returns a list of values returned by p.
 *
 * @param p
 * @param sep
 * @return Parser<T[]>
 */
export function endBy<T>(p: Parser<T>, sep: Parser<any>): Parser<T[]> {
	return endByN(0, Number.MAX_VALUE, p, sep);
}

/**
 * a -> b -> f a -> f b
 * @param f
 * @param p
 */
export function fmap<T, E>(f: (x: T) => E, p: Parser<T>): Parser<E> {
	function mapParser<U>(s: State<U>) {
		const st = parse(p, s);
		return st.success ? ok(st.state, f(st.value!)) : error<E, U>(st.state, st.expected);
	}
	return new Parser(mapParser);
}

/**
 * Lift a value (id)
 *
 * T -> Parser<T>
 *
 * @haskell Applicative pure
 * @param v {T}
 * @return Parser<T>
 */
export function pure<T>(v: T): Parser<T> {
	return fmap(() => v, empty);
}

/**
 * If `p` parsed succeed, return the parsed value. Otherwise return defaultValue.
 *
 * @param p
 * @param defaultValue
 */
export function option<T>(p: Parser<T>, defaultValue?: T): Parser<T> {
	return or(p, pure(defaultValue!));
}

/**
 * Same as @option@ but if `p` parsed failed, return undefined.
 *
 * @param p
 */
export function optional<T>(p: Parser<T>): Parser<T> {
	return option<T>(p, undefined!);
}

/**
 * Skip character parsed by `p` until `p` parsing failed.
 *
 * @param p
 */
export function skipMany<T>(p: Parser<T>): Parser<void> {
	return fmap((xs?: T[]) => undefined, many(p));
}

/**
 * Same as skipMany but parsing p succeed at least once.
 *
 * @param p
 */
export function skipMany1<T>(p: Parser<T>): Parser<void> {
	return seq(m => {
		m(p);
		m(skipMany(p));
	});
}

export function lookAhead<T>(p: Parser<T>): Parser<T> {
	function lookAheadParser<U>(state: State<U>) {
		const st = parse(p, state);
		return st.success ? ok(state, st.value) : st;
	}
	return new Parser(lookAheadParser);
}

/**
 * Regardless of parsing success or failure, it never consume any char.
 *
 * @param p
 */
export function notFollowedBy<T>(p: Parser<T>): Parser<void> {
	function notFollowedByParser<U>(state: State<U>) {
		const st = parse(p, state);
		return st.success ? error<void, U>(state, () => 'not ' + st.value) : ok(state, undefined);
	}
	return new Parser(notFollowedByParser);
}

export function lazy<T>(f: () => Parser<T>): Parser<T> {
	function lazyParser<U>(state: State<U>) {
		return parse(f(), state);
	}
	return new Parser(lazyParser);
}

/**
 * `<?>` operator in haskell
 *
 * @param message
 * @param p
 */
export function label<T>(message: string, p: Parser<T>): Parser<T> {
	function labelParser<U>(state: State<U>) {
		const st = parse(p, state);
		return st.success || st.state.position !== state.position ? st : error<T, U>(state, () => message);
	}
	return new Parser(labelParser);
}

export function satisfy(cond: (char: string, code: number) => boolean): Parser<string> {
	function satisfyChars() {
		const xs: string[] = [];
		for (let i = 32; i <= 126; i++) {
			const char = String.fromCharCode(i);
			if (cond(char, i)) {
				xs.push(char);
			}
		}
		return xs;
	}

	function satisfyParser<U>(state: State<U>) {
		if (state.position < state.source.length) {
			const char = state.source[state.position];
			const code = state.source.charCodeAt(state.position);
			if (cond(char, code)) {
				return ok(state.seek(1), char);
			}
		}
		return error<string, U>(state, () => {
			const xs = satisfyChars();
			return `one of "${xs.join('')}"`;
		});
	}

	return new Parser(satisfyParser);
}

export function until<T>(p: Parser<T>): Parser<string> {
	function untilParser<U>(state: State<U>) {
		if (state.position <= state.source.length) {
			for (let i = state.position; i < state.source.length; i++) {
				const st = parse(p, new State(state.source.slice(i)));
				if (st.success) {
					return ok(state.seek(i - state.position), state.source.slice(state.position, i));
				}
			}
		}
		return error<string, U>(state.seek(state.source.length - state.position), () => `can not match until the end`);
	}
	return new Parser(untilParser);
}

export const Join = {
	many: (p: Parser<string>) => fmap(x => x!.join(''), many(p)),
	many1: (p: Parser<string>) => fmap(x => x!.join(''), many1(p)),
	sepBy1: (p: Parser<string>, sep: Parser<any>) => fmap(x => x!.join(''), sepBy1(p, sep)),
	array: (ps: Parser<string>[]) => fmap(x => x!.join(''), array(ps)),
	series: (...ps: Parser<string>[]) => fmap(x => x!.join(''), series(...ps)),
};

export function oneOf(chars: string): Parser<string> {
	return satisfy((c, _) => chars.indexOf(c) >= 0);
}

export function noneOf(chars: string): Parser<string> {
	return satisfy((c, _) => chars.indexOf(c) === -1);
}

export function char(c: string): Parser<string> {
	return satisfy(char => char === c);
}

export function charCode(cc: number): Parser<string> {
	return satisfy((_, char) => char === cc);
}

export function regexp(re: RegExp): Parser<string> {
	function regexpParser<U>(state: State<U>) {
		if (state.position < state.source.length) {
			const input = state.source.slice(state.position);
			const ms = input.match(re);
			if (ms && ms.index === 0 && ms.length > 0) {
				const m = ms[0];
				return input.indexOf(m) === 0 ? ok(state.seek(m.length), m) : error<string, U>(state, () => '/' + re + '/');
			}
		}
		return error<string, U>(state, () => '' + re);
	}
	return new Parser(regexpParser);
}

export function range(min: string, max: string): Parser<string>;
export function range(min: number, max: string): Parser<string>;
export function range(min: any, max: any): Parser<string> {
	if (typeof min === 'string') {
		min = min.charCodeAt(0);
	}
	if (typeof max === 'string') {
		max = max.charCodeAt(0);
	}
	return satisfy((_, code) => code >= min && code <= max);
}

/**
 * string parser
 *
 * @param text string
 * @param caseSensitive boolean
 * @return Parser<string>
 */
export function string(text: string, caseSensitive: boolean = true): Parser<string> {
	text = caseSensitive ? text : text.toLowerCase();
	function stringParser<U>(s: State<U>): Reply<string, U> {
		const slice = s.source.slice(s.position, s.position + text.length);
		return text === (caseSensitive ? slice : slice.toLowerCase())
			? ok(s.seek(text.length), text)
			: error(s, () => '"' + text + '"');
	}
	return new Parser(stringParser);
}

export const anyChar: Parser<string> = satisfy(_ => true);
export const space = oneOf('\t\r\n');
export const spaces = fmap(xs => xs!.join(), many(space));
export const newLine = oneOf('\r\n');
export const tab = char('\t');
export const upper = oneOf('ABCDEFGHIJKLMNOPQRSTUVWXYZ');
export const lower = oneOf('abcdefghijklmnopqrstuvwxyz');
export const alphaNum = oneOf('ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789');
export const letter = oneOf('ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz');
export const digit = oneOf('0123456789');
export const octDigit = oneOf('01234567');
export const hexDigit = oneOf('0123456789abcdefghABCDEFGH');

/**
 * @haskell Text.Parsec.Expr
 */
export enum Assoc {
	AssocNone,
	AssocLeft,
	AssocRight,
}

export class LeftAssoc<T> {
	constructor(public p: Parser<(l: T, r: T) => T>) {}
}

export class RightAssoc<T> {
	constructor(public p: Parser<(l: T, r: T) => T>) {}
}

export class NoneAssoc<T> {
	constructor(public p: Parser<(l: T, r: T) => T>) {}
}

export class Prefix<T> {
	constructor(public p: Parser<(x: T) => T>) {}
}

export class Postfix<T> {
	constructor(public p: Parser<(x: T) => T>) {}
}

export type AssocOperator<T> = LeftAssoc<T> | RightAssoc<T> | NoneAssoc<T>;

export type Operator<T> = LeftAssoc<T> & RightAssoc<T> & NoneAssoc<T> & Prefix<T> & Postfix<T>;

export class OperatorTable<T> {
	constructor(
		public infixr: Array<Parser<(l: T, r: T) => T>> = [],
		public infixl: Array<Parser<(l: T, r: T) => T>> = [],
		public infix: Array<Parser<(l: T, r: T) => T>> = [],
		public prefix: Array<Parser<(x: T) => T>> = [],
		public postfix: Array<Parser<(x: T) => T>> = [],
	) {}
}

/**
 * @see https://hackage.haskell.org/package/parsec-3.1.14.0/docs/src/Text.Parsec.Expr.html#buildExpressionParser
 *
 * @param operators
 * @param simpleExpr
 */
export function buildExpressionParser<T>(operators: Array<OperatorTable<T>>, simpleExpr: Parser<T>) {
	function makeParser(term: Parser<T>, ops: OperatorTable<T>) {
		const rassocOp = choice(ops.infixr);
		const lassocOp = choice(ops.infixl);
		const nassocOp = choice(ops.infix);
		const prefixOp = choice(ops.prefix);
		const postfixOp = choice(ops.postfix);

		function ambiguous<T>(assoc: string, op: AssocOperator<T>['p']): Parser<T> {
			return triable(
				seq(m => {
					m(op);
					return m(fail(`ambiguous use of a ${assoc} associative operator`));
				}),
			);
		}

		const ambiguousRight = ambiguous('right', rassocOp);
		const ambiguousLeft = ambiguous('left', lassocOp);
		const ambiguousNon = ambiguous('non', nassocOp);

		const postfixP = or(postfixOp, pure((x: T) => x));

		const prefixP = or(prefixOp, pure((x: T) => x));

		function rassocP1(x: T) {
			return or(rassocP(x), pure(x));
		}

		function lassocP1(x: T): Parser<T> {
			return or(lassocP(x), pure(x));
		}

		function rassocP(x: T) {
			return or(
				seq(m => {
					const f = m(rassocOp);
					const y: T = m(
						seq(s => {
							const z = s(termP);
							return s(rassocP1(z));
						}),
					);
					if (m.success) {
						return f(x, y);
					}
				}),
				ambiguousLeft,
				ambiguousNon,
			);
		}

		function lassocP(x: T) {
			return or(
				seq(m => {
					const f = m(lassocOp);
					const y = m(termP);
					if (m.success) {
						return m(lassocP1(f(x, y)));
					}
				}),
				ambiguousRight,
				ambiguousNon,
			);
		}

		const termP = seq(m => {
			const pre = m(prefixP);
			const x = m(term);
			const post = m(postfixP);
			if (m.success) {
				return post(pre(x));
			}
		});

		function nassocP(x: T): Parser<T> {
			return seq(m => {
				const f = m(nassocOp);
				const y = m(termP);
				if (m.success) {
					return m(or(ambiguousRight, ambiguousLeft, ambiguousNon, pure(f(x, y))));
				}
			});
		}

		return seq(m => {
			const x = m(termP);
			if (m.success) {
				return m(label('operator', or(rassocP(x), lassocP(x), nassocP(x), pure(x))));
			}
		});
	}

	return operators.reduce(makeParser, simpleExpr);
}

export interface Language {
	caseSensitive: boolean;
	multiCommentStart?: Parser<string>;
	multiCommentEnd?: Parser<string>;
	singleCommentLine: Parser<string>;
	identifierStart: Parser<string>;
	identifierLetter: Parser<string>;
	opStart: Parser<string>;
	opLetter: Parser<string>;
	reservedNames: string[];
	reservedOps: string[];
	// operator: Parser<string>;
}

export function buildTokenParser(lang: Language) {
	const whiteSpace = skipMany1(oneOf(' \n\t\r'));

	const multiCommentStart = triable(lang.multiCommentStart!);
	const multiCommentEnd = triable(lang.multiCommentEnd!);

	const singleLineComment = label(
		'end of single line comment',
		seq(m => {
			m(lang.singleCommentLine);
			return m(until(string('\n')));
		}),
	);

	const multiLineComment = seq(m => {
		m(multiCommentStart);
		const xs = m(until(multiCommentEnd));
		m(multiCommentEnd);
		return m(pure(xs));
	});

	const whiteSpaceOrComment = skipMany(
		or(whiteSpace, fmap(_ => undefined, multiLineComment), fmap(_ => undefined, singleLineComment)),
	);

	function lexeme<T>(p: Parser<T>): Parser<T> {
		return seq(m => {
			const x = m(p);
			m(whiteSpaceOrComment);
			return x;
		});
	}

	function symbol<T>(name: string): Parser<string> {
		return lexeme(string(name));
	}

	const semi = symbol(';');
	const comma = symbol(',');
	const dot = symbol('.');
	const colon = symbol(':');

	const oper = seq(m => {
		const o = m(lang.opStart);
		const os = m(Join.many(lang.opLetter));
		return o + os;
	});

	function isReservedOp(name: string) {
		return lang.reservedOps.indexOf(name) > -1;
	}

	const operator = lexeme(
		triable(
			seq(m => {
				const name = m(oper);
				return isReservedOp(name) ? m(unexpected(`reserved operator ${name}`)) : name;
			}),
		),
	);

	function reserved(name: string): Parser<string> {
		// Use lexeme to consume whiteSpace or comments after reserved word.
		return lexeme(
			triable(
				seq(m => {
					const n = m(string(name, lang.caseSensitive));
					m(notFollowedBy(lang.identifierLetter));
					return n;
				}),
			),
		);
	}

	function reservedOp(name: string): Parser<string> {
		return lexeme(
			triable(
				seq(m => {
					const n = m(string(name));
					m(notFollowedBy(lang.opLetter));
					return n;
				}),
			),
		);
	}

	function isReservedName(name: string): boolean {
		const rns = lang.caseSensitive ? lang.reservedNames : lang.reservedNames.map(x => x.toLowerCase());
		const cn = lang.caseSensitive ? name : name.toLowerCase();
		return rns.indexOf(cn) > -1;
	}

	const uncheckIdentifier = seq(m => {
		const s = m(lang.identifierStart);
		const sx = m(Join.many(lang.identifierLetter));
		return m.success && s + sx;
	});

	const identifier: Parser<string> = lexeme(
		triable(
			seq(m => {
				const ident = m(uncheckIdentifier);
				if (!ident || isReservedName(ident)) {
					return m(unexpected(`${ident} is reserved word`));
				} else {
					return ident;
				}
			}),
		),
	);

	// ()
	function parens<T>(p: Parser<T>): Parser<T> {
		return between(symbol('('), p, symbol(')'));
	}

	// {}
	function braces<T>(p: Parser<T>): Parser<T> {
		return between(symbol('{'), p, symbol('}'));
	}

	// <>
	function angles<T>(p: Parser<T>): Parser<T> {
		return between(symbol('<'), p, symbol('>'));
	}

	// []
	function brackets<T>(p: Parser<T>): Parser<T> {
		return between(symbol('['), p, symbol(']'));
	}

	const escapeCode = seq(m => {
		const x = m(anyChar);
		switch (x) {
			case 'r':
				return '\\r';
			case 'n':
				return '\\n';
			case '\\':
				return '\\\\';
			case '"':
				return m(unexpected(x));
			default:
				return x;
		}
	});

	const escape = seq(m => {
		m(string('\\'));
		return m(escapeCode);
	});

	function makeCharLetter(quote: "'" | '"') {
		return satisfy((char, code) => code >= 32 && char !== quote && char !== '\\');
	}

	const charLetter = makeCharLetter("'");
	const charLiteral = label('charLiteral', lexeme(between(string("'"), or(charLetter, escape), string("'"))));

	const stringLetter = makeCharLetter('"');
	const stringLiteral = label(
		'stringLiteral',
		lexeme(fmap(xs => xs!.join(''), between(string('"'), many(or(stringLetter, escape)), string('"')))),
	);

	function number(radix: number, digit: Parser<string>): Parser<number> {
		return fmap(xs => xs!.reduce((x, d) => x * radix + parseInt(d), 0), many1(digit));
	}

	const decimal = number(10, digit);

	const hexadecimal = seq(m => {
		m(oneOf('xX'));
		return m(number(16, hexDigit));
	});

	const octal = seq(m => {
		m(oneOf('oO'));
		return m(number(8, octDigit));
	});

	const zeroNumber = seq(m => {
		m(string('0'));
		return m(or(decimal, hexadecimal, octal, pure(0)));
	});

	const nat = or(zeroNumber, decimal);

	// Return a function which accept a natural number and return typed number.
	const sign: Parser<(x: number) => number> = or(
		seq(m => {
			m(string('-'));
			return m(pure((x: number) => -x));
		}),
		seq(m => {
			m(string('+'));
			return m(pure((x: number) => x));
		}),
		pure((x: number) => x),
	);

	const int = seq(m => {
		const f = m(lexeme(sign));
		const x = m(nat);
		return m.success ? f(x) : undefined;
	});

	const exponent = label(
		'exponent',
		seq(m => {
			function power(e: number): number {
				return e < 0 ? 1.0 / power(-e) : Math.pow(10, e);
			}
			m(oneOf('eE'));
			const f = m(sign);
			const n = m(decimal);
			return m.success ? power(f(n)) : undefined;
		}),
	);

	const fraction = label(
		'fraction',
		seq(m => {
			function fract(digits: string[]) {
				return parseFloat('0.' + digits.reduce((s, x) => s + x, ''));
			}
			m(string('.'));
			const digits = m(many1(digit));
			return m.success ? fract(digits) : undefined;
		}),
	);

	function fractExponent(n: number) {
		return or(
			// With fraction
			seq(m => {
				const fract = m(fraction);
				const exponent$ = m(option(exponent, 1.0));
				return m.success ? (n + fract) * exponent$ : undefined;
			}),
			// Integral
			seq(m => {
				const exponent$ = m(exponent);
				return m.success ? n * exponent$ : undefined;
			}),
		);
	}

	const floating = seq(m => {
		const n = m(decimal);
		return m(fractExponent(n));
	});

	const decimalFloat = seq(m => {
		const n = m(decimal);
		return m(option(fractExponent(n), n));
	});

	const zeroNumFloat = or(or(hexadecimal, octal), decimalFloat, fractExponent(0), pure(0));

	const naturalFloat = or(
		seq(m => {
			m(string('0'));
			return m(zeroNumFloat);
		}),
		decimalFloat,
	);

	const naturalOrFloat = lexeme(naturalFloat);
	const float = lexeme(floating);
	const integer = lexeme(int);
	const natural = lexeme(nat);

	return {
		identifier,
		reservedOp,
		reserved,
		operator,
		charLiteral,
		stringLiteral,
		natural,
		integer,
		float,
		naturalOrFloat,
		decimal,
		hexadecimal,
		octal,
		symbol,
		lexeme,
		whiteSpace,
		whiteSpaceOrComment,
		multiLineComment,
		singleLineComment,
		parens,
		braces,
		angles,
		brackets,
		semi,
		comma,
		colon,
		dot,
	};
}
