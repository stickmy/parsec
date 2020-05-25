import * as p from '../src/parsec';
import { Assoc, LeftAssoc, NoneAssoc, oneOf, Parser, Postfix, Prefix, RightAssoc, seq } from '../src/parsec';

declare global {
	namespace jest {
		interface Matchers<R> {
			pEquals: <T, U>(expected: p.Reply<T, U>) => CustomMatcherResult;
		}
	}
}

expect.extend({
	pEquals(received: p.Reply<any, any>, expected: p.Reply<any, any>) {
		const pass = received.equals(expected);

		let message = 'ok';
		if (!pass) {
			message = `\nreceived: ${received}\nexpected: ${expected}`;
		}

		return {
			pass,
			message: () => message,
		};
	},
});

function _infix<T>(p: Parser<(l: T, r: T) => T>, assoc: Assoc) {
	switch (assoc) {
		case Assoc.AssocNone:
			return new NoneAssoc(p);
		case Assoc.AssocLeft:
			return new LeftAssoc(p);
		case Assoc.AssocRight:
			return new RightAssoc(p);
	}
}

export function _prefix<T>(p: Parser<(x: T) => T>) {
	return new Prefix(p);
}

export function _postfix<T>(p: Parser<(x: T) => T>) {
	return new Postfix(p);
}

function makeExpressionParser<T>(operatorTable: Array<p.Operator<T>[]>, simpleExpr: p.Parser<T>) {
	const _operatorTable = operatorTable.map(ops => {
		const rassoc: Array<p.RightAssoc<T>> = ops.filter(op => op instanceof p.RightAssoc);
		const lassoc: Array<p.LeftAssoc<T>> = ops.filter(op => op instanceof p.LeftAssoc);
		const nassoc: Array<p.NoneAssoc<T>> = ops.filter(op => op instanceof p.NoneAssoc);
		const prefix: Array<p.Prefix<T>> = ops.filter(op => op instanceof p.Prefix);
		const postfix: Array<p.Postfix<T>> = ops.filter(op => op instanceof p.Postfix);

		return new p.OperatorTable(
			rassoc.map(x => x.p),
			lassoc.map(x => x.p),
			nassoc.map(x => x.p),
			prefix.map(x => x.p),
			postfix.map(x => x.p),
		);
	});

	return p.buildExpressionParser(_operatorTable, simpleExpr);
}

const lang: p.Language = {
	caseSensitive: true,
	multiCommentStart: p.string('/*'),
	multiCommentEnd: p.string('*/'),
	singleCommentLine: p.string('//'),
	identifierStart: p.oneOf('_$abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'),
	identifierLetter: p.oneOf('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'),
	reservedNames: [
		'function',
		'return',
		'operator',
		'infix',
		'infixl',
		'infixr',
		'prefix',
		'postfix',
		'const',
		'if',
		'else',
		'for',
		'native',
	],
	opStart: oneOf('+-*/=!$%&^~@?_><:|\\.'),
	opLetter: oneOf('+-*/=!$%&^~@?_><:|\\.'),
	reservedOps: [],
};

const lexer = p.buildTokenParser(lang);

function binary<T>(name: string, fun: (a: T, b: T) => T, assoc: Assoc): p.Operator<T> {
	// @ts-ignore
	return _infix(p.fmap(_ => fun, lexer.reservedOp(name)), assoc);
}
function prefix<T>(name: string, fun: (a: T) => T): p.Operator<T> {
	return _prefix(p.fmap(_ => fun, lexer.reservedOp(name)));
}
function postfix<T>(name: string, fun: (a: T) => T): p.Operator<T> {
	return _postfix(p.fmap(_ => fun, lexer.reservedOp(name)));
}

describe('lang basic', () => {
	test('whiteSpace ok 1', () => {
		const source = '123';
		const whiteSpace = p.skipMany1(p.oneOf(' \n\t\r'));
		const expected = p.error(new p.State(source, 0), () => 'one of " "');
		const result = p.parse(whiteSpace, new p.State(source));
		expect(result).pEquals(expected);
	});

	test('angles ok', () => {
		const source = '<div>';
		const expected = p.ok(new p.State(source, 5), 'div');
		const result = p.parse(lexer.angles(p.string('div')), new p.State(source));
		expect(result).pEquals(expected);
	});

	test('brackets ok 1', () => {
		const source = '[123]';
		const expected = p.ok(new p.State(source, 5), '123');
		const result = p.parse(lexer.brackets(p.string('123')), new p.State(source));
		expect(result).pEquals(expected);
	});

	test('parens ok 1', () => {
		const source = '(123)';
		const expected = p.ok(new p.State(source, 5), '123');
		const result = p.parse(lexer.parens(p.string('123')), new p.State(source));
		expect(result).pEquals(expected);
	});

	test('braces ok 1', () => {
		const source = '{123}';
		const expected = p.ok(new p.State(source, 5), '123');
		const result = p.parse(lexer.braces(p.string('123')), new p.State(source));
		expect(result).pEquals(expected);
	});

	test('identifier ok 1', () => {
		const source = 'ident';
		const expected = p.ok(new p.State(source, 5), 'ident');
		const result = p.parse(lexer.identifier, new p.State(source));
		expect(result).pEquals(expected);
	});

	test('identifier error 1', () => {
		const source = 'function';
		const expected = p.error(new p.State(source, 0), () => 'unexpected: function is reserved word');
		const result = p.parse(lexer.identifier, new p.State(source));
		expect(result).pEquals(expected);
	});

	test('int ok 0', () => {
		const source = '0';
		const expected = p.ok(new p.State(source, 1), 0);
		const result = p.parse(lexer.integer, new p.State(source));
		expect(result).pEquals(expected);
	});

	test('int ok 1 (no sign)', () => {
		const source = '123';
		const expected = p.ok(new p.State(source, 3), 123);
		const result = p.parse(lexer.integer, new p.State(source));
		expect(result).pEquals(expected);
	});

	test('int ok 2 (negative sign)', () => {
		const source = '-123';
		const expected = p.ok(new p.State(source, 4), -123);
		const result = p.parse(lexer.integer, new p.State(source));
		expect(result).pEquals(expected);
	});

	test('int ok 3 (positive sign)', () => {
		const source = '+123';
		const expected = p.ok(new p.State(source, 4), 123);
		const result = p.parse(lexer.integer, new p.State(source));
		expect(result).pEquals(expected);
	});

	test('int ok 4', () => {
		const source = '123abc';
		const expected = p.ok(new p.State(source, 3), 123);
		const result = p.parse(lexer.integer, new p.State(source));
		expect(result).pEquals(expected);
	});

	test('int ok 5 (zero)', () => {
		const source = '0';
		const expected = p.ok(new p.State(source, 1), 0);
		const result = p.parse(lexer.integer, new p.State(source));
		expect(result).pEquals(expected);
	});

	test('float ok 1', () => {
		const source = '0.123';
		const expected = p.ok(new p.State(source, 5), 0.123);
		const result = p.parse(lexer.float, new p.State(source));
		expect(result).pEquals(expected);
	});

	test('float ok 2', () => {
		const source = '1.123';
		const expected = p.ok(new p.State(source, 5), 1.123);
		const result = p.parse(lexer.float, new p.State(source));
		expect(result).pEquals(expected);
	});

	test('float ok 3', () => {
		const source = '1.00';
		const expected = p.ok(new p.State(source, 4), 1.0);
		const result = p.parse(lexer.float, new p.State(source));
		expect(result).pEquals(expected);
	});

	test('float ok 4', () => {
		const source = '1.00e-2';
		const expected = p.ok(new p.State(source, 7), 0.01);
		const result = p.parse(lexer.float, new p.State(source));
		expect(result).pEquals(expected);
	});

	test('float ok 5', () => {
		const source = '10.01e2';
		const expected = p.ok(new p.State(source, 7), 1001);
		const result = p.parse(lexer.float, new p.State(source));
		expect(result).pEquals(expected);
	});

	test('zero ok 1', () => {
		const source = '0';
		const expected = p.ok(new p.State(source, 1), 0);
		const result = p.parse(lexer.integer, new p.State(source));
		expect(result).pEquals(expected);
	});

	test('string ok 1', () => {
		const source = '"abc"';
		const expected = p.ok(new p.State(source, 5), 'abc');
		const result = p.parse(lexer.stringLiteral, new p.State(source));
		expect(result).pEquals(expected);
	});

	test('string ok 2', () => {
		const source = String.raw`"abc\r"`;
		const expected = p.ok(new p.State(source, 7), String.raw`abc\r`);
		const result = p.parse(lexer.stringLiteral, new p.State(source));
		expect(result).pEquals(expected);
	});

	test('string ok 3', () => {
		const source = String.raw`"abc\nedf"`;
		const expected = p.ok(new p.State(source, 10), String.raw`abc\nedf`);
		const result = p.parse(lexer.stringLiteral, new p.State(source));
		expect(result).pEquals(expected);
	});

	test('string ok 4', () => {
		const source = String.raw`"abc\\"`;
		const expected = p.ok(new p.State(source, 7), String.raw`abc\\`);
		const result = p.parse(lexer.stringLiteral, new p.State(source));
		expect(result).pEquals(expected);
	});

	test('string ok 5', () => {
		const source = String.raw`"abc\ "`;
		const expected = p.ok(new p.State(source, 7), String.raw`abc `);
		const result = p.parse(lexer.stringLiteral, new p.State(source));
		expect(result).pEquals(expected);
	});

	test('string ok 6', () => {
		const source = String.raw`"abc\\"`;
		const expected = p.ok(new p.State(source, 7), String.raw`abc\\`);
		const result = p.parse(lexer.stringLiteral, new p.State(source));
		expect(result).pEquals(expected);
	});

	test('multi comments ok 1', () => {
		const source = `/*
			This is multi comment.
			Second line.
		*/`;
		const expected = p.ok(new p.State(source, 49), '\n\t\t\tThis is multi comment.\n\t\t\tSecond line.\n\t\t');
		const result = p.parse(lexer.multiLineComment, new p.State(source));
		expect(result).pEquals(expected);
	});

	test('single comment ok 1', () => {
		const source = '// This is a single comment\n';
		const expected = p.ok(new p.State(source, 27), ' This is a single comment');
		const result = p.parse(lexer.singleLineComment, new p.State(source));
		expect(result).pEquals(expected);
	});

	test('reserved ok 1', () => {
		const source = 'var a';
		const expected = p.ok(new p.State(source, 4), 'var');
		const result = p.parse(lexer.reserved('var'), new p.State(source));
		expect(result).pEquals(expected);
	});

	test('reserved ok 2', () => {
		const source = 'var';
		const expected = p.ok(new p.State(source, 3), 'var');
		const result = p.parse(lexer.reserved('var'), new p.State(source));
		expect(result).pEquals(expected);
	});

	test('reserved error 1', () => {
		const source = 'constx';
		const expected = p.error(new p.State(source, 0), () => 'not x');
		const result = p.parse(lexer.reserved('const'), new p.State(source));
		expect(result).pEquals(expected);
	});

	test('parse expression ok 1', () => {
		const table: Array<p.Operator<number>[]> = [[binary('*', (x: number, y: number) => x * y, p.Assoc.AssocLeft)]];
		const term: p.Parser<number> = p.or(lexer.parens(p.lazy(() => expr)), lexer.naturalOrFloat);
		const expr = makeExpressionParser(table, term);

		const source = '3*5';
		const input = new p.State(source, 0);
		const expected = p.ok(new p.State(source, 3), 15);
		const result = p.parse(expr, input);
		expect(result).pEquals(expected);
	});

	test('parse expression ok 2', () => {
		const table: Array<p.Operator<number>[]> = [
			[binary('*', (x, y) => x * y, p.Assoc.AssocLeft), binary('/', (x, y) => x / y, p.Assoc.AssocLeft)],
			[binary('+', (x, y) => x + y, p.Assoc.AssocLeft), binary('-', (x, y) => x - y, p.Assoc.AssocLeft)],
		];
		const term: p.Parser<number> = p.or(lexer.parens(p.lazy(() => expr)), lexer.naturalOrFloat);
		const expr = makeExpressionParser(table, term);

		const source = '1+3*5';
		const input = new p.State(source, 0);
		const expected = p.ok(new p.State(source, 5), 16);
		const result = p.parse(expr, input);
		expect(result).pEquals(expected);
	});

	test('parse expression ok 3', () => {
		const table: Array<p.Operator<number>[]> = [
			[binary('*', (x, y) => x * y, p.Assoc.AssocLeft), binary('/', (x, y) => x / y, p.Assoc.AssocLeft)],
			[binary('+', (x, y) => x + y, p.Assoc.AssocLeft), binary('-', (x, y) => x - y, p.Assoc.AssocLeft)],
		];
		// <expr> ::= <expr> + <term>
		//          | <expr> - <term>
		//          | <term>
		//
		// <term> ::= <term> * <factor>
		//          | <term> / <factor>
		//          | <factor>
		//
		// <factor> ::= ( <expr> )
		//						| Number
		const term: p.Parser<number> = p.or(lexer.parens(p.lazy(() => expr)), lexer.naturalOrFloat);
		const expr = makeExpressionParser(table, term);

		const source = '3*(5+1.2)';
		const input = new p.State(source, 0);
		const expected = p.ok(new p.State(source, 9), 18.6);
		const result = p.parse(expr, input);
		expect(result).pEquals(expected);
	});

	test('parse expression ok 4', () => {
		const table: Array<p.Operator<number>[]> = [
			[binary('*', (x, y) => x * y, p.Assoc.AssocLeft), binary('/', (x, y) => x / y, p.Assoc.AssocLeft)],
			[binary('+', (x, y) => x + y, p.Assoc.AssocLeft), binary('-', (x, y) => x - y, p.Assoc.AssocLeft)],
		];
		const term: p.Parser<number> = p.or(lexer.parens(p.lazy(() => expr)), lexer.naturalOrFloat);
		const expr = makeExpressionParser(table, term);

		const source = '(1+3)*5';
		const input = new p.State(source, 0);
		const expected = p.ok(new p.State(source, 7), 20);
		const result = p.parse(expr, input);
		expect(result).pEquals(expected);
	});

	test('parse expression ok 5', () => {
		const table: Array<p.Operator<number>[]> = [
			[prefix('-', x => -x), prefix('+', x => x)],
			[binary('*', (x, y) => x * y, p.Assoc.AssocLeft), binary('/', (x, y) => x / y, p.Assoc.AssocLeft)],
			[binary('+', (x, y) => x + y, p.Assoc.AssocLeft), binary('-', (x, y) => x - y, p.Assoc.AssocLeft)],
		];
		const term: p.Parser<number> = p.or(lexer.parens(p.lazy(() => expr)), lexer.naturalOrFloat);
		const expr = makeExpressionParser(table, term);
		const source = '(1 * 2) + 3 * (5 - 3) + (-2) * 5';
		const expected = p.ok(new p.State(source, source.length), -2);
		const result = p.parse(expr, new p.State(source, 0));
		expect(result).pEquals(expected);
	});

	test('parse expression ok 6', () => {
		const table: Array<p.Operator<number>[]> = [
			[prefix('-', x => -x), prefix('+', x => x)],
			[binary('*', (x, y) => x * y, p.Assoc.AssocLeft), binary('/', (x, y) => x / y, p.Assoc.AssocLeft)],
			[binary('+', (x, y) => x + y, p.Assoc.AssocLeft), binary('-', (x, y) => x - y, p.Assoc.AssocLeft)],
		];
		const term: p.Parser<number> = p.or(lexer.parens(p.lazy(() => expr)), lexer.naturalOrFloat);
		const expr = makeExpressionParser(table, term);
		const source = '10e2 * 2';
		const expected = p.ok(new p.State(source, source.length), 2000);
		const result = p.parse(expr, new p.State(source, 0));
		expect(result).pEquals(expected);
	});

});
