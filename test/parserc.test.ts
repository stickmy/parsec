import * as p from '../src/parsec';

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

describe('seq', () => {
	test('seq ok 1', () => {
		const source = '/*123*/';
		const expected = p.error(new p.State(source, 5), () => '"/*"');
		const parser = p.seq(m => {
			m(p.string('/*123'));
			m(p.string('/*'));
		});
		const result = p.parse(parser, new p.State(source));
		expect(result).pEquals(expected);
	});

	test('seq ok 2', () => {
		const source = '/*123*/';
		const expected = p.ok(new p.State(source, 7), undefined);
		const parser = p.seq(m => {
			m(p.string('/*123'));
			m(p.string('*/'));
		});
		const result = p.parse(parser, new p.State(source));
		expect(result).pEquals(expected);
	});

	test('seq error 1', () => {
		const source = '/*123*/';
		const expected = p.error(new p.State(source, 0), () => '"123"');
		const parser = p.seq(m => {
			m(p.string('123'));
			m(p.string('/*'));
		});
		const result = p.parse(parser, new p.State(source));
		expect(result).pEquals(expected);
	});
});

describe('sepBy', () => {
	test('sepBy ok 1', () => {
		const source = '';
		const expected = p.ok<string[], any>(new p.State(source), []);
		const parser = p.sepBy(p.string('a'), p.string('_'));
		const result = p.parse(parser, new p.State(source));
		expect(result).pEquals<string[], any>(expected);
	});

	test('sepBy ok 2', () => {
		const source = 'a_a';
		const expected = p.ok<string[], any>(new p.State(source, 3), ['a', 'a']);
		const parser = p.sepBy(p.string('a'), p.string('_'));
		const result = p.parse(parser, new p.State(source));
		expect(result).pEquals<string[], any>(expected);
	});

	test('sepBy error 1', () => {
		const source = 'a_b';
		const expected = p.error<string[], any>(new p.State(source, 2), () => '"a"');
		const parser = p.sepBy(p.string('a'), p.string('_'));
		const result = p.parse(parser, new p.State(source));
		expect(result).pEquals<string[], any>(expected);
	});

	test('sepBy error 1', () => {
		const source = 'a_a_';
		const expected = p.error<string[], any>(new p.State(source, 4), () => '"a"');
		const parser = p.sepBy(p.string('a'), p.string('_'));
		const result = p.parse(parser, new p.State(source));
		expect(result).pEquals<string[], any>(expected);
	});
});

describe('sepBy1', () => {
	test('sepBy1 ok 1', () => {
		const source = 'a_a_a';
		const expected = p.ok(new p.State(source, 5), ['a', 'a', 'a']);
		const parser = p.sepBy1(p.string('a'), p.string('_'));
		const result = p.parse(parser, new p.State(source));
		expect(result).pEquals<string[], any>(expected);
	});

	test('sepBy1 ok 2', () => {
		const source = 'a-b';
		const expected = p.ok<string[], any>(new p.State(source, 1), ['a']);
		const parser = p.sepBy1(p.string('a'), p.string('_'));
		const result = p.parse(parser, new p.State(source));
		expect(result).pEquals<string[], any>(expected);
	});

	test('sepBy1 error 1', () => {
		const source = '';
		const expected = p.error<string[], any>(new p.State(source), () => '"a"');
		const parser = p.sepBy1(p.string('a'), p.string('_'));
		const result = p.parse(parser, new p.State(source));
		expect(result).pEquals<string[], any>(expected);
	});

	test('sepBy1 error 2', () => {
		const source = 'a_b';
		const expected = p.error<string[], any>(new p.State(source, 2), () => '"a"');
		const parser = p.sepBy1(p.string('a'), p.string('_'));
		const result = p.parse(parser, new p.State(source));
		expect(result).pEquals<string[], any>(expected);
	});
});

describe('endBy', () => {
	test('endBy ok 1', () => {
		const source = 'a_a_';
		const expected = p.ok<string[], any>(new p.State(source, 4), ['a', 'a']);
		const parser = p.endBy(p.string('a'), p.string('_'));
		const result = p.parse(parser, new p.State(source));
		expect(result).pEquals<string[], any>(expected);
	});

	test('endBy error 1', () => {
		const source = 'a_a';
		const expected = p.error<string[], any>(new p.State(source, 3), () => '"_"');
		const parser = p.endBy(p.string('a'), p.string('_'));
		const result = p.parse(parser, new p.State(source));
		expect(result).pEquals<string[], any>(expected);
	});
});

describe('pure', () => {
	test('pure ok 1', () => {
		const parser = p.pure('a');
		const expected = p.ok(new p.State(''), 'a');
		const result = p.parse(parser, new p.State(''));
		expect(result).pEquals(expected);
	});
});

describe('option', () => {
	test('option ok 1', () => {
		const source = 'b';
		const expected = p.ok(new p.State(source), ';');
		const parser = p.option(p.string('a'), ';');
		const result = p.parse(parser, new p.State(source));
		expect(result).pEquals(expected);
	});

	test('option ok 2', () => {
		const source = 'a';
		const expected = p.ok(new p.State(source, 1), 'a');
		const parser = p.option(p.string('a'), ';');
		const result = p.parse(parser, new p.State(source));
		expect(result).pEquals(expected);
	});
});

describe('optional', () => {
	test('optional ok 1', () => {
		const source = 'b';
		const expected = p.ok(new p.State(source), undefined);
		const parser = p.optional(p.string('a'));
		const result = p.parse(parser, new p.State(source));
		expect(result).pEquals(expected);
	});
});

describe('skipMany', () => {
	test('skipMany ok 1', () => {
		const source = 'aaab';
		const expected = p.ok(new p.State(source, 3), undefined);
		const parser = p.skipMany(p.string('a'));
		const result = p.parse(parser, new p.State(source));
		expect(result).pEquals(expected);
	});

	test('skipMany ok 2', () => {
		const source = 'aaab';
		const expected = p.ok(new p.State(source, 0), undefined);
		const parser = p.skipMany(p.string('b'));
		const result = p.parse(parser, new p.State(source));
		expect(result).pEquals(expected);
	});
});

describe('skipMany1', () => {
	test('skipMany1 error 1', () => {
		const source = 'aaab';
		const expected = p.error(new p.State(source, 0), () => '"b"');
		const parser = p.skipMany1(p.string('b'));
		const result = p.parse(parser, new p.State(source));
		expect(result).pEquals(expected);
	});
});

describe('lookAhead', () => {
	test('lookAhead ok 1', () => {
		const source = 'aaab';
		const expected = p.ok(new p.State(source, 0), 'a');
		const parser = p.lookAhead(p.string('a'));
		const result = p.parse(parser, new p.State(source));
		expect(result).pEquals(expected);
	});
});

describe('notFollowedBy', () => {
	test('notFollowedBy ok 1', () => {
		const source = 'aaab';
		const expected = p.ok(new p.State(source, 0), undefined);
		const parser = p.notFollowedBy(p.string('b'));
		const result = p.parse(parser, new p.State(source));
		expect(result).pEquals(expected);
	});
});

describe('until', () => {
	test('until ok 1', () => {
		const source = 'xxxxxx*/';
		const expected = p.ok(new p.State(source, 6), 'xxxxxx');
		const parser = p.until(p.string('*/'));
		const result = p.parse(parser, new p.State(source));
		expect(result).pEquals(expected);
	});

	test('until ok 2', () => {
		const source = 'xxxxxx*/';
		const expected = p.ok(new p.State(source, 6), 'xxxxxx');
		const parser = p.until(p.regexp(/\*\//));
		const result = p.parse(parser, new p.State(source));
		expect(result).pEquals(expected);
	});

	test('until error 1', () => {
		const source = 'xxxxxxxx';
		const expected = p.error(new p.State(source, 8), () => 'can not match until the end');
		const parser = p.until(p.regexp(/\*\//));
		const result = p.parse(parser, new p.State(source));
		expect(result).pEquals(expected);
	});

	test('uri parse', () => {
		// http://www.google.com/path1/path2?name=123&age=12
		const param = p.seq(m => {
			const name = m(p.regexp(/[^=]+/));
			m(p.string('='));
			const value = m(p.regexp(/[^&]+/));
			return { name, value };
		});

		const parser = p.seq(m => {
			const scheme = m(p.regexp(/[a-z]+/));
			m(p.string('://'));
			const host = m(p.sepBy1(p.regexp(/[a-z]+/), p.string('.')));
			const port = m(
				p.optional(
					p.seq(s => {
						s(p.string(':'));
						return s(p.regexp(/\d+/));
					}),
				),
			);
			const opt = m(
				p.optional(
					p.seq(s => {
						s(p.string('/'));
						const path = s(p.sepBy(p.regexp(/[^\/?]+/), p.string('/')));
						const params = s(
							p.optional(
								p.seq(x => {
									x(p.string('?'));
									return x(p.sepBy(param, p.string('&')));
								}),
							)
						);
						return { path, params };
					}),
				),
			);
			if (opt) {
				return { scheme, host, port, path: opt.path, params: opt.params };
			}
			return { scheme, host, port };
		});

		const source = 'http://www.google.com/path1/path2?name=123&age=12';
		const result = p.parse(parser, new p.State(source));
		const expected = p.ok(new p.State(source, source.length), {
			scheme: 'http',
			host: ['www', 'google', 'com'],
			path: ['path1', 'path2'],
			params: [{ name: 'name', value: '123' }, { name: 'age', value: '12' }],
		});
		expect(result).pEquals(expected);
	});
});
