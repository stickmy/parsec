function eq<T>(a: T, b: T): boolean {
	return JSON.stringify(a) === JSON.stringify(b);
}

export class State<U> {
	constructor(public source: string, public position: number = 0, public _userState?: U) {}

	seek(delta: number) {
		return new State(this.source, this.position + delta, this._userState);
	}

	getRowColumn(): { raw: number; column: number } {
		const lines = this.source.split('\n');
		let position = 0;
		let raw = 0;
		while (position < this.position) {
			if (this.position <= position + lines[raw].length) {
				break;
			}
			position += lines[raw].length + 1;
			raw++;
		}
		const column = this.position - position;
		return { raw, column };
	}

	rest() {
		return this.source.slice(this.position);
	}

	equals(src: State<U>): boolean {
		return src && this.source === src.source && this.position === src.position && eq(this._userState, src._userState);
	}

	toString() {
		return `
			State: {
				source: ${this.source.length <= 100 ? `"${this.source}"` : `"${this.source.slice(0, 100)}"...`}
				position: ${this.position}
				userState: ${JSON.stringify(this._userState)}
			}
		`;
	}
}

export class Reply<T, U> {
	constructor(public state: State<U>, public success: boolean, public value?: T, public expected?: () => string) {}

	toString() {
		return `
			Reply { 
				success: ${this.success}
				${this.success ? 'value: ' + String.raw`${this.value}` : 'expected: ' + this.expected!()}
				state: ${this.state.toString().replace(/^(?!\s*$)/gm, '\t\t')}
			}
		`;
	}

	equals(st: Reply<T, U>): boolean {
		return (
			st &&
			this.state.equals(st.state) &&
			this.success === st.success &&
			(this.success
				? eq(this.value, st.value)
				: (this.expected === undefined && st.expected === undefined) || this.expected!() === st.expected!())
		);
	}
}

export interface SeqController<U = any, T = any> {
	<S>(p: Parser<S>): S;

	success: boolean;

	state: State<T>;

	_userState: U;
}

export function ok<T, U>(state: State<U>, value?: T): Reply<T, U> {
	return new Reply<T, U>(state, true, value);
}

export function error<T, U>(state: State<U>, expect?: () => string): Reply<T, U> {
	return new Reply<T, U>(state, false, undefined, expect);
}

export class Parser<T> {
	constructor(public runParser: <U>(state: State<U>) => Reply<T, U>) {}

	toString() {
		return this.runParser.toString();
	}
}

export function parse<T, U>(parser: Parser<T>, state: State<U>) {
	return parser.runParser(state);
}
