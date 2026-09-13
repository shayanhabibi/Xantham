export class Base extends Error {
  constructor(code) { super(code); this.code = code; }
}
export class Child extends Base {
  constructor(message) { super("child"); this.detail = message; }
}
