export class Account {
    #id = 0;
    constructor(owner) { this.owner = owner; this._legacy = 0; this.balance = 0; this.secret = ""; }
    audit(note) {}
    rotate() {}
    deposit(amount) { this.balance += amount; return this; }
    get limit() { return 100; }
    set limit(value) {}
    static DEFAULT_OWNER = "nobody";
    static registry = [];
    static open(owner) { return new Account(owner); }
}

export class Token {
    constructor() { this.value = "t"; }
    static issue() { return new Token(); }
}

export class Opaque {
    constructor() { this.handle = 0; }
}

export function audit(account) { return new Opaque(); }
