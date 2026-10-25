export const legacy = { ['$`']: 'legacy' };
export const collision = {
    Item: 'original', Value: 'original-value', ['$`']: 'dollar', ['`']: 'tick',
    ['`value']: 'first', ['value`']: 'second', Create: 'create', ['`Create']: 'key-create',
    ['`Invoke'](value) { if (this !== collision) throw Error('receiver'); return value + ':receiver'; },
    Invoke(value) { return value; },
    ['`SymbolToPrimitive'](value) { if (this !== collision) throw Error('receiver'); return value + ':key'; },
    ['`SymbolTo'](event, value) { if (this !== collision) throw Error('receiver'); return value; },
    [Symbol.toPrimitive](hint) { return hint; },
    ['`on'](event, value) { if (this !== collision) throw Error('receiver'); return value; },
    OnEnd: 'ordinary', ['quote"slash\\$1`']: 'escaped'
};
export class KeyedClass {
    constructor() { this['$`'] = 'instance'; }
    ['`call'](value) { return this['$`'] + ':' + value; }
    static ['$`'] = 'static';
    static ordinary = 'ordinary-static';
    static ['`call'](value) { return this['$`'] + ':' + value; }
}
export function inspectOptions(value) {
    return JSON.stringify([value['`required'], value['required`'], Object.hasOwn(value, '`optional'), value['`optional'] ?? null,
        Object.hasOwn(value, 'Required'), Object.hasOwn(value, 'Required2')]);
}
export const renamedDerived = { ['`value']: 'inherited-key', Value: 12 };
export function inspectChoice(value) {
    return JSON.stringify([value['`tag'], Object.hasOwn(value, 'a'), value.a ?? null, Object.hasOwn(value, 'b'), value.b ?? null, Object.hasOwn(value, 'Tag')]);
}
