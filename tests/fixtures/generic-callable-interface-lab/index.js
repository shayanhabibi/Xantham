exports.identity = value => value;
exports.accept = callback => callback;
exports.pairWith = first => second => [first, second];
exports.anonymous = callback => callback;
exports.inherited = () => exports.identity;
exports.redundant = () => exports.identity;
exports.anonymousIdentity = value => value;
exports.declaredIdentity = value => value;
exports.identityAlias = exports.declaredIdentity;
