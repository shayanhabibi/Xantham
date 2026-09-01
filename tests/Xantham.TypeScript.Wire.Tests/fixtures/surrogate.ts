// The escapes below are the point of this fixture: TypeScript permits a lone surrogate in a
// string literal, and the compiler stores the *cooked* value in the AST string table. A lone
// surrogate has no UTF-8 encoding, so the table is WTF-8 - the three bytes ED A0 80 - which
// strict UTF-8 decoding replaces with U+FFFD. The file itself is plain ASCII.
export const lone = "\uD800";
export const trailing = "ab\uDC00";
export const paired = "😀";
