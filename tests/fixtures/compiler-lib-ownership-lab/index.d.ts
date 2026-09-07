interface Item {
    when: Date;
    value: string;
}

type Items = Array<Item>;
type Brief = Omit<Item, "value">;
type LocalDate = Date;

interface Store {
    update(value: Partial<Item>): void;
}

declare var currentItem: Item;
declare function inspect(items: Items): Brief;

interface WindowLike {
    readonly self: WindowLike & typeof globalThis;
}

declare var windowLike: WindowLike;
