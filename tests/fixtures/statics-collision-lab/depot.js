// The runtime behind `declare module "statics-lab:depot"`.

export class Depot {
    static LIMIT = 7;
    constructor(slot) {
        this.slot = slot;
    }
    static open(slot) {
        return new Depot(slot);
    }
}
