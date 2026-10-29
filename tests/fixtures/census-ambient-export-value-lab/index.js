const state = {
    value: "initial",
    marker: "root-value",
    update(value) {
        this.value = value;
        return this.marker + ":" + this.value;
    },
};
export default state;
