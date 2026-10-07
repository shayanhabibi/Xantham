
import { omit } from "solid-js";

export function CustomInput(props) {
    const PARTAS_OTHERS = omit(props, "value", "title");
    return <input value={props.value}
        title={props.title} />;
}

export function App_view() {
    return <CustomInput value="custom value"
        title="custom title" />;
}
