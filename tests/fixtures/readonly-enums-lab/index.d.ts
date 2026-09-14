import { Access, Waiting } from "permissions-lab";
interface Permissions { access: Readonly<Access>; waiting: Readonly<Waiting> }
export type View = Readonly<Permissions>;
