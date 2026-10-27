import type { Issue } from "./common";
import type { RenamedParsed } from "./mirror";
export interface Container { issue: Issue; }
export interface AliasUse { parsed: RenamedParsed; }
