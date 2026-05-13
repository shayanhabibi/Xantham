// TEST TARGET: deep transitive imports
//
// package.json name: "my-app"
// Entry point imports Dashboard from ./views/dashboard.
// Dashboard transitively imports Widget from ../components/widget.
// 3-level import chain: index → views/dashboard → components/widget.
//
// Expected Sources:
//   AppConfig (this file)   → "my-app" (fallback to package.json)
//   Dashboard (views/)      → "./views/dashboard" (import specifier from here)
//   Widget (components/)    → "../components/widget" (import specifier from dashboard)

import { Dashboard } from "./views/dashboard";

export interface AppConfig {
    title: string;
    dashboard: Dashboard;
}
