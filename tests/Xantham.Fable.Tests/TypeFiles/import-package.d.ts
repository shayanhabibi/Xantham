// TEST TARGET: source name from package import
//
// Imports Button from packages/ui-kit and uses it.
// Verifies that:
//   - AppButton (defined here) gets fallback Source
//   - The re-used Button type from ui-kit retains Source = "ui-kit"

import { Button } from "./packages/ui-kit/index";

export interface AppButton extends Button {
    theme: string;
}

export type ButtonLabel = string;
