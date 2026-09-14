type Tool<I, O> = { execute?: (input: I) => O; title?: string };
export type ToolSet = Record<string,
  (Tool<never, never> | Tool<any, any> | Tool<any, never> | Tool<never, any>)
  & Pick<Tool<any, any>, "execute">>;
