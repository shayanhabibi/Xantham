export type Tool<INPUT extends unknown | never = any, OUTPUT extends unknown | never = any> = {
    /**
     * An optional description of what the tool does.
     * Will be used by the language model to decide whether to use the tool.
     * Not used for provider-defined tools.
     */
    description?: string;
    /**
     * An optional title of the tool.
     */
    title?: string;
    /**
     * Additional provider-specific metadata. They are passed through
     * to the provider from the AI SDK and enable provider-specific
     * functionality that can be fully encapsulated in the provider.
     */
    providerOptions?: any;
    /**
     * Optional metadata about the tool itself (e.g. its source).
     *
     * Unlike `providerOptions`, this metadata is not sent to the language
     * model. Instead, it is propagated onto the resulting tool call's
     * `providerMetadata` so consumers can read it from tool call / result
     * parts and UI message parts. This is useful for sources of dynamic
     * tools (e.g. an MCP server) to identify themselves.
     */
    providerMetadata?: any;
    /**
     * The schema of the input that the tool expects.
     * The language model will use this to generate the input.
     * It is also used to validate the output of the language model.
     *
     * You can use descriptions on the schema properties to make the input understandable for the language model.
     */
    inputSchema: Array<INPUT>;
    /**
     * An optional list of input examples that show the language
     * model what the input should look like.
     */
    inputExamples?: Array<{
        input: NoInfer<INPUT>;
    }>;
    /**
     * Whether the tool needs approval before it can be executed.
     */
    needsApproval?: boolean | Array<[INPUT] extends [never] ? unknown : INPUT>;
    /**
     * Strict mode setting for the tool.
     *
     * Providers that support strict mode will use this setting to determine
     * how the input should be generated. Strict mode will always produce
     * valid inputs, but it might limit what input schemas are supported.
     */
    strict?: boolean;
    /**
     * Optional function that is called when the argument streaming starts.
     * Only called when the tool is used in a streaming context.
     */
    onInputStart?: (options: any) => void | PromiseLike<void>;
    /**
     * Optional function that is called when an argument streaming delta is available.
     * Only called when the tool is used in a streaming context.
     */
    onInputDelta?: (options: {
        inputTextDelta: string;
    } & any) => void | PromiseLike<void>;
    /**
     * Optional function that is called when a tool call can be started,
     * even if the execute function is not provided.
     */
    onInputAvailable?: (options: {
        input: [INPUT] extends [never] ? unknown : INPUT;
    } & any) => void | PromiseLike<void>;
}