export interface Message { text: string; }
export interface View { message: Message & { renderedId?: string }; }
