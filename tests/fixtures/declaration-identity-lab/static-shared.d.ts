export declare class Client {
  constructor(id: string);
  send(value: string): void;
  static readonly tag: string;
  static count: number;
  static create(id: string): Client;
  static create(id: number): Client;
  static identity<T>(value: T): T;
}
