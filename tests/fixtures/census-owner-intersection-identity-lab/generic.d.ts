interface GenericBase<T> { value: T; }
interface GenericBot<T> { payload: GenericBase<T> & { marker: T }; }
interface SeparateBot<T> { payload: GenericBase<T> & { marker: T }; }
interface Other { colo: string; }
type GenericCombined<T> = GenericBot<T> & Other;
