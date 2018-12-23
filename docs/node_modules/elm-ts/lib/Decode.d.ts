import { Either } from 'fp-ts/lib/Either';
import { Type, mixed } from 'io-ts';
export declare type mixed = mixed;
export interface Decoder<a> {
    decode: (value: mixed) => Either<string, a>;
}
export declare function decodeJSON<a>(decoder: Decoder<a>, value: mixed): Either<string, a>;
export declare function map<a, b>(fa: Decoder<a>, f: (a: a) => b): Decoder<b>;
export declare function fromType<a>(type: Type<a, any, mixed>): Decoder<a>;
