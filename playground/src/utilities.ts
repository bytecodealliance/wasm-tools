export type MessageToWorker =
  | { kind: 'parse'; source: string }
  | { kind: 'print'; messageId: number; bytes: Uint8Array, skeleton: boolean };

export function debounce(f: (...args: unknown[]) => void, ms: number) {
  let timeout: number | null;
  return function (this: unknown, ...args: unknown[]) {
    let fresh = timeout == null;
    if (!fresh) clearTimeout(timeout!);
    timeout = setTimeout(() => {
      timeout = null;
      if (!fresh) f.apply(this, args);
    }, ms);
    if (fresh) f.apply(this, args);
  };
}
