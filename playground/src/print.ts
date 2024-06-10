import type { MessageToWorker } from './utilities.js';
import type { PrintPart } from '../component-built/component.js';

function unbase64(chars: string) {
  return new Uint8Array(Array.from(atob(chars), c => c.charCodeAt(0)))
}

let overlay: HTMLDivElement = document.querySelector('#overlay')!;
let upload: HTMLButtonElement = document.querySelector('#upload')!;
let fileInput: HTMLInputElement = document.querySelector('#fileInput')!;
let output: HTMLPreElement = document.querySelector('#output')!;
let skeletonCheckbox: HTMLInputElement = document.querySelector('#skeleton')!;

let lastTarget: EventTarget | null;
let hideOverlayTimer: number;
addEventListener('dragover', e => {
  e.preventDefault(); // necessary to make the drop event work, for some reason
  lastTarget = e.target;
  overlay.style.display = 'flex';
  clearTimeout(hideOverlayTimer);
});

addEventListener('dragleave', e => {
  e.preventDefault();
  if (e.target === document || e.target === lastTarget) {
    // we need a timer here because otherwise the overlay flashes while moving the mouse around the page
    hideOverlayTimer = setTimeout(() => {
      overlay.style.display = 'none';
    }, 10);
  }
});

addEventListener('drop', (e: DragEvent) => {
  e.preventDefault();
  overlay.style.display = 'none';
  let file = e.dataTransfer?.files[0];
  if (!file) return;
  print(file.arrayBuffer());
}, { capture: true });

let messageId = 0;
async function print(f: Promise<ArrayBuffer>) {
  output.innerHTML = 'working...';
  let skeleton = skeletonCheckbox.checked;
  let bytes = new Uint8Array(await f);
  worker.postMessage({ kind: 'print', messageId, bytes, skeleton } satisfies MessageToWorker, [bytes.buffer]);
  ++messageId;
}

let worker = new Worker('worker.js', { type: 'module' });

// we can't just postmessage to the worker right away because it has a top-level await
// so we need to wait for it to tell us it's ready
worker.addEventListener('message', () => {
  worker.addEventListener('message', gotMessage);
  print(Promise.resolve(unbase64('AGFzbQEAAAABDQNgAX8Bf2ABfQBgAAACCwEDZm9vA2JhcgABAwMCAgEEBQFwAQABBQQBAQEBBwUBAWUAAQgBAQoKAgIACwUAQSoaCwsIAQBBAAsCaGk=').buffer));
}, { once: true },);

upload.addEventListener('click', () => {
  fileInput.click();
  fileInput.onchange = () => {
    let file = fileInput.files?.[0];
    if (!file) return;
    print(file.arrayBuffer());
  }
});

function gotMessage({ data }: {
  data: { messageId: number; success: true; source: PrintPart[] } | { messageId: number; success: false; error: string };
}) {
  if (data.messageId !== messageId - 1) {
    // user has already asked for a new file
    return;
  }
  output.innerHTML = '';
  if (data.success) {
    output.append(toDOM(data.source));
  } else {
    let span = document.createElement('span');
    span.style.color = '#a00';
    span.append(data.error);
    output.append(span);
  }
}

function toDOM(parts: PrintPart[]) {
  let root = document.createElement('span');
  let stack = [root];
  for (let part of parts) {
    switch (part.tag) {
      case 'str': {
        stack[0].append(part.val);
        break;
      }
      case 'name':
      case 'literal':
      case 'keyword':
      case 'type':
      case 'comment': {
        let ele = document.createElement('span');
        ele.className = 'print-' + part.tag;
        stack[0].append(ele);
        stack.unshift(ele);
        break;
      }
      case 'reset': {
        stack.shift();
        break;
      }
      default: {
        // @ts-expect-error the above should be exhaustive
        type remaining = typeof part.tag;
        console.error('got unknown print part', part);
      }
    }
  }
  return root;
}
