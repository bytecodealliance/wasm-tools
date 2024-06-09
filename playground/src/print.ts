import type { MessageToWorker } from './utilities.js';

let overlay: HTMLDivElement = document.querySelector('#overlay')!;
let upload: HTMLButtonElement = document.querySelector('#upload')!;
let fileInput: HTMLInputElement = document.querySelector('#fileInput')!;
let output: HTMLTextAreaElement = document.querySelector('#output')!;

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
  print(file);
}, { capture: true });

let messageId = 0;
async function print(f: File) {
  output.value = 'working...';
  let bytes = new Uint8Array(await f.arrayBuffer());
  worker.postMessage({ kind: 'print', messageId, bytes } satisfies MessageToWorker, [bytes.buffer]);
  ++messageId;
}

let worker = new Worker('worker.js', { type: 'module' });

// we can't just postmessage to the worker right away because it has a top-level await
// so we need to wait for it to tell us it's ready
worker.addEventListener('message', () => {
  worker.addEventListener('message', gotMessage);
}, { once: true },);

function gotMessage({ data }: {
  data: { messageId: number; success: true; source: string } | { messageId: number; success: false; error: string };
}) {
  if (data.messageId !== messageId - 1) {
    // user has already asked for a new file
    return;
  }
  if (data.success) {
    output.value = data.source;
  } else {
    output.value = data.error;
  }
}

upload.addEventListener('click', () => {
  fileInput.click();
  fileInput.onchange = () => {
    let file = fileInput.files?.[0];
    if (!file) return;
    print(file);
  }
});
