import { type MessageToWorker, debounce } from './utilities.js';

// TODO replace this function once https://github.com/tc39/proposal-arraybuffer-base64 ships
function base64(bytes: Uint8Array) {
  // @ts-expect-error it's fine to pass a Uint8Array here
  return btoa(String.fromCharCode.apply(null, bytes));
}

let input: HTMLTextAreaElement = document.querySelector('#input')!;
let output: HTMLTextAreaElement = document.querySelector('#output')!;
let downloadButton: HTMLButtonElement = document.querySelector('#download')!;

let worker = new Worker('worker.js', { type: 'module' });

let lastBytes: Uint8Array;
let sent = '';
let statusMessageTimeout = 0;

// we can't just postmessage to the worker right away because it has a top-level await
// so we need to wait for it to tell us it's ready
worker.addEventListener('message', () => {
  worker.addEventListener('message', gotMessage);
  update();
}, { once: true });

function gotMessage({ data }: { data: { success: true, bytes: Uint8Array } | { success: false, error: string }}) {
  clearTimeout(statusMessageTimeout);
  if (input.value === sent) {
    if (data.success) {
      lastBytes = data.bytes;
      output.value = base64(lastBytes);
      downloadButton.disabled = false;
      output.style.color = '';
    } else {
      output.value = data.error;
      downloadButton.disabled = true;
      output.style.color = '#a00';
    }
  } else {
    // the user has already moved on, ignore this result and re-run
    update();
  }
}

function update() {
  sent = input.value;
  worker.postMessage({ kind: 'parse', source: input.value } satisfies MessageToWorker);

  // wait a bit before updating the output to avoid rapid changes when the output can be parsed quickly
  statusMessageTimeout = setTimeout(() => {
    output.value = 'working...';
    downloadButton.disabled = true;
  }, 200);
}

input.addEventListener('input', debounce(update, 100));

downloadButton.addEventListener('click', () => {
  let blob = new Blob([lastBytes], { type: 'application/wasm' });
  let link = document.createElement('a');
  let url = URL.createObjectURL(blob);
  link.href = url;
  link.download = 'wasm-tools-parse-output.wasm';
  link.click();
  URL.revokeObjectURL(url);
});
