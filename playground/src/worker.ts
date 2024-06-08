import { parse } from '../component-built/component.js';

postMessage('loaded');

addEventListener('message', ({ data }) => {
  try {
    let bytes = parse(data);
    postMessage({ success: true, bytes }, [bytes.buffer]);
  } catch (e) {
    postMessage({ success: false, error: (e as Error).message });
  }
});
