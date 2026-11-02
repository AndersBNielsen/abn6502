import { ABN6502 } from './board.js?v=20261020a';

const $ = (id) => document.getElementById(id);
const canvas = $('screen');
const screenInput = $('screenInput');
const consoleEl = $('console');
const keypad = $('keypad');
const programs = $('programs');
keypad.open = matchMedia('(pointer:coarse) and (orientation:portrait)').matches;
const ctx = canvas.getContext('2d', { alpha: false });
const frame = ctx.createImageData(512, 256);
let board, paused = false, last = 0, lastPaint = 0;
let basicEnters = -1;
const keyCodes = {
  KeyA:0x1c,KeyB:0x32,KeyC:0x21,KeyD:0x23,KeyE:0x24,KeyF:0x2b,KeyG:0x34,
  KeyH:0x33,KeyI:0x43,KeyJ:0x3b,KeyK:0x42,KeyL:0x4b,KeyM:0x3a,KeyN:0x31,
  KeyO:0x44,KeyP:0x4d,KeyQ:0x15,KeyR:0x2d,KeyS:0x1b,KeyT:0x2c,KeyU:0x3c,
  KeyV:0x2a,KeyW:0x1d,KeyX:0x22,KeyY:0x35,KeyZ:0x1a,
  Digit0:0x45,Digit1:0x16,Digit2:0x1e,Digit3:0x26,Digit4:0x25,Digit5:0x2e,
  Digit6:0x36,Digit7:0x3d,Digit8:0x3e,Digit9:0x46,
  Space:0x29,Enter:0x5a,NumpadEnter:0x5a,Backspace:0x66,Escape:0x76,
  Tab:0x0d,Minus:0x4e,Equal:0x55,BracketLeft:0x54,BracketRight:0x5b,
  Backslash:0x5d,Semicolon:0x4c,Quote:0x52,Backquote:0x0e,Comma:0x41,
  Period:0x49,Slash:0x4a,ShiftLeft:0x12,ShiftRight:0x59,
  F1:0x05,F2:0x06,F3:0x04,F4:0x0c,F5:0x03,F6:0x0b,F7:0x83,F8:0x0a,
  F9:0x01,F10:0x09,F11:0x78,F12:0x07,
  ArrowUp:0x75,ArrowDown:0x72,ArrowLeft:0x6b,ArrowRight:0x74,
  Home:0x6c,PageUp:0x7d,PageDown:0x7a,Delete:0x71
};
const extended = new Set(['ArrowUp','ArrowDown','ArrowLeft','ArrowRight','Home','PageUp','PageDown','Delete']);
const shiftedPunctuation = `~!@#$%^&*()_+{}|:\"<>?`;

function scan(code, release = false) {
  const raw = keyCodes[code];
  if (raw === undefined || !board) return false;
  if (!release) {
    if (code === 'F4') basicEnters = 0;
    else if (code === 'F8') basicEnters = -1;
    else if ((code === 'Enter' || code === 'NumpadEnter') && basicEnters >= 0 && basicEnters < 2) basicEnters++;
  }
  board.keyScan(...(extended.has(code) ? [0xe0] : []), ...(release ? [0xf0] : []), raw);
  return true;
}
function characterCode(char) {
  return char === ' ' ? 'Space' : /[a-z]/i.test(char) && char.length === 1 ? `Key${char.toUpperCase()}` :
    /[0-9]/.test(char) && char.length === 1 ? `Digit${char}` :
    ({'\n':'Enter','\b':'Backspace','-':'Minus','=':'Equal','[':'BracketLeft',']':'BracketRight',
      '\\':'Backslash',';':'Semicolon',"'":'Quote','`':'Backquote',',':'Comma','.':'Period','/':'Slash',
      '!':'Digit1','@':'Digit2','#':'Digit4','$':'Digit3','%':'Digit5','^':'Digit6','&':'Digit7',
      '*':'Digit8','(':'Digit9',')':'Digit0','_':'Minus','+':'Equal','?':'Slash',':':'Semicolon',
      '"':'Quote','<':'Comma','>':'Period'}[char]);
}
function typedChar(char) {
  if (!board) return false;
  const code = characterCode(char);
  if (!code) return false;
  const shift = char.toUpperCase() === char && /[A-Z]/.test(char) || shiftedPunctuation.includes(char);
  if (shift) board.keyScan(0x12);
  scan(code);
  if (shift) board.keyScan(0xf0, 0x12);
  return true;
}
function render() {
  if (!board) return;
  const data = frame.data;
  const videoOn = !!(board.control & 1);
  const base = board.control & 8 ? 0x800 : 0x2000;
  for (let y = 0; y < 256; y++) {
    const row = y >> 3, glyphRow = y & 7;
    for (let col = 0; col < 64; col++) {
      const code = board.ram[base + row * 64 + col] || 0;
      const pixels = videoOn ? board.chars[glyphRow * 256 + code] : 0;
      let p = (y * 512 + col * 8) * 4;
      for (let bit = 7; bit >= 0; bit--, p += 4) {
        const on = (pixels >> bit) & 1;
        data[p] = on ? 118 : 1;
        data[p + 1] = on ? 246 : 18;
        data[p + 2] = on ? 109 : 7;
        data[p + 3] = 255;
      }
    }
  }
  ctx.putImageData(frame, 0, 0);
}
function stats() {
  if (!board) return;
  $('pc').textContent = '$' + board.cpu.pc.toString(16).toUpperCase().padStart(4, '0');
  $('cycles').textContent = board.cpu.cycles.toLocaleString();
  $('state').textContent = board.cpu.jammed ? 'CPU HALTED' : paused ? 'PAUSED' : board.booted ? 'RUNNING' : 'BOOTING';
}
function animate(now) {
  const dt = Math.min(Math.max((now - last) / 1000, 0), 0.05);
  last = now;
  if (board && !paused) board.run(Math.min(150000, Math.round(dt * board.clockHz)));
  if (now - lastPaint > 45) { render(); stats(); lastPaint = now; }
  requestAnimationFrame(animate);
}
function install(rom, chars) {
  transferCancelled = true;
  board = new ABN6502(rom, chars);
  basicEnters = -1;
  paused = false;
  $('pause').textContent = 'Pause';
  render(); stats();
}
async function load() {
  try {
    const [romResponse, charResponse] = await Promise.all([
      fetch('roms/abn6502-r2.bin'), fetch('roms/abn6502-r2-chars.bin')
    ]);
    if (!romResponse.ok || !charResponse.ok) throw new Error('Firmware fetch failed');
    const [rom, chars] = await Promise.all([romResponse.arrayBuffer(), charResponse.arrayBuffer()]);
    install(new Uint8Array(rom), new Uint8Array(chars));
  } catch (error) { $('state').textContent = 'ROM ERROR'; console.error(error); }
}

function consumeTextInput(event) {
  if (event.isComposing) return;
  for (const char of event.target.value) typedChar(char);
  event.target.value = '';
}
screenInput.addEventListener('input', consumeTextInput);
screenInput.addEventListener('focus', () => {
  if (matchMedia('(pointer:coarse)').matches) { keypad.open = false; programs.open = false; }
});
$('mobileInput').addEventListener('input', consumeTextInput);
const examples = {
  hello: 'NEW\n10 PRINT "HELLO FROM ABN6502"\n20 FOR I=1 TO 5\n30 PRINT I\n40 NEXT I\nRUN\n',
  parade: 'NEW\n10 PRINT "NUMBER PARADE"\n20 FOR N=1 TO 10\n30 PRINT N\n40 NEXT N\n50 PRINT "THE END"\nRUN\n',
  // No ON...GOTO or comparison: the bundled ROM build mishandles both (see README).
  adventure: 'NEW\n10 A$(1)="DRAGON":A$(2)="TREASURE"\n20 PRINT "PICK 1 OR 2"\n30 INPUT G\n40 PRINT A$(G)\nRUN\n'
};
$('example').addEventListener('change', (event) => {
  if (examples[event.target.value]) $('programText').value = examples[event.target.value];
  $('programStatus').textContent = event.target.value ? 'Editable listing ready. Start BASIC first, then type it in.' : 'Paste your own listing. Include RUN to start it.';
});
programs.querySelector('summary').addEventListener('click', () => { if (!programs.open) keypad.open = false; });
keypad.querySelector('summary').addEventListener('click', () => { if (!keypad.open) programs.open = false; });
for (const input of [screenInput, $('mobileInput')]) input.addEventListener('paste', (event) => {
  const paste = event.clipboardData?.getData('text');
  if (!paste) return;
  event.preventDefault();
  programs.open = true;
  if (paste.length > 6000) { $('programStatus').textContent = 'Listing is too long (6,000-character limit).'; return; }
  $('programText').value = paste;
  $('example').value = '';
  $('programStatus').textContent = 'Pasted into the editor. Press “Type into BASIC” to send at a safe speed.';
});
let transferCancelled = false;
let transferring = false;
const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));
async function waitForKeyboard() {
  for (let tries = 0; tries < 500; tries++) {
    if (transferCancelled) throw new Error('Cancelled');
    if (!paused && board?.booted && board.scancodes.length === 0 && board.ram[0xfc] === board.ram[0xfd]) return;
    await sleep(20);
  }
  throw new Error('Keyboard did not become ready');
}
async function typePaced(text) {
  let line = '';
  for (let i = 0; i < text.length; i++) {
    await waitForKeyboard();
    if (text[i] === '\n' && line && line.length <= 40 && !board.snapshotText().some((row) => row.includes(line))) {
      throw new Error(`Stopped: line was not echoed correctly: ${line}. Correct it on the VGA screen before retrying.`);
    }
    if (!typedChar(text[i])) throw new Error(`Unsupported character at position ${i + 1}`);
    line = text[i] === '\n' ? '' : line + text[i];
    await sleep(text[i] === '\n' ? 480 : 60);
    if (i % 12 === 0) $('programStatus').textContent = `Typing ${i + 1} / ${text.length} characters…`;
  }
  await waitForKeyboard();
}
async function transfer(label, action) {
  if (transferring || !board?.booted) { $('programStatus').textContent = 'Wait for the ROM to finish booting.'; return; }
  transferring = true;
  transferCancelled = false;
  $('startBasic').disabled = true;
  $('sendProgram').disabled = true;
  $('cancelProgram').disabled = false;
  try {
    await action();
    $('programStatus').textContent = `${label} sent. Continue on the VGA screen.`;
  } catch (error) {
    $('programStatus').textContent = error.message;
  } finally {
    transferring = false;
    $('startBasic').disabled = false;
    $('sendProgram').disabled = false;
    $('cancelProgram').disabled = true;
  }
}
async function startBasic() {
  if (paused) { paused = false; $('pause').textContent = 'Pause'; }
  // F4 only works from the ROM monitor (inside BASIC it types "{"), and only
  // F4 resets the ROM's keyboard buffer. Reset the machine first so BASIC
  // always starts clean; starting BASIC clears the program anyway.
  $('programStatus').textContent = 'Resetting the board…';
  board.reset();
  basicEnters = -1;
  for (let tries = 0; !board.booted; tries++) {
    if (transferCancelled) throw new Error('Cancelled');
    if (tries > 400) throw new Error('The ROM did not finish booting after reset.');
    await sleep(25);
  }
  await sleep(300);
  scan('F4');
  $('programStatus').textContent = 'Starting BASIC with F4…';
  await sleep(550);
  await typePaced('\n\n');
  if (!board.snapshotText().some((row) => row.includes('OSI 6502 BASIC'))) throw new Error('BASIC did not reach its OK prompt. Try F4 again.');
}
$('startBasic').addEventListener('click', () => transfer('BASIC startup', startBasic));
$('sendProgram').addEventListener('click', () => {
  const normalized = $('programText').value.replace(/\r\n?/g, '\n');
  const listing = normalized.endsWith('\n') ? normalized : normalized + '\n';
  if (!normalized.trim()) { $('programStatus').textContent = 'Enter or choose a BASIC listing first.'; return; }
  if (basicEnters < 2 && !/^NEW\n/i.test(listing)) { $('programStatus').textContent = 'Start BASIC with F4, then press Enter twice for its defaults first.'; return; }
  const invalid = [...listing].find((char) => !characterCode(char));
  if (invalid) { $('programStatus').textContent = `Unsupported character: ${JSON.stringify(invalid)}. Use plain ASCII BASIC.`; return; }
  transfer('Listing', async () => {
    if (paused) { paused = false; $('pause').textContent = 'Pause'; }
    // The ROM's keyboard buffer only resets on F4: after ~128 typed characters
    // in one BASIC session, input overwrites ROM variables and corrupts lines.
    // A listing that starts with NEW clears the program anyway, so restart
    // BASIC first to give it a fresh buffer.
    if (/^NEW\n/i.test(listing)) await startBasic();
    await typePaced(listing);
  });
});
$('cancelProgram').addEventListener('click', () => { transferCancelled = true; });
function isViewerActive() { return document.fullscreenElement === consoleEl || consoleEl.classList.contains('viewer-fullscreen'); }
document.addEventListener('keydown', (event) => {
  if (event.target === $('mobileInput') || event.target === $('romFile') || event.target.closest?.('#programs')) return;
  if ((event.code === 'Enter' || event.code === 'Space') && event.target.closest?.('button,summary')) return;
  if (document.activeElement !== screenInput && document.activeElement !== canvas && !isViewerActive()) return;
  if (event.isComposing) return;
  if (event.code === 'Escape' && consoleEl.classList.contains('viewer-fullscreen')) {
    exitViewer(); event.preventDefault(); return;
  }
  if (event.metaKey || event.altKey || event.ctrlKey) return;
  if (scan(event.code, false)) event.preventDefault();
});
document.addEventListener('keyup', (event) => {
  if (event.target === $('mobileInput') || event.target === $('romFile') || event.target.closest?.('#programs')) return;
  if (document.activeElement !== screenInput && document.activeElement !== canvas && !isViewerActive()) return;
  if (event.code === 'ShiftLeft' || event.code === 'ShiftRight') scan(event.code, true);
});
for (const input of [screenInput, $('mobileInput')]) input.addEventListener('keydown', (event) => {
  if (event.key === 'Enter' || event.key === 'Backspace') {
    if (event.code === 'Unidentified' || event.target === $('mobileInput')) {
      scan(event.key); event.preventDefault();
    }
  }
});
document.querySelectorAll('[data-key]').forEach((button) => {
  if (!button.hasAttribute('data-repeat')) {
    button.addEventListener('click', () => scan(button.dataset.key));
    return;
  }
  let delay, interval;
  const stop = () => { clearTimeout(delay); clearInterval(interval); };
  button.addEventListener('pointerdown', (event) => {
    button.setPointerCapture(event.pointerId);
    scan(button.dataset.key);
    delay = setTimeout(() => {
      interval = setInterval(() => {
        if (board?.scancodes.length < 12) scan(button.dataset.key);
      }, 160);
    }, 450);
  });
  button.addEventListener('pointerup', stop);
  button.addEventListener('pointercancel', stop);
  button.addEventListener('click', (event) => {
    if (event.detail === 0) scan(button.dataset.key); // keyboard activation
  });
});
$('reset').addEventListener('click', () => { transferCancelled = true; board?.reset(); basicEnters = -1; });
$('pause').addEventListener('click', () => {
  paused = !paused; $('pause').textContent = paused ? 'Resume' : 'Pause'; stats();
});
$('step').addEventListener('click', () => {
  if (!board) return;
  paused = true; $('pause').textContent = 'Resume'; board.step(); render(); stats();
});
function syncViewerViewport() {
  if (!consoleEl.classList.contains('viewer-fullscreen')) return;
  const viewport = window.visualViewport;
  consoleEl.style.setProperty('--viewer-top', `${viewport?.offsetTop || 0}px`);
  consoleEl.style.setProperty('--viewer-left', `${viewport?.offsetLeft || 0}px`);
  consoleEl.style.setProperty('--viewer-width', `${viewport?.width || innerWidth}px`);
  consoleEl.style.setProperty('--viewer-height', `${viewport?.height || innerHeight}px`);
}
function updateViewerButton() {
  $('fullscreen').textContent = isViewerActive() ? '⛶ Exit fullscreen' : '⛶ Fullscreen';
  $('fitScreen').textContent = consoleEl.classList.contains('fit-view') ? 'Zoom text' : 'Show full VGA';
}
async function enterViewer() {
  // iOS Safari does not reliably fullscreen arbitrary elements or keep its
  // keyboard usable there. The fixed viewport fallback also covers rejections.
  const ios = /iPad|iPhone|iPod/.test(navigator.userAgent) ||
    (navigator.platform === 'MacIntel' && navigator.maxTouchPoints > 1);
  if (!ios && typeof consoleEl.requestFullscreen === 'function') {
    try {
      await consoleEl.requestFullscreen();
      if (document.fullscreenElement === consoleEl) { updateViewerButton(); return; }
    } catch (error) { console.info('Using in-page fullscreen view', error); }
  }
  consoleEl.classList.add('viewer-fullscreen');
  document.body.classList.add('viewer-open');
  syncViewerViewport();
  updateViewerButton();
}
async function exitViewer() {
  if (document.fullscreenElement === consoleEl) await document.exitFullscreen();
  consoleEl.classList.remove('viewer-fullscreen');
  document.body.classList.remove('viewer-open');
  updateViewerButton();
}
$('fullscreen').addEventListener('click', () => {
  if (isViewerActive()) exitViewer(); else enterViewer();
});
$('fitScreen').addEventListener('click', () => {
  consoleEl.classList.toggle('fit-view');
  updateViewerButton();
});
$('screenEsc').addEventListener('click', () => { scan('Escape'); });
document.addEventListener('fullscreenchange', updateViewerButton);
window.visualViewport?.addEventListener('resize', syncViewerViewport);
window.visualViewport?.addEventListener('scroll', syncViewerViewport);
$('romFile').addEventListener('change', async (event) => {
  const file = event.target.files?.[0];
  if (!file) return;
  if (file.size !== 32768) { alert('Select a 32 KiB ABN6502 ROM image.'); event.target.value = ''; return; }
  const rom = new Uint8Array(await file.arrayBuffer());
  install(rom, board.chars);
});
load();
requestAnimationFrame(animate);
