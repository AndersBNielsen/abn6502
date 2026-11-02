import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import { ABN6502 } from '../site/js/board.js';
const rom = fs.readFileSync(new URL('../site/roms/abn6502-r2.bin', import.meta.url));
const chars = fs.readFileSync(new URL('../site/roms/abn6502-r2-chars.bin', import.meta.url));
test('real firmware boots and draws its greeting in VRAM', () => {
  const board = new ABN6502(rom, chars);
  board.run(8_000_000);
  assert.equal(board.cpu.jammed, false);
  assert.equal(board.control & 9, 9);
  const text = board.snapshotText().join('\n');
  assert.match(text, /GREETINGS PROFESSOR FALKEN/);
  assert.match(text, /SHALL WE PLAY A GAME/);
});
test('PS/2 scan codes pass through the real ROM keyboard parser', () => {
  const board = new ABN6502(rom, chars);
  board.run(8_000_000);
  board.keyScan(0x1c, 0x32);
  board.run(400_000);
  assert.match(board.snapshotText().join('\n'), /ab/);
  assert.equal(board.ram[0xf8], board.ram[0xf9]);
});
test('the ROM monitor starts on cleared RAM and Page Down reveals active zero page', () => {
  const board = new ABN6502(rom, chars);
  board.run(8_000_000);
  assert.equal(board.ram.slice(0, 200).every((value) => value === 0), true);
  assert.equal(board.ram.slice(0xd8, 0x100).some((value) => value !== 0), true);
  board.keyScan(0x05); // F1
  board.run(500_000);
  assert.match(board.snapshotText().join('\n'), /\$0000: 00 00 00 00/);
  board.keyScan(0xe0, 0x7a); // PS/2 Page Down
  board.run(500_000);
  assert.match(board.snapshotText().join('\n'), /\$00E0: (?!00 00 00 00)/);
});
test('F4 opens the bundled BASIC ROM and two Enters accept its defaults', () => {
  const board = new ABN6502(rom, chars);
  board.run(8_000_000);
  board.keyScan(0x0c); // F4
  board.run(2_000_000);
  assert.match(board.snapshotText().join('\n'), /MEMORY SIZE\?/);
  board.keyScan(0x5a); // Enter
  board.run(2_000_000);
  assert.match(board.snapshotText().join('\n'), /TERMINAL WIDTH\?/);
  board.keyScan(0x5a);
  board.run(2_000_000);
  assert.match(board.snapshotText().join('\n'), /OSI 6502 BASIC VERSION 1\.0 REV 3\.2/);
});
