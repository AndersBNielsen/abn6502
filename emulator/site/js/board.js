import { CPU6502 } from './cpu6502.js';

// ABN6502 r2, configured for the firmware's $0800 video jumper setting.
// The keyboard connector is PS/2 over the 6522 shift register on hardware.
// Here browser key events enter the firmware's scan-code staging buffer; the
// original ROM still performs scan-code translation and command handling.
export class ABN6502 {
  constructor(rom, chars) {
    if (rom.length !== 0x8000 || chars.length !== 0x800) throw new Error('Unexpected ROM size');
    this.rom = rom;
    this.chars = chars;
    this.ram = new Uint8Array(0x6000);
    this.via = new Uint8Array(16);
    this.control = 0;
    this.clockHz = 2000000;
    this.cpu = new CPU6502(this);
    this.t1Counter = 0;
    this.t1Latch = 0;
    this.t1Running = false;
    this.lastCycle = 0;
    this.scancodes = [];
    this.booted = false;
    this.reset();
  }
  reset() {
    this.ram.fill(0);
    this.via.fill(0);
    this.control = 0;
    this.scancodes.length = 0;
    this.t1Counter = 0;
    this.t1Running = false;
    this.lastCycle = this.cpu.cycles;
    this.cpu.reset();
    this.booted = false;
  }
  irq() {
    const active = this.via[13] & this.via[14] & 0x7f;
    this.cpu.irqLine = !!active;
  }
  tick() {
    const elapsed = this.cpu.cycles - this.lastCycle;
    this.lastCycle = this.cpu.cycles;
    if (this.t1Running && elapsed) {
      this.t1Counter -= elapsed;
      if (this.t1Counter < 0) {
        this.via[13] |= 0x40;
        if (this.via[11] & 0x40) {
          const period = this.t1Latch + 2;
          this.t1Counter = ((this.t1Counter % period) + period) % period;
        } else this.t1Running = false;
        this.irq();
      }
    }
  }
  step() {
    this.tick();
    this.cpu.step();
    if (!this.booted && this.ram[0xe6] === 0x4d && this.ram[0xe7] === 0x08) this.booted = true;
    this.feedKeyboard();
  }
  run(cycles) {
    const end = this.cpu.cycles + cycles;
    while (this.cpu.cycles < end && !this.cpu.jammed) this.step();
  }
  read(addr) {
    if (addr < 0x6000) return this.ram[addr];
    if ((addr & 0xfff0) === 0x6000) return this.readVia(addr & 15);
    if (addr === 0x7000) return this.control;
    if (addr >= 0x8000) return this.rom[addr - 0x8000];
    return 0xff;
  }
  write(addr, value) {
    if (addr < 0x6000) this.ram[addr] = value;
    else if ((addr & 0xfff0) === 0x6000) this.writeVia(addr & 15, value);
    else if (addr === 0x7000) this.control = value;
  }
  peek(addr) {
    if (addr < 0x6000) return this.ram[addr];
    if ((addr & 0xfff0) === 0x6000) return this.via[addr & 15];
    if (addr === 0x7000) return this.control;
    if (addr >= 0x8000) return this.rom[addr - 0x8000];
    return 0xff;
  }
  readVia(reg) {
    switch (reg) {
      case 0: return this.via[0] | (~this.via[2] & 0xff); // PB inputs high, RF absent
      case 1: case 15: return this.via[1] | (~this.via[3] & 0xff);
      case 4: this.via[13] &= ~0x40; this.irq(); return this.t1Counter & 0xff;
      case 5: return (this.t1Counter >> 8) & 0xff;
      case 13: return this.via[13] | (this.cpu.irqLine ? 0x80 : 0);
      case 14: return this.via[14] | 0x80;
      default: return this.via[reg];
    }
  }
  writeVia(reg, value) {
    this.via[reg] = value & 0xff;
    switch (reg) {
      case 4: case 6: this.t1Latch = (this.t1Latch & 0xff00) | value; break;
      case 5: case 7:
        this.t1Latch = (this.t1Latch & 0xff) | (value << 8);
        if (reg === 5) {
          this.t1Counter = this.t1Latch + 2;
          this.t1Running = true;
          this.via[13] &= ~0x40;
          this.irq();
        }
        break;
      case 13: this.via[13] &= ~value; this.irq(); break;
      case 14:
        this.via[14] = value & 0x80 ? (this.via[14] | value) & 0x7f : (this.via[14] & ~value) & 0x7f;
        this.irq();
        break;
    }
  }
  // The ROM's scbuf stores wire-order bytes and reverses each during parsing.
  static reverse(value) {
    let reversed = 0;
    for (let i = 0; i < 8; i++) { reversed = (reversed << 1) | (value & 1); value >>= 1; }
    return reversed;
  }
  keyScan(...codes) { this.scancodes.push(...codes); }
  feedKeyboard() {
    if (!this.booted || !this.scancodes.length) return;
    const write = this.ram[0xfc], read = this.ram[0xfd];
    if (write >= 7 || write - read >= 5) return;
    this.ram[0x280 + write] = ABN6502.reverse(this.scancodes.shift());
    this.ram[0xfc] = write + 1;
  }
  snapshotText() {
    const rows = [];
    for (let row = 0; row < 32; row++) {
      let line = '';
      for (let col = 0; col < 64; col++) {
        const code = this.ram[0x800 + row * 64 + col];
        line += code >= 32 && code < 127 ? String.fromCharCode(code) : ' ';
      }
      rows.push(line);
    }
    return rows;
  }
}
