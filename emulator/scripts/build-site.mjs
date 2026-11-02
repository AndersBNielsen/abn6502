import fs from 'node:fs/promises';
import crypto from 'node:crypto';
import path from 'node:path';
import { execFileSync } from 'node:child_process';
const root = path.resolve(import.meta.dirname, '..');
const site = path.join(root, 'site');
const dist = path.join(root, 'dist');
for (const [name, size, digest] of [
  ['abn6502-r2.bin', 32768, 'bea5a8b7a49b486e957e258ca880146261d9389c228c9cea8cbe650b55a73720'],
  ['abn6502-r2-chars.bin', 2048, 'bf9b4069145162fce4604ac26cc730c93471d193bec13b0f2512b85c101e152b']
]) {
  const data = await fs.readFile(path.join(site, 'roms', name));
  if (data.length !== size || crypto.createHash('sha256').update(data).digest('hex') !== digest) throw new Error(`${name} checksum failed`);
}
await fs.rm(dist, { recursive: true, force: true });
await fs.cp(site, dist, { recursive: true });
await fs.copyFile(path.join(root, 'LICENSE.md'), path.join(dist, 'LICENSE.md'));
const gitPrefix = execFileSync('git', ['rev-parse', '--show-prefix'], { cwd: root, encoding: 'utf8' }).trim().replace(/\/$/, '');
const sourceTree = gitPrefix ? `HEAD:${gitPrefix}` : 'HEAD';
execFileSync('git', ['archive', '--format=zip', `--output=${path.join(dist, 'abn6502-emulator-source.zip')}`, '--prefix=abn6502-emulator/', sourceTree], { cwd: root });
console.log(`Built ${dist}`);
