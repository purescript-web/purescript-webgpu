export const mapRead = window.GPUBufferUsage
  ? window.GPUBufferUsage.MAP_READ
  : 0x0000;
export const mapWrite = window.GPUBufferUsage
  ? window.GPUBufferUsage.MAP_WRITE
  : 0x0000;
export const copySrc = window.GPUBufferUsage
  ? window.GPUBufferUsage.COPY_SRC
  : 0x0000;
export const copyDst = window.GPUBufferUsage
  ? window.GPUBufferUsage.COPY_DST
  : 0x0000;
export const index = window.GPUBufferUsage
  ? window.GPUBufferUsage.INDEX
  : 0x0000;
export const vertex = window.GPUBufferUsage
  ? window.GPUBufferUsage.VERTEX
  : 0x0000;
export const uniform = window.GPUBufferUsage
  ? window.GPUBufferUsage.UNIFORM
  : 0x0000;
export const storage = window.GPUBufferUsage
  ? window.GPUBufferUsage.STORAGE
  : 0x0000;
export const indirect = window.GPUBufferUsage
  ? window.GPUBufferUsage.INDIRECT
  : 0x0000;
export const queryResolve = window.GPUBufferUsage
  ? window.GPUBufferUsage.QUERY_RESOLVE
  : 0x0000;
