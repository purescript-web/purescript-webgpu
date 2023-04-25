export const sizeImpl = buffer => buffer.size;
export const usageImpl = buffer => buffer.usage;
export const mapStateImpl = buffer => buffer.mapState;
export const mapAsyncImpl = (buffer, mode) => buffer.mapAsync(mode);
export const mapAsyncWithOffsetImpl = (buffer, mode, offset) =>
  buffer.mapAsync(mode, offset);
export const mapAsyncWithSizeImpl = (buffer, mode, size) =>
  buffer.mapAsync(mode, undefined, size);
export const mapAsyncWithOffsetAndSizeImpl = (buffer, mode, offset, size) =>
  buffer.mapAsync(mode, offset, size);
export const getMappedRangeImpl = buffer => buffer.getMappedRange();
export const getMappedRangeWithOffsetImpl = (buffer, offset) =>
  buffer.getMappedRange(offset);
export const getMappedRangeWithSizeImpl = (buffer, size) =>
  buffer.getMappedRange(undefined, size);
export const getMappedRangeWithOffsetAndSizeImpl = (buffer, offset, size) =>
  buffer.getMappedRange(offset, size);
export const unmapImpl = buffer => buffer.unmap();
export const destroyImpl = buffer => buffer.destroy();
