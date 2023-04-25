export const beginRenderPassImpl = (commandEncoder, descriptor) =>
  commandEncoder.beginRenderPass(descriptor);
export const beginComputePassImpl = (commandEncoder, descriptor) =>
  commandEncoder.beginComputePass(descriptor);
export const copyBufferToBufferImpl = (
  commandEncoder,
  source,
  sourceOffset,
  destination,
  destinationOffset,
  size
) =>
  commandEncoder.copyBufferToBuffer(
    source,
    sourceOffset,
    destination,
    destinationOffset,
    size
  );
export const copyBufferToTextureImpl = (
  commandEncoder,
  source,
  destination,
  copySize
) => commandEncoder.copyBufferToTexture(source, destination, copySize);
export const copyTextureToBufferImpl = (
  commandEncoder,
  source,
  destination,
  copySize
) => commandEncoder.copyTextureToBuffer(source, destination, copySize);
export const copyTextureToTextureImpl = (
  commandEncoder,
  source,
  destination,
  copySize
) => commandEncoder.copyTextureToTexture(source, destination, copySize);
export const clearBufferImpl = (commandEncoder, buffer) =>
  commandEncoder.clearBuffer(buffer);
export const clearBufferWithOffsetImpl = (commandEncoder, buffer, offset) =>
  commandEncoder.clearBuffer(buffer, offset);
export const clearBufferWithSizeImpl = (commandEncoder, buffer, size) =>
  commandEncoder.clearBuffer(buffer, undefined, size);
export const clearBufferWithOffsetAndSizeImpl = (
  commandEncoder,
  buffer,
  offset,
  size
) => commandEncoder.clearBuffer(buffer, offset, size);
export const writeTimestampImpl = (commandEncoder, querySet, queryIndex) =>
  commandEncoder.writeTimestamp(querySet, queryIndex);
export const resolveQuerySetImpl = (
  commandEncoder,
  querySet,
  firstQuery,
  queryCount,
  destination,
  destinationOffset
) =>
  commandEncoder.resolveQuerySet(
    querySet,
    firstQuery,
    queryCount,
    destination,
    destinationOffset
  );
export const finishImpl = commandEncoder => commandEncoder.finish();
export const pushDebugGroupImpl = (commandEncoder, groupLabel) =>
  commandEncoder.pushDebugGroup(groupLabel);
export const popDebugGroupImpl = commandEncoder =>
  commandEncoder.popDebugGroup();
export const insertDebugMarkerImpl = (commandEncoder, markerLabel) =>
  commandEncoder.insertDebugMarker(markerLabel);
