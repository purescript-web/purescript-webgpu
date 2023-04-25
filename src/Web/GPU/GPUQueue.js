export const submitImpl = (queue, commandBuffers) =>
  queue.submit(commandBuffers);
export const onSubmittedWorkDoneImpl = queue => queue.onSubmittedWorkDone();
export const writeBufferImpl = (queue, buffer, bufferOffset, data) =>
  queue.writeBuffer(buffer, bufferOffset, data);
export const writeBufferWithOffsetImpl = (
  queue,
  buffer,
  bufferOffset,
  data,
  dataOffset
) => queue.writeBuffer(buffer, bufferOffset, data, dataOffset);
export const writeBufferWithSizeImpl = (
  queue,
  buffer,
  bufferOffset,
  data,
  size
) => queue.writeBuffer(buffer, bufferOffset, data, undefined, size);
export const writeBufferWithOffsetAndSizeImpl = (
  queue,
  buffer,
  bufferOffset,
  data,
  dataOffset,
  size
) => queue.writeBuffer(buffer, bufferOffset, data, dataOffset, size);
export const writeTextureImpl = (queue, destination, data, dataLayout, size) =>
  queue.writeTexture(destination, data, dataLayout, size);
export const copyExternalImageToTextureImpl = (
  queue,
  source,
  destination,
  copySize
) => queue.copyExternalImageToTexture(source, destination, copySize);
