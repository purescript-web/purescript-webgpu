export const setPipelineImpl = (computePassEncoder, pipeline) =>
  computePassEncoder.setPipeline(pipeline);
export const dispatchWorkgroupsImpl = (computePassEncoder, size) =>
  computePassEncoder.dispatchWorkgroups(size);
export const dispatchWorkgroupsXImpl = (computePassEncoder, size) =>
  computePassEncoder.dispatchWorkgroups(size);
export const dispatchWorkgroupsXYImpl = (computePassEncoder, x, y) =>
  computePassEncoder.dispatchWorkgroups(x, y);
export const dispatchWorkgroupsXYZImpl = (computePassEncoder, x, y, z) =>
  computePassEncoder.dispatchWorkgroups(x, y, z);
export const dispatchWorkgroupsIndirectImpl = (
  computePassEncoder,
  indirectBuffer,
  indirectOffset
) =>
  computePassEncoder.dispatchWorkgroupsIndirect(indirectBuffer, indirectOffset);
export const endImpl = computePassEncoder => computePassEncoder.end();
export const setBindGroupImpl = (computePassEncoder, index, bindGroup) =>
  computePassEncoder.setBindGroup(index, bindGroup);
export const setBindGroupWithDynamicOffsetsImpl = (
  computePassEncoder,
  index,
  bindGroup,
  dynamicOffsets
) => computePassEncoder.setBindGroup(index, bindGroup, dynamicOffsets);
export const setBindGroupWithDyanmicOffsetBoundsImpl = (
  computePassEncoder,
  index,
  bindGroup,
  data,
  dynamicOffsetsDataStart,
  dynamicOffsetsDataLength
) =>
  computePassEncoder.setBindGroup(
    index,
    bindGroup,
    data,
    dynamicOffsetsDataStart,
    dynamicOffsetsDataLength
  );
export const pushDebugGroupImpl = (computePassEncoder, groupLabel) =>
  computePassEncoder.pushDebugGroup(groupLabel);
export const popDebugGroupImpl = computePassEncoder =>
  computePassEncoder.popDebugGroup();
export const insertDebugMarkerImpl = (computePassEncoder, markerLabel) =>
  computePassEncoder.insertDebugMarker(markerLabel);
