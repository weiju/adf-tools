package org.dmpp.adf

/**
 * The logical layer builds on the abstractions in the physical layer.
 * In this layer, the basic building blocks are, well, blocks. Logical
 * blocks are typed and have specific data according to their type.
 * Blocks only preserve their own integrity, they are not aware whether
 * they are contained in a directory or a logical volume.
 * The containing LogicalVolume instance knows the relationships of the
 * blocks to preserve their integrity. For example, a client would be
 * able to change the name or other attribute of a block and the block
 * will update its checksum. But: it will not update its lastModifiedTime
 * attribute since this is a global concept which is handled by either
 * the containing directory or the logical volume.
 */
package object logical { }
