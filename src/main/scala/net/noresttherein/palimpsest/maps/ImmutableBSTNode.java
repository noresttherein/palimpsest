package net.noresttherein.palimpsest.maps;

/** Base class for binary search tree nodes. Implemented in Java to have the left and right nodes exposed as fields
 *  rather than getters.
 */
class ImmutableBSTNode<T extends ImmutableBSTNode<T>> {
    T left;
    T right;
//    int size = 1;

    ImmutableBSTNode() {}

    ImmutableBSTNode(T left, T right) {
        this.left = left;
        this.right = right;
    }
}
