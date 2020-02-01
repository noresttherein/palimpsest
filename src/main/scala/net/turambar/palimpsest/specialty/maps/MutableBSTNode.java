package net.turambar.palimpsest.specialty.maps;

/** Base class for binary search tree nodes. Implemented in Java to have the left and right nodes exposed as fields
 *  rather than getters.
 */
class MutableBSTNode<T extends MutableBSTNode<T>> {
    T left;
    T right;

    MutableBSTNode() {}

    MutableBSTNode(T left, T right) {
        this.left = left;
        this.right = right;
    }
/*
*/
}
