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

    T rotateRight() {
        T root = left;
        left = root.right;
        root.right = (T) this;
        return root;
    }

    T rotateLeft() {
        T root = right;
        right = root.left;
        root.left = (T) this;
        return root;
    }

    T rotateLeftRight() {
        T root = left.right;
        left.right = root.left;
        root.left = left;
        left = root.right;
        root.right = (T) this;
        return root;
    }

    T rotateRightLeft() {
        T root = right.left;
        right.left = root.right;
        root.right = right;
        right = root.left;
        root.left = (T) this;
        return root;
    }

}
