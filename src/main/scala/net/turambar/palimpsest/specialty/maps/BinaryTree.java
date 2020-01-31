package net.turambar.palimpsest.specialty.maps;

/** Base class for binary search tree nodes. Implemented in Java to have the left and right nodes exposed as fields
 *  rather than getters.
 */
class BinaryTree<T extends BinaryTree<T>> {
    T left;
    T right;

    BinaryTree() {}

    BinaryTree(T left, T right) {
        this.left = left;
        this.right = right;
    }

/*
    int size() {
        int size = 1;
        if (left != null)
            size += left.size();
        if (right != null)
            size += right.size();
        return size;
    }
*/

    int depth() {
        int depth = 0;
        if (left != null)
            depth = left.depth();
        if (right != null) {
            int d = right.depth();
            if (d > depth)
                depth = d;
        }
        return depth + 1;
    }



    static<T extends BinaryTree<T>> int size(T tree) {
        if (tree == null)
            return 0;
        return size(tree.left) + 1 + size(tree.right);
    }

    static<T extends BinaryTree<T>> T node(T tree, int index) {
        while (tree != null) {
            T left = tree.left;
            int lsize = 0;
            if (left != null)
                lsize = size(left); //.size();
            if (index < lsize)
                tree = left;
            else if (index > lsize) {
                index -= lsize + 1;
                tree = tree.right;
            } else
                return tree;
        }
        return null;
    }

    static<T extends BinaryTree<T>> T min(T tree) {
        if (tree == null)
            return null;
        T left = tree.left;
        while (left != null) {
            tree = left;
            left = left.left;
        }
        return tree;
    }

    static<T extends BinaryTree<T>> T max(T tree) {
        if (tree == null)
            return null;
        T right = tree.right;
        while (right != null) {
            tree = right;
            right = right.right;
        }
        return tree;
    }


}
