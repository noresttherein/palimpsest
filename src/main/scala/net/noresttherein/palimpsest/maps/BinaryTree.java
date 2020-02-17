package net.noresttherein.palimpsest.maps;

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

    void replace(T child, T replacement) {
        if (child == left)
            left = replacement;
        else
            right = replacement;
    }



    public int count() {
        int size = 1;
        if (left != null)
            size += left.count();
        if (right != null)
            size += right.count();
        return size;
    }

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


    /** Replaces the given child of this tree with its left child, fixing subtree references to preserve the same
     * infix ordering. This reduces the depth of the child.left subtree and increases the depth of the child.right
     * subtree if child.left is left-heavy.
     * @param child a subtree of this node: either this.left or this.right.
     */
    void rotateRight(T child) {
        T root = child.left;
        child.left = root.right;
        root.right = child;
        if (left == child)
            left = root;
        else
            right = root;
    }

    T rotateRight() {
        T root = left;
        left = root.right;
        root.right = (T) this;
        return root;
    }

    /** Replaces the given child of this tree with its right child, fixing subtree references to preserve the same
     * infix orde`ring. This reduces the depth of the child.right subtree and increases the depth of the child.left
     * subtree if child.right is right-heavy.
     * @param child a subtree of this node: either this.left or this.right.
     */
    void rotateLeft(T child) {
        T root = child.right;
        child.right = root.left;
        root.left = child;
        if (left == child)
            left = root;
        else
            right = root;
    }

    T rotateLeft() {
        T root = right;
        right = root.left;
        root.left = (T) this;
        return root;
    }




    /** Performs two opposing rotations: first a left rotation for node.left, then a right rotation for
     *  the new root (node.left.right). This is equivalent to: <code>rotateRight(rotateLeft(node.left))</code>
     *  but is implemented directly. This operation is used to fix the tree balancing when the path which depth needs
     *  to be shortened runs along node.left.right, that is node is left-heavy but node.left is right-heavy.
     * @return node.left.right
     */
    static<T extends BinaryTree<T>> T rotateLeftRight(T node) {
        T left = node.left;
        T root = left.right;
        left.right = root.left;
        root.left = left;
        node.left = root.right;
        root.right = node;
        return root;
    }

    /** Performs two opposing rotations: first a left rotation for node.left, then a right rotation for
     *  the new root (node.left.right). This is equivalent to: <code>rotateRight(rotateLeft(node.left))</code>
     *  but is implemented directly. This operation is used to fix the tree balancing when the path which depth needs
     *  to be shortened runs along node.left.right, that is node is left-heavy but node.left is right-heavy.
     * @return node.left.right
     */
    static<T extends BinaryTree<T>> T rotateRightLeft(T node) {
        T right = node.right;
        T root = right.left;
        right.left = root.right;
        root.right = right;
        node.right = root.left;
        root.left = node;
        return root;
    }



    static<T extends BinaryTree<T>> int count(T tree) {
        if (tree == null)
            return 0;
        return count(tree.left) + 1 + count(tree.right);
    }

    static<T extends BinaryTree<T>> T node(T tree, int index) {
        while (tree != null) {
            T left = tree.left;
            int lsize = 0;
            if (left != null)
                lsize = count(left); //.size();
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
