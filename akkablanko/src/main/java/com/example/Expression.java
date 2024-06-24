package com.example;

import java.util.Random;

/**
 * Interface for mathematical Expressions
 */
public sealed interface Expression permits Expression.Add, Expression.Mul, Expression.Sub, Expression.Val {
    /**
     * Addition term: left + right
     * @param left
     * @param right
     */
    record Add(Expression left, Expression right) implements Expression{
        public String toString(){
            return "(" + left.toString() + "+" + right.toString() + ")";
        }
        public int eval() {
            return left.eval() + right.eval();
        }
        public int runtime(){
            return 1 + Math.max(left.runtime(), right.runtime());
        }
    }

    /**
     * Multiplication term: left * right
     * @param left
     * @param right
     */
    record Mul(Expression left, Expression right) implements Expression{
        public String toString(){
            return "(" + left.toString() + "*" + right.toString() + ")";
        }
        public int eval() {
            return left.eval() * right.eval();
        }

        public int runtime(){
            return 1 + Math.max(left.runtime(), right.runtime());
        }
    }

    /**
     * Subtraction term: left - right
     * @param left
     * @param right
     */
    record Sub(Expression left, Expression right) implements Expression{
        public int runtime(){
            return 1 + Math.max(left.runtime(), right.runtime());
        }
        public String toString(){
            return "(" + left.toString() + "-" + right.toString() + ")";
        }
        public int eval() {
            return left.eval() - right.eval();
        }
    }

    /**
     * Encapsulates a single integer
     * @param inner
     */
    record Val(int inner) implements Expression{
        public int runtime(){
            return 0;
        }
        public String toString(){
            return String.valueOf(inner);
        }public int eval() {
            return inner;
        }
    }

    /**
     * @return A string representation of the expression
     */
    String toString();

    /**
     * @return The computed result of the expression
     */
    int eval();

    /**
     * @return The estimated maximal runtime
     */
    int runtime();

    /**
     * Build a random expression of max depth `max_depth` and has integer values up to `max_val`.
     * @param max_depth
     * @param max_val
     * @return A random expression
     */
    static Expression generateExpression(int max_depth, int max_val){
        var x = new Random();
        int choice = x.nextInt(4);
        if (max_depth <= 1) {
            choice = 4;
        }

        return switch (choice) {
            case 0 -> new Add(
                    generateExpression(max_depth-1, max_val),
                    generateExpression(max_depth-1, max_val)
            );
            case 1 -> new Mul(
                    generateExpression(max_depth-1, max_val),
                    generateExpression(max_depth-1, max_val)
            );
            case 2 -> new Sub(
                    generateExpression(max_depth-1, max_val),
                    generateExpression(max_depth-1, max_val)
            );
            default -> new Val(x.nextInt(max_val+1)+1);
        };
    }
}
