package com.example;

import akka.actor.typed.ActorRef;
import akka.actor.typed.Behavior;
import akka.actor.typed.javadsl.AbstractBehavior;
import akka.actor.typed.javadsl.ActorContext;
import akka.actor.typed.javadsl.Behaviors;
import akka.actor.typed.javadsl.Receive;

public class Formatter extends AbstractBehavior<Formatter.Message> {

    private ActorRef<PrintAndEvaluator.Message> replyToMain; // Holds reference to printAndEvaluator (pEA)
    private String leftString; // Saves result for left part of the expression
    private String rightString; // Saves result for right part of the expression
    private String operator; // Saves the used operator in the middle
    private ActorRef<Formatter.Message> parent; // Saves the "creator" actor ref for when creating another actor for subexpressions
    private boolean leftReceived;
    private boolean rightReceived;
    private Boolean parentIsLeft; // Saves position of parent that created child

    private Formatter(ActorContext<Message> context) {
        super(context);
    }

    public static Behavior<Message> create() {
        return Behaviors.setup(Formatter::new);
    }

    public interface Message {}

    public record Init(Expression expression, ActorRef<PrintAndEvaluator.Message> mainActor) implements Message {}
    public record AddExp(Expression expression, ActorRef<Formatter.Message> parent, Boolean isParentLeft) implements Message {}
    public record MulExp(Expression expression, ActorRef<Formatter.Message> parent, Boolean isParentLeft) implements Message {}
    public record SubExp(Expression expression, ActorRef<Formatter.Message> parent, Boolean isParentLeft) implements Message {}
    public record ValExp(Expression expression, ActorRef<Formatter.Message> parent, Boolean isLeft, Boolean isParentLeft) implements Message {}
    public record ReceiveResult(String result, Boolean isLeft, Boolean isParentLeft) implements Message {}

    @Override
    public Receive<Message> createReceive() {
        return newReceiveBuilder()
                .onMessage(Init.class, this::onInit)
                .onMessage(AddExp.class, this::onAddExp)
                .onMessage(MulExp.class, this::onMulExp)
                .onMessage(SubExp.class, this::onSubExp)
                .onMessage(ValExp.class, this::onValExp)
                .onMessage(ReceiveResult.class, this::onReceiveResult)
                .build();
    }

    private Behavior<Message> onInit(Init msg) {
        replyToMain = msg.mainActor;
        Expression expr = msg.expression;

        //Checks for most outer type of expression
        if (expr instanceof Expression.Val) {
            String result = String.valueOf(((Expression.Val) expr).inner());
            replyToMain.tell(new PrintAndEvaluator.FormattedResult(result)); // If its only one val, it's immediately returned to pEA
        } else {
            ActorRef<Formatter.Message> self = getContext().getSelf();
            if (expr instanceof Expression.Add) {
                self.tell(new AddExp(expr, self, null)); // Sends the expression, itself as the parent and null, since it's not a subexpression
            } else if (expr instanceof Expression.Mul) {
                self.tell(new MulExp(expr, self, null));
            } else if (expr instanceof Expression.Sub) {
                self.tell(new SubExp(expr, self, null));
            }
        }
        return this;
    }

    private Behavior<Message> onAddExp(AddExp msg) {
        operator = "+";
        parent = msg.parent;
        parentIsLeft = msg.isParentLeft; // Saving position of parent
        processSubExpressions(((Expression.Add) msg.expression).left(), ((Expression.Add) msg.expression).right());
        return this;
    }

    private Behavior<Message> onMulExp(MulExp msg) {
        operator = "*";
        parent = msg.parent;
        parentIsLeft = msg.isParentLeft;
        processSubExpressions(((Expression.Mul) msg.expression).left(), ((Expression.Mul) msg.expression).right());
        return this;
    }

    private Behavior<Message> onSubExp(SubExp msg) {
        operator = "-";
        parent = msg.parent;
        parentIsLeft = msg.isParentLeft;
        processSubExpressions(((Expression.Sub) msg.expression).left(), ((Expression.Sub) msg.expression).right());
        return this;
    }

    private Behavior<Message> onValExp(ValExp msg) {
        String result = String.valueOf(((Expression.Val) msg.expression).inner());
        msg.parent.tell(new ReceiveResult(result, msg.isLeft, msg.isParentLeft));
        return this;
    }

    private Behavior<Message> onReceiveResult(ReceiveResult msg) {
        if (Boolean.TRUE.equals(msg.isLeft)) { // Safeguard against null, checks whether result is left or right child
            leftString = msg.result;
            leftReceived = true;
            getContext().getLog().info("Left result received: {}", leftString);
        } else if (Boolean.FALSE.equals(msg.isLeft)){
            rightString = msg.result;
            rightReceived = true;
            getContext().getLog().info("Right result received: {}", rightString);
        }

        if (leftReceived && rightReceived) { // If both are true, subexpression is finished
            String combinedResult = "(" + leftString +  operator + rightString + ")";
            getContext().getLog().info("Combined result: {}", combinedResult);
            if (parentIsLeft != null) { // If parent is null, the current actor is the original formattor
                parent.tell(new ReceiveResult(combinedResult, parentIsLeft, null)); // Send combined result back to parent with the correct position of the parent
            } else {
                getContext().getLog().info("Sending result to pea");
                replyToMain.tell(new PrintAndEvaluator.FormattedResult(combinedResult));
            }
        }
        return this;
    }

    private void processSubExpressions(Expression left, Expression right) {
        ActorRef<Message> leftChild = getContext().spawnAnonymous(Formatter.create());
        ActorRef<Message> rightChild = getContext().spawnAnonymous(Formatter.create());

        if (left instanceof Expression.Val) {
            leftChild.tell(new ValExp(left, getContext().getSelf(), true, true)); // Include parentIsLeft
        } else if (left instanceof Expression.Add) {
            leftChild.tell(new AddExp(left, getContext().getSelf(), true)); // Include parentIsLeft
        } else if (left instanceof Expression.Mul) {
            leftChild.tell(new MulExp(left, getContext().getSelf(), true)); // Include parentIsLeft
        } else if (left instanceof Expression.Sub) {
            leftChild.tell(new SubExp(left, getContext().getSelf(), true)); // Include parentIsLeft
        }

        if (right instanceof Expression.Val) {
            rightChild.tell(new ValExp(right, getContext().getSelf(), false, false)); // Include parentIsLeft
        } else if (right instanceof Expression.Add) {
            rightChild.tell(new AddExp(right, getContext().getSelf(), false)); // Include parentIsLeft
        } else if (right instanceof Expression.Mul) {
            rightChild.tell(new MulExp(right, getContext().getSelf(), false)); // Include parentIsLeft
        } else if (right instanceof Expression.Sub) {
            rightChild.tell(new SubExp(right, getContext().getSelf(), false)); // Include parentIsLeft
        }
    }
}