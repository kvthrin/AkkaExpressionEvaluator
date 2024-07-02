package com.example;

import akka.actor.typed.ActorRef;
import akka.actor.typed.Behavior;
import akka.actor.typed.javadsl.*;

import java.time.Duration;
import java.util.Objects;

public class Evaluator extends AbstractBehavior<Evaluator.Message> {

    private final TimerScheduler<Message> timers;
    private ActorRef<PrintAndEvaluator.Message> replyToMain; // Holds reference to printAndEvaluator (pEA)
    private int leftValue; // Saves result for left part of the expression
    private int rightValue; // Saves result for right part of the expression
    private String operator; // Saves the used operator in the middle
    private ActorRef<Evaluator.Message> parent; // Saves the "creator" actor ref for when creating another actor for subexpressions
    private boolean leftReceived;
    private boolean rightReceived;
    private Boolean parentIsLeft; // Saves position of parent that created child

    private Evaluator(ActorContext<Message> context, TimerScheduler<Message> timers) {
        super(context);
        this.timers = timers;
    }

    public static Behavior<Message> create() {
        return Behaviors.setup(context -> Behaviors.withTimers(timers -> new Evaluator(context, timers)));
    }

    public interface Message {}

    public record Init(Expression expression, ActorRef<PrintAndEvaluator.Message> mainActor) implements Message {}
    public record AddExp(Expression expression, ActorRef<Evaluator.Message> parent, Boolean isParentLeft) implements Message {}
    public record MulExp(Expression expression, ActorRef<Evaluator.Message> parent, Boolean isParentLeft) implements Message {}
    public record SubExp(Expression expression, ActorRef<Evaluator.Message> parent, Boolean isParentLeft) implements Message {}
    public record ValExp(Expression expression, ActorRef<Evaluator.Message> parent, Boolean isLeft, Boolean isParentLeft) implements Message {}
    public record ReceiveResult(int result, Boolean isLeft, Boolean isParentLeft) implements Message {}
    public record CalculateResult() implements Message {}

    @Override
    public Receive<Message> createReceive() {
        return newReceiveBuilder()
                .onMessage(Init.class, this::onInit)
                .onMessage(AddExp.class, this::onAddExp)
                .onMessage(MulExp.class, this::onMulExp)
                .onMessage(SubExp.class, this::onSubExp)
                .onMessage(ValExp.class, this::onValExp)
                .onMessage(ReceiveResult.class, this::onReceiveResult)
                .onMessage(CalculateResult.class, this::onCalculateResult)
                .build();
    }

    private Behavior<Message> onInit(Init msg) {
        replyToMain = msg.mainActor;
        Expression expr = msg.expression;

        // Checks for most outer type of expression
        if (expr instanceof Expression.Val) {
            int result = ((Expression.Val) expr).inner();
            replyToMain.tell(new PrintAndEvaluator.EvaluatedResult(result)); // If its only one val, it's immediately returned to pEA
        } else {
            ActorRef<Evaluator.Message> self = getContext().getSelf();
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
        int result = ((Expression.Val) msg.expression).inner();
        msg.parent.tell(new ReceiveResult(result, msg.isLeft, msg.isParentLeft));
        return this;
    }

    private Behavior<Message> onReceiveResult(ReceiveResult msg) {
        if (Boolean.TRUE.equals(msg.isLeft)) { // Safeguard against null, checks whether result is left or right child
            leftValue = msg.result;
            leftReceived = true;
            getContext().getLog().info("Left result received: {}", leftValue);
        } else if (Boolean.FALSE.equals(msg.isLeft)){
            rightValue = msg.result;
            rightReceived = true;
            getContext().getLog().info("Right result received: {}", rightValue);
        }

        if (leftReceived && rightReceived) { // If both are true, subexpression is finished
            timers.startSingleTimer(new Evaluator.CalculateResult(), Duration.ofSeconds(1));
        }
        return this;
    }

    private Behavior<Message> onCalculateResult(CalculateResult msg) { // Extra message to ensure calculation takes 1 sec
        int combinedResult = 0;
        if(Objects.equals(operator, "+")){
            combinedResult = leftValue + rightValue;
        }
        else if(Objects.equals(operator, "-")){
            combinedResult = leftValue - rightValue;
        }
        else if(Objects.equals(operator, "*")){
            combinedResult = leftValue * rightValue;
        }
        getContext().getLog().info("Combined result: {}", combinedResult);
        if (parentIsLeft != null) { // If parent is null, the current actor is the original evaluator
            parent.tell(new ReceiveResult(combinedResult, parentIsLeft, null)); // Send combined result back to parent with the correct position of the parent
        } else {
            getContext().getLog().info("Sending result to pea");
            replyToMain.tell(new PrintAndEvaluator.EvaluatedResult(combinedResult));
        }
        return this;
    }


    private void processSubExpressions(Expression left, Expression right) {
        ActorRef<Message> leftChild = getContext().spawnAnonymous(Evaluator.create());
        ActorRef<Message> rightChild = getContext().spawnAnonymous(Evaluator.create());

        if (left instanceof Expression.Val) {
            leftChild.tell(new ValExp(left, getContext().getSelf(), true, true));
        } else if (left instanceof Expression.Add) {
            leftChild.tell(new AddExp(left, getContext().getSelf(), true));
        } else if (left instanceof Expression.Mul) {
            leftChild.tell(new MulExp(left, getContext().getSelf(), true));
        } else if (left instanceof Expression.Sub) {
            leftChild.tell(new SubExp(left, getContext().getSelf(), true));
        }

        if (right instanceof Expression.Val) {
            rightChild.tell(new ValExp(right, getContext().getSelf(), false, false));
        } else if (right instanceof Expression.Add) {
            rightChild.tell(new AddExp(right, getContext().getSelf(), false));
        } else if (right instanceof Expression.Mul) {
            rightChild.tell(new MulExp(right, getContext().getSelf(), false));
        } else if (right instanceof Expression.Sub) {
            rightChild.tell(new SubExp(right, getContext().getSelf(), false));
        }
    }
}
