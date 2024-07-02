package com.example;

import akka.actor.typed.ActorRef;
import akka.actor.typed.Behavior;
import akka.actor.typed.javadsl.AbstractBehavior;
import akka.actor.typed.javadsl.ActorContext;
import akka.actor.typed.javadsl.Behaviors;
import akka.actor.typed.javadsl.Receive;
import akka.actor.typed.javadsl.TimerScheduler;

import java.time.Duration;
import java.util.Objects;

public class Evaluator extends AbstractBehavior<Evaluator.Message> {

    private final TimerScheduler<Message> timers;
    private ActorRef<PrintAndEvaluator.Message> replyToMain;  // Holds reference to printAndEvaluator (pEA)
    private int leftResult; // Saves result for left part of the expression
    private int rightResult; // Saves result for right part of the expression
    private String operator; // Saves the "creator" actor ref for when creating another actor for subexpressions
    private ActorRef<Evaluator.Message> parent;
    private boolean leftReceived;
    private boolean rightReceived;
    private Boolean parentIsLeft; // Saves position in the expression of parent that created child

    private Evaluator(ActorContext<Message> context, TimerScheduler<Message> timers) {
        super(context);
        this.timers = timers;
    }

    public static Behavior<Message> create() {
        return Behaviors.setup(context -> Behaviors.withTimers(timers -> new Evaluator(context, timers)));
    }

    public interface Message {}

    public record Init(Expression expression, ActorRef<PrintAndEvaluator.Message> mainActor) implements Message {}
    public record SplitExpression(Expression expression, ActorRef<Evaluator.Message> parent, Boolean isParentLeft, String operator) implements Message {}
    public record ValExp(Expression expression, ActorRef<Evaluator.Message> parent, Boolean isLeft, Boolean isParentLeft) implements Message {}
    public record ReceiveResult(int result, Boolean isLeft, Boolean isParentLeft) implements Message {}
    public record CalculateResult() implements Message {}

    @Override
    public Receive<Message> createReceive() {
        return newReceiveBuilder()
                .onMessage(Init.class, this::onInit)
                .onMessage(SplitExpression.class, this::onSplitExpression)
                .onMessage(ValExp.class, this::onValExp)
                .onMessage(ReceiveResult.class, this::onReceiveResult)
                .onMessage(CalculateResult.class, this::onCalculateResult)
                .build();
    }

    private Behavior<Message> onInit(Init msg) {
        replyToMain = msg.mainActor;
        Expression expr = msg.expression;

        //Checks for most outer type of expression
        if (expr instanceof Expression.Val) { // Initial expression is simply a value
            int result = ((Expression.Val) expr).inner();
            replyToMain.tell(new PrintAndEvaluator.EvaluatedResult(result)); // Gets immediately returned to pEA
        } else {
            handleExpression(expr, getContext().getSelf(), null);  // isLeft is null, since it's not a subexpression
        }
        return this;
    }

    private Behavior<Message> onSplitExpression(SplitExpression msg) { // Initiating the evaluation of the left and right subexpressions
        operator = msg.operator;
        parent = msg.parent;
        parentIsLeft = msg.isParentLeft;

        if (msg.expression instanceof Expression.Add) {
            processSubExpressions(((Expression.Add) msg.expression).left(), ((Expression.Add) msg.expression).right());
        } else if (msg.expression instanceof Expression.Mul) {
            processSubExpressions(((Expression.Mul) msg.expression).left(), ((Expression.Mul) msg.expression).right());
        } else if (msg.expression instanceof Expression.Sub) {
            processSubExpressions(((Expression.Sub) msg.expression).left(), ((Expression.Sub) msg.expression).right());
        }
        return this;
    }

    private Behavior<Message> onValExp(ValExp msg) {
        int result = ((Expression.Val) msg.expression).inner();
        msg.parent.tell(new ReceiveResult(result, msg.isLeft, msg.isParentLeft));
        return this;
    }

    private Behavior<Message> onReceiveResult(ReceiveResult msg) {
        if (Boolean.TRUE.equals(msg.isLeft)) {  // Safeguard against null, checks whether result is left or right child
            leftResult = msg.result;
            leftReceived = true;
        //    getContext().getLog().info("Left result received: {}", leftResult);
        } else if (Boolean.FALSE.equals(msg.isLeft)){
            rightResult = msg.result;
            rightReceived = true;
      //      getContext().getLog().info("Right result received: {}", rightResult);
        }

        if (leftReceived && rightReceived) {
            timers.startSingleTimer(new CalculateResult(), Duration.ofSeconds(1)); // Extra message to ensure calculation takes 1 sec
        }
        return this;
    }

    private Behavior<Message> onCalculateResult(CalculateResult msg) {
        int combinedResult = 0;
        if (Objects.equals(operator, "+")) {
            combinedResult = leftResult + rightResult;
        } else if (Objects.equals(operator, "-")) {
            combinedResult = leftResult - rightResult;
        } else if (Objects.equals(operator, "*")) {
            combinedResult = leftResult * rightResult;
        }
        //getContext().getLog().info("Combined result: {}", combinedResult);
        if (parentIsLeft != null) { // If parent is null, the current actor is the original evaluator
            parent.tell(new ReceiveResult(combinedResult, parentIsLeft, null)); // Send combined result back to parent with the position of the parent
        } else {
            replyToMain.tell(new PrintAndEvaluator.EvaluatedResult(combinedResult));
        }
        return this;
    }

    private void processSubExpressions(Expression left, Expression right) { // Creates child actors for subexpressions
        ActorRef<Message> leftChild = getContext().spawnAnonymous(Evaluator.create());
        ActorRef<Message> rightChild = getContext().spawnAnonymous(Evaluator.create());

        handleExpression(left, leftChild, true);
        handleExpression(right, rightChild, false);
    }

    private void handleExpression(Expression expr, ActorRef<Evaluator.Message> actorRef, Boolean isLeft) { // Checking for type of expression
        if (expr instanceof Expression.Val) {
            actorRef.tell(new ValExp(expr, getContext().getSelf(), isLeft, isLeft));
        } else if (expr instanceof Expression.Add) {
            actorRef.tell(new SplitExpression(expr, getContext().getSelf(), isLeft, "+"));
        } else if (expr instanceof Expression.Mul) {
            actorRef.tell(new SplitExpression(expr, getContext().getSelf(), isLeft, "*"));
        } else if (expr instanceof Expression.Sub) {
            actorRef.tell(new SplitExpression(expr, getContext().getSelf(), isLeft, "-"));
        }
    }
}