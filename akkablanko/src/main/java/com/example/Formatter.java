package com.example;

import akka.actor.typed.ActorRef;
import akka.actor.typed.Behavior;
import akka.actor.typed.javadsl.AbstractBehavior;
import akka.actor.typed.javadsl.ActorContext;
import akka.actor.typed.javadsl.Behaviors;
import akka.actor.typed.javadsl.Receive;

public class Formatter extends AbstractBehavior<Formatter.Message> {

    private ActorRef<PrintAndEvaluator.Message> replyToMain;
    private ActorRef<Formatter.Message> lastActor;

    private String leftString;
    private String rightString;
    private String operator;

    private Formatter(ActorContext<Message> context) {
        super(context);
    }


    public record Init(Expression expression, ActorRef<PrintAndEvaluator.Message> mainActor) implements Message{}
    public record AddExp(Expression expression, ActorRef<Formatter.Message> last) implements Message{}
    public record MulExp(Expression expression, ActorRef<Formatter.Message> last) implements Message {}
    public record SubExp(Expression expression, ActorRef<Formatter.Message> last) implements Message {}
    public record ValExp(Expression expression, ActorRef<Formatter.Message> last, Boolean left) implements Message {}
    public record ReceiveResult(String result, Boolean left) implements Message {}
    public static Behavior<Formatter.Message> create() {
        return Behaviors.setup(context -> new Formatter(context));
    }

    public interface Message {}

    @Override
    public Receive<Formatter.Message> createReceive() {
        return newReceiveBuilder()
                .onMessage(Init.class, this::onInit)
                .onMessage(AddExp.class, this::onAddExp)
                .onMessage(MulExp.class, this::onMulExp)
                .onMessage(SubExp.class, this::onSubExp)
                .onMessage(ValExp.class, this::onValExp)
                .onMessage(ReceiveResult.class, this::onReceiveResult)
                .build();
    }


    private Behavior<Formatter.Message> onAddExp(AddExp msg){
        operator = "+";
        lastActor = msg.last;
        Expression.Add addExp = (Expression.Add) msg.expression;
        Expression left = addExp.left();
        Expression right = addExp.right();
        return this;
    }

    private Behavior<Formatter.Message> onMulExp(MulExp msg){
        operator = "*";
        lastActor = msg.last;
        Expression.Mul mulExp = (Expression.Mul) msg.expression;
        Expression left = mulExp.left();
        Expression right = mulExp.right();
        return this;
    }

    private Behavior<Formatter.Message> onSubExp(SubExp msg){
        operator = "-";
        lastActor = msg.last;
        Expression.Sub subExp = (Expression.Sub) msg.expression;
        Expression left = subExp.left();
        Expression right = subExp.right();
        return this;
    }

    private Behavior<Formatter.Message> onValExp(ValExp msg){
        String result = String.valueOf(((Expression.Val) msg.expression).inner());
        lastActor.tell(new ReceiveResult(result, msg.left));
        return this;
    }

    private Behavior<Message> onInit(Init msg){
        replyToMain = msg.mainActor;

        if (msg.expression instanceof Expression.Val) {

        } else if (msg.expression instanceof Expression.Add) {
            getContext().getSelf().tell(new Formatter.AddExp(msg.expression, getContext().getSelf()));
        } else if (msg.expression instanceof Expression.Mul) {
            getContext().getSelf().tell(new Formatter.MulExp(msg.expression, getContext().getSelf()));
        } else if (msg.expression instanceof Expression.Sub) {
            getContext().getSelf().tell(new Formatter.SubExp(msg.expression, getContext().getSelf()));
        }

        return this;
    }

    private Behavior <Message> onReceiveResult(ReceiveResult msg){

        if (msg.left) {
            leftString = msg.result;
        } else {
            rightString = msg.result;
        }

        if (leftString != null && rightString != null) {
            String result = leftString + " " + operator + " " + rightString;
            getContext().getLog().info("result: {}", result);
        }

        return this;

    }


    private void processSubExpression(Expression left, Expression right, String operator){
        ActorRef<Message> leftChild = getContext().spawnAnonymous(Formatter.create());
        ActorRef<Message> rightChild = getContext().spawnAnonymous(Formatter.create());

        if (left instanceof Expression.Val) {
            leftChild.tell(new Formatter.ValExp(left, getContext().getSelf(), true));
        } else if (left instanceof Expression.Add) {
            leftChild.tell(new Formatter.AddExp(left, getContext().getSelf()));
        } else if (left instanceof Expression.Mul) {
            leftChild.tell(new Formatter.MulExp(left, getContext().getSelf()));
        } else if (left instanceof Expression.Sub) {
            leftChild.tell(new Formatter.SubExp(left, getContext().getSelf()));
        }

        if (right instanceof Expression.Val) {
            rightChild.tell(new Formatter.ValExp(left, getContext().getSelf(), false));
        } else if (left instanceof Expression.Add) {
            rightChild.tell(new Formatter.AddExp(left, getContext().getSelf()));
        } else if (left instanceof Expression.Mul) {
            rightChild.tell(new Formatter.MulExp(left, getContext().getSelf()));
        } else if (left instanceof Expression.Sub) {
            rightChild.tell(new Formatter.SubExp(left, getContext().getSelf()));
        }
    }
}
