package com.example;

import akka.actor.typed.ActorRef;
import akka.actor.typed.Behavior;
import akka.actor.typed.javadsl.AbstractBehavior;
import akka.actor.typed.javadsl.ActorContext;
import akka.actor.typed.javadsl.Behaviors;
import akka.actor.typed.javadsl.Receive;

public class PrintAndEvaluator extends AbstractBehavior<PrintAndEvaluator.Message> {

    private PrintAndEvaluator(ActorContext<Message> context) {
        super(context);
    }

    public interface Message {}

    public record StartExpression(Expression expression) implements Message {}
    public record FormattedResult(String result) implements Message {}

    public static Behavior<Message> create() {
        return Behaviors.setup(PrintAndEvaluator::new);
    }

    @Override
    public Receive<PrintAndEvaluator.Message> createReceive() {
        return newReceiveBuilder()
                .onMessage(StartExpression.class, this::onStartExpression)
                .onMessage(FormattedResult.class, this::onFormattedResult)
                .build();
    }

    private Behavior<Message> onStartExpression(StartExpression startExpression) {
        ActorRef<Formatter.Message> formatter = this.getContext().spawn(Formatter.create(), "formatter");
        formatter.tell(new Formatter.Init(startExpression.expression, getContext().getSelf()));
        return this;
    }

    private Behavior<Message> onFormattedResult(FormattedResult formattedResult) {
        getContext().getLog().info("Formatted Expression: {}", formattedResult.result);
        return this;
    }
    }

