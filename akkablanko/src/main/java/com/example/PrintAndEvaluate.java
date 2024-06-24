package com.example;

import akka.actor.typed.Behavior;
import akka.actor.typed.javadsl.AbstractBehavior;
import akka.actor.typed.javadsl.ActorContext;
import akka.actor.typed.javadsl.Behaviors;
import akka.actor.typed.javadsl.Receive;

public class PrintAndEvaluate extends AbstractBehavior<PrintAndEvaluate.Message> {

    private PrintAndEvaluate(ActorContext<Message> context) {
        super(context);
    }

    public interface Message{}

    public record StartExpression(Expression expression) implements Message {}

    public static Behavior<Message> create() {
        return Behaviors.setup(PrintAndEvaluate::new);
    }

    @Override
    public Receive<PrintAndEvaluate.Message> createReceive() {
        return newReceiveBuilder()
                .onMessage(StartExpression.class, this::onStartExpression)
                .build();
    }

    private Behavior<Message> onStartExpression(StartExpression startExpression) {
        return this;
    }
    }

