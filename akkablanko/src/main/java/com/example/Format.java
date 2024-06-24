package com.example;

import akka.actor.typed.Behavior;
import akka.actor.typed.javadsl.AbstractBehavior;
import akka.actor.typed.javadsl.ActorContext;
import akka.actor.typed.javadsl.Behaviors;
import akka.actor.typed.javadsl.Receive;

public class Format extends AbstractBehavior<Format.Message> {

    private Format(ActorContext<Message> context) {
        super(context);
    }


    public static Behavior<Format.Message> create() {
        return Behaviors.setup(Format::new);
    }

    public interface Message {
    }

    @Override
    public Receive<Format.Message> createReceive() {
        return newReceiveBuilder()
                .build();
    }

}
