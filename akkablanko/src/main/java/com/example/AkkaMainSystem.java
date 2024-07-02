package com.example;

import akka.actor.typed.ActorRef;
import akka.actor.typed.Behavior;
import akka.actor.typed.javadsl.AbstractBehavior;
import akka.actor.typed.javadsl.ActorContext;
import akka.actor.typed.javadsl.Behaviors;
import akka.actor.typed.javadsl.Receive;

public class AkkaMainSystem extends AbstractBehavior<AkkaMainSystem.Create> {

    public static class Create {
    }

    public static Behavior<Create> create() {
        return Behaviors.setup(AkkaMainSystem::new);
    }

    private AkkaMainSystem(ActorContext<Create> context) {
        super(context);
    }

    @Override
    public Receive<Create> createReceive() {
        return newReceiveBuilder().onMessage(Create.class, this::onCreate).build();
    }

    private Behavior<Create> onCreate(Create command) {
        Expression test =  Expression.generateExpression(6, 9);
        getContext().getLog().info("Actual format : {}", test);
        getContext().getLog().info("Actual value : {}", test.eval());
        getContext().getLog().info("Needed time: {}", test.runtime());
        ActorRef<PrintAndEvaluator.Message> pae = this.getContext().spawn(PrintAndEvaluator.create(),"pae");
        pae.tell(new PrintAndEvaluator.StartExpression(test));
        return this;
    }
}
