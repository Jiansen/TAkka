package org.enmas.typed.client

import takka.actor.TypedActor
import org.enmas.typed.messaging.ClientMessage

trait Client extends TypedActor[ClientMessage]