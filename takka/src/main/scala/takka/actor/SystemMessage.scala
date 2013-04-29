package takka.actor

private[takka] sealed trait SystemMessage

case object ReceiveTimeout extends SystemMessage