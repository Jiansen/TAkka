# Gatling on GitHub

* gatling  https://github.com/excilys/gatling

* gatling-highcharts  https://github.com/excilys/gatling-highcharts

Date of Retrieved: 9 July 2012

Learn more about gatling at [gatling wiki](https://github.com/excilys/gatling/wiki)


# Fix to retrieved code
[isuss#605](https://github.com/excilys/gatling/issues/605): Add type parameter to following classes

com.excilys.ebi.gatling.recorder.ui.frame.ConfigurationFrame.scala

* Line 53: val cbFilterStrategies = new JComboBox[FilterStrategy.Value]
* Line 59: val cbOutputEncoding = new JComboBox[Charset]

com.excilys.ebi.gatling.recorder.ui.frame.RunningFrame.scala

* Line 42: private val eventsInfo = new DefaultListModel[EventInfo]
* Line 43: private val hostsCertificate = new DefaultListModel[String]

# Using TAkka

Akka actors in gatling-core and gatling-http are replaced by TAkka actors.  Source code of [TAkka.actor](https://github.com/Jiansen/TAkka) is copied to gatling-core, which is the base of gatling-http.

## Modified files
<pre><code>
com.excilys.ebi.gatling.core  
                         |- action  
                             |- builder
                                  |- ActionBuilder.scala
                                  |- EndActionBuilder.scala
                                  |- ExpPauseActionBuilder.scala
                                  |- IfActionBuilder.scala
                                  |- PauseActionBuilder.scala
                                  |- SimpleActionBuilder.scala
                                  |- StartActionBuilder.scala
                                  |- WhileActionBuilder.scala
                             |- Action.scala
                             |- IfAction.scala
                             |- package.scala
                             |- PauseAction.scala
                             |- SimpleAction.scala
                             |- StartAction.scala
                             |- WhileAction.scala
                         |- result.writer.DataWriter.scala
                         |- runner.Runner.scala
                         |- scenario.Scenario.scala
                         |- structure
                             |- loop.handler.ConditionalLoopHandlerBuilder
                             |- AbstractStructureBuilder.scala
                             |- ChainBuilder.scala
com.excilys.ebi.gatling.http  
                         |- action
                             |- HttpRequestAction.scala
                             |- HttpRequestAction.scalaHttpRequestActionBuilder.scala
                         |- ahc
                             |- GatlingAsyncHandler.scala
                             |- GatlingAsyncHandlerActor.scala
                             |- package.scala
</code></pre>

## Main Changes
* ConsoleDataWriter is rewritten using FSM
* FileDataWriter is rewritten using FSM
* Actions is a subclass of Actor[Session]
* DataWriter is a subclass of Actor[DataWriterMessage]

#TODO
* test


