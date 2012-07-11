# Gatling on GitHub

* gatling  https://github.com/excilys/gatling

* gatling-highcharts  https://github.com/excilys/gatling-highcharts

Retrieve Date: 9 July 2012

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

In following, we listed files that adopt takka API.  For each file, we count (the number of modified lines / the number of total lines in the akka version).  Copyright declarations, 15 lines per file, are not counted.  Comments and documentations, however,  are counted as we believe that they are important components of software implementation.

<pre><code>
com.excilys.ebi.gatling.core
                         |- action
                             |- builder
                                  |- ActionBuilder.scala  (4/23)
                                  |- EndActionBuilder.scala (2/33)
                                  |- ExpPauseActionBuilder.scala (3/57)
                                  |- IfActionBuilder.scala  (3/61)
                                  |- PauseActionBuilder.scala  (3/73)
                                  |- SimpleActionBuilder.scala  (3/31)
                                  |- StartActionBuilder.scala  (3/28)
                                  |- WhileActionBuilder.scala  (4/56)
                             |- Action.scala  (4/26)
                             |- IfAction.scala  (2/37)
                             |- package.scala  (1/7)
                             |- PauseAction.scala  (2/48)
                             |- SimpleAction.scala  (2/33)
                             |- StartAction.scala  (2/36)
                             |- WhileAction.scala  (3/45)
                         |- result.writer
                                      |- ConsoleDataWriter.scala  (12/75)
                                      |- DataWriter.scala  (6/55)
                                      |- FileDataWriter.scala  (20/106)
                         |- runner.Runner.scala (2/100)
                         |- scenario.Scenario.scala  (2/3)
                         |- structure
                             |- AbstractStructureBuilder.scala  (3/174)
                             |- ChainBuilder.scala (3/45)
com.excilys.ebi.gatling.http
                         |- action
                             |- HttpRequestAction.scala  (3/99)
                             |- HttpRequestAction.scalaHttpRequestActionBuilder.scala  (4/43)
                         |- ahc
                             |- GatlingAsyncHandler.scala  (3/71)
                             |- GatlingAsyncHandlerActor.scala  (10/260)
                             |- package.scala  (2/10)
</code></pre>

## Main Changes
* ConsoleDataWriter is rewritten using FSM
* FileDataWriter is rewritten using FSM
* Actions is a subclass of Actor[Session]
* DataWriter is a subclass of Actor[DataWriterMessage]

#TODO
* test


