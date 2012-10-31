# Scoko on GitHub

* https://github.com/mashupbots/socko

Retrieve Date: 9 July 2012

Learn more about socko at [sockoweb.org](http://sockoweb.org/)


# Using TAkka

Akka actors in socko-webserver and socko-examples replaced by TAkka actors.

## Modified files

In following, we listed files that adopt takka API.  For each file, we count (the number of modified lines / the number of total lines in the akka version).  Copyright declarations, 19 lines per file, are not counted.  Comments and documentations, however,  are counted as we believe that they are important components of software implementation.

<pre><code>
Socko Web Server  (62/2174)
org.mashupbots.socko
                 |- events
                     |- HttpEventConfig.scala         (2/23)
                     |- HttpRequestEvent.scala        (2/67)
                     |- WebSocketEventConfig.scala    (3/6)
                     |- WebSocketFrameEvent.scala     (1/131)
                 |- handlers
                     |- SnoopHandler.scala            (10/183)
                     |- StaticContentHandler.scala    (13/773)
                     |- WebSocketBroadcaseter.scala   (7/53)
                 |- infrastructure.WebLogWriter.scala (14/33)
                 |- routes.Routes.scala               (5/486)
                 |- webserver
                     |- RequestHandler.scala          (1/276)
                     |- WebServer.scala               (4/143)

Socko Web Server Test  (35/1015)
org.mashupbots.socko
                 |- handlers
                     |- SnoopSpec.scala               (7/279)
                     |- StaticContentSpec.scala       (6/611)
                 |- routes
                     |- HostRouteSpec.scala           (1/100)
                     |- TestProcessingContext.scala   (21/25)

Socko Examples  (130/1833)
org.mashupbots.socko.examples
                      |- benchmark
                          |- BenchmarkApp.scala             (5/200)
                          |- DynamicBenchmarkHandler.scala  (8/12)
                      |- config
                          |- AkkaConfigApp.scala            (5/78)
                          |- CodedConfigApp.scala           (4/57)
                      |- fileupload
                          |- FileUploadApp.scala            (4/195)
                          |- FileUploadHandler.scala        (2/86)
                      |- quickstart
                          |- HelloApp.scala                 (5/55)
                          |- HelloHandler.scala             (6/19)
                      |- routes
                          |- RouteApp.scala                 (5/85)
                          |- TimeHandler.scala              (8/67)
                      |- secure
                          |- SecureApp.scala                (5/89)
                          |- SecureHelloHandler.scala       (5/17)
                      |- snoop
                          |- SnoopApp.scala                 (4/49)
                      |- spdy
                          |- DynamicHandler.scala           (5/18)
                          |- SpdyApp.scala                  (6/228)
                      |- streaming
                          |- StreamingApp.scala             (4/46)
                          |- StreamingHandler.scala         (5/29)
                      |- weblog
                          |- CustomwebLogApp.scala          (11/72)
                          |- HelloHandler.scala             (5/16)
                          |- WebLogApp.scala                (4/53)
                      |- websocket
                          |- ChatApp.scala                  (6/85)
                          |- ChatHandler.scala              (7/102)
                          |- webSocketApp.scala             (5/74)
                          |- WebSocketHandler.scala         (6/101)


</code></pre>

## Main Changes
***

#TODO
* test


