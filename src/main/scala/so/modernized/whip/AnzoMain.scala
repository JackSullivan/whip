package so.modernized.whip

import java.util.Properties
import org.openanzo.client.{AnzoClientDictionary, AnzoClientConfigurationFactory, AnzoClient}
import org.openanzo.combus.{CombusDictionary, CombusProperties}
import org.openanzo.glitter.query.PatternSolution
import org.openanzo.services.ServicesProperties

object AnzoMain {

  def clientConf(username:String, password:String) = {
    val properties = new Properties
    CombusProperties.setHost(properties, "localhost")
    CombusProperties.setPort(properties, 61618)
    CombusProperties.setUseSsl(properties, false)
    ServicesProperties.setUser(properties, "default")
    ServicesProperties.setPassword(properties, "123")
    properties.put("http.port", "8080")

    val configGraph = AnzoClientConfigurationFactory.createJMSConfiguration(username, password, CombusProperties.getHost(properties), CombusProperties.getPort(properties), CombusProperties.getUseSsl(properties))
    AnzoClientDictionary.setUseCometd(configGraph, false)
    CombusDictionary.setUseSsl(configGraph, false)
    configGraph.put("http.port", "8080")
    ServicesProperties.setTimeout(configGraph, 9500000)
    AnzoClientConfigurationFactory.configureNonPersistedClient(configGraph)
    configGraph
  }

  def main(args:Array[String]): Unit = {
    val anzo = new AnzoClient(clientConf("sysadmin", "123"))

  }


}
