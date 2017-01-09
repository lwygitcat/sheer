package org.lehvolk.common.ws.example;

import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebResult;
import javax.jws.WebService;

@WebService(targetNamespace = "http://emample.lehvolk.org/", name = "Processing")
public interface ProcessingBean {

 @WebResult(targetNamespace = "http://emample.lehvolk.org/process")
 @WebMethod(operationName = "process", action = "http://emample.lehvolk.org/process")
 public ProcessOutput process(@WebParam ProcessInput input);
}
Here is service class: 
package org.lehvolk.common.ws.example;

import java.net.URL;

import javax.xml.namespace.QName;
import javax.xml.ws.Service;
import javax.xml.ws.WebEndpoint;
import javax.xml.ws.WebServiceClient;

@WebServiceClient(name = "Processing",
  wsdlLocation = ProcessingService.WSDL_LOCATION,
  targetNamespace = "http://example.lehvolk.org/")
public class ProcessingService extends Service {

 public final static String WSDL_LOCATION = "META-INF/wsdl/service.wsdl";
 public final static QName SERVICE = new QName("http://example.lehvolk.org", "ProcessingService");
 public final static QName Processing = new QName("http://example.lehvolk.org", "Processing");

 public ProcessingService() {
  super(getWsdlLocation(), SERVICE);
 }

 /**
  * @return returns Management
  */
 @WebEndpoint(name = "Processing")
 public ProcessingBean getPort() {
  return super.getPort(Processing, ProcessingBean.class);
 }

 private static URL getWsdlLocation() {
  return ProcessingService.class.getResource(WSDL_LOCATION);
 }
}
For web-service client we should create new instance of ProcessingService and then call getPort method. Actually getPort method return some proxy-instance. ProcessingService class is thread-safe so it's enought to have one instance of Service in your application. Proxy-objects returned from getPort method have more difficult structure and they not thread-safe. So you shouldn't use one instance of this proxy from more than one thread. Configurating port for ssl is expensive operation too. That means that best choise is to store port-instances in some cache (I will use ehcache for this purposes). We need to garantee that port instance will not be used in another thread., i.e. we should in one thread get and remove instance from cache and after making call to remote web-service put it back in cache. Here is wrapper of port instance:
package org.lehvolk.common.ws;

/**
 * Wrapper of web-service port
 * 
 * @param <T> - type of port
 */
public class WebServicePort<T> {

 private final T port;
 private final String soapVersion;
 private final String address;

 /**
  * Constructs instance with parameters specified
  * 
  * @param port - web-service port
  * @param soapVersion - version of soap protocol
  * @param address - web-service address
  */
 public WebServicePort(T port, String soapVersion, String address) {
  this.port = port;
  this.soapVersion = soapVersion;
  this.address = address;
 }

 // getters
}
Here is interface to ports pool:
package org.lehvolk.common.ws.pool;

import org.lehvolk.common.ws.WebServicePort;

/**
 * Interface of WS ports pool
 * 
 * @param <T> - generic type of port
 */
public interface WebServicePortPool<T> {

 /**
  * get port by address from configuration
  * 
  * @return instance of port
  */
 public WebServicePort<T> getPort();

 /**
  * @param address - web-service address
  * @return port for given address
  */
 public WebServicePort<T> getPort(String address);

 /**
  * Initialize service
  */
 public void postConstruct();

 /**
  * @param port - web-service port
  */
 public void putPort(WebServicePort<T> port);

 /**
  * Reinitialize pool with new configuration
  */
 public void reset();

 /**
  * Shutdowns pool
  */
 public void shutdown();
}
And here there is interface for configuraion port instance:
package org.lehvolk.common.ws;

import javax.net.ssl.SSLSocketFactory;

/**
 * Interface of class, for configuration web-service port, e.g. timeout, security aspects.
 */
public interface WSConfigurator {

 /**
  * Configures web-service port
  * 
  * @param <T> - type of port
  * @param port - web-service port
  * @param wsAddress - address of web-service
  * @param connTimeout - connection timeout
  * @param readTimeout - socket read timeout
  * @param sf - {@link SSLSocketFactory} instance
  * @param verifyHost - host verification enabled
  * @return - configured web-service port
  */
 public <T> T configurePort(T port, String wsAddress, long connTimeout, long readTimeout, SSLSocketFactory sf,
   boolean verifyHost);

}
Ports pool should encapsulate configure logic on port creation (configure ssl connection, ws address and pool config). Configuration should have ssl configuration, common confiuration (addresses, timeouts) and cache configuration. I will use SSLConfiguration (and SSLUtils for generation SSLCOntext) from this post. And here is configuration for other instances (as always this classes can be stored in xml).
package org.lehvolk.common.ws.pool;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * Configuration of WS-ports pool
 */
@XmlRootElement(name = "ports-pool-configuration")
@XmlAccessorType(XmlAccessType.FIELD)
public class PortsPoolConfiguration{

 @XmlElement(name = "cache-name", required = true)
 private String cacheName;
 @XmlElement(name = "pool-size", required = false)
 private Integer poolSize = 100;

 //getters and setters
}
package org.lehvolk.common.ws.pool;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.lehvolk.common.ssl.SSLConfiguration;

/**
 * Configuration of web-service clients
 */
@XmlRootElement(name = "ws-client-configuration")
@XmlAccessorType(XmlAccessType.FIELD)
public class WSClientConfiguration {

 @XmlElement(name = "connection-timeout", required = false)
 private Long connectionTimeout = 30 * 1000L; //30 seconds

 @XmlElement(name = "ws-address", required = true)
 private String wsAddress;

 @XmlElement(name = "pool-config", required = true)
 private PortsPoolConfiguration poolConfig;

 @XmlElement(name = "socket-read-timeout", required = false)
 private Long socketReadTimeout = 60 * 1000L; //60 seconds

 @XmlElement(name = "ssl-config", required = false)
 private SSLConfiguration sslConfiguration;

 @XmlElement(name = "protocol-version")
 private String protocolVersion = "1.1";

 //getters and setters
}
And here is base implementation of WebServicePortPool:
package org.lehvolk.common.ws.pool;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLSocketFactory;

import net.sf.ehcache.Cache;
import net.sf.ehcache.CacheManager;
import net.sf.ehcache.Ehcache;
import net.sf.ehcache.Element;
import net.sf.ehcache.exceptionhandler.CacheExceptionHandler;
import net.sf.ehcache.store.MemoryStoreEvictionPolicy;

import org.apache.log4j.Logger;
import org.lehvolk.common.ssl.SSLConfiguration;
import org.lehvolk.common.ssl.SSLUtils;
import org.lehvolk.common.ws.WSConfigurator;
import org.lehvolk.common.ws.WebServicePort;

/**
 * @param <T> service parameterization
 */
public abstract class AbstractWebServicePortPool<T> implements WebServicePortPool<T> {

 private static class WebServiceAddressKey {

  private final String address;
  private final Long stamp;

  /**
   * @param address - web-service address
   */
  public WebServiceAddressKey(String address) {
   this(address, System.nanoTime());
  }

  /**
   * @param address - web-service address
   * @param stamp - last time of port using
   */
  public WebServiceAddressKey(String address, Long stamp) {
   String adr;
   if (address == null || (adr = address.trim()).isEmpty()) {
    throw new IllegalArgumentException("Cannot create WebServicePortPool with null or empty address value '"
      + address + "'");
   }
   this.address = adr;
   this.stamp = stamp;
  }

  /**
   * @see java.lang.Object#equals(java.lang.Object)
   */
  @Override
  public boolean equals(Object obj) {
   if (obj instanceof WebServiceAddressKey) {
    WebServiceAddressKey key = (WebServiceAddressKey) obj;
    boolean result = key.address.equals(address);
    if (result && stamp != null && key.stamp != null) {
     return stamp.equals(key.stamp);
    }
    return result;
   }
   return false;
  }

  /**
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode() {
   int result = 17;
   result = result * 31 + address.hashCode();
   return result;
  }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString() {
   return String.format("WebServiceAddressKey [%s, %d]", address, stamp);
  }
 }

 protected Logger log = Logger.getLogger(getClass());

 /** WS configurator instance */
 @Inject
 protected WSConfigurator configurator;

 /** WS client configuration */
 protected volatile WSClientConfiguration configuration;

 /** SSLSocket factory instance */
 protected SSLSocketFactory sslFactory = null;

 private Cache cache;

 /**
  * {@inheritDoc}
  */
 @Override
 @SuppressWarnings("unchecked")
 public WebServicePort<T> getPort(String address) {
  WebServiceAddressKey key = new WebServiceAddressKey(address, null);
  synchronized (cache) {
   Element element = cache.get(key);
   if (element != null) {
    cache.remove(element.getObjectKey());
    return (WebServicePort<T>) element.getObjectValue();
   }
  }
  return createPort(address);
 }

 /**
  * {@inheritDoc}
  */
 @Override
 @PostConstruct
 public void postConstruct() {
  configuration = getConfiguration();
  sslFactory = getSSLSocketFactory();
  createCache(configuration.getPoolConfig());
 }

 private void createCache(PortsPoolConfiguration poolConfig) {
  CacheManager manager = CacheManager.getInstance();
  if (manager.cacheExists(poolConfig.getCacheName())) {
   throw new IllegalArgumentException("Cache with name '" + poolConfig.getCacheName() + "' already exists");
  }
  cache = new Cache(
    poolConfig.getCacheName(),
    poolConfig.getPoolSize() == null ? 0 : poolConfig.getPoolSize(),
    MemoryStoreEvictionPolicy.FIFO,
    false,
    null,
    false,
    0, 0,
    false,
    0,
    null);

  manager.addCache(cache);

  // assign exception handler
  cache.setCacheExceptionHandler(new CacheExceptionHandler() {

   @Override
   public void onException(Ehcache cache, Object key, Exception e) {
    String msg = String.format("Exception occurred while operating %s %s", cache.getName(), key == null ? "" : key);
    log.error(msg, e);
   }
  });
 }

 /**
  * Create port instance
  * 
  * @param address - address
  * @return port instance
  */
 protected abstract WebServicePort<T> createPort(String address);

 /**
  * @return configuration for service
  */
 protected abstract WSClientConfiguration getConfiguration();

 /**
  * {@inheritDoc}
  */
 @Override
 public void putPort(WebServicePort<T> port) {
  if (port == null) {
   return;
  }
  WebServiceAddressKey key = new WebServiceAddressKey(port.getAddress());
  cache.put(new Element(key, port));
 }

 /**
  * get port by address from configuration
  * 
  * @return instance of port
  */
 @Override
 public WebServicePort<T> getPort() {
  return getPort(configuration.getWsAddress());
 }

 private SSLSocketFactory getSSLSocketFactory() {
  SSLConfiguration sslConf = configuration.getSslConfiguration();
  if (sslConf != null && sslConf.getEnabled()) {
   try {
    SSLContext ctx = SSLUtils.createSSLContext(configuration.getSslConfiguration());
    return ctx.getSocketFactory();
   } catch (Exception e) {
    throw new IllegalArgumentException("Error ssl initialization", e);
   }
  }
  return null;

 }

 /**
  * {@inheritDoc}
  */
 @Override
 public void reset() {
  configuration = getConfiguration();
  sslFactory = getSSLSocketFactory();
  cache.removeAll();
 }

 /**
  * {@inheritDoc}
  */
 @Override
 public void shutdown() {
  if (cache != null) {
   CacheManager.getInstance().removeCache(configuration.getPoolConfig().getCacheName());
  }
 }
}
In AbstractWebServicePortPool there are two abstract methods: getConfiguration (to provide different ways of getting configuration) and createPort. The last one to provide creation of port instance. Also there is WSConfigurator field with @Inject annotation (if someone doesn't use CDI then setter or special construct should be used in implementation). 
An simple implemenation for our Processing service you can see below:
package org.lehvolk.common.ws.example;

import java.io.File;

import javax.inject.Singleton;
import javax.xml.bind.JAXB;

import org.lehvolk.common.ws.WebServicePort;
import org.lehvolk.common.ws.pool.AbstractWebServicePortPool;
import org.lehvolk.common.ws.pool.WSClientConfiguration;

// for ejb singleton use javax.ejb.Singleton instead of javax.inject.Singleton
@Singleton
public class ProcessingWebServicePool extends AbstractWebServicePortPool<ProcessingBean> {

 private ProcessingService service = new ProcessingService();

 /**
  * {@inheritDoc}
  */
 @Override
 protected WebServicePort<ProcessingBean> createPort(String address) {
  ProcessingBean bean = service.getPort();
  WebServicePort<ProcessingBean> port = new WebServicePort<ProcessingBean>(bean, null, address);
  boolean verifyHost = configuration.getSslConfiguration() == null ? false : configuration.getSslConfiguration()
    .getVerifyHost();
  return configurator.configurePort(port, address, configuration.getConnectionTimeout(),
    configuration.getSocketReadTimeout(), sslFactory, verifyHost);
 }

 /**
  * {@inheritDoc}
  */
 @Override
 protected WSClientConfiguration getConfiguration() {
  return JAXB.unmarshal(new File("META-INF/conf/ws-conf.xml"), WSClientConfiguration.class);
 }
}
If application deployed in environment which supports J2EE6 then should be used EJB singleton annotation and configurae binding for WSConfigurator (for example by @Produces annotation in some factory). If Guice uses for CDI then binding should be written in AbstractModule implementation (and for convenience Guice can be configured to invoke after creation method annotated with @PostConstruct). Below there is code of uses of this pool:
@Stateless
public class SomeBean {

 @Inject 
 private ProcessingWebServicePool pool;

 pubic void call(){
  WebServicePort<ProcessingBean> port = pool.getPort();
  try{
   //logic
  } catch(Exception e) {
   //exception logic
  } finally {
   pool.putPort(port);
  }
 }

}
And now the final part of post - implementations of WSConfiguration for CXF and JAX-WS. 
Here is one for CXF:
package org.lehvolk.common.ws.impl;

import java.util.Map;

import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLSocketFactory;
import javax.xml.ws.BindingProvider;

import org.apache.cxf.configuration.jsse.TLSClientParameters;
import org.apache.cxf.endpoint.Client;
import org.apache.cxf.frontend.ClientProxy;
import org.apache.cxf.transport.http.HTTPConduit;
import org.apache.cxf.transports.http.configuration.ConnectionType;
import org.apache.cxf.transports.http.configuration.HTTPClientPolicy;
import org.apache.log4j.Logger;
import org.lehvolk.common.ws.WSConfigurator;

/**
 * CXF-specific implementation of {@link WSConfigurator}
 */
public class CXFWSConfigurator implements WSConfigurator {

 private Logger log = Logger.getLogger(getClass());

 /**
  * {@inheritDoc}
  */
 @Override
 public <T> T configurePort(T port, String wsAddress, long connTimeout, long readTimeout, SSLSocketFactory sf,
   boolean verifyHost) {
  Map<String, Object> reqCtx = ((BindingProvider) port).getRequestContext();
  reqCtx.put(BindingProvider.ENDPOINT_ADDRESS_PROPERTY, wsAddress);

  Client client = ClientProxy.getClient(port);
  HTTPConduit http = (HTTPConduit) client.getConduit();

  HTTPClientPolicy httpClientPolicy = new HTTPClientPolicy();
  httpClientPolicy.setConnectionTimeout(connTimeout);
  httpClientPolicy.setReceiveTimeout(readTimeout);
  httpClientPolicy.setConnection(ConnectionType.CLOSE);

  http.setClient(httpClientPolicy);

  TLSClientParameters tls = new TLSClientParameters();

  if (sf != null) {
   tls.setSSLSocketFactory(sf);
   tls.setDisableCNCheck(!verifyHost);
   http.setTlsClientParameters(tls);
  } else {
   try {
    tls.setSSLSocketFactory(SSLContext.getDefault().getSocketFactory());
    http.setTlsClientParameters(tls);
   } catch (Exception e) {
    log.error("Error of port default SSL configuration applying", e);
    throw new IllegalArgumentException("fail to configure ws client by configuration", e);
   }
  }
  return port;
 }
}

And here is for JAX-WS:
package org.lehvolk.common.ws.impl;

import java.util.Map;

import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLSocketFactory;
import javax.xml.ws.BindingProvider;

import org.apache.log4j.Logger;
import org.lehvolk.common.ws.WSConfigurator;

import com.sun.xml.internal.ws.client.BindingProviderProperties;

/**
 * JAX-WS specific implementation
 */
public class JAXWSConfigurator implements WSConfigurator {

 private Logger log = Logger.getLogger(getClass());

 /**
  * {@inheritDoc}
  */
 @Override
 public <T> T configurePort(T port, String wsAddress, long connTimeout, long readTimeout, SSLSocketFactory sf,
   boolean verifyHost) {

  Map<String, Object> reqCtx = ((BindingProvider) port).getRequestContext();

  reqCtx.put(BindingProvider.ENDPOINT_ADDRESS_PROPERTY, wsAddress);
  reqCtx.put(BindingProviderProperties.CONNECT_TIMEOUT, connTimeout);
  reqCtx.put(BindingProviderProperties.REQUEST_TIMEOUT, readTimeout);

  // if it possible implement own javax.net.ssl.HostnameVerifier and set it in property
  // reqCtx.put(BindingProviderProperties.HOSTNAME_VERIFIER, new HostnameVerifierImpl());

  if (sf == null) {
   try {
    reqCtx.put(BindingProviderProperties.SSL_SOCKET_FACTORY, SSLContext.getDefault().getSocketFactory());
   } catch (Exception e) {
    log.error("Error of port default SSL configuration applying", e);
    throw new IllegalArgumentException("fail to configure ws client by configuration", e);
   }
  } else {
   reqCtx.put(BindingProviderProperties.SSL_SOCKET_FACTORY, sf);
  }
  return port;
 }
}
