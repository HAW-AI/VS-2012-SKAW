/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package mware_lib;

import java.io.IOException;
import java.net.Socket;
import java.net.UnknownHostException;

/**
 *
 * @author sebastian
 */
public class ObjectBroker {
   private final String serviceHost;
	private final int listenPort;

	private ObjectBroker(String serviceHost, int listenPort) {
		this.serviceHost = serviceHost;
		this.listenPort = listenPort;
	}

	// Das hier zurückgelieferte Objekt soll der zentrale Einstiegspunkt
	// der Middleware aus Anwendersicht sein.
	// Parameter: Host und Port, bei dem die Dienste (Namensdienst)
	// kontaktiert werden sollen.
	public static ObjectBroker getBroker(String serviceHost, int listenPort) {
		return new ObjectBroker(serviceHost, listenPort);
	}

	// Liefert den Namensdienst (Stellvetreterobjekt).
	public NameService getNameService() {

		Socket socket = null;
		try {
			socket = new Socket(serviceHost, listenPort);
		} catch (UnknownHostException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return new NameServiceStub(socket);
	}
}