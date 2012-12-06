package ns;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;

public final class Launcher {

	private static int port;

	public static void main(String[] args) {
		port = Integer.parseInt(args[0]);
		ServerSocket serverSocket = null;
		try {
			serverSocket = new ServerSocket(port);
			System.out.println("Nameserver ready");
			while (true) {
				Socket socket = serverSocket.accept();
				Communicator comm = new Communicator(socket);
				comm.setDaemon(true);
				System.out.println("starting new ns thread");
				comm.start();
			}

		} catch (IOException e) {
			e.printStackTrace();
		}
	}

}
