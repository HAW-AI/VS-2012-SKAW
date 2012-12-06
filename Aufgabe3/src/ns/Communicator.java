package ns;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;

final class Communicator extends Thread {
	private final Socket socket;
	private PrintWriter out;
	private BufferedReader in;
	private String inputLine;
	private String[] inputTokens;

	Communicator(Socket socket) {
		this.socket=socket;
		try {
			out = new PrintWriter(socket.getOutputStream(), true);
			in = new BufferedReader(new InputStreamReader(
					socket.getInputStream()));
		} catch (IOException e) {
			e.printStackTrace();
		}
//		System.out.println("new socket created");
	}

	@Override
	public void run() {
		try {
			System.out.println("Communicator started");
			while (!socket.isClosed() && ((inputLine = in.readLine()) != null)) {
				inputTokens = inputLine.split(";");
				if (inputTokens[0].equals("rebind")) {
					rebind(inputTokens);
				} else if (inputTokens[0].equals("resolve")) {
					resolve(inputTokens);
				} else {
					error("unknown command: " + inputTokens[0]);
				}
			}
			in.close();
			out.close();
			socket.close();
		} catch (IOException e) {
//			e.printStackTrace();
		} finally {
			System.out.println("Communicator stopped");
		}
	}

	private void rebind(String[] tokens) {
		if (tokens.length != 5) {
			error("rebind: wrong number of params");
		} else {
			String name = tokens[1].trim();
			String type = tokens[2].trim();
			String host = tokens[3].trim();
			int port = -1;
			try {
				port = Integer.parseInt(tokens[4]);
			} catch (NumberFormatException e) {
			}
			if (name.isEmpty()) {
				error("rebind: name must not be empty");
			} else if (type.isEmpty()) {
				error("rebind: type must not be empty");
			} else if (host.isEmpty()) {
				error("rebind: host must not be empty");
			} else if (port < 0 || port > 65535) {
				error("rebind: port must be an integer and 0<=port<=65535");
			} else {
				NameserviceDb.rebind(new ObjectInfo(name, type, host, port));
				System.out.println("rebind: " + name + "," + type + "," + host
						+ "," + port);
				out.println("ok");
			}
		}
	}

	private void resolve(String[] tokens) {
		if (tokens.length != 2) {
			error("resolve: wrong number of params");
		} else {
			String name = tokens[1].trim();
			if (name.isEmpty()) {
				error("resolve: name must not be empty");
			} else {
				ObjectInfo obj = NameserviceDb.resolve(name);
				if (obj == null) {
					System.out.println("resolve: name not found: " + name);
					out.println("nameNotFound");
				} else {
					String type = obj.type();
					String host = obj.host();
					int port = obj.port();
					System.out.println("resolve: " + name + "," + type + ","
							+ host + "," + port);
					out.println("result," + name + "," + type + "," + host
							+ "," + port);
				}
			}
		}
	}

	private void error(String message) {
		System.out.println("ERROR: " + message);
		out.println("Exception," + message);
	}
}
