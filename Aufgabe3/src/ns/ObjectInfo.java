package ns;

final class ObjectInfo {

	private final String name;
	private final String type;
	private final String host;
	private final int port;

	ObjectInfo(String name, String type, String host, int port) {
		if (name == null || name.trim().isEmpty() || type == null
				|| type.trim().isEmpty() || host == null
				|| host.trim().isEmpty() || port < 0 || port > 65535)
			throw new IllegalArgumentException();
		this.name = name;
		this.type = type;
		this.host = host;
		this.port = port;
	}

	String name() {
		return name;
	}

	String type() {
		return type;
	}

	String host() {
		return host;
	}

	int port() {
		return port;
	}
}
