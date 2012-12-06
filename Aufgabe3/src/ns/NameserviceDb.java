package ns;

import java.util.HashMap;
import java.util.Map;

final class NameserviceDb {

	private static Map<String, ObjectInfo> bindings = new HashMap<String, ObjectInfo>();
	
	synchronized static void rebind(ObjectInfo obj) {
		System.out.println("Rebind");
		bindings.put(obj.name(), obj);
	}
	
	static ObjectInfo resolve(String name) {
		System.out.println("Resolve");
		return bindings.get(name);
	}
}
