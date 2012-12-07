/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package mware_lib;

import java.net.InetSocketAddress;



/**
 *
 * @author sebastian
 */
public class Utilities {

    static Object createProxy(String name, String type, String host, int port) {
		System.out.println("Creating proxy for "+name+" "+type+" "+host+" "+port);
		Object result = null;
		try {
			final Class<?>[] CONSTRUCTOR_SIGNATURE = {
					Class.forName("java.lang.String"),
					Class.forName("java.net.InetSocketAddress") };
			final Object[] CONSTRUCTOR_ARGS = { name,
					new InetSocketAddress(host, port) };
			if (type.equals("branch_access.Manager")) {
				result = Class.forName("branch_access.ManagerProxy")
						.getConstructor(CONSTRUCTOR_SIGNATURE)
						.newInstance(CONSTRUCTOR_ARGS);
			} else if (type.equals("cash_access.Account")) {
				result = Class.forName("cash_access.AccountProxy")
						.getConstructor(CONSTRUCTOR_SIGNATURE)
						.newInstance(CONSTRUCTOR_ARGS);
			}
		} catch (Exception e) {
			e.printStackTrace();
			throw new RuntimeException();
		}
		return result;
	}

	static Skeleton createSkeleton(String name, Object servant) {
		Object result = null;
		String type = getTypeForObject(servant);
		try {
			if (type.equals("branch_access.Manager")) {
				final Class<?>[] CONSTRUCTOR_SIGNATURE = {
						Class.forName("java.lang.String"),
						Class.forName("branch_access.Manager") };
				final Object[] CONSTRUCTOR_ARGS = { name,
						Class.forName("branch_access.Manager").cast(servant)};
				result = Class.forName("branch_access.ManagerSkeleton")
						.getConstructor(CONSTRUCTOR_SIGNATURE)
						.newInstance(CONSTRUCTOR_ARGS);
			} else if (type.equals("cash_access.Account")) {
				final Class<?>[] CONSTRUCTOR_SIGNATURE = {
						Class.forName("java.lang.String"),
						Class.forName("cash_access.Account") };
				final Object[] CONSTRUCTOR_ARGS = { name,
						Class.forName("cash_access.Account").cast(servant)};
				result = Class.forName("cash_access.AccountSkeleton")
						.getConstructor(CONSTRUCTOR_SIGNATURE)
						.newInstance(CONSTRUCTOR_ARGS);
			}
		} catch (Exception e) {
			 e.printStackTrace();
			throw new RuntimeException();
		}
		return (Skeleton)result;
	}

    
    public static String getTypeForObject(Object obj) {
		String type = "java.lang.Object";
		Class<?> clazz = obj.getClass();
		while (clazz.getSuperclass() != null) {
			if (clazz.toString().equals("class branch_access.Manager")) {
				type = "branch_access.Manager";
				break;
			} else if (clazz.toString().equals("class cash_access.Account")) {
				type = "cash_access.Account";
				break;
			}
			clazz = clazz.getSuperclass();
		}
		return type;
	}

    public static String join(String separator, String... args) {
       StringBuilder result = new StringBuilder();
       for(String s:args){
           result.append(separator);
           result.append(s);
       }
       return result.toString();
    }

    
}
