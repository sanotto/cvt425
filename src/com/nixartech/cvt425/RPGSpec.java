package com.nixartech.cvt425;

import java.lang.reflect.Constructor;

public abstract class RPGSpec {

	public static RPGSpec getInstanceFor(String spec) {
		try {
			String className="com.nixartech.cvt425.RPGSpecType"+spec.toUpperCase();
			Class<?> clazz = Class.forName(className);
			Constructor<?> constructor = clazz.getConstructor();
			Object instance = constructor.newInstance();

			return (RPGSpec) instance;
		} catch (Exception e) {
			return null;
		}
	}

	public abstract RPGConvertionResult parse(String input, int indent);

}
