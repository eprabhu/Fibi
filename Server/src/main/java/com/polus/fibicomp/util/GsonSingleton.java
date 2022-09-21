package com.polus.fibicomp.util;

import com.google.gson.Gson;

public class GsonSingleton {

	private static Gson obj;

	// private constructor to force use of getInstance() to create Singleton object
	private GsonSingleton() {
	}

	public static Gson getInstance() {
		if (obj == null)
			obj = new Gson();
		return obj;
	}

}
