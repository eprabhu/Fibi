package com.polus.fibicomp.codetable.dao;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.ClassPathResource;
import org.springframework.core.io.Resource;
import org.springframework.stereotype.Service;

import com.polus.fibicomp.common.dao.CommonDao;

@Service
public class JSONParser {

	protected static Logger logger = LogManager.getLogger(JSONParser.class.getName());

	@Autowired
	public CommonDao commonDao;

	public static Map<String, Object> getJSONData() {
		Map<String, Object> retMap = new HashMap<>();
		try {
			Resource resource = new ClassPathResource("codetable.json");
			InputStream in = resource.getInputStream();
			String codeTableJSON = readFile(in);
			JSONObject jsonObj = new JSONObject(codeTableJSON);
			if (jsonObj != JSONObject.NULL) {
				retMap = toMap(jsonObj);
			}
		} catch (Exception e) {
			return retMap;
		}
		return retMap;
	}

	public static Map<String, Object> toMap(JSONObject object) throws JSONException {
		Map<String, Object> map = new HashMap<>();
		@SuppressWarnings("unchecked")
		Iterator<String> keysItr = object.keys();
		while (keysItr.hasNext()) {
			String key = keysItr.next();
			Object value = object.get(key);
			if (value instanceof JSONArray) {
				value = toList((JSONArray) value);
			} else if (value instanceof JSONObject) {
				value = toMap((JSONObject) value);
			}
			map.put(key, value);
		}
		return map;
	}

	public static List<Object> toList(JSONArray array) throws JSONException {
		List<Object> list = new ArrayList<>();
		for (int i = 0; i < array.length(); i++) {
			Object value = array.get(i);
			if (value instanceof JSONArray) {
				value = toList((JSONArray) value);
			} else if (value instanceof JSONObject) {
				value = toMap((JSONObject) value);
			}
			list.add(value);
		}
		return list;
	}

	public static String readFile(InputStream input) {
		BufferedReader buffer = new BufferedReader(new InputStreamReader(input));
		return buffer.lines().collect(Collectors.joining("\n"));
	}

	public Map<String, Object> getJSONDataForReport(String jsonName) {
		Map<String, Object> retMap = new HashMap<>();
		try {
			JSONObject jsonObj = getJSONDataByFileName(jsonName);
			if (jsonObj != JSONObject.NULL) {
				retMap = toMap(jsonObj);
			}
		} catch (Exception e) {
			return retMap;
		}
		return retMap;
	}

	public JSONObject getJSONDataByFileName(String jsonName) {
		try {
			Resource resource = null;
			resource = new ClassPathResource(jsonName);
			InputStream in = resource.getInputStream();
			String reportJSON = readFile(in);
			return new JSONObject(reportJSON);
		} catch (Exception e) {
			logger.error("error occured in getJSONDataByFileName : {}", e.getMessage());
			return new JSONObject();
		}
	}

}
