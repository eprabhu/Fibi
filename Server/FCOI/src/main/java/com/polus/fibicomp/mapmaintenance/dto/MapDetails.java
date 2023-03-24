package com.polus.fibicomp.mapmaintenance.dto;


import java.util.ArrayList;
import java.util.HashMap;

public class MapDetails {
	
	private ArrayList<HashMap<String, Object>> mapDetailList;
	private ArrayList<HashMap<String, Object>> mapList;
	private ArrayList<Integer> deletedMapDetailList;

	public ArrayList<HashMap<String, Object>> getMapDetailList() {
		return mapDetailList;
	}
	public void setMapDetailList(ArrayList<HashMap<String, Object>> mapDetailList) {
		this.mapDetailList = mapDetailList;
	}
	public ArrayList<HashMap<String, Object>> getMapList() {
		return mapList;
	}
	public void setMapList(ArrayList<HashMap<String, Object>> mapList) {
		this.mapList = mapList;
	}
	public ArrayList<Integer> getDeletedMapDetailList() {
		return deletedMapDetailList;
	}
	public void setDeletedMapDetailList(ArrayList<Integer> deletedMapDetailList) {
		this.deletedMapDetailList = deletedMapDetailList;
	}

}
