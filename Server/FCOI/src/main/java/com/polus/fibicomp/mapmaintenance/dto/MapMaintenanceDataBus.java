package com.polus.fibicomp.mapmaintenance.dto;

import java.util.ArrayList;
import java.util.HashMap;

public class MapMaintenanceDataBus {

	
	private Integer mapId;
	private ArrayList<HashMap<String, Object>> roleDescription;
	private MapDetails mapDetails = new MapDetails();
	private UnitList unitList = new UnitList();
	private Integer countOfMapUsed;
	private MapMaintenance mapMaintenance = new MapMaintenance();
	private Integer moduleCode;
	private Integer subModuleCode;
	
	public MapDetails getMapDetails() {
		return mapDetails;
	}

	public void setMapDetails(MapDetails mapDetails) {
		this.mapDetails = mapDetails;
	}


	public ArrayList<HashMap<String, Object>> getRoleDescription() {
		return roleDescription;
	}

	public void setRoleDescription(ArrayList<HashMap<String, Object>> roleDescription) {
		this.roleDescription = roleDescription;
	}
    
	public MapMaintenance getMapMaintenance() {
		return mapMaintenance;
	}

	public void setMapMaintenance(MapMaintenance mapMaintenance) {
		this.mapMaintenance = mapMaintenance;
	}

	public Integer getMapId() {
		return mapId;
	}

	public void setMapId(Integer mapId) {
		this.mapId = mapId;
	}

	public UnitList getUnitList() {
		return unitList;
	}

	public void setUnitList(UnitList unitList) {
		this.unitList = unitList;
	}

	public Integer getCountOfMapUsed() {
		return countOfMapUsed;
	}

	public void setCountOfMapUsed(Integer countOfMapUsed) {
		this.countOfMapUsed = countOfMapUsed;
	}

	public Integer getModuleCode() {
		return moduleCode;
	}

	public void setModuleCode(Integer moduleCode) {
		this.moduleCode = moduleCode;
	}

	public Integer getSubModuleCode() {
		return subModuleCode;
	}

	public void setSubModuleCode(Integer subModuleCode) {
		this.subModuleCode = subModuleCode;
	}
	
}
