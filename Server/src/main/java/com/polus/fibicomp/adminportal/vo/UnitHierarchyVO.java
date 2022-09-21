package com.polus.fibicomp.adminportal.vo;

import java.util.List;
import com.polus.fibicomp.adminportal.dto.UnitHierarchy;
import com.polus.fibicomp.adminportal.pojo.UnitAdministratorType;

public class UnitHierarchyVO {
	
	private String unitNumber;
	private List<UnitHierarchy> unitHierarchyList;
	private List<UnitAdministratorType> unitAdministratorTypeList;

	public String getUnitNumber() {
		return unitNumber;
	}
	
	public void setUnitNumber(String unitNumber) {
		this.unitNumber = unitNumber;
	}
	
	public List<UnitHierarchy> getUnitHierarchyList() {
		return unitHierarchyList;
	}
	
	public void setUnitHierarchyList(List<UnitHierarchy> unitHierarchyList) {
		this.unitHierarchyList = unitHierarchyList;
	}
	
	public List<UnitAdministratorType> getUnitAdministratorTypeList() {
		return unitAdministratorTypeList;
	}
	
	public void setUnitAdministratorTypeList(List<UnitAdministratorType> unitAdministratorTypeList) {
		this.unitAdministratorTypeList = unitAdministratorTypeList;
	}
	
	}
