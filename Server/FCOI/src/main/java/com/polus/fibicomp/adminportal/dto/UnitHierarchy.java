/**
 * @author NikhilPrabha & AjithPeter 
 */

package com.polus.fibicomp.adminportal.dto;

import java.util.ArrayList;

public class UnitHierarchy {
	
	public String unitNumber;
	public String unitName;
	public String parentUnitNumber;
	public ArrayList<UnitHierarchy> childUnits;
	
	public String getUnitNumber() {
		return unitNumber;
	}
	public void setUnitNumber(String unitNumber) {
		this.unitNumber = unitNumber;
	}
	public String getUnitName() {
		return unitName;
	}
	public void setUnitName(String unitName) {
		this.unitName = unitName;
	}
	public String getParentUnitNumber() {
		return parentUnitNumber;
	}
	public void setParentUnitNumber(String parentUnitNumber) {
		this.parentUnitNumber = parentUnitNumber;
	}
	public ArrayList<UnitHierarchy> getChildUnits() {
		return childUnits;
	}
	public void setChildUnits(ArrayList<UnitHierarchy> childUnits) {
		this.childUnits = childUnits;
	}
	
}
