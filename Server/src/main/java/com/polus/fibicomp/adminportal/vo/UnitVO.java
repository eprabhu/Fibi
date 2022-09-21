package com.polus.fibicomp.adminportal.vo;

import java.util.List;
import com.polus.fibicomp.pojo.Unit;
import com.polus.fibicomp.pojo.UnitAdministrator;

public class UnitVO {

	private Unit unit;
	private List<UnitAdministrator> unitAdministrators;
	private String acType;
	private Boolean parentUnitChanged;

	public Unit getUnit() {
		return unit;
	}

	public void setUnit(Unit unit) {
		this.unit = unit;
	}

	public List<UnitAdministrator> getUnitAdministrators() {
		return unitAdministrators;
	}

	public void setUnitAdministrators(List<UnitAdministrator> unitAdministrators) {
		this.unitAdministrators = unitAdministrators;
	}

	public String getAcType() {
		return acType;
	}

	public void setAcType(String acType) {
		this.acType = acType;
	}

	public Boolean getParentUnitChanged() {
		return parentUnitChanged;
	}

	public void setParentUnitChanged(Boolean parentUnitChanged) {
		this.parentUnitChanged = parentUnitChanged;
	}

}
