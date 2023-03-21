package com.polus.fibicomp.grantcall.vo;

import java.util.ArrayList;
import java.util.List;

import com.polus.fibicomp.grantcall.pojo.GrantCallKPI;
import com.polus.fibicomp.grantcall.pojo.GrantCallKPICriteria;
import com.polus.fibicomp.grantcall.pojo.KPIType;

public class GrantCallKPIVO {

	private Integer grantCallId;

	private String message;

	private String updateUser;

	private List<KPIType> kpiTypes;

	private GrantCallKPI grantCallKpi;
	
	private List<GrantCallKPI> grantCallKpis;

	private Integer grantCallKpiId;

	private Integer grantCallKpiCriteriaId;

	private List<GrantCallKPICriteria> grantCallKpiCriterias;

	public GrantCallKPIVO() {
		grantCallKpi = new GrantCallKPI();
		grantCallKpis = new ArrayList<GrantCallKPI>();
		grantCallKpiCriterias = new ArrayList<GrantCallKPICriteria>();
	}

	public Integer getGrantCallId() {
		return grantCallId;
	}

	public void setGrantCallId(Integer grantCallId) {
		this.grantCallId = grantCallId;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public List<KPIType> getKpiTypes() {
		return kpiTypes;
	}

	public void setKpiTypes(List<KPIType> kpiTypes) {
		this.kpiTypes = kpiTypes;
	}

	public GrantCallKPI getGrantCallKpi() {
		return grantCallKpi;
	}

	public void setGrantCallKpi(GrantCallKPI grantCallKpi) {
		this.grantCallKpi = grantCallKpi;
	}

	public List<GrantCallKPI> getGrantCallKpis() {
		return grantCallKpis;
	}

	public void setGrantCallKpis(List<GrantCallKPI> grantCallKpis) {
		this.grantCallKpis = grantCallKpis;
	}

	public Integer getGrantCallKpiId() {
		return grantCallKpiId;
	}

	public void setGrantCallKpiId(Integer grantCallKpiId) {
		this.grantCallKpiId = grantCallKpiId;
	}

	public Integer getGrantCallKpiCriteriaId() {
		return grantCallKpiCriteriaId;
	}

	public void setGrantCallKpiCriteriaId(Integer grantCallKpiCriteriaId) {
		this.grantCallKpiCriteriaId = grantCallKpiCriteriaId;
	}

	public List<GrantCallKPICriteria> getGrantCallKpiCriterias() {
		return grantCallKpiCriterias;
	}

	public void setGrantCallKpiCriterias(List<GrantCallKPICriteria> grantCallKpiCriterias) {
		this.grantCallKpiCriterias = grantCallKpiCriterias;
	}

	

}
