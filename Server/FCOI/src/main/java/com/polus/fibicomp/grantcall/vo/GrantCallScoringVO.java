package com.polus.fibicomp.grantcall.vo;

import java.util.ArrayList;
import java.util.List;

import com.polus.fibicomp.grantcall.pojo.GrantCallScoringCriteria;
import com.polus.fibicomp.grantcall.pojo.ScoringCriteria;

public class GrantCallScoringVO {

	private Integer grantCallId;

	private String message;

	private String updateUser;

	private Integer grantScoringCriteriaId;
	
	private List<ScoringCriteria> scoringCriteria;

	private GrantCallScoringCriteria grantCallScoringCriteria;

	private List<GrantCallScoringCriteria> grantCallScoringCriterias;	

	public GrantCallScoringVO() {
		grantCallScoringCriteria = new GrantCallScoringCriteria();
		grantCallScoringCriterias = new ArrayList<GrantCallScoringCriteria>();
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

	public Integer getGrantScoringCriteriaId() {
		return grantScoringCriteriaId;
	}

	public void setGrantScoringCriteriaId(Integer grantScoringCriteriaId) {
		this.grantScoringCriteriaId = grantScoringCriteriaId;
	}

	public List<ScoringCriteria> getScoringCriteria() {
		return scoringCriteria;
	}

	public void setScoringCriteria(List<ScoringCriteria> scoringCriteria) {
		this.scoringCriteria = scoringCriteria;
	}

	public GrantCallScoringCriteria getGrantCallScoringCriteria() {
		return grantCallScoringCriteria;
	}

	public void setGrantCallScoringCriteria(GrantCallScoringCriteria grantCallScoringCriteria) {
		this.grantCallScoringCriteria = grantCallScoringCriteria;
	}

	public List<GrantCallScoringCriteria> getGrantCallScoringCriterias() {
		return grantCallScoringCriterias;
	}

	public void setGrantCallScoringCriterias(List<GrantCallScoringCriteria> grantCallScoringCriterias) {
		this.grantCallScoringCriterias = grantCallScoringCriterias;
	}

}
