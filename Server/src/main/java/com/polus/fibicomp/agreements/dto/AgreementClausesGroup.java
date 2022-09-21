package com.polus.fibicomp.agreements.dto;

import java.util.HashMap;
import java.util.Map;

import com.polus.fibicomp.agreements.pojo.ClausesGroup;

public class AgreementClausesGroup {

	private Integer clausesGroupCode;

	private ClausesGroup clausesGroup;

	private Map<Integer, String> clauses;

	public AgreementClausesGroup() {
		clauses = new HashMap<>();
	}

	public Integer getClausesGroupCode() {
		return clausesGroupCode;
	}

	public void setClausesGroupCode(Integer clausesGroupCode) {
		this.clausesGroupCode = clausesGroupCode;
	}

	public ClausesGroup getClausesGroup() {
		return clausesGroup;
	}

	public void setClausesGroup(ClausesGroup clausesGroup) {
		this.clausesGroup = clausesGroup;
	}

	public Map<Integer, String> getClauses() {
		return clauses;
	}

	public void setClauses(Map<Integer, String> clauses) {
		this.clauses = clauses;
	}

}
