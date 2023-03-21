package com.polus.fibicomp.award.vo;

import java.util.HashMap;
import java.util.List;

import com.polus.fibicomp.award.dto.AwardHierarchyDto;
public class AwardHierarchyVO {

	private AwardHierarchyDto awardHierarchy;
	private List<HashMap<String, Object>> hierarchyBudgetVersions;

	public List<HashMap<String, Object>> getHierarchyBudgetVersions() {
		return hierarchyBudgetVersions;
	}

	public void setHierarchyBudgetVersions(List<HashMap<String, Object>> hierarchyBudgetVersions) {
		this.hierarchyBudgetVersions = hierarchyBudgetVersions;
	}

	public AwardHierarchyDto getAwardHierarchy() {
		return awardHierarchy;
	}

	public void setAwardHierarchy(AwardHierarchyDto awardHierarchy) {
		this.awardHierarchy = awardHierarchy;
	}
	
}
