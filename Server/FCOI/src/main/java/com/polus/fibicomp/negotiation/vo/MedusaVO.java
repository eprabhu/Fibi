package com.polus.fibicomp.negotiation.vo;

import java.util.List;

import com.polus.fibicomp.negotiation.dto.Medusa;

public class MedusaVO {
	
	Integer indexId;
	String indexName;
	
	public List<Medusa> parentUnits;
	public List<Medusa> childUnits;
    public List<Medusa> medusaHierarchyList;
	
  	public String getIndexName() {
		return indexName;
	}

	public void setIndexName(String indexName) {
		this.indexName = indexName;
	}

	public List<Medusa> getChildUnits() {
		return childUnits;
	}

	public void setChildUnits(List<Medusa> childUnits) {
		this.childUnits = childUnits;
	}

	public List<Medusa> getParentUnits() {
		return parentUnits;
	}

	public void setParentUnits(List<Medusa> parentUnits) {
		this.parentUnits = parentUnits;
	}

	public Integer getIndexId() {
		return indexId;
	}

	public void setIndexId(Integer indexId) {
		this.indexId = indexId;
	}

	public List<Medusa> getMedusaHierarchyList() {
		return medusaHierarchyList;
	}

	public void setMedusaHierarchyList(List<Medusa> medusaHierarchyList) {
		this.medusaHierarchyList = medusaHierarchyList;
	}
	
}
