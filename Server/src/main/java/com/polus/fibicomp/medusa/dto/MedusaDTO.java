package com.polus.fibicomp.medusa.dto;

import java.util.ArrayList;
import java.util.List;

public class MedusaDTO {

	private String moduleName;

	private String projectNumber;

	private List<MedusaDTO> medusa;

	private int moduleCode;

	public int getModuleCode() {
		return moduleCode;
	}

	public void setModuleCode(int moduleCode) {
		this.moduleCode = moduleCode;
	}

	public MedusaDTO() {
		medusa = new ArrayList<>();
	}

	public String getModuleName() {
		return moduleName;
	}

	public void setModuleName(String moduleName) {
		this.moduleName = moduleName;
	}

	public String getProjectNumber() {
		return projectNumber;
	}

	public void setProjectNumber(String projectNumber) {
		this.projectNumber = projectNumber;
	}

	public List<MedusaDTO> getMedusa() {
		return medusa;
	}

	public void setMedusa(List<MedusaDTO> medusa) {
		this.medusa = medusa;
	}

}
